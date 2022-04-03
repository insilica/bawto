library(RPostgreSQL)
library(dplyr)
library(foreach)
library(RSysrev)
library(dmetar)
library(meta)
library(ggplot2)
library(XML)
library(methods)
library(DiagrammeR)
library(DiagrammeRsvg)
library(magrittr)
library(jsonlite)

# BASE TABLES 
# Need to set your .token first see github.com/sysrev/RSysrev 
grouplabels <- RSysrev::sysrev.getGroupLabelAnswers(70431)

## ---- Articles ------------- 
# Articles TODO, these should be retreivable by api, why isn't that working?
articles <- read.csv("../shared_data/extraction_articles.csv",stringsAsFactors = F) %>% 
  tidyr::separate_rows(Authors,sep=";") %>% 
  mutate(Authors = trimws(Authors)) %>% 
  group_by(across(-Authors)) %>% summarize(last_author = last(Authors)) %>% ungroup() %>%
  (function(articlesDF){
    rootXML = xmlRoot(xmlParse(file="../shared_data/extraction_articles_endnote.xml"))
    xmlDF = xmlToDataFrame(rootXML[[1]]) %>% 
      select(Article.ID = `rec-number`,year=dates,title=titles,abstract) %>% 
      mutate(Article.ID = as.numeric(Article.ID))
    articlesDF %>% left_join(xmlDF,by="Article.ID")
  }) %>% 
  mutate(study = paste(last_author,year,sep = " ")) %>% 
  select(Article.ID,Title,Journal,Abstract,year,study) %>% distinct()

## ---- Reasons for exclusion articles ---------------
exclusion_articles <- read.csv("../shared_data/reasons_for_exclusion_articles.csv",header=T,sep=",",stringsAsFactors = F) %>% 
  select(Article.ID,Title) %>% distinct() %>% 
  inner_join(articles %>% select(Extraction.Article.ID=Article.ID,Title),by="Title") %>% 
  select(Article.ID,Extraction.Article.ID)

## ---- baseDF --------------
# Contains user answers that aren't part of a group label (which clinical NCTtrial, authors, journals)
baseDF <- RSysrev::sysrev.getLabelAnswers(70431) %>% 
  select(Article.ID=article.id,User.ID=reviewer.id,lbl.name,answer) %>% 
  filter(lbl.name %in% c("Include","NCT Clinical Trial Identifier","NOT-NCT Trial Identifier")) %>% 
  group_by(Article.ID,User.ID,lbl.name) %>% summarize(answer=paste(answer,collapse=";")) %>% 
  reshape2::dcast(Article.ID + User.ID ~ lbl.name, value.var = "answer") %>%
  select(Article.ID,User.ID,Include,NCTtrial=`NCT Clinical Trial Identifier`,otherTrial=`NOT-NCT Trial Identifier`) %>% 
  filter(Include == 'true')

## ---- POP --------------
# Population table describes mutations of population, demographics, biomarkers, etc.
pop1 <-   grouplabels$Populations %>% 
  select(Article.ID,User.ID,Pop_ID,Pop_Variable,Biomarker=Pop_Value,Mutation.Type=`Mutation Type`,Description=description) %>% 
  rowwise() %>% 
  mutate_at(vars(Pop_Variable,Biomarker,Description),~ifelse(trimws(.)=="",NA,trimws(.))) %>% 
  mutate(Biomarker = gsub(", ",",",Biomarker)) %>% 
  ungroup() %>% 
  group_by(Article.ID,User.ID,Pop_ID) %>% mutate(id = uuid::UUIDgenerate()) %>% ungroup()

biomarker_types <- c("germline gene set","germline mutation hgnc","germline mutation hgnc;somatic mutation hgnc",
                     "somatic gene set","somatic gene set;germline gene set","somatic mutation hgnc",
                     "somatic mutation hgnc;germline mutation hgnc")
pop2 <- pop1 %>% 
  filter(Pop_Variable %in% biomarker_types) %>% 
  group_by(id,Article.ID,User.ID,Pop_ID) %>% 
  summarize(
    Biomarker = paste(Biomarker,collapse=","),
    Biomarker_Detail = toJSON(tibble(
      Biomarker = Biomarker,
      Biomarker.Type = Pop_Variable,
      Mutation.Type = Mutation.Type,
      Description = Description
    ), auto_unbox = TRUE)) %>% 
  ungroup() %>% 
  inner_join(baseDF,by=c("Article.ID","User.ID")) %>% filter(Include == "true") %>% 
  select(id,Article.ID,User.ID,Pop_ID,Biomarker,Biomarker_Detail)

pop <- pop1 %>% select(id,Article.ID,User.ID,Pop_ID) %>% distinct() %>% 
  mutate(Biomarker="NONE",Biomarker_Detail=NA) %>% 
  select(id,Article.ID,User.ID,Pop_ID,Biomarker,Biomarker_Detail) %>% 
  filter(!(id %in% pop2$id)) %>% 
  rbind(pop2)

# There should be exactly one row for each article + user + pop_id
stopifnot(pop %>% group_by(Article.ID,User.ID,Pop_ID) %>% count() %>% (function(df){max(df$n)}) == 1)

## ---- Intervention --------------
# Intervention table contains treatment descriptions
norm_therapy <- read.csv("../shared_data/treatment_normalization.csv",header = T,sep = "\t",stringsAsFactors = F) %>% select(Name, Normal_Name)
normalize_therapy <- function(name){ 
  data_frame(Name=name) %>% left_join(norm_therapy,by="Name") %>% 
    mutate(Normal_Name = ifelse(is.na(Normal_Name),Name,Normal_Name)) %>% 
    (function(df){df$Normal_Name})
}

intervention <- grouplabels$Interventions %>% select(-User.Name) %>% 
  mutate(Int_Variable=trimws(Int_Variable),Int_Variable_Unit = trimws(Int_Variable_Unit)) %>% 
  rowwise() %>% 
  mutate(SubInt_ID = ifelse(nchar(SubInt_ID) > 0,SubInt_ID,NA)) %>% 
  mutate(Int_Variable_Unit = ifelse(nchar(Int_Variable_Unit) > 0,Int_Variable_Unit,NA)) %>% 
  mutate(Int_Value = ifelse(is.na(Int_Variable_Unit),Int_Value,paste(Int_Value,Int_Variable_Unit))) %>% 
  ungroup() %>% 
  group_by(Article.ID,User.ID,Int_ID,SubInt_ID,Int_Variable) %>% filter(n() == 1) %>% ungroup() # TODO dangerous error fix

intervention <- intervention %>% 
  reshape2::dcast(Article.ID + User.ID + Int_ID + SubInt_ID ~ Int_Variable,value.var="Int_Value") %>% 
  group_by(Article.ID,User.ID,Int_ID) %>% 
  summarize(
    therapies = paste(sort(unique(normalize_therapy(treatment))),collapse=","),
    therapy_detail = toJSON(tibble(
      SubInt_ID = SubInt_ID,
      treatment = normalize_therapy(treatment),
      raw_treatment = treatment,
      dose      = `dose value`,
      duration  = `duration value`,
      continuous_cycles = `Continuous Cycles?`,
      crossover_allowed = `crossover allowed?`,
      dose_escalation = `Dosage Escalation?`,
      recommended_dose = `Dosage Escalation?`,
      cessation = `treatment cessation`
    ), auto_unbox = TRUE)) %>% 
  ungroup()

# There should be exactly one row for each article + user + pop_id
stopifnot(intervention %>% group_by(Article.ID,User.ID,Int_ID) %>% count() %>% (function(df){max(df$n)}) == 1)

## ---- Disease --------------
# TODO we need to review each trial again to confirm disease type in view of the updated normalization
disease <- grouplabels$Disease %>% select(Article.ID,User.ID,disease=`Prostate Cancer`) %>% 
  inner_join(baseDF,by=c("Article.ID","User.ID")) %>% 
  mutate(disease = toupper(trimws(disease))) %>% 
  filter(Include == "true") %>% (function(df){
    # Simple normalization 
    # [localized, nonmetastatic, metastatic hormon sensitive, 
    # metastatic castration resistant, non-metastatic castration resistant] See
    # https://www.cancernetwork.com/view/emerging-categories-disease-advanced-prostate-cancer-and-their-therapeutic-implications
    normal <- read.csv("../shared_data/disease_normal.csv",stringsAsFactors = F,header=T, sep="\t") %>% 
      select(disease=Name,Normal)
    df %>% inner_join(normal) %>% select(Article.ID,User.ID,disease=Normal)
  })

## ---- Adverse Outcomes --------------
# Adverse outcome table (empty for now)
ae  <- grouplabels$`Adverse Events` %>% 
  inner_join(baseDF,by=c("Article.ID","User.ID")) %>% 
  filter(Include == "true") %>% 
  mutate(Adverse_Event = trimws(Adverse_Event)) %>% 
  mutate(Grade = gsub(" ","",Grade)) %>% 
  mutate(AE_Value = as.numeric(AE_Value)) %>% 
  mutate(experiment_num_patients = as.numeric(experiment_num_patients)) %>% 
  mutate(Pop_ID = gsub(" ","",Pop_ID)) %>% 
  mutate(Int_ID = gsub(" ","",Int_ID)) %>% 
  mutate(AE_ID = uuid::UUIDgenerate()) %>% 
  select(Article.ID,User.ID,AE_ID,NCTtrial,Pop_ID,Int_ID,experiment_num_patients,Adverse_Event,Grade,AE_Value)

## ---- Utility Functions --------------
fix_int <- function(id){ paste(sort(strsplit(gsub(" ","",id),",")[[1]]),collapse=";") }

## ---- Single outcomes --------------
# Single outcome table (no ratios, eg. pfs in months). It can be joined with population and intervention tables
# TODO some of the outcome values here aren't numeric
single_outcomes <- grouplabels$`Single_Pop_Outcomes` %>% 
  mutate(Int_ID = gsub(" ","",Int_ID),Pop_ID=gsub(" ","",Pop_ID)) %>% 
  (function(df){
    groupcols <- colnames(df)[which(colnames(df) != "Int_ID")]
    df %>% tidyr::separate_rows(Int_ID,sep=",") %>% 
      inner_join(intervention,by=c("Article.ID","User.ID","Int_ID")) %>% 
      group_by_at(groupcols) %>% 
      summarize(Int_ID         = paste(Int_ID,collapse=","),
                therapies      = paste(therapies,collapse=" or "),
                therapy_detail = paste(therapy_detail,collapse=" or ")) %>% 
      ungroup()
  }) %>% 
  (function(df){
    groupcols <- colnames(df)[which(colnames(df) != "Pop_ID")]
    df %>% tidyr::separate_rows(Pop_ID,sep=",") %>% 
      inner_join(pop,by=c("Article.ID","User.ID","Pop_ID")) %>% 
      group_by_at(groupcols) %>% summarize(Pop_ID          = paste(Pop_ID,collapse=","),
                                           Biomarker       = paste(Biomarker,collapse=","),
                                           Biomarker_Detail= paste(Biomarker_Detail,collapse=",")) %>% ungroup()
  }) %>% 
  select(Article.ID,User.ID,Int_ID,Pop_ID,therapies,therapy_detail,Biomarker,Biomarker_Detail,
         Experiment_num_patients,OC_Variable,OC_Units,OC_Value,OC_CI,OC_Range,
         OC_Pvalue,Model_type) %>% distinct() %>% 
  mutate_if(is.character,trimws) %>% 
  mutate_if(is.character,~ ifelse(.=="",NA,.)) %>% 
  mutate(OC_Variable = case_when(
    OC_Variable == "median duration PSA Progression Free Survival (PSA PFS)" ~ "PSA PFS",
    OC_Variable == "PSA Response Rate" ~ "PSA RR",
    OC_Variable == "median  duration overall survivability (OS)" ~ "OS",
    OC_Variable == "median duration aggregate PFS (aPFS)" ~ "aPFS",
    OC_Variable == "median duration radiographic Progression Free Survival (rPFS)" ~ "rPFS",
    T ~ OC_Variable
  ))

# TODO we need some new tests
# stopifnot(nrow(grouplabels$`Single_Pop_Outcomes` %>% inner_join(baseDF)) == nrow(single_oc))

## ---- Relative outcomes --------------
# Relative outcome table contains hazard ratios mostly.  It can be joined with population and intervention tables
# TODO need to redo this 
rel_oc       <- grouplabels$Relative_Pop_Outcomes %>%
  mutate(OC_Variable = trimws(OC_Variable)) %>%
  mutate(OC_Value = as.numeric(OC_Value)) %>%
  mutate(OC_CI = as.numeric(OC_CI)) %>%
  filter(Control_Int_ID != "unknown",Control_Pop_ID!="unknown") %>%
  rowwise() %>%
  mutate(CI_MIN = suppressWarnings(as.numeric(strsplit(OC_Range,"-")[[1]][1])),
         CI_MAX = suppressWarnings(as.numeric(strsplit(OC_Range,"-")[[1]][2]))) %>%
  mutate(Int_ID = fix_int(Int_ID)) %>%
  mutate(Pop_ID = fix_int(Pop_ID))
#   mutate(Control_Int_ID = fix_int(Control_Int_ID)) %>% 
#   mutate(Control_Pop_ID = fix_int(Control_Pop_ID)) %>% 
#   mutate(ROC_ID = uuid::UUIDgenerate()) %>% # outcome id is useful for joining on later
#   ungroup() %>% 
#   mutate(OC_Units = NA) %>%
#   mutate(OC_Variable = case_when(
#     OC_Variable == "aggregate Progression Free Survival (aPFS) Hazard Ratio" ~ "aPFS",
#     OC_Variable == "Overall Survivability (OS) Hazard ratio" ~ "OS",
#     OC_Variable == "PSA Progression Free Survival (PSA PFS) Hazard Ratio" ~ "PSA PFS",
#     OC_Variable == "radiographic Progression Free Survival (rPFS) Hazard Ratio" ~ "rPFS")) %>% 
#   select(Article.ID,User.ID,Pop_ID,Int_ID,
#          Experiment_Num_Patients,
#          OC_Variable,OC_Units,OC_Value,OC_CI,OC_Range,OC_Pvalue,
#          CI_MIN,CI_MAX,
#          `Model Type`,Control_Num_Patients,Control_Pop_ID,Control_Int_ID,
#          OC_ID=ROC_ID)
# 
# stopifnot(nrow(grouplabels$Relative_Pop_Outcomes %>% 
#                  inner_join(baseDF) %>% 
#                  filter(Control_Int_ID != "unknown",Control_Pop_ID!="unknown")) 
#           == 
#             nrow(rel_oc))

## ---- TESTS ------------
library(assertthat)
setequal(pop$Article.ID %>% unique(),baseDF$Article.ID)



