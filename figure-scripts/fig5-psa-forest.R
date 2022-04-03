library(tidyverse)
library(magrittr)
load("cache.image")

HRD.baseDF[which(HRD.baseDF$short_name=="PROfound-1"),]

spop_ratio %>% inner_join(biomarker.names,by="biomarker") %>% filter(DRD=="DRD")

spop_plot_raw <- spop_ratio %>% inner_join(biomarker.names,by="biomarker") %>% 
  filter(DRD=="DRD") %>% filter(duration_months > 20) %>% 
  rowwise() %>% mutate(biomarker.group = biomarker.type.map(biomarker)) %>% ungroup() %>% 
  rowwise() %>% mutate(biomarker       = biomarker.name.map(biomarker)) %>% ungroup() %>% 
  mutate(biomarker_modifier = ifelse(biomarker_modifier %in% c("mutated","altered","qualifying or suspected deleterious alteration"),"", biomarker_modifier)) %>% 
  mutate(trial_treatment = sprintf("%s %s",short_name,treatment)) %>% 
  mutate(color = ifelse(biomarker_positive == "positive","chartreuse1","firebrick4"))

plotDF <- spop_plot_raw %>% 
  group_by(biomarker) %>% mutate(minrate = max(rate)) %>% ungroup() %>% 
  select(pop_modifier,trial,short_name,biomarker,biomarker_modifier,treatment,num_response,num_patients,biomarker_positive,
         color,minrate,rate,outcome_type) %>% distinct() %>% 
  arrange(trial,short_name,-minrate,treatment,biomarker_positive!="positive",-rate) %>% mutate(row = row_number()) %>% 
  filter(treatment != "AAP or AAP + veliparib") %>% 
  mutate(treatment = ifelse(treatment == "AAP or Enz;crossover to olaparib permitted","AAP or Enz",treatment)) %>% 
  mutate(treatment = ifelse(treatment == "enzalutamide + pembrolizumab","Enz + Pem",treatment))
  

psaDF <- plotDF %>% select(short_name,biomarker,biomarker_modifier,biomarker_positive,treatment,num_patients,num_response,outcome_type,row,color) %>% filter(outcome_type=="psa response")
radDF <- plotDF %>% select(short_name,biomarker,biomarker_modifier,biomarker_positive,treatment,num_patients,num_response,outcome_type,color,row) %>% 
  filter(outcome_type %in% c("radiographic response","RECIST/PCWG3 CR+PR", "RECIST_CR;RECIST_PR")) 

totDF <- psaDF %>% 
  full_join(radDF,by=c("short_name","biomarker","biomarker_modifier","biomarker_positive","treatment","color")) %>% 
  mutate_at(vars(num_response.y,num_response.x,num_patients.y,num_patients.x),  .funs= ~ ifelse(is.na(.),100,.)) %>% 
  mutate(lcolor = ifelse(num_response.x==100,"blue",color)) %>% 
  mutate(rcolor = ifelse(num_response.y==100,"blue",color)) %>% select(-color) %>% 
  mutate(row = ifelse(is.na(row.x),row.y,row.x))

makefig <- function(pdf,titleval,ratetitle,patienttitle){
  
  pdf$rate = pdf$num_response / pdf$num_patients
  
  m.gen <- meta::metaprop(
    event    = num_response,
    n        = num_patients,
    studlab  = biomarker,
    data     = pdf,
    title    = titleval)
    
    meta::forest(m.gen,
               sortvar = pdf$row,
               subgroup = T,
               print.subgroup.labels = T,
               xlim=c(0.0,1.0),
               col.study = pdf$color,
               col.square = pdf$color,
               col.square.lines = "black",
               col.label.left = "red",
               
               # studlab = T,
               comb.fixed = F,
               comb.random = F,
               leftlabs  = c("short_name","biomarker","biomarker_modifier","treatment"),
               leftcols  = c("short_name","biomarker","biomarker_modifier","treatment"),
               rightcols = c("rate","num_patients"),
               rightlabs = c(ratetitle,patienttitle),
               
               smlab=sprintf(titleval)
  )
}

svg("/home/thomas/tmp/psa_response.svg",width = 18, height=36,pointsize=12,onefile = T)
makefig(totDF %>% rename(num_response = num_response.x, num_patients=num_patients.x, color=lcolor),"PSA Response","rate","patients")
dev.off()
svg("/home/thomas/tmp/rad_response.svg",width = 18, height=36,pointsize=12,onefile = T)
makefig(totDF %>% rename(num_response = num_response.y, num_patients=num_patients.y, color=rcolor),"RAD Response","rate","patients")
dev.off()