# This code can only be run by loading the cache first. It is mostly useful as reference
library(tidyverse)
library(googlesheets4)
library(patchwork)

cache = new.env()
load("cache.image",envir=cache)


bawto2hrd <- range_read(extractions_sheet,sheet = "bawto_to_hrd",col_types = 'c') %>% 
  select(bawto.article.id,Article.ID=hrd.2.article.id) %>% 
  mutate_at(vars(bawto.article.id,Article.ID),as.numeric)


# RSysrev has been replaced by rsr, see r.sysrev.com
# Unfortunately this code was written with the older version and this variable must be loaded from cache
# bawtoGL  <- RSysrev::sysrev.getGroupLabelAnswers(70431) %>% lapply(function(df){
#   df %>% rename(bawto.article.id = Article.ID) %>% left_join(bawto2hrd)
# })
bawtoGL  <- cache$bawtoGL

AE.names <- range_read(extractions_sheet,sheet = "short-ae-name",col_types = 'c')
bawtoAE.names <- range_read(extractions_sheet,sheet = "bawto-pop-int-names",col_types = 'c') %>% 
  mutate(Article.ID = as.numeric(Article.ID)) %>% 
  filter(include != "F")

bawtoAE  <- bawtoGL$`Adverse Events` %>% 
  inner_join(HRD.baseDF %>% select(Article.ID,short_name),by="Article.ID") %>%
  filter(User.Name %in% c("tbozada1","tom")) %>% 
  inner_join(bawtoAE.names,by=c("Article.ID","Pop_ID","Int_ID","short_name")) %>% 
  mutate_at(vars(AE_Value,experiment_num_patients,order),as.numeric) %>% 
  mutate_at(vars(Adverse_Event,Grade),trimws) %>% 
  mutate(rate = AE_Value / experiment_num_patients)

AE.countDF <- bawtoAE %>% 
  filter(!is.na(Grade),Grade != "",Adverse_Event != "maximum grad for patient") %>% 
  select(rg_name,order,treatment_class,experiment_num_patients,Adverse_Event,Grade,AE_Value) %>% 
  reshape2::dcast(rg_name + order + treatment_class + experiment_num_patients + Adverse_Event ~ Grade,value.var="AE_Value",
        fun.aggregate=first,fill=NA_real_) %>% 
  rowwise() %>% 
  mutate(serious = sum(`3`,`4`,`3; 4`,na.rm = T)) %>% 
  mutate(serious = max(serious,`3; 4; Severe AE (SAE)`,`Severe AE (SAE)`,na.rm=T)) %>% 
  mutate(non_serious = sum(`1`,`2`,na.rm=T)) %>% 
  mutate(non_serious = max(non_serious,`1; 2`,na.rm=T)) %>% 
  mutate(any = `any grade`) %>% 
  mutate(any = ifelse(is.na(any),serious+non_serious,any)) %>% 
  ungroup() %>% 
  mutate(`grade 1/2` = any-serious) %>% 
  mutate(`grade 3+` = serious) %>% 
  select(rg_name,order,treatment_class,patients=experiment_num_patients,event=Adverse_Event,`grade 1/2`, `grade 3+`) %>% 
  reshape2::melt(id.vars = c("rg_name","order","treatment_class","patients","event"),
       variable.name="grade",value.name = "affected") %>% 
  mutate(rate = affected / patients) %>% 
  filter(patients > 20,event != "Any AE") %>% 
  mutate(event = ifelse(event == "asthenia","asthenia/fatigue",event)) %>% 
  mutate(event = ifelse(event == "fatigue","asthenia/fatigue",event)) %>% 
  mutate(event = ifelse(event == "pneumonitis","pneumonia",event)) %>% 
  mutate(event = ifelse(event == "thrombocytopenia","platelet count decreased",event))

rg_name_treatments = AE.countDF %>% 
  select(rg_name,treatment_class) %>% 
  distinct() %>% mutate(has_treatment=T) %>% 
  complete(rg_name,treatment_class,fill=list(has_treatment=F))

ev.studies <- AE.countDF %>% group_by(event) %>% summarize(ev.studies = n_distinct(rg_name)) %>% ungroup()
anyDF      <- AE.countDF %>% 
  complete(nesting(rg_name,order),treatment_class,grade,event) %>% 
  group_by(rg_name) %>% mutate(tot = sum(rate,na.rm = T)) %>% ungroup() %>% 
  group_by(event) %>% mutate(
    tot_rate = sum(rate,na.rm=T),
    event_tot = sum(rate>0,na.rm=T),
    med_rate = median(rate,na.rm=T),
    mean_rate = mean(rate,na.rm=T)
    ) %>% 
  ungroup() %>% 
  inner_join(rg_name_treatments,by=c("rg_name","treatment_class")) %>% 
  inner_join(ev.studies,by="event") %>% 
  # filter(event_tot > 5,treatment_class != "immunotherapy") %>% 
  filter(ev.studies > 3,treatment_class != "immunotherapy") %>% 
  mutate(rate_bucket = case_when(
    !has_treatment ~ "no treatment",
    rate > 0.2 ~ "20%+",
    rate > 0.1 ~ "10%-20%",
    rate > 0   ~ "0%-10%",
    rate == 0  ~ "0%",
    is.na(rate) ~ "no record")) %>% 
  mutate(rate_bucket = factor(rate_bucket,levels=c("0%","0%-10%","10%-20%","20%+","no record","no treatment")))
  
axis.text.size = unit(11,"pt") 
strip.text.size = unit(14,"pt")

colfunc <- colorRampPalette(c("green", "red"))
B <- ggplot(anyDF,aes(y=reorder(rg_name,-order),x=reorder(event,-mean_rate),fill=rate_bucket)) + 
  geom_tile(col="black") + facet_grid(grade ~ treatment_class) + 
  xlab("") + ylab("") + 
  theme(axis.text.y = element_text(size = axis.text.size),
        axis.text.x = element_text(angle=45,hjust=1,size=axis.text.size),
        strip.text = element_text(size=strip.text.size)) + 
  scale_fill_manual(values=c(colfunc(4),"darkgray","purple"),
                    guide= guide_legend(title="B. rate",title.position = "top",direction="horizontal",nrow=2))
B
plotB <- anyDF %>% 
  inner_join(AE.names) %>% 
  group_by(event,short_ae_name,mean_rate,grade,treatment_class) %>% 
  summarize(rate=median(rate,na.rm=T)) %>% 
  group_by(event) %>% mutate(event_num = row_number()) %>% ungroup()

A <- ggplot(plotB,aes(x=reorder(short_ae_name,-mean_rate),y=rate,fill=grade)) + geom_col() + 
  scale_y_continuous(expand=c(0,0)) + facet_grid( ~ treatment_class) + 
  scale_fill_manual(values=c("darkturquoise","firebrick3"),
                    guide= guide_legend(title = "A. grade", title.position = "top",direction="horizontal")) + 
  theme_bw() + xlab("") + ylab("") + 
  theme(axis.text.y = element_text(size=axis.text.size),
        axis.text.x = element_text(angle=45,hjust=1),
        strip.text  = element_text(size=strip.text.size))  
A

plotC <- AE.countDF %>% filter(event=="anemia")
grade_names <- list("grade 1/2" = "anemia\ngrade 1/2","grade 3+" = "anemia\ngrade 3+")
grade_labeller <- function(value){return(grade_names[value])}

C <- ggplot(plotC,aes(x=reorder(rg_name,-order),y=rate,fill=treatment_class)) + geom_col(position="dodge") + coord_flip() + 
  theme_bw() + 
  facet_grid( grade ~.,labeller=as_labeller(grade_labeller)) + 
  scale_y_continuous(expand=c(0,0)) + 
  scale_fill_discrete(guide=guide_legend(title="C. treatment class",title.position = "top",direction="horizontal")) + 
  xlab("") + ylab("") + 
  theme(
        axis.text.x = element_text(size=axis.text.size),
        axis.text.y = element_text(size=axis.text.size),
        strip.text  = element_text(size=strip.text.size),
        ) 
C
A + guide_area() + B + C + plot_layout(heights = c(1,2),widths = c(3,1),guides = "collect") + plot_annotation(tag_levels='A')
# ggsave("./figures/adverse_events.svg",width = 20,height=10,units = "in",limitsize = F)