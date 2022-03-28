library(tidyverse)
library(rsr)
library(kiln)

# OPEN SOURCING CODE - WORK IN PROGRESS
HRD.tbl <- function(){ # this will be a brick
    HRD    <- 81395 # sysrev.com/p/81395
    HRD.lbl    <- rsr::get_labels(HRD)  |> select(lid,lbl.name=short_label)
    HRD.baseDF <- rsr::get_answers(HRD) |> halfbaked::mpipe() |>
        filter(user_id==139) |> # final decision made by user 139
        inner_join(HRD.lbl,by="lid") |>
        select(aid,lbl.name,answer) %>% 
    group_by(Article.ID,lbl.name) %>% summarize(answer=paste(answer,collapse=";")) %>% 
  reshape2::dcast(Article.ID~lbl.name,value.var = "answer") %>% rename(trial=`NCT Trial ID`)

ct.tbl <- function(){ # This will be a kiln brick
    con <- DBI::dbConnect(RPostgres::Postgres(),
        dbname="aact", host="aact-db.ctti-clinicaltrials.org", port=5432,
        user=Sys.getenv("AACT_user"), password=Sys.getenv("AACT_pass"))
    on.exit(dbDisconnect(con))
    
    tbl(con,"studies") |> 
        select(nct_id,start_date,primary_completion_date,completion_date) |>
        filter(nct_id %in% ) |> 
        collect()
    }

library(vistime)

trial_status_dates <- range_read(extractions_sheet,sheet = "trial-status-dates",col_types = "c") %>% 
  mutate(date = as.Date(date,format="%b %d %Y")) %>% 
  select(trial,event,date)

hrd_trials <- HRD.baseDF %>% 
  select(trial,short_name) %>%
  mutate(trial_short_name = case_when(
    grepl("TOPARP",short_name,ignore.case = T) ~ "TOPARP",
    grepl("TRITON",short_name,ignore.case = T) ~ "TRITON",
    grepl("PROfound",short_name,ignore.case = T) ~ "PROfound",
    T~short_name
    )) %>% select(trial,short_name=trial_short_name) %>% distinct() 

spectral = RColorBrewer::brewer.pal(6,"Spectral")

df2 <- ctgov.time %>% 
  reshape2::melt(id.vars="nct_id") %>% select(trial=nct_id,event=variable,date=value) %>% 
  rbind(trial_status_dates) %>% 
  filter(!(event %in% c("primary_completion_date","completed"))) %>% 
  group_by(trial) %>% arrange(date) %>% mutate(end = lead(date)) %>% ungroup() %>% 
  mutate(event=as.factor(event)) %>% 
  mutate(color = case_when(event=="start_date" ~ spectral[1],
                           event=="recruiting" ~ spectral[2],
                           event=="active_not_recruiting" ~ spectral[3],
                           event=="suspended"~ "red",
                           event=="primary_completion_date" ~ "green", 
                           event=="completion_date" ~ "black")) %>% 
  inner_join(hrd_trials,by="trial") %>% select(trial=short_name,event,start=date,end,color) %>% 
  filter(event != "completion_date")

test = df2 %>% mutate(x=1,y=1)
dummy <- ggplot(data=test, aes(x=x,y=y,fill=event)) + geom_bar(stat="identity") + scale_fill_manual(values=c(spectral[3],spectral[2],spectral[1],"red",spectral[3],"red","red"))
leg <- ggpubr::get_legend(dummy)

timeline = gg_vistime(df2,col.group="trial",col.event="event",show_labels = F,linewidth = 12) + 
  scale_x_datetime(breaks = breaks_width("6 months"), labels = date_format("%b-%Y")) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1,size=16)) + 
  theme(axis.text.y = element_text(size=18))

ggsave("../figures/timeline.legend.svg",plot=dummy)
ggsave(plot = timeline,filename = "../figures/timeline.svg",width=12,height=6,units = "in")