library(tidyverse)
library(magrittr)
library(googlesheets4)
library(plotly)


scale = c(RColorBrewer::brewer.pal(name = "Reds",n=9) %>% rev(),RColorBrewer::brewer.pal(name = "Greens",n=9)) %>% rev()
minoutcome = 0.2
maxoutcome = 2
tree <- range_read("https://docs.google.com/spreadsheets/d/1-8IdZiim0Oe7F2ExQe9XpNxruyWH2YHxyeBI0q_3Hfs/edit#gid=0", 
                   sheet = "Two Arm OS HR - A",col_types = "c") %>% 
  mutate(outcome= as.numeric(outcome)) %>% 
  mutate(colors = cut(outcome,breaks = seq(minoutcome,maxoutcome,0.1),ordered_result = T)) %>% 
  rowwise() %>% mutate(color = ifelse(outcome < minoutcome, scale[1], 
                                      ifelse(outcome > maxoutcome,tail(scale,n=1),scale[colors]))) %>% ungroup() %>%
  mutate(color=ifelse(is.na(outcome),"yellow",color)) %>% 
  mutate(patients = as.numeric(patients)) %>% 
  mutate(prevalence = round(100*patients / 387)) %>% 
  rowwise() %>% mutate(text = ifelse(leaf=="F",
                                     sprintf("%s %s<br>%s%%",gsub("0\\.",".",outcome),treemap_conf_text,prevalence),
                                     sprintf("%s<br>%s<br>%s%%",gsub("0\\.",".",outcome),treemap_conf_text,prevalence))) %>% ungroup()


# Generates Figure 4A
fig <- plot_ly(tree,
               type     = "treemap",
               text     = ~text,
               labels   = ~biomarker,
               values   = ~patients,
               textinfo ="label+value+percent parent+percent root",
               texttemplate = "<b>%{label}</b><br>%{text}",
               parents  = ~parent,
               marker   = list(colors = ~color),
               branchvalues="total"
) #%>% layout(uniformtext=list(minsize=10, mode='show',maxsize=18))
fig
