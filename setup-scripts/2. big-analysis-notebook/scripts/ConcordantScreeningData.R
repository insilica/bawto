getConcordantScreeningData <- function(){
  
  
  base_screening   <- RSysrev::sysrev.getLabelAnswers(70431)
  pubmed_screening <- RSysrev::sysrev.getLabelAnswers(64435)
  ct_screening     <- RSysrev::sysrev.getLabelAnswers(63101)
  
  exclusion_reason <- RSysrev::sysrev.getLabelAnswers(77566) %>% 
    filter(lbl.name=="Reason for Exclusion") %>% 
    select(article.id,answer) %>% select(Article.ID = article.id,answer) %>% 
    inner_join(exclusion_articles,by="Article.ID") %>% 
    select(-Article.ID,Article.ID=Extraction.Article.ID) %>% 
    inner_join(articles,by="Article.ID") %>% 
    select(Article.ID,reason=answer) %>% 
    group_by(reason) %>% count()
  
  ctScreen <- ct_screening %>% 
    filter(lbl.name=="Include") %>% 
    mutate(answer = ifelse(answer=="true",TRUE,FALSE)) %>% 
    select(article.id,answer.resolve,answer,reviewer.id) %>% 
    (function(df){
      resolved <- df %>% filter(!is.na(answer.resolve)) %>% select(article.id,answer) %>% 
        mutate(resolved=TRUE)
      concord  <- df %>% select(reviewer.id,article.id,answer,answer.resolve)  %>% 
        group_by(article.id) %>% 
        filter(all(is.na(answer.resolve))) %>% 
        summarize(answer=first(answer)) %>% mutate(resolved=FALSE)
      rbind(resolved,concord)
    }) %>% group_by(answer) %>% count() 
  
  pubmed_screen   <- pubmed_screening %>% 
    filter(lbl.name=="Include") %>% 
    mutate(answer = ifelse(answer=="true",TRUE,FALSE)) %>% 
    select(article.id,answer.resolve,answer,reviewer.id) %>% 
    (function(df){
      resolved <- df %>% filter(!is.na(answer.resolve)) %>% select(article.id,answer) %>% mutate(resolved=TRUE) %>% distinct()
      concord  <- df %>% select(reviewer.id,article.id,answer,answer.resolve)  %>% 
        filter(!article.id %in% resolved$article.id) %>% 
        group_by(article.id) %>% 
        filter(all(answer==TRUE) | all(answer==FALSE)) %>% 
        summarize(answer=first(answer)) %>% mutate(resolved=FALSE)
      rbind(resolved,concord)
    }) %>% group_by(answer) %>% count() 
  
  fulltext_screen <- base_screening %>% 
    filter(lbl.name=="Include") %>% 
    mutate(answer = ifelse(answer=="true",TRUE,FALSE)) %>% 
    select(article.id,answer.resolve,answer,reviewer.id) %>% 
    (function(df){
      resolved <- df %>% filter(!is.na(answer.resolve)) %>% select(article.id,answer) %>% mutate(resolved=TRUE) %>% distinct()
      concord  <- df %>% select(reviewer.id,article.id,answer,answer.resolve)  %>% 
        filter(!article.id %in% resolved$article.id) %>% 
        group_by(article.id) %>% 
        filter(all(answer==TRUE) | all(answer==FALSE)) %>% 
        summarize(answer=first(answer)) %>% mutate(resolved=FALSE)
      rbind(resolved,concord)
    }) %>% group_by(answer) %>% count() 
  
  
  ctgov  <- sum(ctScreen$n)
  ctgov_excl <- ctScreen[which(ctScreen$answer==FALSE),"n"]$n
  ctgov_incl <- ctScreen[which(ctScreen$answer==TRUE),"n"]$n
  ctgov_ass  <- 78 # See sysrev.com/p/64435
  
  pm        <- 3+8934 # see https://sysrev.com/u/688/p/64435/add-articles
  pm_screen <- sum(pubmed_screen$n)
  pm_excl   <- pubmed_screen[which(pubmed_screen$answer==FALSE),"n"]$n
  
  fscreen   <- sum(fulltext_screen$n)
  fexcl     <- fulltext_screen[which(fulltext_screen$answer==FALSE),"n"]$n
  
  no_data_for_prostate_cancer <- exclusion_reason[which(exclusion_reason$reason == "no data specifically for prostate cancer patients"),"n"]$n
  no_mutation_data            <- exclusion_reason[which(exclusion_reason$reason == "no mutation data"),"n"]$n
  no_outcome_data             <- exclusion_reason[which(exclusion_reason$reason == "no outcome or AE data based on mutation"),"n"]$n
  not_prostate_cancer         <- exclusion_reason[which(exclusion_reason$reason == "Not prostate cancer"),"n"]$n
  synthesis <- fulltext_screen[which(fulltext_screen$answer==TRUE),"n"]$n
  
  return(list(ctgov=ctgov,ctgov_excl=ctgov_excl,
              ctgov_incl=ctgov_incl,
              ctgov_ass=ctgov_ass,
              pm=pm,
              pm_screen=pm_screen,
              pm_excl=pm_excl,
              fscreen=fscreen,
              fexcl=fexcl,
              synthesis=synthesis,
              no_data_for_prostate_cancer_patients=no_data_for_prostate_cancer + not_prostate_cancer,
              no_mutation_data=no_mutation_data,
              no_outcome_data=no_outcome_data))
}

