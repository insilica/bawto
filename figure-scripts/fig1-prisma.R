# Due to updates for RSysrev, this code can only be run by loading the cache first. 
# It is mostly useful as reference

library(rsvg)
load("cache.image")


# answers <- rsr::get_answers(70431)

# studies <- mawtoLabels$Populations %>%
#   mutate(Pop_Value = trimws(Pop_Value)) %>%
#   filter(Pop_Value %in% c(DRD, "DRD", "HRD", "HRR", "HHR", "DNA repair", "repair", "homologous")) %>%
#   select(Article.ID) %>%
#   distinct()

# source("../scripts/DataNormalization.R")
# source("../scripts/ConcordantScreeningData.R")
# screen <- getConcordantScreeningData()
# screen$synthesis2 <- screen$synthesis + 5

g <- DiagrammeR::grViz(sprintf(
  'digraph prisma {
    newrank=true;
    node [shape="box", fontsize = 12, width=2];
    graph [compound=true,splines=ortho, nodesep=0.25, rankdir=TB];

    subgraph cluster_0 {
      label="A. ClinicalTrials.gov BAWTO Screen"
      style=filled;
      fillcolor=lightgrey;
      margin=8
      c_id    [label="Records found   \non clinicaltrials.gov\n(n=%d)"];
      c_scr   [label="Records screened\n(n=%d)"];
      c_excl  [label="Records excluded\n(n=%d)"];
      c_incl  [label="Records included\n(n=%d)"]
      c_doc   [label="Related Records \nidentified\n(n=%d)", fillcolor=darkslategray1, style=filled];

      c_id -> c_scr;
      c_scr -> c_excl;
      c_scr -> c_incl;
      c_incl  -> c_doc;
    }

    subgraph cluster_1 {
      label="B. PubMed BAWTO Screen"
      style=filled;
      fillcolor=lightgrey;
      margin=8
      p_id    [label="Records identified\non pubmed.gov\n(n=%d)"];
      c2_id    [label="Records identified\nfrom clinicaltrials.gov*\n(n=%d)", fillcolor=darkslategray1, style=filled];

      dedup     [label="Records after\nduplicates removed\n(n=%d)",width=3]
      screening [label="Records screened\n(n=%d)",width=2]
      excluded  [label="Records excluded\n(n=%d)",width=2]

      c2_id -> dedup;
      p_id  -> dedup;
      dedup -> screening;
      screening -> excluded;

      fulltext_scr  [label="full-text articles assessed\n for eligibility\n(n=%d)"];
      fulltext_excl [label="Full-text articles excluded\n (n=%d)\n\nno data for prostate cancer patients (n=%d)\nno mutation data (n = %d)\nno outcome data (n=%d)"];
      screening -> fulltext_scr;
      fulltext_scr -> fulltext_excl;

      fulltext_incl [label="studies included in\nquantitative synthesis\n(n=%d)", fillcolor=lightpink,style=filled];
      fulltext_scr -> fulltext_incl;
    }

    subgraph cluster_2 {
      label="C. HRD and DRD in mCRPC"
      margin=8

      other_records [label="studies in conference abstracts\n and other reviews\n(n=5)"];
      fulltext_incl2 [label="studies included in\nquantitative synthesis\n(n=%d)", fillcolor=lightpink,style=filled];

      dedup_2 [label = "Records after\nduplicates removed\n(n=52)",width=3]
      hrd_screen [label="Records Screened\n(n=%d)"];
      hrd_excl [label="Records Excluded\n(n=%d)\n\nNo HRD/DRD biomarkers\n(n=39)"];
      hrd_incl [label="studies included in\nquantitative synthesis\n(n=%d)", fillcolor=lightgoldenrodyellow,style=filled];

      fulltext_incl2 -> dedup_2;
      other_records -> dedup_2;
      dedup_2 -> hrd_screen;
      hrd_screen -> hrd_excl;
      hrd_screen -> hrd_incl;

      { rank=same; hrd_screen; hrd_excl; }
    }

      { rank=same; c_scr; c_excl; }
      { rank=same; p_id; c2_id;}
      { rank=same; screening; excluded;}
      { rank=same; fulltext_scr; fulltext_excl; }

  }',
  screen$ctgov, screen$ctgov, screen$ctgov_excl, screen$ctgov_incl, screen$ctgov_ass,
  screen$pm, screen$ctgov_ass, screen$pm_screen, screen$pm_screen, screen$pm_excl, screen$fscreen,
  screen$fexcl, screen$no_data_for_prostate_cancer, screen$no_mutation_data, screen$no_outcome_data,
  screen$synthesis,
  screen$synthesis,
  screen$synthesis2, # records screened
  screen$synthesis - nrow(studies), # records excluded
  nrow(HRD.baseDF) # included studies
))

# Export the image
# g %>%
#   export_svg() %>%
#   charToRaw() %>%
#   rsvg_svg("/home/thomas/tmp/graph.svg")