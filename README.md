# Biomarkers Associated With Therapeutic Outcomes (BAWTO) In Prostate Cancer
Scripts and gists for creating/analyzing data synthesized from prostate cancer clinical trial publications.

Three sysrev projects were created for this analysis:

  1. [sysrev.com/p/63101](https://sysrev.com/u/139/p/63101): screening project for clinicaltrials.gov metadata
  2. [sysrev.com/p/68027](https://sysrev.com/o/2/p/68027): screening project for pubmed abstracts 
  3. [sysrev.com/p/70431](https://sysrev.com/p/70431): pubmed data extraction project

# Directory structure

- **/figures** - Publication figures
- **/setup-scripts** - Scripts to set up / analyze sysrev projects
  -  **/1. find-ctgov-trials.rmd**  
  A notebook to populate sysrev.com/p/63101 w/ mCRPC biomaker clinical trials from clinicaltrials.gov.
  -  **/2. big-analysis-notebook/big-analysis-notebook.rmd**
  A notebook to analyze all sysrev data. Some dependencies are broken, results are stored in `./cache.image`
- **/figure-scripts** scripts to build each publication image
  - **fig1-prisma.R** - prisma diagram for every project
  - **fig2-timeline.R** -  clinical trial timelines
  - **fig3-heatmap.Rmd** - heatmap shows which trials use which biomarkers
  - **fig4-HR-treemap.R** - plotly treemap for hazard ratios
  - **fig5-psa-forest.R** - forest plot for PSA and PFS metrics
  - **fig6-adverse-event.R** - adverse event conditional distribution plots
  - **fig7-models.Rmd** - model based partitioning
- `/cache.image` - some of the dependencies in the big-analysis-notebook are broken. This cache was created to make image generation in `figure-scripts` easily reproducible.