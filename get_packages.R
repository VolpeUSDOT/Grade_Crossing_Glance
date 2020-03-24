# Get all necessary packages across data prep and analysis scripts for the grade crossing glance analysis 

loadpacks <- c(
  "kableExtra",
  "knitr",
  "plotly",
  "readxl",
  "sjPlot",
  "tidyverse"
  )

for(i in loadpacks){if(length(grep(i, (.packages(all.available=T))))==0) install.packages(i, dependencies =TRUE)}