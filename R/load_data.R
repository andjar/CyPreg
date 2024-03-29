source("R/import_data.R")
source("R/import_stats.R")

# Load studies ----
df_studies <- read.csv("data/studies.csv", fileEncoding = "UTF-8-BOM", sep = ";")
df_studies$filename <- paste0("data/", df_studies$type, "/", df_studies$file)

all_variables <- read.csv("data/variables.csv", fileEncoding = "UTF-8-BOM", sep = ";")

# Read cytokines ----
cytokines <- read.csv("data/cytokines.csv", fileEncoding = "UTF-8-BOM", sep = ";", as.is = TRUE)
cytokines$printname = gsub("alpha","\u03B1",cytokines$printname)
cytokines$printname = gsub("beta","\u03B2",cytokines$printname)
cytokines$printname = gsub("gamma","\u03B3",cytokines$printname)
cytokines$printname = gsub("delta","\u03B4",cytokines$printname)
cytokines$printname = gsub("epsilon","\u03B5",cytokines$printname)

# Load aggregated data ----
df_aggregated <- data.frame()
for(i in which(df_studies$type == "aggregated")){
  df_aggregated <- dplyr::bind_rows(import_data(df_studies[i,], all_variables), df_aggregated)
}

# Load aggregated data statistics ----
df_mann_whitney <- import_stats_mann_whitney(df_studies, all_variables, df_aggregated)

# Load raw data ----
df_raw <- data.frame()
