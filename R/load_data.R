source("R/import_data.R")

# Load studies ----
df_studies <- read.csv("data/studies.csv", fileEncoding = "UTF-8-BOM", sep = ";")
df_studies$filename <- paste0("data/", df_studies$type, "/", df_studies$file)

all_variables <- read.csv("data/variables.csv", fileEncoding = "UTF-8-BOM", sep = ";")
cytokines <- read.csv("data/cytokines.csv", fileEncoding = "UTF-8-BOM", sep = ";")

# Load aggregated data ----
df_aggregated <- data.frame()
for(i in 1:nrow(df_studies)){
  df_aggregated <- dplyr::bind_rows(import_data(df_studies[i,], all_variables), df_aggregated)
}

# Load raw data ----
df_raw <- data.frame()