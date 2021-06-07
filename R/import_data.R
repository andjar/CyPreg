import_data <- function(study){
  
  df <- read.csv(study$filename, fileEncoding = "UTF-8-BOM", sep = ";")
  df <- merge(df, study)
  if(study$type == "aggregated"){
    if("value.median" %in% colnames(df)){
      df$value <- df$value.median
      df$value.measure <- "median"
      df <- df[,colnames(df) != "value.median"]
    }else if("value.mean" %in% colnames(df)){
      df$value <- df$value.mean
      df$value.measure <- "mean"
      df <- df[,colnames(df) != "value.mean"]
    }
  }else if(study$type == "raw"){
    
  }else{
    stop("Study type not recognized...")
  }
  
  
  return(df)
}