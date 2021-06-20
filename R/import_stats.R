import_stats_mann_whitney <- function(study, all_variables, df_aggregated){
  
  df_mann_whitney_raw <- read.csv(paste0("data/statistics/mann_whitney.csv"), fileEncoding = "UTF-8-BOM", sep = ";")
    
    df <- Reduce(rbind, lapply(unique(df_mann_whitney_raw$file), function(i){
      df_temp <- subset(df_mann_whitney_raw, file == i)
      dfd_temp <- subset(df_aggregated, filename == i)
      df_temp$cytokine.1 <- df_temp$cytokine.1 - 1
      df_temp$cytokine.2 <- df_temp$cytokine.2 - 1
      df_temp$value.1 <- dfd_temp$value[df_temp$cytokine.1]
      df_temp$value.2 <- dfd_temp$value[df_temp$cytokine.2]
      a <- strsplit(df_temp$group, ", ", fixed = TRUE)[[1]]
      a <- Reduce(cbind, lapply(a, function(aa) which(aa == colnames(df_aggregated))))
      
      if(length(a)>1){
        df_temp$group.1 <- do.call(paste, c(dfd_temp[ df_temp$cytokine.1 , a], sep="-"))
        df_temp$group.2 <- do.call(paste, c(dfd_temp[ df_temp$cytokine.2 , a], sep="-"))
      }else{
        df_temp$group.1 <- dfd_temp[ df_temp$cytokine.1 , a]
        df_temp$group.2 <- dfd_temp[ df_temp$cytokine.2 , a]
      }
      
      
      if(any(dfd_temp$cytokine[df_temp$cytokine.1] != dfd_temp$cytokine[df_temp$cytokine.2])){
        stop("Cytokines didn't match between groups")
      }
      
      df_temp$cytokine <- dfd_temp$cytokine[df_temp$cytokine.1]
      
      df_temp
    }))
  
  return(df)
}
