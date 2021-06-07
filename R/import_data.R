import_data <- function(study, all_variables){
  
  df <- read.csv(study$filename, fileEncoding = "UTF-8-BOM", sep = ";")
  allowed_variables <- subset(all_variables, type %in% c("both", study$type))$variable
  allowed_variables <- paste(allowed_variables,collapse="|")
  if(any(!grepl(allowed_variables,colnames(df)))){
    stop(paste0("A variable in ",study$file," did not match variables.csv"))
  }
  
  df <- merge(df, study)
  if(study$type == "aggregated"){
    nOfCols <- ncol(df)
    for(i in 1:nOfCols){
      
    ## Move point estimates
      if(grepl("median", colnames(df)[i])){
        tmp_basename <- sub("(.*).median.*", "\\1", colnames(df)[i])
        if(!any(grepl(paste0("^",tmp_basename,"$"), colnames(df)))){
          ### The column does not exist from before
          df[,ncol(df) + 1] <- df[,i]
          colnames(df)[ncol(df)] <- tmp_basename
          df[,ncol(df) + 1] <- ifelse(!is.na(df[,i]),"Median",NA)
          colnames(df)[ncol(df)] <- paste0(tmp_basename,".def")
        }else{
          ### The column exists already, replace if empty
          df[, colnames(df) == tmp_basename] <- ifelse(is.na(df[, colnames(df) == tmp_basename]), df[,i], df[, colnames(df) == tmp_basename])
          df[, colnames(df) == paste0(tmp_basename,".def")] <- ifelse(is.na(df[, colnames(df) == paste0(tmp_basename,".def")]) & !is.na(df[,i]), "Median", df[, colnames(df) == paste0(tmp_basename,".def")])
        }
      }
      
      if(grepl("mean", colnames(df)[i])){
        tmp_basename <- sub("(.*).mean.*", "\\1", colnames(df)[i])
        if(!any(grepl(paste0("^",tmp_basename,"$"), colnames(df)))){
          ### The column does not exist from before
          df[,ncol(df) + 1] <- df[,i]
          colnames(df)[ncol(df)] <- tmp_basename
          df[,ncol(df) + 1] <- ifelse(!is.na(df[,i]),"Mean",NA)
          colnames(df)[ncol(df)] <- paste0(tmp_basename,".def")
        }else{
          ### The column exists already, replace if empty
          df[, colnames(df) == tmp_basename] <- ifelse(is.na(df[, colnames(df) == tmp_basename]), df[,i], df[, colnames(df) == tmp_basename])
          df[, colnames(df) == paste0(tmp_basename,".def")] <- ifelse(is.na(df[, colnames(df) == paste0(tmp_basename,".def")]) & !is.na(df[,i]), "Mean", df[, colnames(df) == paste0(tmp_basename,".def")])
        }
      }
    }
    
    df$ga <- NA
    if(any(grepl("ga.days", colnames(df)))){
      df$ga <- df$ga.days
    }
    if(any(grepl("ga.weeks", colnames(df)))){
      df$ga <- ifelse(is.na(df$ga),df$ga.weeks*7, df$ga)
    }
    
    ## Move spread
    if(any(grepl("value.percentile", colnames(df)))){
      tmp_cols <- which(grepl("value.percentile", colnames(df)))
      tmp_percentiles <- as.numeric(sub("value.percentile.(.*)", "\\1", colnames(df)[tmp_cols]))
      df$value.lower <- df[, tmp_cols[tmp_percentiles == min(tmp_percentiles)]]
      df$value.lower.def <- paste0(min(tmp_percentiles)," percentile")
      df$value.upper <- df[, tmp_cols[tmp_percentiles == max(tmp_percentiles)]]
      df$value.upper.def <- paste0(max(tmp_percentiles)," percentile")
    }else if(any(grepl("value.sd", colnames(df)))){
      df$value.lower <- df$value - df$value.sd * 1.97
      df$value.lower.def <- paste0("Lower 95% CI")
      df$value.upper <- df$value + df$value.sd * 1.97
      df$value.lower.def <- paste0("Upper 95% CI")
    }
    
  }else if(study$type == "raw"){
    
  }else{
    stop("Study type not recognized...")
  }
  
  
  return(df)
}