
estimateEffect <- function(a = NA,  #a = the minimum value,
                           b = NA,  #b = the maximum value,
                           q1 = NA, #q1 = the first quartile,
                           q3 = NA, #q3 = the third quartile,
                           m = NA,  #m = the median,
                           n = NA   #n = the sample size.
                           ){
  
  estimate_X_case_1 <- function(n, a, b, m){
    return(
      (a+2*m+b)/4 + (a-2*m+b)/(4*n)
    )
  }
  
  estimate_S_case_1 <- function(n, a, b){
    if(n <= 0){
      stop("n must be larger than 0")
    }else if(n <= 50){
      eta_Z <- c(0, 1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.97, 3.078, 3.173, 3.259, 3.336, 3.407, 3.472, 3.532, 3.588, 3.64, 3.689, 3.735, 3.778, 3.819, 3.858, 3.895, 3.931, 3.964, 3.997, 4.027, 4.057, 4.086, 4.113, 4.139, 4.165, 4.189, 4.213, 4.236, 4.259, 4.28, 4.301, 4.322, 4.341, 4.361, 4.379, 4.398, 4.415, 4.433, 4.45, 4.466, 4.482, 4.498)
      S <- (b-a)/eta_Z[n]
    }else{
      eta_Z <- 2*qnorm( (n-0.375) / (n+0.25) )
      S <- (b-a)/eta_Z
    }
    return(S)
  }
  
  estimate_X_case_2 <- function(n, a, b, m, q1, q3){
    LB <- (a+q1+m+q3)/4 + (4*b-a-q1-m-q3)/(4*n)
    UB <- (q1+m+q3+b)/4 + (4*a-q1-m-q3-b)/(4*n)
    return( (LB+UB)/2 )
  }
  
  estimate_S_case_2 <- function(n, a, b, m, q1, q3){
    
    # Sjekk om denne stemmer for mindre n ogs?
    S <- (b-a)/(4*qnorm( (n-0.375)/(n+0.25) )) + (q3-q1)/(4*qnorm( (0.75*n-0.125)/(n+0.25) ))
    
    return(S)
  }
  
  estimate_X_case_3 <- function(n,  m, q1, q3){
    return(
      (q1+m+q3)/3
    )
  }
  
  estimate_S_case_3 <- function(n,  m, q1, q3){
    
    # Sjekk om denne stemmer for mindre n ogs?
    S <- (q3-q1)/(2*qnorm( (0.75*n-0.125)/(n+0.25*n) ))
    
    return(S)
  }
  
  # Our reference provides calculations for three scenarios
  # https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-135
  if(is.na(q1) & is.na(q3) & !is.na(a) & !is.na(b) & !is.na(m) & !is.na(n)){
    # Case 1
    return(
      data.frame(
        X = estimate_X_case_1(n = n, a = a, b = b, m = m),
        S = estimate_S_case_1(n = n, a = a, b = b)
      )
    )
  }else if(!is.na(q1) & !is.na(q3) & !is.na(a) & !is.na(b) & !is.na(m) & !is.na(n)){
    # Case 2
    return(
      data.frame(
        X = estimate_X_case_2(n = n, a = a, b = b, m = m, q1 = q1, q3 = q3),
        S = estimate_S_case_2(n = n, a = a, b = b, m = m, q1 = q1, q3 = q3)
      )
    )
  }else if(!is.na(q1) & !is.na(q3) & is.na(a) & is.na(b) & !is.na(m) & !is.na(n)){
    # Case 3
    return(
      data.frame(
        X = estimate_X_case_3(n = n, m = m, q1 = q1, q3 = q3),
        S = estimate_S_case_3(n = n, m = m, q1 = q1, q3 = q3)
      )
    )
  }else{
    warning("Scenario not recognized; the set of values provided is not supported at the moment")
    return(
      data.frame(
        X = NA,
        S = NA
      )
    )
  }
  
}



prep_meta <- function(dff, Author, c, e){
  return(
    data.frame(
      Author = Author,
      median.e = subset(dff, condition == e)$value.median,
      median.c = subset(dff, condition == c)$value.median,
      mean.e = subset(dff, condition == e)$value.mean,
      mean.c = subset(dff, condition == c)$value.mean,
      sd.e = subset(dff, condition == e)$value.sd,
      sd.c = subset(dff, condition == c)$value.sd,
      n.e = subset(dff, condition == e)$n,
      min.e = subset(dff, condition == e)$value.percentile.0,
      max.e = subset(dff, condition == e)$value.percentile.100,
      q1.e = subset(dff, condition == e)$value.percentile.25,
      q3.e = subset(dff, condition == e)$value.percentile.75,
      n.c = subset(dff, condition == c)$n,
      q1.c = subset(dff, condition == c)$value.percentile.25,
      q3.c = subset(dff, condition == c)$value.percentile.75,
      min.c = subset(dff, condition == c)$value.percentile.0,
      max.c = subset(dff, condition == c)$value.percentile.100
    )
  )
}
