
# The following code allows downloading climate data for variables
# of interest with minimal missingness from desired locations. The 'get_GSOD'
# function in the package GSODR is used.

years. <- 1994:2014
variables <- c("MAX", "MIN", "PRCP", "RH", "SNDP")


var_data <- pblapply(1:5, function(x){
  var <- variables[x]
  
  dat <- pblapply(1:44, function(i) {
    
    print(paste("Retreiving data for location", i, "..."), quote = F)
    
    is_data_small <- 0
    is_var_there <- FALSE
    is_missing <- 0.75
  
    clo_st <- sort(dt_mx[rownames(dt_mx) == loc[i], ], decreasing = F)
  
    condition <- TRUE
    
      for(j in 1:length(clo_st)) {
        
        st_name <- names(clo_st[j])
        
        file_name <- paste0(loc[i], "_station_", st_name)
        
        st_dat <- get_GSOD(years = years., station = st_name,
                           country = "US",  CSV = T, 
                           filename = file_name, dsn = '~/' )
        
        if(!is.data.frame(st_dat)){next}

        st_dat <-  st_dat[st_dat[["STNID"]] %in% st_name, ]
        
        
        is_data_small <- dim(st_dat)[1]
        is_var_there <- var  %in%  names(st_dat)
        is_missing <- sum(is.na(st_dat[[var]]))/length(st_dat[[var]]) 
        
        
        
        condition <- (is_data_small < (340 * (length(years.))) ||
                        !is_var_there ||
                        is_missing > 0.10) && j <= length(clo_st)    
        
        if (condition == TRUE) {
          print(paste0("Retrying for", loc[i],
                       ", station ", st_name, " ..."))
          next
        } else {
            break
          }
        
      }
      
  
    if (is_missing > 0.10) {
        print(paste0(is_missing, "%", var, 
                     " data missing form station ",
                     st_dat[["STNID"]][1], ", in location ", 
                     i, "."), quote = F)
    }
      
  
    location <- rep(loc[i], nrow(st_dat))
    distance <- dt_mx[i, c(st_name)]
    distance_from_location <- rep(distance, nrow(st_dat))
  
    st_dat <- cbind(st_dat, location, distance_from_location)
    st_dat
  })
return(dat)
})
