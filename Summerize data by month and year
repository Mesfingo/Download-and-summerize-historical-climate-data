# The function takes the output from the function 'filter_var_coverage_distance', extractes 
# data for a given time range, summerize data  by month or year for each location of interest,
# and outputs a dataframe with condensed information. 
 
 # 1.  dat = object output by the function 'filter_var_coverage_distance'
 # 2.  by = one of the values in {"year", "Year", "Month", "month'} to indivate what kind of summary is required
 # 3.  var = single length caracter that is an abbreviation for the climate variable of interest
 # 4.  starts = lower bound for the date of interest ('yyyy-mm-dd') 
 # 5.  ends = upper bound for the date of interest ('yyyy-mm-dd')

**********************************************************************


summerize_var <- function(dat, by, var, st, en){

  if(!( by %in% c("year", "Year", "month", "Month"))){
    stop("The value for the argument 'by' is not specified correctly")
  }
  if(!(var %in% names(dat))){
    stop("The value for the argument 'by' is not specified correctly")
  }  
  
  if(!require(data.table, quietly = T)){
    install.packages("data.table")
    message("The package 'data.table' has been installed." )
  }

mo <- strftime(dat$date, "%m", tz = "GMT")
yr <- strftime(dat$date, "%Y", tz = "GMT")
var_data <- data.frame(dat, yr, mo)

st <- as.Date(st, tz = "GMT")
en <- as.Date(en, tz = "GMT")
var_data <- var_data[as.Date(var_data$date, tz = "GMT") 
                     %between% c(st, en), ]

var_data[[var]] <- as.numeric(var_data[[var]])/10

  if (by %in% c("month", "Month")){
    var <- var_data[[var]]
    mean_mo <- aggregate(var ~ mo + locID, data = var_data, FUN = mean, na.rm = T)
    mean_mo <-reshape(mean_mo, 
                 idvar = "locID",
                 timevar = "mo",
                 direction = "wide")
    names(mean_mo) <- month.name
    return(mean_mo)
  }


mean_yr <- aggregate(var ~ yr + locID, data = var_data, FUN = mean, na.rm = T)
mean_yr <-reshape(mean_yr, 
                  idvar = "locID",
                  timevar = "yr",
                  direction = "wide")
return(mean_yr)
}
