# loc = a dataframe geographical coordinated of the locations of interest
# mn = lower bound for coverage ('yyyy-mm-dd')
# mx = upper bound for coverage ('yyyy-mm-dd')
# rad = radius for search
# lim = maximum number of station to be retreived for a given location



get_station_data <- function (loc, mn, mx, lim, rad){
   
   # load/install important packages
   if(!suppressMessages(require("data.table", quietly = T))){
     install.packages("data.table")
     message("The package 'data.table' has been installed." )
   }
   
   if(!suppressMessages(require("pbapply", quietly = T))){
     install.packages("pbapply")
     message("The package 'pbapply' has been installed." )
   }
   
    if(!suppressMessages(require("rnoaa", quietly = T))){
     install.packages("rnoaa")
     message("The package 'rnoaa' has been installed." )
   }

  sta_list <- meteo_nearby_stations(loc, year_min = mn, 
                                  year_max = mx, radius = rad, 
                                  limit = lim)
 
  # get a table with distance for each  station
  locID <- as.character(paste("location", 1:length(sta_list), sep = "")) # make unique ID for each location
  
   sta_list_locID <- mapply(cbind,  sta_list, "locID" = locID, SIMPLIFY = F ) # add location ID to station list
   sta_df_locID <- do.call(rbind,  sta_list_locID) # make a table of location IDs and station IDs
   sta_df_locID <- sta_df_locID[!duplicated(sta_df_locID$id), ] # keep only rows with  unique  location IDs 

   #  get a vector of station ID for dowloading data
  sta_ids <- sta_df_locID$id
  station_data <- pblapply(1:length(sta_ids), function(i){ 
     dat <- tryCatch(ghcnd_search(stationid = sta_ids[i], 
                                  date_min = mn, 
                                  date_max =  mx), 
                                    error = function(e) list("empty"))
     return(dat)
   })
   return(station_data)
   }
