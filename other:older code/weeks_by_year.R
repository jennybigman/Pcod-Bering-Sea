### create lists of dates for every year

# create lists of dates for which to extract bottom temp data -- we want one data point per week for
    # years 1970 - 2020 
    
    year_start_func <- function(x){
    	paste0(x, "-01-01")
    }
   
    years <- seq(1970, 2020, by = 1)
    
    start_dates <- sapply(years, year_start_func)
    
  	date_func <- function(x){
  	seq.Date(as.Date(x), by = "week", length.out = 52)
  	}
  
  	start_dates <- as.Date(start_dates)
  	
  	dates_list <- lapply(start_dates,function(x) { 
    			date_func(x)
		})
