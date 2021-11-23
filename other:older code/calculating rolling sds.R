	sds_MJ <- NA
  
  for(i in 6:47){
  	win <- (i - 5):(i + 5)
  	sds_MJ[i] <- sd(yr_stats_MJ$mean_sp_hab_suit[win])
  }
  
  sds_MJ_noNA <- na.omit(sds_MJ)
  
  means_MJ <- NA
  
  for(i in 6:47){
  	win <- (i - 5):(i + 5)
  	means_MJ[i] <- mean(yr_stats_MJ$mean_sp_hab_suit[win])
  }
  
  means_MJ_noNA <- na.omit(means_MJ)
  mike_stats_MJ <- as.data.frame(cbind(yrs_mike, means_MJ_noNA, sds_MJ_noNA))

  rolling_stats_MJ <- cbind(rolling_stats_MJ, mike_stats_MJ)
  

  
    # calculate rolling mean of sd 
  roll_sd_yr_func <- function(x) {
  			
  	sds<- NA
  
  	for(i in 6:47){
  	win <- (i - 5):(i + x)
  	sds[i] <- sd(yr_stats$mean_sp_hab_suit[win])
  	}
  
		sds
  }
  
  nums <- c(-3, -1, 1, 3, 5, 7, 9)
  
  roll_sds <- sapply(nums, roll_sd_yr_func) %>%
  	as.data.frame() %>%
  	set_names(window_widths) %>%
  	gather("window_widths") %>%
  	rename(sd = value) %>%
  	mutate(year = rep((1970:2016), 7)) %>%
  	na.omit()