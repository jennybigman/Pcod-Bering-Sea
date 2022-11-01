	# from dan ovando (https://www.weirdfishes.blog/blog/fitting-bayesian-models-with-stan-and-r/#examine-spawning-and-recruitment-data)
	
	warmups <- 1000
	total_iterations <- 2000
	max_treedepth <-  10
	n_chains <-  4
	n_cores <- 4
	adapt_delta <- 0.95

	data <- list(
		n = nrow(stock_recruit_dat),
	  r = stock_recruit_dat$raw_recruits,
	  ssb = stock_recruit_dat$SSB_raw,
	  max_r = max(stock_recruit_dat$raw_recruits))

	bh_fit <- stan(
	  file = here("./bh_model.stan"),
	  data = data,
	  chains = n_chains,
	  warmup = warmups,
	  iter = total_iterations,
	  cores = n_cores,
	  refresh = 250,
	  init = list(
	    list(
	      h = 0.4,
	      log_alpha = log(1 * data$max_r),
	      log_beta = log(2* max(stock_recruit_dat$SSB_raw))
	    ),
	    list(
	      h = 0.21,
	      log_alpha = log(3 * data$max_r),
	      log_beta = log(.5 *max(stock_recruit_dat$SSB_raw))
	    ),
	    list(
	      h = 0.8,
	      log_alpha = log(1 * data$max_r),
	      log_beta = log(1.1*max(stock_recruit_dat$SSB_raw))
	    ),
	    list(
	      h = 0.3,
	      log_alpha = log(.8 * data$max_r),
	      log_beta = log(5*max(stock_recruit_dat$SSB_raw))
	    )
	  ),
	  control = list(max_treedepth = max_treedepth,
	                 adapt_delta = adapt_delta)
	)

	# get output
	
	bh_summary <- summary(bh_fit)$summary %>% 
  	as.data.frame() %>% 
  	mutate(variable = rownames(.)) %>% 
  	dplyr::select(variable, everything()) %>% 
  	as_data_frame()

	bh_summary %>% head()
	
	rhat <- bh_summary %>% 
  	filter(str_detect(variable,'rhat') & !str_detect(variable,'log') & !str_detect(variable,'pp'))

	stock_recruit_dat <- stock_recruit_dat %>% 
	  mutate(mean_rhat = rhat$mean,
	         lower = rhat$`2.5%`,
	         upper = rhat$`97.5%`)

	stock_recruit_dat %>% 
	  ggplot() + 
	  geom_point(aes(SSB_raw, raw_recruits)) + 
	  geom_line(aes(SSB_raw, mean_rhat)) + 
	  geom_ribbon(aes(SSB_raw, ymin = lower, ymax = upper), alpha = 0.25)
	

	###### fit with sp hab suit
	
	data2 <- list(
		n = nrow(stock_recruit_dat),
	  r = stock_recruit_dat$raw_recruits,
	  ssb = stock_recruit_dat$SSB_raw,
	  max_r = max(stock_recruit_dat$raw_recruits),
		mean_hab_suit = stock_recruit_dat$mean_hab_suit)

	bh_fit_cov <- stan(
	  file = here("./bh_model_covariate.stan"),
	  data = data2,
	  chains = n_chains,
	  warmup = warmups,
	  iter = total_iterations,
	  cores = n_cores,
	  refresh = 250,
	  init = list(
	    list(
	      h = 0.4,
	      log_alpha = log(1 * data$max_r),
	      log_beta = log(2* max(stock_recruit_dat$SSB_raw))
	    ),
	    list(
	      h = 0.21,
	      log_alpha = log(3 * data$max_r),
	      log_beta = log(.5 *max(stock_recruit_dat$SSB_raw))
	    ),
	    list(
	      h = 0.8,
	      log_alpha = log(1 * data$max_r),
	      log_beta = log(1.1*max(stock_recruit_dat$SSB_raw))
	    ),
	    list(
	      h = 0.3,
	      log_alpha = log(.8 * data$max_r),
	      log_beta = log(5*max(stock_recruit_dat$SSB_raw))
	    )
	  ),
	  control = list(max_treedepth = max_treedepth,
	                 adapt_delta = adapt_delta)
	)

	# get output
	
	bh_summary_cov <- summary(bh_fit_cov)$summary %>% 
  	as.data.frame() %>% 
  	mutate(variable = rownames(.)) %>% 
  	dplyr::select(variable, everything()) %>% 
  	as_data_frame()

	bh_summary_cov %>% head()
	
	rhat_cov <- bh_summary_cov %>% 
  	filter(str_detect(variable,'rhat') & !str_detect(variable,'log') & !str_detect(variable,'pp'))

	stock_recruit_dat <- stock_recruit_dat %>% 
	  mutate(mean_rhat_cov = rhat_cov$mean,
	         lower_cov = rhat_cov$`2.5%`,
	         upper_cov = rhat_cov$`97.5%`)

	stock_recruit_dat %>% 
	  ggplot() + 
	  geom_point(aes(SSB_raw, raw_recruits)) + 
	  geom_line(aes(SSB_raw, mean_rhat_cov)) + 
	  geom_ribbon(aes(SSB_raw, ymin = lower_cov, ymax = upper_cov), alpha = 0.25)
     