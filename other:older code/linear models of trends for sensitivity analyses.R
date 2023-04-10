	library(patchwork)
	
	#place_label <- function(label, size = 3, ...) {
  #annotate("text", label = label, x = -Inf, y = Inf, 
  #         hjust = 0, vjust = 1, size = size, ...)
	#}


	## cauchy models ####
	
	# linear models of hindcast ####
	
	cauch_hind_yr <- ROMS_cauchy_sens_hind %>%
		group_by(year, iter) %>%
    summarise(mean_sphabsuit_cauch = mean(sphabsuit_cauch))
	
	cauch_hind_yr_fits <- cauch_hind_yr %>%
    group_by(iter) %>%
    do(broom::tidy(lm(mean_sphabsuit_cauch ~ year, data = .), conf.int = .95)) %>%
    ungroup %>% rename(model = iter)
	
	cauch_hind_yr_fits <- cauch_hind_yr_fits %>%
		dplyr::filter(term == "year")

	in_paper <- tibble(
		model = 0,
		term = "year",
		estimate = 0.0006616,
		std.error = 0.0004994, 
		statistic = NA,
		p.value = 0.191,
		conf.low = -0.0003408616,
		conf.high = 0.001664144)
	
	cauch_hind_yr_fits <- bind_rows(in_paper, cauch_hind_yr_fits)
	
	cauch_hind_yr_fits$model[cauch_hind_yr_fits$model == "0"] <- "in paper"

	
	# make plot
	coef_plot_cauch_hind <- 
		secret_weapon(cauch_hind_yr_fits, var = "year") +
	  xlab("Coefficient estimate") + 
		ylab("Iteration") +
		theme_bw() +
		scale_colour_grey() +
		scale_x_continuous(
			breaks = c(0, 0.001),
			labels = c(0, 0.001)
		) +
		theme(legend.position = "none") +
		ggtitle("a)")
	
	
	# projection ####
	
	# low emission scenario
	cauch_proj_yr_ssp126 <- ROMS_cauchy_sens_proj %>%
		filter(scenario == "ssp126") %>%
		group_by(year, iter) %>%
    summarise(mean_sphabsuit_cauch = mean(sphabsuit_cauch))
	
	cauch_proj_yr_ssp126_fits <- cauch_proj_yr_ssp126 %>%
    group_by(iter) %>%
    do(broom::tidy(lm(mean_sphabsuit_cauch ~ year, data = .), conf.int = .95)) %>%
    ungroup %>% rename(model = iter)
	
	cauch_proj_yr_ssp126_fits <- cauch_proj_yr_ssp126_fits %>%
		dplyr::filter(term == "year")

	in_paper <- tibble(
		model = 0,
		term = "year",
		estimate = 0.0007879,
		std.error = 0.0002051,
		statistic = NA,
		p.value = 0.000157,
		conf.low =  0.0003838231,
		conf.high = 0.001191981)
	
	cauch_proj_yr_ssp126_fits <- bind_rows(in_paper, cauch_proj_yr_ssp126_fits)
	
	cauch_proj_yr_ssp126_fits$model[cauch_proj_yr_ssp126_fits$model == "0"] <- "in paper"

	
	# make plot
	coef_plot_cauch_proj_ssp126 <- 
		secret_weapon(cauch_proj_yr_ssp126_fits, var = "year") +
	  xlab("Coefficient estimate") + 
		ylab("Iteration") +
		scale_colour_grey() +
		ggtitle("b)") +
		scale_x_continuous(
			breaks = c(0.0006, 0.001),
			labels = c("0.0006", 0.001)
		) +
		theme_bw() +
		theme(legend.position = "none",
					axis.text.y = element_blank(),
					axis.title.y = element_blank(),
					axis.ticks.y = element_blank())
	
 # high emission scenario
	cauch_proj_yr_ssp585 <- ROMS_cauchy_sens_proj %>%
		filter(scenario == "ssp585") %>%
		group_by(year, iter) %>%
    summarise(mean_sphabsuit_cauch = mean(sphabsuit_cauch))
	
	cauch_proj_yr_ssp585_fits <- cauch_proj_yr_ssp585 %>%
    group_by(iter) %>%
    do(broom::tidy(lm(mean_sphabsuit_cauch ~ year, data = .), conf.int = .95)) %>%
    ungroup %>% rename(model = iter)
	
	cauch_proj_yr_ssp585_fits <- cauch_proj_yr_ssp585_fits %>%
		dplyr::filter(term == "year")
	

	in_paper <- tibble(
		model = 0,
		term = "year",
		estimate =  0.0029634,
		std.error = 0.0001989,
		statistic = NA,
		p.value = NA,
		conf.low =   0.002571493,
		conf.high = 0.003355326)
	
	cauch_proj_yr_ssp585_fits <- bind_rows(in_paper, cauch_proj_yr_ssp585_fits)
	
	cauch_proj_yr_ssp585_fits$model[cauch_proj_yr_ssp585_fits$model == "0"] <- "in paper"

	# make plot
	coef_plot_cauch_proj_ssp585 <- 
		secret_weapon(cauch_proj_yr_ssp585_fits, var = "year") +
	  xlab("Coefficient estimate") + 
		ylab("Iteration") +
		scale_color_grey() +
		scale_x_continuous(
			breaks = c(0.0025, 0.003),
			labels = c(0.0025, 0.003)
		) +
		ggtitle("c)") +
		theme_bw() +
		theme(legend.position = "none",
					axis.text.y = element_blank(),
					axis.title.y = element_blank(),
					axis.ticks.y = element_blank())
	
	# plot together 
	
 plot1 <- coef_plot_cauch_hind 

 plot2 <- coef_plot_cauch_proj_ssp126

 plot3 <- coef_plot_cauch_proj_ssp585 


 coef_plots_cauch <- plot1 + plot2 + plot3 


 ggsave("./scripts/manuscript figure scripts/used in ms/pngs of figs/coef_plots_cauch.tiff",
			 coef_plots_cauch, dpi = 500)


 
 ############################################ not used
 
 	# any trend?
	
	# hindcast
	ROMS_cauchy_sens_hind$iter_c <- as.character(ROMS_cauchy_sens_hind$iter)
	
	lm_func_hind <- function(x){
		
		new_dat <- ROMS_cauchy_sens_hind %>% filter(iter_c == x)
		
		fit <- lm(sphabsuit_cauch ~ year, data = new_dat)
		
		fits <- coef(fit)[2]
		lwr <- confint(fit)[2, 1]
		upr <- confint(fit)[2, 2]
		iter_c <- x
		
		fit_out <- tibble(fits, lwr, upr, iter_c)
		
	}
	
	iters <- unique(ROMS_cauchy_sens_hind$iter_c)
	
	lm_list <- lapply(iters, lm_func_hind)
	
	lm_hind_out <- bind_rows(lm_list)
	
	lm_hind_out_cauch <- lm_hind_out %>%
		rename(mean = fits, 
					 lower = lwr, 
					 upper = upr, 
					 iter = iter_c) %>%
		mutate(scenario = "hindcast")
	
	# projection
	
	ROMS_cauchy_sens_proj$iter_c <- as.character(ROMS_cauchy_sens_proj$iter)

	lm_func_proj <- function(x, y){
		
		new_dat <- ROMS_cauchy_sens_proj %>% filter(iter_c == x & scenario == y)
		
		fit <- lm(sphabsuit_cauch ~ year, data = new_dat)
		
		fits <- coef(fit)[2]
		lwr <- confint(fit)[2, 1]
		upr <- confint(fit)[2, 2]
		iter_c <- x
		scenario <- y
		
		fit_out <- tibble(fits, lwr, upr, iter_c, scenario)
		
	}
	
	iters <- unique(ROMS_cauchy_sens_hind$iter_c)
	
	scens <- rep(unique(ROMS_cauchy_sens_proj$scenario), each = 10)
	
	
	lm_list_proj <- mapply(lm_func_proj, 
												 x = iters, y = scens, 
												 SIMPLIFY = FALSE)
	
	lm_proj_out <- bind_rows(lm_list_proj)
	
	lm_proj_out_cauchy <- lm_proj_out %>%
		rename(mean = fits, 
					 lower = lwr, 
					 upper = upr, 
					 iter = iter_c) 

	lm_cauchy_outs <- bind_rows(lm_hind_out_cauch, lm_proj_out_cauchy)

	lm_cauchy_outs <- fread(file = here("./data/lm_cauchy_outs.csv")) 
		
	lm_cauchy_outs$iter <- as.character(lm_cauchy_outs$iter)

	mean = c(0.0007, 0.0008, 0.003)
	lower = c(-0.0003, 0.0004, 0.002)
	upper = c(0.002, 0.001, 0.004)
	iter = c("in paper", "in paper", "in paper")
	scenario = c("hindcast", "ssp126", "ssp585")
	
	paper_tib <- tibble(mean, lower, upper, iter, scenario)
	
	lm_cauchy_outs <- bind_rows(lm_cauchy_outs, paper_tib)
	