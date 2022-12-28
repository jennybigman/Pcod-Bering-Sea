	# stock-recruitment relationship with spawning habitat suitability as covariate

	#### annual habitat suitability index ####
  	
	yearly_hab_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(mean_hab_suit = mean(sp_hab_suit))
	
	#### load recruitment and spawning stock biomass data ####
	
	recruitment_dat <- read_csv(here("./data/SAFE_Pcod_EBS_2021_Recruits.csv")) %>% 
		na.omit() %>%
		mutate(raw_recruits = recruits * 1000,
					 log_raw_recruits = log(raw_recruits),
					 log_stdev = log(st_dev),
					 lower = log(raw_recruits - st_dev),
					 upper = log(raw_recruits + st_dev))
	
	years_recruit <- (min(recruitment_dat$year):max(recruitment_dat$year))
	hab_suit_recruit <- yearly_hab_dat_hind %>% filter(year %in% years_recruit)
	recruit_habsuit <- merge(hab_suit_recruit, recruitment_dat, by = "year")

	recruit_habsuit_en <- recruit_habsuit %>%
		filter(model == "ensemble")
	
	ssb_dat <- read_csv(here("./data/SpawningStockBiomass_Thompson_etal_2021.csv")) %>%
		mutate(SSB_raw = SSB_mil_ton * 10^6)

	# overlap in years
	
	range_func <- function(df){
		range(df$year)
	}
	
	range_func(recruit_habsuit_en)
	range_func(ssb_dat)		
	range_func(yearly_hab_dat_hind)	
	
	yrs_keep <- 1977:2020

	recruit_dat_trim <- recruit_habsuit_en %>%
		filter(year %in% yrs_keep)

	ssb_dat_trim <- ssb_dat %>%
		filter(year %in% yrs_keep)
		
	yearly_hab_dat_hind_trim <- yearly_hab_dat_hind %>%
		filter(year %in% yrs_keep)
	
	# function to join multiple dfs
	df_list <- list(recruit_dat_trim, ssb_dat_trim, yearly_hab_dat_hind_trim)
	
	stock_recruit_dat <- df_list %>% reduce(full_join) 
	
	# calculate recruits/SSB
	stock_recruit_dat <- stock_recruit_dat %>%
		mutate(rec_SSB = raw_recruits/SSB_raw,
					 log_rec_SSB = log(rec_SSB))

	## prelim plots
	
	# no lag
	ggplot(data = stock_recruit_dat, aes(x = SSB_raw, y = raw_recruits)) +
		geom_point()
	
	ggplot(data = stock_recruit_dat, aes(x = mean_hab_suit, y = rec_SSB)) +
		geom_point()

	ggplot(data = stock_recruit_dat, aes(x = mean_hab_suit, y = log(rec_SSB))) +
		geom_point()
	
	#### fit model Tom Hurst suggested: rec/ssb ~ sp hab suit index ####
	
	cor.test(stock_recruit_dat$mean_hab_suit, stock_recruit_dat$log_rec_SSB, 
			method = "pearson")

	xaxis_title <- "Log(number of recruits/spawning stock biomass)"
	
	SSBrecruit_habsuit_plot <-
		ggplot(data = stock_recruit_dat, aes(x = mean_hab_suit, y = log_rec_SSB)) +
		geom_point() +
		scale_x_continuous(
			breaks = c(0.2, 0.3, 0.4),
			labels = c(0.2, 0.3, 0.4),
			name = "Annual spawning habitat suitability") +
		scale_y_continuous(
			breaks = c(2.5, 3.0, 3.5, 4.0),
			labels = c(2.5, 3.0, 3.5, 4.0),
			limits= c(2.2, 4.01),
			name =  stringr::str_wrap(xaxis_title, width = 10)) +
		theme_bw() +
		annotate("text", label = "Pearson's correlation\ncoefficient = -0.8",
						 y = 4, x = 0.23, size = 3, alpha = 0.7) +
		theme(
			axis.text = element_text(size = 10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title.x = element_text(size = 12, color = "grey50"),
  	  axis.title.y = element_text(size = 12, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
			

	ggsave("./output/plots/SSBrecruit_habsuit_plot.png",
			 SSBrecruit_habsuit_plot,
			 width = 5, height = 5, units = "in")
	
	#### fit model Tom Hurst suggested: rec/ssb ~ sp hab suit area ####
	c_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.9) 
	
	c_area_hind_dat_sum <- c_area_hind_dat %>%
		group_by(latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	c_area_hind_dat_sum_yr <- c_area_hind_dat_sum %>%
		group_by(year) %>%
		dplyr::summarize(area = sum(area_km2)) %>% 
		mutate(sp_hab_threshold = "core")
	
	# potential habitat = sum of area where sps >= 0.5
	
	p_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.5) 
	
	p_area_hind_dat_sum <- p_area_hind_dat %>%
		group_by(latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	p_area_hind_dat_sum_yr <- p_area_hind_dat_sum %>%
		group_by(year) %>%
		dplyr::summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "potential")

	hind_area_yr <- bind_rows(c_area_hind_dat_sum_yr, p_area_hind_dat_sum_yr)

	yrs_area <- 1977:2016
	
	core_area_trim <- c_area_hind_dat_sum_yr %>%
		filter(year %in% yrs_area)
	
	pot_area_trim <- p_area_hind_dat_sum_yr %>%
		filter(year %in% yrs_area)
	
	hind_area_yr_trim <- hind_area_yr %>%
		filter(year %in% yrs_area)

		stock_recruit_dat_trim <- stock_recruit_dat %>%
		filter(year %in% yrs_area)
	
	stock_recruit_dat_trim_core <- inner_join(stock_recruit_dat_trim, core_area_trim)
	stock_recruit_dat_trim_pot <- inner_join(stock_recruit_dat_trim, pot_area_trim)

	# fit a model
	
	# core
	cor.test(stock_recruit_dat_trim_core$area, stock_recruit_dat_trim_core$log_rec_SSB, 
			method = "pearson")
	
	# potential
	cor(stock_recruit_dat_trim_pot$area, stock_recruit_dat_trim_core$log_rec_SSB, 
			method = "pearson")
	
	cor.test(stock_recruit_dat_trim_pot$area, stock_recruit_dat_trim_core$log_rec_SSB, 
			method = "pearson")

	stock_recruit_dat_area <- inner_join(stock_recruit_dat_trim, hind_area_yr_trim)
		
	xaxis_title <- "Log(number of recruits/spawning stock biomass)"
	
	SSBrecruit_area_plot <-
	 ggplot(data = stock_recruit_dat_area, 
					 aes(x = area, y = log_rec_SSB, group = sp_hab_threshold, color = sp_hab_threshold)) +
	 geom_point() +
		scale_color_manual(values = c("#00345C", '#01579B')) +
	 scale_x_continuous(
	  	name =	"Area (x 10<sup>5</sup> km<sup>2</sup>)",
	  	breaks = c(100000, 300000, 500000),
	  	labels = c(1,3,5)) +
		scale_y_continuous(
			breaks = c(2.5, 3.0, 3.5, 4.0),
			labels = c(2.5, 3.0, 3.5, 4.0),
			limits= c(2.2, 4.01),
			name =  stringr::str_wrap(xaxis_title, width = 15)) +
		theme_bw() +
		theme(
			legend.title = element_blank(),
			axis.text = element_text(size = 10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title.x = element_markdown(size = 12, color = "grey50"),
  	  axis.title.y = element_text(size = 12, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
			

	ggsave("./output/plots/SSBrecruit_area_plot.png",
			 SSBrecruit_area_plot,
			 width = 5, height = 5, units = "in")
	
	#### with just recruits and area ####
	
	# fit a model
	
	# core
	mod <- lm(log10(log_raw_recruits) ~ area, data = stock_recruit_dat_trim_core)
	summary(mod)
	confint(mod) 
	
	cor.test(stock_recruit_dat_trim_core$area, stock_recruit_dat_trim_core$log_raw_recruits, 
			method = "pearson")
	
	# potential
	mod <- lm(log_raw_recruits ~ area, data = stock_recruit_dat_trim_pot)
	summary(mod)
	confint(mod) 
	
	cor.test(stock_recruit_dat_trim_pot$area, stock_recruit_dat_trim_core$log_raw_recruits, 
			method = "pearson")

	xaxis_title <- "Log(number of recruits/spawning stock biomass)"
	
	recruit_area_plot <-
	 ggplot(data = stock_recruit_dat_area, 
					 aes(x = area, y = log_raw_recruits, group = sp_hab_threshold, color = sp_hab_threshold)) +
	 geom_point() +
		scale_color_manual(values = c("#00345C", '#01579B')) +
	 scale_x_continuous(
	  	name =	"Area (x 10<sup>5</sup> km<sup>2</sup>)",
	  	breaks = c(100000, 300000, 500000),
	  	labels = c(1,3,5)) +
		scale_y_continuous(
			breaks = c(18, 19, 20),
			labels = c(18, 19, 20),
		name =  "Log(abundance)") +
		theme_bw() +
		theme(
			legend.title = element_blank(),
			axis.text = element_text(size = 10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title.x = element_markdown(size = 12, color = "grey50"),
  	  axis.title.y = element_text(size = 12, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
			

	ggsave("./output/plots/recruit_area_plot.png",
			 recruit_area_plot,
			 width = 5, height = 5, units = "in")
	
	### fit a model to assess whether spawning habitat suitability explains recruits/SSB

	# fit one model without habitat suitability as a covariate and one with
	
	ricker_sr <- lm(log_rec_SSB ~ SSB_raw, data = stock_recruit_dat)
	
	ricker_sr_habsuit <- lm(log_rec_SSB ~ SSB_raw + mean_hab_suit, data = stock_recruit_dat)	
	
	AICc(ricker_sr)
	
	AICc(ricker_sr_habsuit)
	