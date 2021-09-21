# area metric 

#### by year ####

core_area_dat <- ROMS_dat_hind_trim %>%
	filter(sp_hab_suit >= 0.9) %>%
	filter(year != 2021)

core_area_dat_sum_yr <- core_area_dat %>%
	group_by(year) %>%
	summarize(total_core_area = sum(sp_hab_suit))


# potential habitat = sum of area where sps >= 0.5

pot_area_dat <- ROMS_dat_hind_trim %>%
	filter(sp_hab_suit >= 0.5) %>%
	filter(year != 2021)

pot_area_dat_sum_yr <- pot_area_dat %>%
	group_by(year) %>%
	summarize(total_pot_area = sum(sp_hab_suit))


# plot

plot <-    
   	ggplot(data = core_area_dat_sum_yr) +
   	geom_line(aes(x = year, y = total_core_area), color = "#00345C", size = 1) +
	  geom_line(data = pot_area_dat_sum_yr, aes(x = year, y = total_pot_area), 
	  					color = "#01579B", size = 1) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = expression(paste("Total area", ' '(km^{2}))),
	  	breaks = c(0, 20000, 40000, 60000),
	  	labels = c(0, 20000, 40000, 60000)
	  ) +
   	xlim(1970, 2020) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  	  axis.text=element_text(size=10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 12),
  	  axis.title= element_text(size = 14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
	ggsave("./output/plots/total_area_year.png",
			 plot,
			 width = 10, height = 7, units = "in")


#### by month ###

# core habitat = sum of area where sps >= 0.9

core_area_dat_sum_mo <- core_area_dat %>%
	group_by(month_name, year) %>%
	summarize(total_core_area = sum(sp_hab_suit))

# potential habitat = sum of area where sps >= 0.5

pot_area_dat_sum_mo <- pot_area_dat %>%
	group_by(month_name, year) %>%
	summarize(total_pot_area = sum(sp_hab_suit))


# plot

plot <-    
   	ggplot(data = core_area_dat_sum) +
   	geom_line(aes(x = year, y = total_core_area), color = "#00345C", size = 1) +
	  geom_line(data = pot_area_dat_sum, aes(x = year, y = total_pot_area), 
	  					color = "#01579B", size = 1) +
		facet_wrap(~ month_name) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = expression(paste("Total area", ' '(km^{2}))),
	  	breaks = c(0, 5000, 10000, 15000),
	  ) +
   	xlim(1970, 2020) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  		strip.background = element_blank(),
  		strip.text = element_text(size = 12, face = "bold"),
  	  axis.text=element_text(size=12, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 12),
  	  axis.title= element_text(size = 14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
	ggsave("./output/plots/total_area_month_year.png",
			 plot,
			 width = 10, height = 7, units = "in")
	
	
	### total area with temp ####
	
	core_area_dat_sum_yr_temp <- core_area_dat %>%
		group_by(year) %>%
		summarize(total_core_area = sum(sp_hab_suit),
							mean_temp = mean(temp))

# potential habitat = sum of area where sps >= 0.5
	
	pot_area_dat_sum_yr_temp <- pot_area_dat %>%
		group_by(year) %>%
		summarize(total_pot_area = sum(sp_hab_suit),
							mean_temp = mean(temp))

	plot <-    
   	ggplot(data = core_area_dat_sum_yr_temp) +
   	geom_line(aes(x = year, y = total_core_area), color = "#00345C", size = 1) +
	  geom_line(data = pot_area_dat_sum_yr, aes(x = year, y = total_pot_area), 
	  					color = "#01579B", size = 1) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = expression(paste("Total area", ' '(km^{2}))),
	  	breaks = c(0, 20000, 40000, 60000),
	  	labels = c(0, 20000, 40000, 60000)
	  ) +
   	xlim(1970, 2020) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  	  axis.text=element_text(size=10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 12),
  	  axis.title= element_text(size = 14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
	ggsave("./output/plots/total_area_year.png",
			 plot,
			 width = 10, height = 7, units = "in")

