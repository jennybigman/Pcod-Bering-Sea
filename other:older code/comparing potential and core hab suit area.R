# comparing potential and core spawning habitat suitablity area

   	ggplot() +
		#geom_line(data = rolling_mean_hind, 
   	#					aes(x = year, y = area), 
   	#					color = light_black) +
		#geom_line(data = rolling_mean_proj, 
   	#					aes(x = year, y = area), 
   	#					color = light_black) +
	 	geom_line(aes(year, area), alpha = 0.3,
            data = hind_area_yr %>% filter(sp_hab_threshold == "potential"), color = "red") +
		#geom_line(data = proj_area_yr, 
		#					aes(year, area, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		#facet_grid(sp_hab_threshold ~ scen_f, scales = "free") +
	  geom_line(aes(year, area), alpha = 0.3,
            data = hind_area_yr %>% filter(sp_hab_threshold == "core"), color = "black") 