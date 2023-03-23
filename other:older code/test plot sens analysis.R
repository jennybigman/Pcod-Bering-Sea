
ROMS_cauchy_sens_hind_sum <- ROMS_cauchy_sens_hind %>%
	dplyr::filter(iter == 1:2)

ROMS_cauchy_sens_proj_sum <- ROMS_cauchy_sens_proj %>%
	dplyr::filter(iter == 1:2)




	sens_cauchy_plot_test <- 
	 	ggplot(data = ROMS_cauchy_sens_proj_sum, aes(x = year, y = sphabsuit_cauch), 
	 				 color = iter) +
   	geom_line(data = ROMS_cauchy_sens_hind_sum, 
   						aes(x = year, y = sphabsuit_cauch), 
   						alpha = 0.3) +
		geom_line(data = ROMS_cauchy_sens_proj_sum,
							aes(year, sphabsuit_cauch, 
									group = iter, 
									color = iter), alpha = 0.3) +
		facet_wrap(~ scenario) +
		xlab("Year") + 
   	theme_bw()
   
			
	 ggsave("./scripts/manuscript figure scripts/used in ms/pngs of figs/sens_cauchy_plot_test.png",
			 sens_cauchy_plot_test)

