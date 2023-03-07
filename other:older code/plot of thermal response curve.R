 
# thermal response curve for Pcod hatch success

	# load in data
	eggs <- read.csv("../data and code from Laurel & Rogers 2020 CJFAS/laurelandrogerscode/Pcod and WP hatch probabilities.csv")
	
	eggs<-eggs[,1:8]
	eggs$Ntot<-eggs$Hatched+eggs$Dead
	eggs$Phatch<-eggs$Hatched/(eggs$Hatched+eggs$Dead)
	eggs<-eggs[eggs$Species=="Pcod",] #Pcod
	eggs$pMax<-eggs$Phatch/max(eggs$Phatch)
	
	res2 <- nls( Phatch ~ k/(1+((Temp_act-mu)/sigma)^2), start=c(mu=5,sigma=2,k=1) , data = eggs, weights=Ntot)
	v2 <- summary(res2)$parameters[,"Estimate"]

	cauchy <- function(x) v2[3]/(1+((x-v2[1])/v2[2])^2)

	therm_response_curve <- 
		ggplot(eggs) +
		geom_point(aes(x = Temp_act, y = Phatch), color = "black", size = 2) +
		geom_function(fun = cauchy, aes(col = "Temp_act"), color = "black", size = 0.5) +
		xlab("Temperature (˚C)") +
		scale_y_continuous(
			name = "Proportion\nsuccessful hatch") +
		ggtitle("Thermal response of hatch success") +
		theme_bw() +
		theme(legend.position = "none",
					plot.title = element_text(hjust = 0.5),
					plot.tag.position = c(0.05, 0.87),
  				axis.text = element_text(size= 12, colour = "black"),
  				axis.title = element_text(size=14, color = "black"),
  				axis.line = element_blank(),
  				axis.ticks = element_line(colour = "black"),
					panel.grid = element_blank(),
					plot.margin = margin(0, 0, 0, 0, "cm"),
					panel.border = element_rect(fill = NA, color = "black"))
	
	 ggsave(here("./output/plots/therm_response_curve.png"),
			 therm_response_curve,
			 width = 5, height = 4, units = "in")


