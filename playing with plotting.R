# 02 - plot bottom temps

# add two columns: one with the date in Date format and one for just the month number
all_temp_dat$date <- as.Date(all_temp_dat$time)

all_temp_dat$month <- month(all_temp_dat$date)

all_temp_dat$day <- day(all_temp_dat$date)

# create dataset of one year and month
dat_Jan_1971 <- all_temp_dat %>%
						filter(., year == 1971) %>%
						filter(., month == 1) %>%
						filter(., day == 3) %>%
						na.omit() %>%
						select(latitude, longitude, val) %>%
						round(2)

dat_subset <- dat_Jan_1971[1:50, ]


dat_temp_sum <- all_temp_dat %>%
								group_by(latitude, longitude) %>%
								summarise(mean_temp = mean(val))

# plot (Lauren's code)
#ggplot(dat_Aug2_1970,aes(x = longitude, y = latitude)) + 
#  geom_point(aes(x = longitude, y = latitude), size = 0.8) + 
#  geom_tile(aes(fill=val)) +
#  scale_fill_viridis_c(name = "Temperature (C)") + 
#  theme_void() + 
#  coord_quickmap() + 
#  ggtitle("Mytitel", subtitle = "Mysubtitle") 


test_map <- qplot() +
	geom_tile(data = dat_temp_sum, aes(x = longitude, y = latitude, fill = mean_temp)) +
	scale_fill_viridis(discrete = FALSE) +
	theme_bw()

ggsave("test_map.png", 
			 plot = test_map,
			 width = 20, height = 20, units = "cm")
