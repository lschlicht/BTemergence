x2 = subset(x2, select = c('ID', 'box.breeding', 'yid', 'date_', 'breeding_season', 'time_to_sunrise_min', 'z_time_to_sunrise_min', 'time_to_sunset_min', 'z_time_to_sunset_min', 'daylength_min', 'actualDaylength', 'sex', 'rel_day', 'rel_day_dusk', 'precipitation_dawn', 'z_precipitation_dawn', 'precipitation_dusk', 'z_precipitation_dusk', 'avgRainfall', 'temperature_dawn', 'temperature_dusk', 'avgTemperature', 'z_avgTemperature', 'age', 'sex_age', 'sex2', 'age2', 'YDAY', 'clutch', 'hatchDate', 'fledgeDate'))

save(x2, file = "/ds/grpkempenaers/Lotte/R Studio projects/BTemergence/data/BTemergenceData.RData")
#data("BTemergenceData")
