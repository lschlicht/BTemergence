require(BTemergenceData)
USER = 'lschlicht'


#fetch data - 2 min: data save in folder "data" ####
a = Sys.time()
YEARS = 2011:2017
x = runFetchData(con = dbcon(user = USER), YEARS = YEARS)
Sys.time()-a
x = copy(x[[1]])

#save basic dataset
save(x, file = paste0("~/BTemergence/data/", Sys.Date(), "_BasicDataset.RData"))

#remove potential replacements broods (verified replacements removed previously)
length(unique(x[, yid])) #983
length(unique(x[sex == 1, yid])) #389
length(unique(x[sex == 2, yid])) #594
x = subset(x, replacementClutchOutlier == 0)
length(unique(x[, yid])) #39 replacement clutches removed
length(unique(x[sex == 1, yid])) #9
length(unique(x[sex == 2, yid])) #31

x[, z_temperature_dawn := scale(temperature_dawn)]
x[, z_temperature_dusk := scale(temperature_dusk)]
x[, z_avgTemperature := scale(avgTemperature)]
x[, z_humidity_dawn := scale(humidity_dawn)]
x[, z_humidity_dusk:= scale(humidity_dusk)]
x[, z_avgHumidity := scale(avgHumidity)]
x[, z_precipitation_dawn := scale(precipitation_dawn)]
x[, z_precipitation_dusk := scale(precipitation_dusk)]
x[, z_avgRainfall := scale(avgRainfall)]

x[, sex2 := factor(sex)]

#save full dataset
save(x, file = paste0("~/BTemergence/data/", Sys.Date(), "_FullDataset.RData"))
