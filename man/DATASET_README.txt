Full dataset of blue tit activity: BTemergenceData.csv

A data table with 35193 rows and 31 columns

ID: Bird band number
box.breeding: number of the box where the bird bred in the upcoming season
yid: combination of breeding_season and ID
date_: date on which the specific data point was recorded
breeding_season: the breeding season of that datapoint, always recorder form summer of one year to summer of the next year. Breeding_season = 2018 for example includes data from summer 2017 to summer 2018
time_to_sunrise_min: Emergence time in minutes to sunrise
z_time_to_sunrise_min: scale(time_to_sunrise_min): centered and devided by the standard deviation
time_to_sunset_min: Cessation of activity in minutes to sunset
z_time_to_sunset_min: scale(time_to_sunset_min): centered and devided by the standard deviation
daylength_min: Total length of the individual's activity (24 hours - the time difference between cessation and onset of activity) in minutes
actualDaylength: Total time the sun was above the horizon in hours
sex: 1 = male, 2 = female, NA = unknown
rel_day: Specifies how many days the date on which the current emergence time was recorded is away from the first egg of this bird in the current breeding_season. Only defined for individuals that did breed.
rel_day_dusk: rel_day - 1. Needs to be defined, because the cessation of activity is always recorded on the night BEFORE the emergence time.
precipitation_dawn: Precipitation in mm/h in the hour during which the bird emerged
z_precipitation_dawn: scaled precipitation_dawn (centered and devided by the standard deviation)
precipitation_dusk: Precipitation in mm/h in the hour during which the bird entered the enstbox in the evening
z_precipitation_dusk: scaled precipitation_dusk (centered and devided by the standard deviation)
avgRainfall average: daily rainfall (including the hours between dawn and dusk)
temperature_dawn: Temperature in C in the hour during which the bird emerged
temperature_dusk: Temperature in C in the hour during which the bird entered the nestbox in the evening
avgTemperature: average daily temperature (including the hours between dawn and dusk)
z_avgTemperature: scaled avgTemperature (centered and devided by the standard deviation)
age: 1 = first year, 2 = older
sex_age: sex and age pasted together
sex2: same as sex, but levels exchanged in order to model the factors in alternating order: here, 1 = female, 2 = male
age2: same as age, but levels exchanged in order to model the factors in alternating order: here, 1 = adult, 2 = yearling
YDAY: Julian day = day of the year, but days after summer recieve a negative sign and belong to the upcoming breeding season (e.g. -1 = 30th December)
clutch: total clutch size in this breeding season
hatchDate: date of the first hatchling of the current breeding season
fledgeDate: date when the entire brood had been found to have hatched
