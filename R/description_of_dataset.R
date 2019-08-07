#' Full dataset
#'
#' @docType data
#'
#' @usage data(BTemergenceData)
#'
#' @format An object of class \code{"data.table"}.
#'
#' @keywords datasets
#' @format A data table with 35193 rows and 31 columns
#' \item {ID} Bird band number
#' \item {box.breeding} number of the box where the bird bred in the upcoming season
#' \item {yid} combination of breeding_season and ID
#' \item {date_} date on which the specific data point was recorded
#' \item {breeding_season} the breeding season of that datapoint, always recorder form summer of one year to summer of the next year. Breeding_season = 2018 for example includes data from summer 2017 to summer 2018
#' \item {time_to_sunrise_min} Emergence time in minutes to sunrise
#' \item {z_time_to_sunrise_min} scale(time_to_sunrise_min): centered and devided by the standard deviation
#' \item {time_to_sunset_min} Cessation of activity in minutes to sunset
#' \item {z_time_to_sunset_min} scale(time_to_sunset_min): centered and devided by the standard deviation
#' \item {daylength_min} Total length of the individual's activity (24 hours - the time difference between cessation and onset of activity) in minutes
#' \item {actualDaylength} Total time the sun was above the horizon in hours
#' \item {sex} 1 = male, 2 = female, NA = unknown
#' \item {rel_day} Specifies how many days the date on which the current emergence time was recorded is away from the first egg of this bird in the current breeding_season. Only defined for individuals that did breed.
#' \item {rel_day_dusk} rel_day - 1. Needs to be defined, because the cessation of activity is always recorded on the night BEFORE the emergence time.
#' \item {precipitation_dawn} Precipitation in mm/h in the hour during which the bird emerged
#' \item {z_precipitation_dawn} scaled precipitation_dawn (centered and devided by the standard deviation)
#' \item {precipitation_dusk} Precipitation in mm/h in the hour during which the bird entered the enstbox in the evening
#' \item {z_precipitation_dusk} scaled precipitation_dusk (centered and devided by the standard deviation)
#' \item {avgRainfall average} daily rainfall (including the hours between dawn and dusk)
#' \item {temperature_dawn} Temperature in C in the hour during which the bird emerged
#' \item {temperature_dusk} Temperature in C in the hour during which the bird entered the enstbox in the evening
#' \item {avgTemperature} average daily temperature (including the hours between dawn and dusk)
#' \item {z_avgTemperature} scaled avgTemperature (centered and devided by the standard deviation)
#' \item {age} 1 = first year, 2 = older
#' \item {sex_age} sex and age pasted together
#' \item {sex2} same as sex, but levels exchanged in order to model the factors in alternating order: here, 1 = female, 2 = male
#' \item {age2} same as age, but levels exchanged in order to model the factors in alternating order: here, 1 = adult, 2 = yearling
#' \item {YDAY} Julian day = day of the year, but days after summer recieve a negative sign and belong to the upcoming breeding season (e.g. -1 = 30th December)
#' \item {clutch} total clutch size in this breeding season
#' \item {hatchDate} date of the first hatchling of the current breeding season
#' \item {fledgeDate} date when the entire brood had been found to have hatched
"x2"

