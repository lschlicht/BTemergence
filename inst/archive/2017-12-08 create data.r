setwd("/ds/grpkempenaers/Lotte/R Studio projects/Data for package BTemergence")

require(BTemergenceData)
USER = 'lschlicht'


#fetch data - (2 min) 21 minutes: data save in folder "data" ####
a = Sys.time()
YEARS = 2011:2017
x = runFetchData(con = dbcon(user = USER), YEARS = YEARS)
Sys.time()-a
x = copy(x[[1]])

#save basic dataset
save(x, file = paste0(getwd(), "/data/", Sys.Date(), "_BasicDataset.RData"))

#remove uncertain sleep
x = subset(x, sleep == 1)

#remove potential replacements broods (verified replacements removed previously)
length(unique(x[, yid])) #1349
length(unique(x[sex == 1, yid])) #595
length(unique(x[sex == 2, yid])) #754
x = subset(x, replacementClutchOutlier == 0)
length(unique(x[, yid])) #41 replacement clutches removed
length(unique(x[sex == 1, yid])) #7
length(unique(x[sex == 2, yid])) #34

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

#remove female in 2012 sleeping in box 41, because of time drift in box 41
x = subset(x, !(breeding_season == 2012 & ID == "B2X7402" & box.sleep == 41)) #N = 17

#add day-of-year column
x[, YDAY := yday(date_)]
x[YDAY > 250, YDAY := as.integer(YDAY - 366)]
x[, age2 := as.factor(age)]


#manually check and remove all instances where one individual slept in more than one box in a single night
#how many?
length(which(duplicated(subset(x, select = c('ID', 'year_', 'YDAY'))) == TRUE)) #183
length(which(duplicated(subset(x, select = c('ID', 'year_', 'YDAY'))) == TRUE))/nrow(x) #0.5%

#have they been checked before (object REMOVE can be loaded)?
a = list.files(paste0(getwd(), "/data/"))
a = a[grep("REMOVE", a)]
a = a[length(a)]
if(length(a) > 0 & readline("load existing data? (y/n)") == 'y') load(paste0(getwd(), "/data/", a)) else {  REMOVE = c() }

#If not, then check manually
  dup = which(duplicated(subset(x, select = c('ID', 'year_', 'YDAY'))) == TRUE)
  which(dup %in% REMOVE)
  for(i in sort(dup, decreasing = TRUE)) {
  tmp = subset(x, breeding_season == x[i,breeding_season] & ID == x[i, ID] & YDAY == x[i,YDAY], select = c('box.breeding', 'box.sleep', 'in_', 'out_'))

  if(!(i %in% REMOVE) & nrow(tmp) > 1) {
    remove = NA
    if(tmp[1, in_] <= tmp[2,in_] & tmp[1, out_] >= tmp[2, out_]) remove = 1 else if(tmp[2, in_] <= tmp[1,in_] & tmp[2, out_] >= tmp[1, out_]) remove = 2
    print(tmp)
    print(remove)
    if(is.na(remove)) {
    remove = readline(prompt = "Which rows should be removed?")
    remove = as.numeric(unlist(strsplit(remove, ' ')))
    }
    if(length(remove) > 0) { #save entries to remove
      for(l in remove) {
        REMOVE[length(REMOVE)+1] = which(x[,breeding_season == x[i,breeding_season]] & x[,ID == x[i, ID]] & x[,YDAY == x[i,YDAY]])[l]
      }
    }
  }
  }


  REMOVE
  save(REMOVE, file = paste0(getwd(), "/data/", Sys.Date(), "_REMOVE.RData"))

length(REMOVE) #184
x = x[-REMOVE,]


x = subset(x, breeding_season < 2018)
#save full dataset
save(x, file = paste0(getwd(), "/data/", Sys.Date(), "_FullDataset.RData"))

#remove boxplot-outliers for running the models: x2 is the model dataset
time_to_sunrise_out = boxplot(x[,time_to_sunrise_min], plot = FALSE)$stats
time_to_sunset_out = boxplot(x[,time_to_sunset_min], plot = FALSE)$stats
x2 = subset(x,
            time_to_sunrise_min >= time_to_sunrise_out[1] & time_to_sunrise_min <= time_to_sunrise_out[5] &
              time_to_sunset_min >= time_to_sunset_out[1] & time_to_sunset_min <= time_to_sunset_out[5])
nrow(x)
nrow(x2)
nrow(x2)/nrow(x) #92.93%



#save model dataset
save(x2, file = paste0(getwd(), "/data/", Sys.Date(), "_ModelDataset.RData"))

