calls = function(working_directory = getwd()) {
#fetch data (ca. 1.9 minutes on Linux 16 cores, 4.5 minutes on Windows4(?) cores)
setwd(working_directory)
con = dbcon(user = "lschlicht")
input = whichData(years = 2011:2016, boxes = 1:277, from = "-01-01", to = "-12-31")
x = queryEventsAll(con, input)
x = addBreeding(con, x)
x = addWeather(con, x)
x = addIndVar(con, x)
x = addSunriseSunset(x)
x = addSleep(x)
x = addNestStage(con, x)
x = addBreedingStage(x)
x[, breeder := ifelse(is.na(box.breeding), "no", "yes")]
x[, breeder_sex := paste(breeder, sex)]
x[, breedingAttempt := paste(year_, box.breeding, sep = '_')]
snb = copy(x)
x = subset(x, sleep == 1)
x = subset(x, breeder == "yes")
x = subset(x, !is.na(rel_day))
x[, fac_breeding_stage := factor(breeding_stage, levels =
       c('nonBreeding', 'territoryEstablishment', 'nestBuilding', 'nestCompleted', 'preLaying', 'laying', 'incubation', 'earlyProvisioning', 'lateProvisioning', 'postFleding'))]

fe = subset(x, select = c('year_', 'box.breeding', 'firstEgg'))
fe = unique(fe, by = names(fe))
fe[, z_firstEgg := scale(as.IDate(firstEgg)), by = 'year_']
fe[, c_firstEgg := scale(as.IDate(firstEgg), scale = FALSE), by = 'year_']

x = merge(x, fe, by.x = c('year_', 'box.breeding', 'firstEgg'), by.y = c('year_', 'box.breeding', 'firstEgg'))
setkey(x, year_, box.breeding, ID, date_)
x[, lag1_rel_day := shift(rel_day), by = list(year_, box.breeding, ID)]
x[, lag1_rel_day_emergence_min := shift(time_to_sunrise_min), by = list(year_, box.breeding, ID)]
x2 = subset(x, sex == 2)

closeCon(con)
#end fetch data


#plot data
#breeder vs. non-breeder
jpeg(filename = "breeder_vs_nonbreeder.jpg", width = 1440, height = 720)
plotEmergence(subset(snb, sleep == 1 & yearDay > 60 & yearDay < 180), xx = "yearDay", colour_group = "breeder_sex")
dev.off()
#too little data

jpeg(filename = "emergenceAcrossYear.jpg", width = 1440, height = 720)
plotEmergence(x)
dev.off()


jpeg(filename = "sleepOnsetAcrossYear.jpg", width = 1440, height = 720)
plotEmergence(x, yy = "time_to_sunset_min", ylabel = "Time in minutes to sunset")
dev.off()


jpeg(filename = "emergenceAcrossYearDay.jpg", width = 1440, height = 720)
plotEmergence(x, xx = "yearDay", yy = "time_to_sunrise_min", xlabel = "yearDay", ylabel = "Time in minutes to sunrise")
dev.off()

jpeg(filename = "sleepOnsetAcrossYearDay.jpg", width = 1440, height = 720)
plotEmergence(x, xx = "yearDay", yy = "time_to_sunset_min", xlabel = "yearDay", ylabel = "Time in minutes to sunset")
dev.off()


jpeg(filename = "emergenceMinus10To5.jpg", width = 1440, height = 720)
plotEmergence(subset(x, rel_day > -10 & rel_day < 5))
dev.off()

jpeg(filename = "sleepOnsetMinus10To5.jpg", width = 1440, height = 720)
plotEmergence(subset(x, rel_day > -10 & rel_day < 5), xx = "rel_day", yy = "time_to_sunset_min", ylabel = "Time in minutes to sunset")
dev.off()
#end plot data

#models
#across the year
fmsr = modelSunrisetSex(x, response = "time_to_sunrise_min")
fmsr = summary(fmsr)
fmss = modelSunrisetSex(x, response = "time_to_sunset_min")
fmss = summary(fmss)



#preLaying and before
fmsrpl = modelPreLaying(x, response = "time_to_sunrise_min")
fmrspl = summary(fmsrpl)
fmsspl = modelPreLaying(x, response = "time_to_sunset_min")
fmsspl = summary(fmsspl)

jpeg(filename = "emergenceBreedingStage.jpg", width = 1440, height = 720)
ggplot(subset(x, !is.na(breeding_stage)), aes(fac_breeding_stage,
                                              time_to_sunrise_min, colour = factor(sex))) +
  geom_boxplot() + xlab('Breeding stage') + ylab('Time in minutes to
                                                 sunrise')
dev.off()

jpeg(filename = "sleepOnsetBreedingStage.jpg", width = 1440, height = 720)
  ggplot(subset(x, !is.na(breeding_stage)), aes(fac_breeding_stage,
                                                time_to_sunset_min, colour = factor(sex))) +
    geom_boxplot() + xlab('Breeding stage') + ylab('Time in minutes to
                                                   sunrise')
dev.off()

#individual effects
#1. energy demand
#a. female time of breeding, age
m = lmer(time_to_sunrise_min ~ age+z_firstEgg + (1|breedingAttempt) +
           (1|year_), data = subset(x, sex == 2))
hist(resid(m))
plot(m)
energy_sr_age_fE = summary(m)

m = lmer(time_to_sunrise_min ~ age + (1|breedingAttempt) +
           (1|year_), data = subset(x, sex == 2))
hist(resid(m))
plot(m)
energy_sr_age = summary(m)

m = lmer(time_to_sunrise_min ~ z_firstEgg + (1|breedingAttempt) +
           (1|year_), data = subset(x, sex == 2))
hist(resid(m))
plot(m)
energy_sr_fE = summary(m)

m = lmer(time_to_sunset_min ~ age+z_firstEgg + (1|breedingAttempt) +
           (1|year_), data = subset(x, sex == 2))
hist(resid(m))
plot(m)
energy_ss_age_fE = summary(m)

m = lmer(time_to_sunset_min ~ age + (1|breedingAttempt) +
           (1|year_), data = subset(x, sex == 2))
hist(resid(m))
plot(m)
energy_ss_age = summary(m)

m = lmer(time_to_sunset_min ~ z_firstEgg + (1|breedingAttempt) +
           (1|year_), data = subset(x, sex == 2))
hist(resid(m))
plot(m)
energy_ss_fE = summary(m)

m = lmer(time_to_sunrise_min ~ minAge+z_firstEgg + (1|breedingAttempt) +
           (1|year_), data = subset(x, sex == 2))
hist(resid(m))
plot(m)
summary(m)

m = lmer(time_to_sunset_min ~ minAge+z_firstEgg + (1|breedingAttempt) +
           (1|year_), data = subset(x, sex == 2))
hist(resid(m))
plot(m)
summary(m)
#---> late breeders emerge later, but older birds don't differ additionally
#---> early breeders go to sleep later, and older birds go to sleep later

#b. weather
m = lmer(time_to_sunrise_min ~ humidity+temperature+z_firstEgg +
           (1|breedingAttempt) + (1|year_), data = subset(x, sex == 2))
hist(resid(m))
plot(m)
energy_sr_environment = summary(m)

m = lmer(time_to_sunset_min ~ humidity+temperature+z_firstEgg +
           (1|breedingAttempt) + (1|year_), data = subset(x, sex == 2))
hist(resid(m))
plot(m)
energy_ss_environment = summary(m)
#---> early breeders emerge later, at higher humidity and higher temperature birds emerge later
#---> at higher humidity and higher temperature birds go to sleep earlier, now no effect of first egg left when controlling for weather (see previous analysis in contrast)
#---> in the right direction, but main effect is in wrong direction (later emergence during peak energetic investment = chick feeding)




#2. Sleep/Rest
#a. sleep onset should be equivalent to emergence
m = lmer(time_to_sunrise_min ~ time_to_sunset_min + (1|breedingAttempt)
         + (1|year_), data = subset(x, sex == 2))
hist(resid(m))
plot(m)
summary(m)
m = lmer(time_to_sunrise_min ~ time_to_sunset_min + (1|breedingAttempt)
         + (1|year_), data = subset(x, sex == 1))
hist(resid(m))
plot(m)
summary(m)
#---> across the season: a later emergence is correlated to an earlier sleep onset (male model doesn't fit so well, but ok)


m = lmer(time_to_sunrise_min ~ time_to_sunset_min+breeding_stage +
           (1|breedingAttempt) + (1|year_), data = subset(x, sex == 2))
hist(resid(m))
plot(m)
summary(m)
m = lmer(time_to_sunrise_min ~ time_to_sunset_min+breeding_stage +
           (1|breedingAttempt) + (1|year_), data = subset(x, sex == 1))
hist(resid(m))
plot(m)
summary(m)
#---> effect present when correcting for breeding stage (males and females)

m = lmer(time_to_sunrise_min ~
           time_to_sunset_min+I(as.ITime(sunrise)/60/60) + (1|breedingAttempt) +
           (1|year_), data = subset(x, sex == 2))
hist(resid(m))
plot(m)
rest_sr_ss_sr_f = summary(m)
m = lmer(time_to_sunrise_min ~
           time_to_sunset_min+I(as.ITime(sunrise)/60/60) + (1|breedingAttempt) +
           (1|year_), data = subset(x, sex == 1))
hist(resid(m))
plot(m)
rest_sr_ss_sr_m = summary(m)
#---> effect present when correcting for sunrise time (males -0.17, and females -0.17)

m = lmer(time_to_sunrise_min ~ I(((as.ITime(sunset)/60/60) -
                                   (as.ITime(sunrise)/60/60))) + (1|breedingAttempt) + (1|year_), data =
           subset(x, sex == 2))
hist(resid(m))
plot(m)
summary(m) #4.24


m = lmer(time_to_sunrise_min ~ I(((as.ITime(sunset)/60/60) -
                                   (as.ITime(sunrise)/60/60))) + (1|breedingAttempt) + (1|year_), data =
           subset(x, sex == 1))
hist(resid(m))
plot(m)
summary(m) #0.46


m = lmer(time_to_sunset_min ~ I(((as.ITime(sunset)/60/60) -
                                    (as.ITime(sunrise)/60/60))) + (1|breedingAttempt) + (1|year_), data =
           subset(x, sex == 2))
hist(resid(m))
plot(m)
summary(m) #-6.07


m = lmer(time_to_sunset_min ~ I(((as.ITime(sunset)/60/60) -
                                    (as.ITime(sunrise)/60/60))) + (1|breedingAttempt) + (1|year_), data =
           subset(x, sex == 1))
hist(resid(m))
plot(m)
summary(m) #-4.16
#---> positive effect of daylength on emergence times, negative effect on sleep onset for both males and females

#3. Nest Building
#nest completed vs. nestBuilding
m = lmer(time_to_sunset_min ~ humidity+temperature+z_firstEgg +
           (1|breedingAttempt) + (1|year_), data = subset(x, sex == 1))
m = lmer(time_to_sunset_min ~ z_firstEgg +
           (1|breedingAttempt) + (1|year_), data = subset(x, sex == 1))
m = lmer(time_to_sunrise_min ~ humidity+temperature+z_firstEgg +
           (1|breedingAttempt) + (1|year_), data = subset(x, sex == 1))
m = lmer(time_to_sunrise_min ~ z_firstEgg +
           (1|breedingAttempt) + (1|year_), data = subset(x, sex == 1))
hist(resid(m))
plot(m)
summary(m)

#---> some effects (see above); no effects for male first egg when controlling for weather, otherwise negative borderline for sunset and positive for sunrise

#4. Egg laying
#a. delay at the first egg for females only;
tmp = subset(x, rel_day %in% c(-1:0))
m = lmer(time_to_sunrise_min ~ factor(rel_day) + (1|breedingAttempt), data = subset(tmp, sex == 2))
hist(resid(m))
plot(m)
eggs_sr_dayMinus1To0 = summary(m)

tmp = subset(x, rel_day %in% c(-1:0))
m = lmer(time_to_sunrise_min ~ factor(rel_day) + (1|breedingAttempt), data = subset(tmp, sex == 1))
hist(resid(m))
plot(m)
summary(m)
#---> yes, there is a delay at the first egg and only for females, but too few data for males

#b. only in the morning, not in the evening.
tmp = subset(x, rel_day %in% c(-1:0))
m = lmer(time_to_sunset_min ~ factor(rel_day) + (1|breedingAttempt), data = subset(tmp, sex == 2))
hist(resid(m))
plot(m)
eggs_ss_dayMinus1To0 = summary(m)
#---> correct

#c. laying gaps: we don't know the exact laying gaps and the emergence times are not constant enough to see them. But: Does the existence of laying gaps explain variance in emergence? Does it explain variance in the evening (then it's not egg laying), only for females or also for males? Control for the number of data points
tmp = subset(x, breeding_stage == 'laying')
tmp[, var_time_to_sunrise_min := var(time_to_sunrise_min, na.rm = TRUE), by = list(year_, ID, firstEgg)]
tmp[, var_time_to_sunset_min := var(time_to_sunset_min, na.rm = TRUE), by = list(year_, ID, firstEgg)]
tmp[, no_sleep := length(time_to_sunrise_min), by = list(year_, ID, firstEgg)]

tmp = subset(tmp, select = c('year_', 'box.breeding', 'firstEgg', 'ID', 'sex', 'clutch', 'breedingAttempt', 'var_time_to_sunrise_min', 'var_time_to_sunset_min', 'no_sleep', 'laying_gap'))
tmp = unique(tmp, by = names(tmp))

tmp[, se_time_to_sunrise_min := var_time_to_sunrise_min / sqrt(no_sleep)]
tmp[, se_time_to_sunset_min := var_time_to_sunset_min / sqrt(no_sleep)]
tmp[!is.na(laying_gap), laying_gapYN := ifelse(laying_gap>0, 1, 0)]

fsr = lmer(log(var_time_to_sunrise_min+0.1) ~ laying_gap+no_sleep+(1|ID), data = subset(tmp, sex == 2)) #positive strong, positive strong
fss = lm(log(var_time_to_sunset_min+10) ~ laying_gap+no_sleep+(1|ID), data = subset(tmp, sex == 2)) #negative weak, positive weak
msr = lm(log(var_time_to_sunrise_min+1) ~ laying_gap+no_sleep+(1|ID), data = subset(tmp, sex == 1)) #no effects
mss = lm(log(var_time_to_sunset_min+1) ~ laying_gap+no_sleep+(1|ID), data = subset(tmp, sex == 1)) #no effects
boxplot(var_time_to_sunrise_min ~ laying_gap, data = subset(tmp, sex == 2), varwidth = TRUE)
boxplot(se_time_to_sunrise_min ~ laying_gap, data = subset(tmp, sex == 2), varwidth = TRUE, ylim = c(0, 700))


jpeg(filename = "VarianceEmergence_layingGap_Females.jpg", width = 1440, height = 720)
boxplot(var_time_to_sunrise_min ~ laying_gapYN, data = subset(tmp, sex == 2), varwidth = TRUE, ylim = c(0, 1200), ylab = "Variance in emergence times", xlab = "0 laying gaps / > 0 laying gaps")
dev.off()
jpeg(filename = "VarianceSleepOnset_layingGap_Females.jpg", width = 1440, height = 720)
boxplot(var_time_to_sunset_min ~ laying_gapYN, data = subset(tmp, sex == 2), varwidth = TRUE, ylim = c(0, 1200), ylab = "Variance in sleep onset", xlab = "0 laying gaps / > 0 laying gaps")
dev.off()
#---> yes, laying gaps seem to influence variation in emergence times:more laying breaks lead to higher var in the morning and lower var in the evening. Driven by weather?

#---> interestingly, the day -2 effect is present in the evening as well


#5. Territory defence
#a. EPP - see Marilia's project

#b. intruders: early emerging bird ---> fewer intruders: male argument ---> restrict to males
tmp = subset(snb, sex == 1 & breeding_stage %in% c('nestBuilding', 'nestCompleted', 'preLaying'))
tmp[, no_male_intrusions := length(validity), by = list(box.sleep, resident)]
tmp[, no_male_intruders := length(unique(ID)), by = list(box.sleep, resident)]
tmp[sleep == 1, mean_emergence_time := mean(time_to_sunrise_min), by = breedingAttempt]
tmp = subset(tmp, !is.na(mean_emergence_time), select = c('year_', 'box.breeding', 'breedingAttempt', 'firstEgg', 'age', 'minAge', 'mean_emergence_time', 'no_male_intrusions', 'no_male_intruders', 'ID'))
tmp = unique(tmp, by = names(tmp))

m = glmer(no_male_intruders ~ mean_emergence_time+age+(1|year_), data = tmp, family = 'poisson')
hist(resid(m))
plot(m)
tiem = summary(m)
#no effect

jpeg(filename = "TerritoryDefence_intruders_emergence.jpg", width = 1440, height = 720)
plot(no_male_intruders ~ mean_emergence_time, data = tmp)
dev.off()

m = lmer(log(no_male_intrusions) ~ mean_emergence_time+age+(1|ID), data = tmp)
hist(resid(m))
plot(m)
tiem2 = summary(m)
#no effect


#6. Pair bonding
#a. pair coordination: does male emergence time predict female emergence time during pre-laying and laying?
tmp = subset(x, breeding_stage %in% c('laying', 'preLaying', 'nestCompleted'))
tmp[, both_sleeping := length(box.breeding), by = list(date_, breedingAttempt)]
tmp = subset(tmp, both_sleeping == 2)


tmp = merge(subset(tmp, sex == 1), subset(tmp, sex == 2), by = c('year_', 'box.breeding', 'firstEgg', 'date_', 'sleep', 'nest_start', 'nest_completed', 'breeding_stage', 'z_firstEgg', 'breedingAttempt'), suffixes = c('.male', '.female'))
tmp[, layingYN := ifelse(breeding_stage == 'laying', 1, 0)]
m = lmer(time_to_sunrise_min.female ~ time_to_sunrise_min.male+layingYN + (1|breedingAttempt), data = tmp)
m = lmer(time_to_sunrise_min.female ~ time_to_sunrise_min.male + (1|breedingAttempt), data = subset(tmp, breeding_stage != 'laying'))
m = lmer(time_to_sunset_min.female ~ time_to_sunset_min.male + (1|breedingAttempt), data = subset(tmp, breeding_stage != 'laying'))
plot(m)
hist(resid(m))
pbcoord = summary(m)
#----> not at sunrise, but positive at sunset

tmp = subset(x, !breeding_stage %in% c('nonBreeding'))
tmp[, both_sleeping := length(box.breeding), by = list(date_, breedingAttempt)]
tmp = subset(tmp, both_sleeping == 2)
tmp = merge(subset(tmp, sex == 1), subset(tmp, sex == 2), by = c('year_', 'box.breeding', 'firstEgg', 'date_', 'sleep', 'nest_start', 'nest_completed', 'breeding_stage', 'z_firstEgg', 'breedingAttempt'), suffixes = c('.male', '.female'))
tmp[, layingYN := ifelse(breeding_stage == 'laying', 1, 0)]
m = lmer(time_to_sunrise_min.female ~ time_to_sunrise_min.male+layingYN + (1|breedingAttempt), data = tmp)
m = lmer(time_to_sunrise_min.female ~ time_to_sunrise_min.male + (1|breedingAttempt), data = subset(tmp, breeding_stage != 'laying' & breeding_stage != 'incubation'))
m = lmer(time_to_sunset_min.female ~ time_to_sunset_min.male + (1|breedingAttempt), data = subset(tmp, breeding_stage != 'laying' & breeding_stage != 'incubation'))
plot(m)
hist(resid(m))
pbcoord2 = summary(m)
#----> positive both morning and evening, but bad fit for the morning! Check again

#b. EPP - see Marilia's project


#7. Male dawn song (for females only)
#tight fit with male song: does male emergence predict female emergence across the year?
tmp = subset(x, breeding_stage != 'nonBreeding')
tmp[, both_sleeping := length(box.breeding), by = list(date_, breedingAttempt)]
tmp = subset(tmp, both_sleeping == 2)
tmp = merge(subset(tmp, sex == 1), subset(tmp, sex == 2), by = c('year_', 'box.breeding', 'firstEgg', 'date_', 'sleep', 'nest_start', 'temperature', 'humidity', 'nest_completed', 'yearDay', 'breeding_stage', 'z_firstEgg', 'breedingAttempt'), suffixes = c('.male', '.female'))
tmp[, layingIncubatingYN := ifelse(breeding_stage %in% c('laying', 'incubation'), 1, 0)]

m = lmer(time_to_sunrise_min.female ~ time_to_sunrise_min.male + (1|breeding_stage) + (1|yearDay) + (1|breedingAttempt), data = tmp)
hist(resid(m))
summary(m)

mcheck = lmer(time_to_sunrise_min.female ~ time_to_sunrise_min.male + (1|breeding_stage) + (1|yearDay) + (1|breedingAttempt), data = subset(tmp, time_to_sunrise.male <= 4000 & time_to_sunrise.female <= 4000)) #because of residual distribution)

m = lmer(time_to_sunrise_min.female ~ time_to_sunrise_min.male+layingIncubatingYN+temperature+humidity+ (1|breeding_stage) + (1|yearDay) + (1|breedingAttempt), data = tmp)
hist(resid(m))
plot(m)
dsmf = summary(m)
#---> clearly yes, even when correcting for yearDay, temperature and humidity

#8. Change in sunrise (see above)



#9. EPP
#see Marilia's project

####################################
####################################
####################################

#repeatabilities/correlations
#1. correlations from one day to the next
tmp = subset(x, rel_day - lag1_rel_day == 1)
#tmp[, fac := paste(lag1_rel_day_emergence_min,breeding_stage)]
tmp[sex == 1, sex2 := 'male']
tmp[sex == 2, sex2 := 'female']

OUT = list()
for(i in -20 : 41) {
  print(i)
  rm(m1, m2, m3)
  try({m1 = lmer(time_to_sunrise_min ~ 0+lag1_rel_day_emergence_min + (1|ID), data = subset(tmp, rel_day == i & sex == 1))}, silent = TRUE)
  try({m2 = lmer(time_to_sunrise_min ~ 0+lag1_rel_day_emergence_min + (1|ID), data = subset(tmp, rel_day == i & sex == 2))}, silent = TRUE)

  if(exists('m1')) a1 = coefficients(summary(m1))[1,] else a1 = c(NA, NA, NA)
  if(exists('m2')) a2 = coefficients(summary(m2))[1,] else a2 = c(NA, NA, NA)

  out = data.table(rbind(a1, a2))
  out[, rel_day := i]
  out[, sex := c('male', 'female')]
  out[, Nobs := c(ifelse(exists('m1'), m1@pp$LamtUt@Dim[2], c(NA)),
                  ifelse(exists('m2'), m2@pp$LamtUt@Dim[2], c(NA)))]
  out[, Nind := c(ifelse(exists('m1'), m1@pp$LamtUt@Dim[1], c(NA)),
                  ifelse(exists('m2'), m2@pp$LamtUt@Dim[1], c(NA)))]

  OUT[[length(OUT)+1]] = out
}

OUT = rbindlist(OUT)
setnames(OUT, names(OUT), make.names(names(OUT)))
OUT[, lower := Estimate - 1.96*Std..Error]
OUT[, upper := Estimate + 1.96*Std..Error]

#remove all datapoints that are based on less than 15 individuals
OUT[Nind < 15, ':=' (Estimate = NA, Std..Error = NA, t.value = NA, lower = NA, upper = NA)]
OUT[sex == 'male', offset := +0.3]
OUT[sex == 'female', offset := -0.3]

jpeg(filename = "lag1 correlations.jpg", width = 720, height = 480)
plot(c(0.5, 124.5), c(-0, 1.3), type = 'n', xaxt = 'n', xlab = 'day to first egg', ylab = 'correlation of 1 lag')
points((1:124)+OUT[,offset], OUT[, Estimate], col = c('red', 'blue'), pch = 16)
arrows((1:124)+OUT[,offset], OUT[, upper], (1:124)+OUT[,offset], OUT[, lower], angle = 90, length = 0.05, code = 3, col = c('red', 'blue'))
abline(h = c(0,1), lty = 3)
abline(v = c((0:12)*10 + 1.5), lty = 3)
axis(1, at = c((0:12)*10 + 1.5), labels = c((-4:8)*5))
dev.off()

#from day of first egg to hatch date
tmp = subset(x, rel_day == 0 | date_ == hatchDate)
setkey(tmp, year_, box.breeding, ID, date_)
tmp[, lagHatch_rel_day := shift(rel_day), by = list(year_, box.breeding, ID)]
tmp[, lagHatch_rel_day_emergence_min := shift(time_to_sunrise_min), by = list(year_, box.breeding, ID)]
tmp = subset(tmp, !is.na(lagHatch_rel_day_emergence_min))


m1 = lm(time_to_sunrise_min ~ lagHatch_rel_day_emergence_min, data = subset(tmp, sex == 2))
#remove mmultiple individuals
tmp[, IDlength := length(sex), by = list(ID)]
table(tmp$IDlength)
tmp2 = subset(tmp, IDlength < 2)
m1 = lm(time_to_sunrise_min ~ lagHatch_rel_day_emergence_min, data = subset(tmp2, sex == 2))

plot(m1)
hist(resid(m1))
summary(m1)

#from day -1 to hatch date
tmp = subset(x, rel_day == -1 | date_ == hatchDate)
setkey(tmp, year_, box.breeding, ID, date_)
tmp[, lagHatch_rel_day := shift(rel_day), by = list(year_, box.breeding, ID)]
tmp[, lagHatch_rel_day_emergence_min := shift(time_to_sunrise_min), by = list(year_, box.breeding, ID)]
tmp = subset(tmp, !is.na(lagHatch_rel_day_emergence_min))


m1 = lm(time_to_sunrise_min ~ lagHatch_rel_day_emergence_min, data = subset(tmp, sex == 2))
#remove mmultiple individuals
tmp[, IDlength := length(sex), by = list(ID)]
table(tmp$IDlength)
tmp2 = subset(tmp, IDlength < 2)
m1 = lm(time_to_sunrise_min ~ lagHatch_rel_day_emergence_min, data = subset(tmp2, sex == 2))

plot(m1)
hist(resid(m1))
summary(m1)


m = lmer(I(log(time_to_sunrise_min+120)) ~ I(log(lag1_rel_day_emergence_min+120))*sex + (1|ID), data = subset(tmp, rel_day %in% c(-20:-5)))
#---> females emerge later than males and for females the correlation is much weaker

#random slopes/repeatabilities
#throughout the season
m = lmer(time_to_sunrise_min ~ breeding_stage + rel_day*factor(sex, levels = c(2,1)) + (rel_day|ID), data = subset(x, rel_day %in% c(-10:40)))
hist(subset(x, rel_day %in% c(-10:40))$time_to_sunrise_min)
plot(m)
hist(resid(m))
random_ss = summary(m)
#random effects:
#ID nested in sex:
293.140 / (293.140 + 0.139 + 229.517) #56.07%
#random slope of rel_day for ID:
0.139/ (293.140 + 0.139 + 229.517) #0.02%


#day -10 to -2
m = lmer(time_to_sunrise_min ~ I(rel_day+2)*factor(sex, levels = c(2,1)) + (rel_day|ID), data = subset(x, rel_day %in% c(-10:-2) & time_to_sunrise < 80)) #had to be removed for model fit
plot(m)
hist(resid(m))
random_ss_minus10ToMinus2 = summary(m)
#---> females emerged on average 18.73 minutes before sunrise (***)
#---> with each day, females emerged 0.89 minutes earlier (***)
#---> males emerged on average 17.05 minutes before the females (***)
#---> with each day, males emerged 0.11 minutes earlier than expected from the females, and therefore 1.00 minutes earlier (interaction n.s.)
#random effects:
#ID:
121.058 / (121.058 + 1.317 + 64.688) #64.71%
#random slope of rel_day for ID:
1.317 / (121.058 + 1.317 + 64.688) #0.70%

#day -2 to 0
m = lmer(time_to_sunrise_min ~ I(rel_day)*factor(sex, levels = c(2,1)) + (rel_day|ID), data = subset(x, rel_day %in% c(-2:0) & time_to_sunrise < 80)) #had to be removed for model fit
hist(subset(x, rel_day %in% c(-2:0))$time_to_sunrise_min)
plot(m)
hist(resid(m))
random_ss_minus2To0 = summary(m)
#---> females emerged on average 20.35 minutes before sunrise (***)
#---> with each day, females emerged 5.06 minutes later (***)
#---> males emerged on average 16.14 minutes before the females (***)
#---> with each day, males emerged 3.57 minutes earlier than expected from the females, and therefore 5.05 - 3.57 = 1.48 minutes later (interaction trend)
#random effects:
#ID:
23.892 / (23.892 + 2.587 + 97.769) #19.2%
#random slope of rel_day for ID:
2.587 / (23.892 + 2.587 + 97.769) #2.08%


#---> high repeatabilities across the whole season, strong correlations from one day to the next, but weak correlations (if any) across the season
#---> also, weak repeatabilities for any slopes
#---> use emergence times within the relevant time period, which doesn't matter too much (keep seasonal patterns in mind), use as much data as possible, because the behaviour itself is highly variable.


#FITNESS
#fitness in terms of clutch, hatched, fledged
tmp = subset(x, !(breeding_stage %in% c('nonBreeding', 'postFledging')))
tmp[, z_time_to_sunrise_min := scale(time_to_sunrise_min), by = list(year_, rel_day)]
tmp[, mean_z_time_to_sunrise_min := mean(z_time_to_sunrise_min, na.rm = TRUE), by = list(year_, ID, breeding_stage)]
tmp = subset(tmp, select = c('mean_z_time_to_sunrise_min', 'clutch', 'fledged', 'ID', 'hatched', 'z_firstEgg', 'sex', 'year_', 'breeding_stage', 'fac_breeding_stage'))
tmp = unique(tmp, by = names(tmp))


###CHANGE VARIABLE HERE:
tmp[, VAR := clutch]
runthrough = unique(paste(na.omit(tmp$sex), na.omit(tmp$breeding_stage)))
runthrough = strsplit(runthrough, " ")
DD = list()
for(i in 1 : length(runthrough)) {
  print(i)
  m = lmer(VAR ~ mean_z_time_to_sunrise_min+z_firstEgg + (1|ID), data = subset(tmp, VAR >= 4 & sex == runthrough[[i]][1] & breeding_stage == runthrough[[i]][2]))

  hist(resid(m))
  Sys.sleep(0.5)

  plot(resid(m) ~ fitted(m))
  Sys.sleep(0.5)

  dd = data.table(coefficients(summary(m)))
  dd[, var := c('intercept', 'mean_z_time_to_sunrise_min', 'z_firstEgg')]
  dd[, sex := runthrough[[i]][1]]
  dd[, breeding_stage := runthrough[[i]][2]]
  DD[[length(DD)+1]] = dd
}
DD = rbindlist(DD)
DD[, fac_breeding_stage := factor(breeding_stage, levels =
                                   c('territoryEstablishment', 'nestBuilding', 'nestCompleted', 'preLaying', 'laying', 'incubation', 'earlyProvisioning', 'lateProvisioning'))]
setkey(DD, fac_breeding_stage)

boxplot(subset(DD, var == 'z_firstEgg')$Estimate)
boxplot(subset(DD, var == 'z_firstEgg')$'t value')
#all estimates for first egg negative all but one significant or borderline

DD = subset(DD, var == 'mean_z_time_to_sunrise_min')

jpeg(filename = "fitness_clutch_emergence.jpg", width = 720, height = 480)
par(mar = c(10.1, 4.1, 4.1, 2.1))
plot(c(0.5, 16.5), c(-1, 1), type = 'n', xaxt = 'n', xlab = '', ylab = 'Estimate clutch ~ emergence')
points(1:16, DD$Estimate, col = DD$sex, pch = 16)
arrows(1:16, DD$Estimate - 1.96*DD$`Std. Error`, 1:16, DD$Estimate + 1.96*DD$`Std. Error`, col = DD$sex, angle = 90, length = 0.05, code = 3)
abline(h = 0, lty = 3)
axis(1, at = c((1:8)*2 - 0.5), labels = levels(DD$fac_breeding_stage), las = 3)
dev.off()
#---> CLUTCH: no differences between the individual time frames, negative slpoe for females during late provisioning only: later emerging females had smaller clutches during that time frame only.

###hatched
tmp[, VAR := hatched]
runthrough = unique(paste(na.omit(tmp$sex), na.omit(tmp$breeding_stage)))
runthrough = strsplit(runthrough, " ")
DD = list()
for(i in 1 : length(runthrough)) {
  print(i)
  m = lmer(VAR ~ mean_z_time_to_sunrise_min+z_firstEgg + (1|ID), data = subset(tmp, VAR >= 4 & sex == runthrough[[i]][1] & breeding_stage == runthrough[[i]][2]))

  hist(resid(m))
  Sys.sleep(0.5)

  plot(resid(m) ~ fitted(m))
  Sys.sleep(0.5)

  dd = data.table(coefficients(summary(m)))
  dd[, var := c('intercept', 'mean_z_time_to_sunrise_min', 'z_firstEgg')]
  dd[, sex := runthrough[[i]][1]]
  dd[, breeding_stage := runthrough[[i]][2]]
  DD[[length(DD)+1]] = dd
}
DD = rbindlist(DD)
DD[, fac_breeding_stage := factor(breeding_stage, levels =
                                    c('territoryEstablishment', 'nestBuilding', 'nestCompleted', 'preLaying', 'laying', 'incubation', 'earlyProvisioning', 'lateProvisioning'))]
setkey(DD, fac_breeding_stage)

boxplot(subset(DD, var == 'z_firstEgg')$Estimate)
boxplot(subset(DD, var == 'z_firstEgg')$'t value')
#all estimates for first egg negative all but one significant or borderline

DD = subset(DD, var == 'mean_z_time_to_sunrise_min')

jpeg(filename = "fitness_hatched_emergence.jpg", width = 720, height = 480)
par(mar = c(10.1, 4.1, 4.1, 2.1))
plot(c(0.5, 16.5), c(-1, 1), type = 'n', xaxt = 'n', xlab = '', ylab = 'Estimate hatched ~ emergence')
points(1:16, DD$Estimate, col = DD$sex, pch = 16)
arrows(1:16, DD$Estimate - 1.96*DD$`Std. Error`, 1:16, DD$Estimate + 1.96*DD$`Std. Error`, col = DD$sex, angle = 90, length = 0.05, code = 3)
abline(h = 0, lty = 3)
axis(1, at = c((1:8)*2 - 0.5), labels = levels(DD$fac_breeding_stage), las = 3)
dev.off()
#---> HATCHED: not much differences between the individual time frames, negative slope for females during early and late provisioning: later emerging females had fewer hatchlings during that time frame only. Positive slope for males during territory establishment: later emerging males had lmore hatchlings.

##fledged
tmp[, VAR := fledged]
runthrough = unique(paste(na.omit(tmp$sex), na.omit(tmp$breeding_stage)))
runthrough = strsplit(runthrough, " ")
DD = list()
for(i in 1 : length(runthrough)) {
  print(i)
  m = lmer(VAR ~ mean_z_time_to_sunrise_min+z_firstEgg + (1|ID), data = subset(tmp, VAR >= 4 & sex == runthrough[[i]][1] & breeding_stage == runthrough[[i]][2]))

  hist(resid(m))
  Sys.sleep(0.5)

  plot(resid(m) ~ fitted(m))
  Sys.sleep(0.5)

  dd = data.table(coefficients(summary(m)))
  dd[, var := c('intercept', 'mean_z_time_to_sunrise_min', 'z_firstEgg')]
  dd[, sex := runthrough[[i]][1]]
  dd[, breeding_stage := runthrough[[i]][2]]
  DD[[length(DD)+1]] = dd
}
DD = rbindlist(DD)
DD[, fac_breeding_stage := factor(breeding_stage, levels =
                                    c('territoryEstablishment', 'nestBuilding', 'nestCompleted', 'preLaying', 'laying', 'incubation', 'earlyProvisioning', 'lateProvisioning'))]
setkey(DD, fac_breeding_stage)

boxplot(subset(DD, var == 'z_firstEgg')$Estimate)
boxplot(subset(DD, var == 'z_firstEgg')$'t value')
#all estimates for first egg negative all but one significant or borderline

DD = subset(DD, var == 'mean_z_time_to_sunrise_min')

jpeg(filename = "fitness_fledged_emergence.jpg", width = 720, height = 480)
par(mar = c(10.1, 4.1, 4.1, 2.1))
plot(c(0.5, 16.5), c(-1, 1), type = 'n', xaxt = 'n', xlab = '', ylab = 'Estimate fledged ~ emergence')
points(1:16, DD$Estimate, col = DD$sex, pch = 16)
arrows(1:16, DD$Estimate - 1.96*DD$`Std. Error`, 1:16, DD$Estimate + 1.96*DD$`Std. Error`, col = DD$sex, angle = 90, length = 0.05, code = 3)
abline(h = 0, lty = 3)
axis(1, at = c((1:8)*2 - 0.5), labels = levels(DD$fac_breeding_stage), las = 3)
dev.off()
#---> FLEDGED: not much differences between the individual time frames, negative slope for females during early and late provisioning: later emerging females had fewer fledglings during that time frame only. Positive slope for males during territory establishment and for females during preLaying: later emerging males/females had more fledglings.

#across the season
tmp[, VAR := clutch]
m = lmer(VAR ~ mean_z_time_to_sunrise_min*sex+z_firstEgg + (1|ID), data = subset(tmp, VAR >= 4))
hist(resid(m))
plot(m)
fitness_clutch_ss = summary(m)
#CLUTCH: no effect across all data (only that early breeders have larger clutches.)
tmp[, VAR := hatched]
m = lmer(VAR ~ mean_z_time_to_sunrise_min*sex+z_firstEgg + (1|ID), data = subset(tmp, VAR >= 4))
hist(resid(m))
plot(m)
fitness_hatched_ss = summary(m)
#HATCHED: no effect across all data (only that early breeders have more hatchlings.)
tmp[, VAR := fledged]
m = lmer(VAR ~ mean_z_time_to_sunrise_min*sex+z_firstEgg + (1|ID), data = subset(tmp, VAR >= 4))
hist(resid(m))
plot(m)
fitness_fledged_ss = summary(m)
#FLEDGED: no effect across all data (only that early breeders have more fledglings.)




m = lmer(clutch ~ mean_z_time_to_sunrise_min*breeding_stage+z_firstEgg + (1|ID), data = subset(tmp, clutch >= 4 & sex == 2))
hist(resid(m))
plot(m)
fitness_clutch_ss_f = summary(m)

m = lmer(clutch ~ 0+mean_z_time_to_sunrise_min+z_firstEgg + (1|year_) + (1|ID), data = subset(tmp, clutch >= 4 & sex == 2))
hist(resid(m))
plot(m)
fitness_clutch_ss_m = summary(m)
#---> late breeding females and males have smaller clutches
#---> late emergering females and males don't have smaller clutches


m = lmer(hatched ~ mean_z_time_to_sunrise_min+z_firstEgg + (1|ID), data = subset(tmp, hatched >= 4 & sex == 2))
fitness_hatched_ss_f = summary(m)

m = lmer(hatched ~ mean_z_time_to_sunrise_min+z_firstEgg + (1|ID) + (1|year_), data = subset(tmp, hatched >= 4 & sex == 1))

hist(resid(m))
plot(m)
fitness_hatched_ss_m = summary(m)
#---> late breeding females and males have fewer hatchlings
#---> late emerging females have fewer hatchlings, no effects for males

m = lmer(fledged ~ mean_z_time_to_sunrise_min+z_firstEgg + (1|ID), data = subset(tmp, fledged >= 4 & sex == 2))
fitness_fledged_ss_f = summary(m)

m = lmer(fledged ~ mean_z_time_to_sunrise_min+z_firstEgg + (1|ID), data = subset(tmp, fledged >= 4 & sex == 1 ))
hist(resid(m))
plot(m)
fitness_fledged_ss_m = summary(m)

#---> late breeders have fewer fledglings
#---> late emergers don't have fewer/more fledglings


#emergence on day -1
tmp = subset(x, rel_day == -2)
tmp[, z_time_to_sunrise_min := scale(time_to_sunrise_min), by = year_]
m = lmer(clutch ~ z_time_to_sunrise_min + (1|ID), data = subset(tmp, clutch >= 6))
m = lmer(clutch ~ z_firstEgg + (1|ID), data = subset(tmp, clutch >= 4 & sex == 2))
m = lmer(hatched ~ z_firstEgg + (1|ID), data = subset(tmp, clutch >= 4 & sex == 2))
m = lmer(fledged ~ z_firstEgg + (1|ID), data = subset(tmp, clutch >= 4 & sex == 2))
hist(resid(m))
plot(m)
summary(m)
#for the females sleeping on day -2 there is no correlaction even between first egg and clutch size/hatched/fledged.

return(list(x, x2))
}
