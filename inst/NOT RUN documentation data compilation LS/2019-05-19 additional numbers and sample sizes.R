#when to cut off nest data: 2 weeks: roughly 95% of nests were completed within 2 weeks ###
quantile(x$nest_start - x$nest_completed, prob = 0.05, na.rm = TRUE) #-14

###non-breeders vs. breeders ####????
table(unique(x[, list(breeder, ID, breeding_season)])[, breeder])

###Table S1: Sex and age and day(te) ####
for(i in 1 : length(var_day.sex.age)) {
  print(i)
  print(summary(var_day.sex.age[[i]]$gam))
  tmp = subset(var_day.sex.age[[i]]$gam$model, select = c('sex', 'age', 'ID'))
  print(table(tmp$sex, tmp$age))
  tmp = unique(tmp)
  print(table(tmp$sex, tmp$age))
}
###Table S2: Sex and age and day(te) ####
#1. YDAY/precipitation/sunrise
summary(var_day.sex.env_T[[1]][[1]]$gam)
tmp = (var_day.sex.env_T[[1]][[1]]$gam$model)
nrow(tmp)
length(unique(tmp$ID))

#2. YDAY/precipitation/sunset
summary(var_day.sex.env_T[[1]][[3]]$gam)
tmp = (var_day.sex.env_T[[1]][[3]]$gam$model)
nrow(tmp)
length(unique(tmp$ID))

#3. YDAY/temperature/sunrise
summary(var_day.sex.env_T[[1]][[2]]$gam)
tmp = (var_day.sex.env_T[[1]][[2]]$gam$model)
nrow(tmp)
length(unique(tmp$ID))

#4. YDAY/temperature/sunset
summary(var_day.sex.env_T[[1]][[4]]$gam)
tmp = (var_day.sex.env_T[[1]][[4]]$gam$model)
nrow(tmp)
length(unique(tmp$ID))




#5. rel_day/precipitation/sunrise ####
summary(var_day.sex.env_T[[2]][[1]]$gam)
tmp = (var_day.sex.env_T[[2]][[1]]$gam$model)
nrow(tmp)
length(unique(tmp$ID))

#6. rel_day/precipitation/sunset
summary(var_day.sex.env_T[[2]][[3]]$gam)
tmp = (var_day.sex.env_T[[2]][[3]]$gam$model)
nrow(tmp)
length(unique(tmp$ID))

#7. rel_day/temperature/sunrise
summary(var_day.sex.env_T[[2]][[2]]$gam)
tmp = (var_day.sex.env_T[[2]][[2]]$gam$model)
nrow(tmp)
length(unique(tmp$ID))

#8. rel_day/temperature/sunset
summary(var_day.sex.env_T[[2]][[4]]$gam)
tmp = (var_day.sex.env_T[[2]][[4]]$gam$model)
nrow(tmp)
length(unique(tmp$ID))


#Further sample sizes/Methods
############################NEWNEWNEWNEWNEW
#overall sample sizes: ####
nrow(x)   #38627
nrow(x2)  #35853
nrow(x2)/nrow(x) #0.93
nrow(x) - nrow(x2)

length(unique(x2$ID))
length(unique(x2$breedingAttempt))
length(unique(x2$year_))
unique(x2$year_)
nrow(x2)/length(unique(x2$year_))
nrow(x2)/length(unique(x2$ID))
nrow(x2)/length(unique(x2$ID))
mean(rle(sort(x2$ID))$lengths)
median(rle(sort(x2$ID))$lengths)
min(sort(table(x2$ID)))
max(sort(table(x2$ID)))
min(sort(table(x2[ , paste(breeding_season, ID)])))
max(sort(table(x2[ , paste(breeding_season, ID)])))

ss = unique(subset(x2, select = c('ID', 'year_', 'sex', 'age')))
table(ss$sex, ss$age)


range(x2[, avgTemperature], na.rm = TRUE)
range(x2[, precipitation_dawn], na.rm = TRUE)
range(x2[, precipitation_dusk], na.rm = TRUE)

table(x$breeding_season)
table(unique(subset(x2, select = c('breeding_season', 'ID')))$breeding_season)
table(x2$breeding_season)


#check model performance ####
for(i in 1 : length(var_day.sex)) {
print(i)
plot_gamm_models(var_day.sex[[i]])
Sys.sleep(3)
} #perfect

for(i in 1 : length(var_day.sex.age)) {
  print(i)
  plot_gamm_models(var_day.sex.age[[i]])
  Sys.sleep(3)
} #some not great, but most perfect

for(i in 1 : length(var_day.sex.env_T)) {
  print(i)
  plot_gamm_models(var_day.sex.env_T[[i]])
  Sys.sleep(3)
} #perfect

