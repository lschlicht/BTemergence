#when to cut off nest data: 2 weeks: roughly 95% of nests were completed within 2 weeks ###
quantile(x$nest_start - x$nest_completed, prob = 0.05, na.rm = TRUE) #-14



############################OLDOLDOLDOLDOLD
###Table S1: Sex and age and day(te) ####
#1. YDAY/time to sunrise
summary(unlist(var_day.sex.age_T, recursive = FALSE)[[1]]$mer)
#2. YDAY/time to sunset
summary(unlist(var_day.sex.age_T, recursive = FALSE)[[2]]$mer)
#3. rel_day/time to sunrise
summary(unlist(var_day.sex.age_T, recursive = FALSE)[[3]]$mer)
#4. rel_day/time to sunset
summary(unlist(var_day.sex.age_T, recursive = FALSE)[[4]]$mer)

###Table S2: Sex and age and day(te) ####
#1. YDAY/precipitation/sunrise
summary(var_day.sex.env_T[[1]][[1]]$gam)
#2. YDAY/precipitation/sunset
summary(var_day.sex.env_T[[1]][[3]]$gam)
#3. YDAY/temperature/sunrise
summary(var_day.sex.env_T[[1]][[2]]$gam)
#4. YDAY/temperature/sunset
summary(var_day.sex.env_T[[1]][[4]]$gam)

###non-breeders vs. breeders ####


#5. rel_day/precipitation/sunrise ####
summary(var_day.sex.env_T[[2]][[1]]$gam)
#6. rel_day/precipitation/sunset
summary(var_day.sex.env_T[[2]][[3]]$gam)
#7. rel_day/temperature/sunrise
summary(var_day.sex.env_T[[2]][[2]]$gam)
#8. rel_day/temperature/sunset
summary(var_day.sex.env_T[[2]][[4]]$gam)



#2. YDAY/time to sunset ####
summary(unlist(var_day.sex.age_T, recursive = FALSE)[[2]]$mer)
#3. rel_day/time to sunrise
summary(unlist(var_day.sex.age_T, recursive = FALSE)[[3]]$mer)
#4. rel_day/time to sunset
summary(unlist(var_day.sex.age_T, recursive = FALSE)[[4]]$mer)




####
for(i in 1 : length(z_var_day.sex.env)) {
  print("##############################")
  print(summary(z_var_day.sex.env[[i]]$gam))
}




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
} #not great, but ok given side-point

for(i in 1 : length(var_day.sex.env)) {
  print(i)
  plot_gamm_models(var_day.sex.env[[i]])
  Sys.sleep(3)
} #perfect

for(i in 1 : length(var_day.sex3.env)) {
  print(i)
  plot_gamm_models(var_day.sex3.env[[i]])
  Sys.sleep(3)
} #perfect

for(i in 1 : length(z_var_day.sex.env.age)) {
  print(i)
  plot_gamm_models(z_var_day.sex.env.age[[i]])
  Sys.sleep(3)
} #very good

for(i in 1 : length(z_var_day.sex.env)) {
  print(i)
  plot_gamm_models(z_var_day.sex.env[[i]])
  Sys.sleep(3)
}

for(i in 1 : length(z_var_day.sex3.env)) {
  print(i)
  plot_gamm_models(z_var_day.sex3.env[[i]])
  Sys.sleep(3)
}




#overall sample sizes: ####
nrow(x)
length(unique(x$ID))
length(unique(x$breedingAttempt))
length(unique(x$year_))
unique(x$year_)
nrow(x)/length(unique(x$year_))
nrow(x)/length(unique(x$ID))
min(sort(table(x$ID)))
max(sort(table(x$ID)))
min(sort(table(x$breedingAttempt)))
max(sort(table(x$breedingAttempt)))

ss = unique(subset(x, select = c('ID', 'year_', 'sex', 'age')))
table(ss$sex, ss$age)
nrow(x2)
nrow(x)
nrow(x2)/nrow(x)

#table 1: order: sunrise, sunset, proportion ####
for(i in 1 : length(var_day.sex.age)) {
  print("##############################")
  print(summary(var_day.sex.age2[[i]]$mer))
}

#Table S1 ####
#see run models: 0. Environmental variables

#Table 2 ####
for(i in 1 : length(var_day.sex.env)) {
  print("##############################")
  print(summary(var_day.sex.env[[i]]$gam))
}

for(i in 1 : length(var_day.sex3.env)) {
  print("##############################")
  print(summary(var_day.sex3.env[[i]]$gam))
}

range(x2[, avgTemperature], na.rm = TRUE)
range(x2[, avgRainfall], na.rm = TRUE)
