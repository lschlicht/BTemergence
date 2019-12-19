#0. Environmental variables ####
ENV2 = dcast(x2, date_ ~ ., value.var = c("temperature_dawn", "temperature_dusk", "avgTemperature", "precipitation_dawn", "precipitation_dusk", "avgRainfall", "humidity_dawn", "humidity_dusk", "avgHumidity", "precipitation_dawn", "precipitation_dusk", "temperature_dawn", "temperature_dusk"), fun = mean, na.rm = TRUE)
m1 = lm(avgTemperature ~ avgHumidity, data = ENV2)
summary(m1)#24%
m2 = lm(avgTemperature ~ avgRainfall, data = ENV2)
summary(m2) #0%
m6 = lm(avgHumidity ~ avgRainfall, data = ENV2)
summary(m6) #8%
m = lm(avgHumidity ~ avgTemperature + avgRainfall, data = ENV2)
plot(m)
summary(m) #33%

cor(ENV2$avgTemperature, ENV2$avgHumidity, use = "pairwise.complete.obs") #0.49
cor(ENV2$avgRainfall, ENV2$avgHumidity, use = "pairwise.complete.obs") #0.28
cor(ENV2$avgTemperature, ENV2$avgRainfall, use = "pairwise.complete.obs") #0.05

cor(ENV2$avgTemperature, ENV2$temperature_dawn, use = "pairwise.complete.obs")  #0.90
cor(ENV2$avgTemperature, ENV2$temperature_dusk, use = "pairwise.complete.obs") #0.98

cor(ENV2$avgRainfall, ENV2$precipitation_dawn, use = "pairwise.complete.obs")  #0.07
cor(ENV2$avgRainfall, ENV2$precipitation_dusk, use = "pairwise.complete.obs") #0.58

#1. Selection of "k": CAREFUL, THIS ONLY SELECTS K AND TAKES VERY LONG TO COMPUTE! ####

runthrough = data.table(expand.grid(k_gamm = c(10, 20, 40, 80), response = c("time_to_sunrise_min", "time_to_sunset_min"), stringsAsFactors = FALSE))

select.k1 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], by_gamm = "sex_age", k_gamm = variables[1], explanatory = c(NULL), data = data), data = subset(x2, YDAY < 90) )

select.k2 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], by_gamm = "sex_age", k_gamm = variables[1], explanatory = c(NULL), data = data), data = subset(x2, rel_day >= -21) )

select.k = list(select.k1, select.k2)
save(select.k, file = paste0(getwd(), "/models/select.k.RData"))

for(i in 1 : length(select.k1)) { print("################################"); print(summary(select.k1[[i]]$gam)) }
for(i in 1 : length(select.k)) { print("################################"); print(summary(select.k1[[i]]$mer)) }

#----> k=20 would probably suffice, but edf not "much" smaller than k, so to be on the safe side use k=40 as a default, because doubling k does not improve/change the model further after this. This k is implemented as default value into the function.


K = 40
#Figure 1-3. variable ~ day, split by sex ####
runthrough = c("time_to_sunrise_min", "time_to_sunset_min", "daylength")

sr_m1 = gamm4(time_to_sunrise_min ~ s(YDAY, bs = 'tp', k = K), random = ~(1|ID), data = subset(x2, YDAY < 90 & sex == 1))
sr_m2 = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp', k = K), random = ~(1|ID), data = subset(x2, rel_day >= -21 & sex == 1))
sr_f1 = gamm4(time_to_sunrise_min ~ s(YDAY, bs = 'tp', k = K), random = ~(1|ID), data = subset(x2, YDAY < 90 & sex == 2))
sr_f2 = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp', k = K), random = ~(1|ID), data = subset(x2, rel_day >= -21 & sex == 2))

ss_m1 = gamm4(time_to_sunset_min ~ s(YDAY, bs = 'tp', k = K), random = ~(1|ID), data = subset(x2, YDAY < 90 & sex == 1))
ss_m2 = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp', k = K), random = ~(1|ID), data = subset(x2, rel_day >= -21 & sex == 1))
ss_f1 = gamm4(time_to_sunset_min ~ s(YDAY, bs = 'tp', k = K), random = ~(1|ID), data = subset(x2, YDAY < 90 & sex == 2))
ss_f2 = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp', k = K), random = ~(1|ID), data = subset(x2, rel_day >= -21 & sex == 2))

act_m1 = gamm4(daylength ~ s(YDAY, bs = 'tp', k = K), random = ~(1|ID), data = subset(x2, YDAY < 90 & sex == 1))
act_m2 = gamm4(daylength ~ s(rel_day, bs = 'tp', k = K), random = ~(1|ID), data = subset(x2, rel_day >= -21 & sex == 1))
act_f1 = gamm4(daylength ~ s(YDAY, bs = 'tp', k = K), random = ~(1|ID), data = subset(x2, YDAY < 90 & sex == 2))
act_f2 = gamm4(daylength ~ s(rel_day, bs = 'tp', k = K), random = ~(1|ID), data = subset(x2, rel_day >= -21 & sex == 2))

var_day.sex = list(sr_m1, sr_m2, sr_f1, sr_f2, ss_m1, ss_m2, ss_f1, ss_f2, act_m1, act_m2, act_f1, act_f2)
save(var_day.sex, file = paste0(getwd(), "/models/var_day.sex_F.RData"))

#Table S1. variable ~ day split by age and sex ####
#runthrough = data.table(expand.grid(age = c(1,2), sex = c(1,2)))
L = list()
#for(i in 1:nrow(runthrough)) {
  tmp = subset(x2, YDAY < 90)# & sex == runthrough[i, sex] & age == runthrough[i, age])
  L[[length(L)+1]] = gamm4(time_to_sunrise_min ~ s(YDAY, bs = 'tp', k = K)+factor(sex)*factor(age), random = ~(1|ID), data = tmp)
  L[[length(L)+1]] = gamm4(time_to_sunset_min ~ s(YDAY, bs = 'tp', k = K)+factor(sex)*factor(age), random = ~(1|ID), data = tmp)

  tmp = subset(x2, rel_day >= -21)# & sex == runthrough[i, sex] & age == runthrough[i, age])
  L[[length(L)+1]] = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp', k = K)+factor(sex)*factor(age), random = ~(1|ID), data = tmp)
  L[[length(L)+1]] = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp', k = K)+factor(sex)*factor(age), random = ~(1|ID), data = tmp)
#}

var_day.sex.age = L

save(var_day.sex.age, file = paste0(getwd(), "/models/var_day.sex.age_T.RData"))

#Figure 4: variable ~ day.sex.age; intercept removed ####
runthrough = c("time_to_sunrise_min", "time_to_sunset_min")

var_day.sex.age_F = list(
      lapply(runthrough, FUN = function(variables, data) gamm4.2(response = variables[1], explanatory = 'sex_age', k_gamm = K, by_gamm = "sex2", data = data, rm.intercept = TRUE, smooth_over = 'YDAY' ), data = subset(x2, YDAY < 90)),
      lapply(runthrough, FUN = function(variables, data) gamm4.2(response = variables[1], explanatory = 'sex_age', k_gamm = K, by_gamm = "sex2", data = data, rm.intercept = TRUE, smooth_over = 'rel_day' ), data = subset(x2, rel_day >= -21))
)
save(var_day.sex.age_F, file = paste0(getwd(), "/models/var_day.sex.age_F.RData"))



#Figure 5. variable ~ day.sex.age.env; scaled environment #####
runthrough = data.table(explanatory = c(
  "z_precipitation_dawn*sex2*age2", "z_precipitation_dusk*sex2*age2","z_avgRainfall*sex2*age2", "z_avgTemperature*sex2*age2"),
  response = c(rep("z_time_to_sunrise_min",4), rep("z_time_to_sunset_min",4)), stringsAsFactors = FALSE)

z_var_day.sex.env1 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'YDAY', rm.intercept = TRUE), data = subset(x2, YDAY < 90 & !is.na(age)) )
z_var_day.sex.env2 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'rel_day', rm.intercept = TRUE), data = subset(x2, rel_day >= -21) )
z_var_day.sex.env = list(z_var_day.sex.env1, z_var_day.sex.env2)
save(z_var_day.sex.env, file = paste0(getwd(), "/data/z_var_day.sex.env_F_withage.RData"))


models = unlist(z_var_day.sex.env, recursive = FALSE)
for(i in 1 : length(models)) {
  print("##############################################################")
  print(summary(models[[i]]$gam))
}
#no major effects of age, only one that may be relevant (see MS)
#---> remove three way interaction with age and run again

#Results without threeway interactions. variable ~ day.sex.age.env; scaled environment; without three-way interaction #####
runthrough = data.table(explanatory = c(
  "z_precipitation_dawn*sex2 + z_precipitation_dawn*age2 + sex2*age2","z_avgTemperature*sex2 + z_avgTemperature*age2 + sex2*age2", "z_precipitation_dusk*sex2 + z_precipitation_dusk*age2 + sex2*age2", "z_avgTemperature*sex2 + z_avgTemperature*age2 + sex2*age2"),
  response = c(rep("z_time_to_sunrise_min",2), rep("z_time_to_sunset_min",2)), stringsAsFactors = FALSE)

z_var_day.sex.env1 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'YDAY', rm.intercept = TRUE), data = subset(x2, YDAY < 90 & !is.na(age)) )
z_var_day.sex.env2 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'rel_day', rm.intercept = TRUE), data = subset(x2, rel_day >= -21) )
z_var_day.sex.env_no3wayint = list(z_var_day.sex.env1, z_var_day.sex.env2)
save(z_var_day.sex.env_no3wayint, file = paste0(getwd(), "/data/z_var_day.sex.env_F_withageno3wayint.RData"))


models = unlist(z_var_day.sex.env_no3wayint, recursive = FALSE)
for(i in 1 : length(models)) {
  print("##############################################################")
  print(summary(models[[i]]$gam))
}
#no major effects of age, only one that may be relevant (see MS)
#---> remove age


######for males ######
runthrough = data.table(explanatory = c(
  "z_precipitation_dawn*sex2","z_avgRainfall*sex2", "z_avgTemperature*sex2",
  "z_precipitation_dusk*sex2","z_avgRainfall*sex2", "z_avgTemperature*sex2"),
  response = c(rep("z_time_to_sunrise_min",3), rep("z_time_to_sunset_min",3)), stringsAsFactors = FALSE)

z_var_day.sex.env1 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'YDAY', rm.intercept = TRUE), data = subset(x2, YDAY < 90 & !is.na(age)) )
z_var_day.sex.env2 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'rel_day', rm.intercept = TRUE), data = subset(x2, rel_day >= -21) )
z_var_day.sex.env_males = list(z_var_day.sex.env1, z_var_day.sex.env2)
save(z_var_day.sex.env_males, file = paste0(getwd(), "/data/z_var_day.sex.env_males_F.RData"))

######for females ######
x2[, sex3 := factor(sex2, levels = c("2", "1"))]
runthrough = data.table(explanatory = c(
  "z_precipitation_dawn*sex3","z_avgRainfall*sex3", "z_avgTemperature*sex3",
  "z_precipitation_dusk*sex3","z_avgRainfall*sex3", "z_avgTemperature*sex3"),
  response = c(rep("z_time_to_sunrise_min",3), rep("z_time_to_sunset_min",3)), stringsAsFactors = FALSE)

z_var_day.sex.env1 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'YDAY', rm.intercept = TRUE), data = subset(x2, YDAY < 90 & !is.na(age)) )
z_var_day.sex.env2 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'rel_day', rm.intercept = TRUE), data = subset(x2, rel_day >= -21) )
z_var_day.sex.env_females = list(z_var_day.sex.env1, z_var_day.sex.env2)
save(z_var_day.sex.env_females, file = paste0(getwd(), "/data/z_var_day.sex.env_females_F.RData"))



models = unlist(z_var_day.sex.env, recursive = FALSE)
for(i in 1 : length(models)) {
  print("##############################################################")
  print(summary(models[[i]]$gam))
}

#Table S2. variable ~ day.sex.age.env; non-scaled environment #####
runthrough = data.table(explanatory = c(
  "precipitation_dawn*sex2", "precipitation_dusk*sex2","avgRainfall*sex2", "avgTemperature*sex2"),
  response = c(rep("time_to_sunrise_min",4), rep("time_to_sunset_min",4)), stringsAsFactors = FALSE)
runthrough = runthrough[c(-2, -3, -5, -7)] #to run avg rainfall, don't remove rows 3 and 7
var_day.sex.env1 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'YDAY', rm.intercept = FALSE), data = subset(x2, YDAY < 90) )
var_day.sex.env2 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'rel_day', rm.intercept = FALSE), data = subset(x2, rel_day >= -21) )
var_day.sex.env_T = list(var_day.sex.env1, var_day.sex.env2)
save(var_day.sex.env_T, file = paste0(getwd(), "/data/var_day.sex.env_T.RData"))

# Repeatabilities #####
runthrough = data.table(explanatory = c(
  "z_precipitation_dawn + z_avgTemperature", "z_precipitation_dusk + z_avgTemperature"),
  response = c("z_time_to_sunrise_min", "z_time_to_sunset_min"), stringsAsFactors = FALSE)
K = 40
z_var_day.sex.env.rpt1 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "age", explanatory = variables[1], data = data, smooth_over = 'YDAY', rm.intercept = TRUE), data = subset(x2, YDAY < 90 & !is.na(age) & sex == 1) )
z_var_day.sex.env.rpt2 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "age", explanatory = variables[1], data = data, smooth_over = 'YDAY', rm.intercept = TRUE), data = subset(x2, YDAY < 90 & !is.na(age) & sex == 2) )
z_var_day.sex.env.rpt3 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "age", explanatory = variables[1], data = data, smooth_over = 'rel_day', rm.intercept = TRUE), data = subset(x2, rel_day >= -21 & sex == 1) )
z_var_day.sex.env.rpt4 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], k_gamm = K, by_gamm = "age", explanatory = variables[1], data = data, smooth_over = 'rel_day', rm.intercept = TRUE), data = subset(x2, rel_day >= -21 & sex == 2) )


z_var_day.sex.env.rpt = list(z_var_day.sex.env.rpt1, z_var_day.sex.env.rpt2, z_var_day.sex.env.rpt3, z_var_day.sex.env.rpt4)
save(z_var_day.sex.env.rpt, file = paste0(getwd(), "/data/z_var_day.sex.env.rpt.RData"))


models = unlist(z_var_day.sex.env.rpt, recursive = FALSE)

for(i in 1 : length(models)) {
tmp = as.data.table(VarCorr(models[[i]]$mer))
tmp = tmp[grp == random_gamm, vcov]/sum(tmp[grp == random_gamm, vcov], tmp[grp == "Residual", vcov])
print(tmp)
}
#order:
#sunrise+ YDAY    + male    : 0.67
#sunset + YDAY    + male    : 0.79
#sunrise+ YDAY    + female  : 0.46
#sunset + YDAY    + female  : 0.37
#sunrise+ rel_day + male    : 0.49
#sunset + rel_day + male    : 0.58
#sunrise+ rel_day + female  : 0.39
#sunset + rel_day + female  : 0.29

#number of distinct

bla = unique(subset(x2, select = c('sex', 'ID', 'yid')))
bla[, N := length(sex), by = ID]
bla = unique(subset(bla, select = c('sex', 'ID', 'N')))
bla[, N2more := ifelse(N > 1, 1, 0)]
table(bla$N, bla$sex)

a = table(bla$N2more, bla$sex)
a[1,1]/sum(a[1,1], a[2,1])
a[1,2]/sum(a[1,2], a[2,2])
sum(a[1,])/sum(a)
sum(a[2,])
