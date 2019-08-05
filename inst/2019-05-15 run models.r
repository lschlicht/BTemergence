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

#1. Selection of "k" ####

runthrough = data.table(expand.grid(k_gamm = c(10, 20, 40, 80), response = c("time_to_sunrise_min", "time_to_sunset_min"), stringsAsFactors = FALSE))

select.k1 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], by_gamm = "sex_age", k_gamm = variables[1], explanatory = c(NULL), data = data), data = subset(x2, YDAY < 90) )

select.k2 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], by_gamm = "sex_age", k_gamm = variables[1], explanatory = c(NULL), data = data), data = subset(x2, rel_day >= -21) )

select.k = list(select.k1, select.k2)
save(select.k, file = paste0(getwd(), "/models/select.k.RData"))

for(i in 1 : length(select.k1)) { print("################################"); print(summary(select.k1[[i]]$gam)) }
for(i in 1 : length(select.k)) { print("################################"); print(summary(select.k1[[i]]$mer)) }

#----> k=20 would probably suffice, but edf not "much" smaller than k, so to be on the safe side use k=40 as a default, because doubling k does not improve/change the model further after this. This k is implemented as default value into the function.

#Figure 1-3. variable ~ day, split by sex ####
runthrough = c("time_to_sunrise_min", "time_to_sunset_min", "daylength")
K = 40
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
K = 40

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
      lapply(runthrough, FUN = function(variables, data) gamm4.2(response = variables[1], explanatory = 'sex_age', by_gamm = "sex2", data = data, rm.intercept = TRUE, smooth_over = 'YDAY' ), data = subset(x2, YDAY < 90)),
      lapply(runthrough, FUN = function(variables, data) gamm4.2(response = variables[1], explanatory = 'sex_age', by_gamm = "sex2", data = data, rm.intercept = TRUE, smooth_over = 'rel_day' ), data = subset(x2, rel_day >= -21))
)
save(var_day.sex.age_F, file = paste0(getwd(), "/models/var_day.sex.age_F.RData"))



#Figure 5. variable ~ day.sex.age.env; scaled environment #####
runthrough = data.table(explanatory = c(
  "z_precipitation_dawn*sex2*age2", "z_precipitation_dusk*sex2*age2","z_avgRainfall*sex2*age2", "z_avgTemperature*sex2*age2"),
  response = c(rep("z_time_to_sunrise_min",4), rep("z_time_to_sunset_min",4)), stringsAsFactors = FALSE)

z_var_day.sex.env1 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'YDAY', rm.intercept = TRUE), data = subset(x2, YDAY < 90 & !is.na(age)) )
z_var_day.sex.env2 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'rel_day', rm.intercept = TRUE), data = subset(x2, rel_day >= -21) )
z_var_day.sex.env = list(z_var_day.sex.env1, z_var_day.sex.env2)
save(z_var_day.sex.env, file = paste0(getwd(), "/data/z_var_day.sex.env_F_withage.RData"))


models = unlist(z_var_day.sex.env, recursive = FALSE)
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

z_var_day.sex.env1 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'YDAY', rm.intercept = TRUE), data = subset(x2, YDAY < 90 & !is.na(age)) )
z_var_day.sex.env2 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'rel_day', rm.intercept = TRUE), data = subset(x2, rel_day >= -21) )
z_var_day.sex.env_males = list(z_var_day.sex.env1, z_var_day.sex.env2)
save(z_var_day.sex.env_males, file = paste0(getwd(), "/data/z_var_day.sex.env_males_F.RData"))

######for females ######
x2[, sex3 := factor(sex2, levels = c("2", "1"))]
runthrough = data.table(explanatory = c(
  "z_precipitation_dawn*sex3","z_avgRainfall*sex3", "z_avgTemperature*sex3",
  "z_precipitation_dusk*sex3","z_avgRainfall*sex3", "z_avgTemperature*sex3"),
  response = c(rep("z_time_to_sunrise_min",3), rep("z_time_to_sunset_min",3)), stringsAsFactors = FALSE)

z_var_day.sex.env1 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'YDAY', rm.intercept = TRUE), data = subset(x2, YDAY < 90 & !is.na(age)) )
z_var_day.sex.env2 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'rel_day', rm.intercept = TRUE), data = subset(x2, rel_day >= -21) )
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
var_day.sex.env1 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'YDAY', rm.intercept = FALSE), data = subset(x2, YDAY < 90) )
var_day.sex.env2 = apply(runthrough, MARGIN = 1, FUN = function(variables, data) gamm4.2(response = variables[2], by_gamm = "sex_age", explanatory = variables[1], data = data, smooth_over = 'rel_day', rm.intercept = FALSE), data = subset(x2, rel_day >= -21) )
var_day.sex.env_T = list(var_day.sex.env1, var_day.sex.env2)
save(var_day.sex.env_T, file = paste0(getwd(), "/data/var_day.sex.env_T.RData"))


#check for rhythmicity - no discernable rhythmicity####
TS = subset(x, sex == 1, select = c("date_", 'YDAY', 'time_to_sunrise_min', 'time_to_sunset_min'))
TS = dcast(TS, YDAY ~ ., value.var = c("time_to_sunrise_min", "time_to_sunset_min"), fun = median, na.rm = TRUE)
TS = subset(TS, YDAY <= 90 & YDAY >= 50) #complete time series


M = list()
for(i in 1:20) {
  M[[length(M)+1]] = data.table(matrix(coefficients(summary(lm(time_to_sunrise_min ~ shift(time_to_sunrise_min, n=i), data = TS))  )[2,1:2], nrow = 1))
}
M = rbindlist(M)
setnames(M, names(M), c("Est", "SE"))
M[, lag := 1:20]

plot(Est ~ lag, data = M)
arrows(M[, lag], M[, Est-1.96*SE], M[, lag], M[,Est+1.96*SE], code = 3, angle = 90, length = 0.05)
abline(h = 0, lty  = 3)
summary(m)


mod_list = list()
for(delta in 2:40) {

  print(paste0(Sys.time(), ": ", delta))
  hh = subset(x2, select = c('breeding_season', 'ID', 'box.breeding', 'date_', 'id', 'yid', 'time_to_sunrise_min', 'time_to_sunset_min'))
  hh[, day_shifted := date_ + delta]
  HH = split(hh, hh[,yid])
  for(i in 1 : length(HH)) {
    tmp = copy(HH[[i]])
    for(j in 1 : nrow(tmp)) {
      tmp2 = subset(tmp, date_ == tmp[j, day_shifted])
      tmp[j, ttsr.shift := ifelse(nrow(tmp2) < 1, as.numeric(NA), as.numeric(tmp2[, time_to_sunrise_min]))]
      tmp[j, ttss.shift := ifelse(nrow(tmp2) < 1, as.numeric(NA), as.numeric(tmp2[, time_to_sunset_min]))]
    }
    tmp = subset(tmp,!is.na(ttss.shift))


    #tmp = tmp[which(tmp[, date_] %in% tmp[, day_shifted]),]
    HH[[i]] = copy(tmp)
  }
  HH = rbindlist(HH) - no

  tmp1 = data.table(matrix(summary(lmer(time_to_sunrise_min ~ ttsr.shift+ (1|yid), data = HH))$coefficients[2,], nrow = 1))
  tmp2 = data.table(matrix(summary(lmer(time_to_sunset_min ~ ttss.shift+ (1|yid), data = HH))$coefficients[2,], nrow = 1))
  tmp = rbind(tmp1, tmp2)
  setnames(tmp, names(tmp), c("Est", "SE", "t"))
  tmp[, var := c("time_to_sunrise_min", "time_to_sunset_min")]
  tmp[, delta := delta]
  tmp -> mod_list[[length(mod_list)+1]]
}
mod_list = rbindlist(mod_list)


par(mfrow = c(2,1))
par(mar = c(2.5, 4.5, 0.1, 0.1))
dat = subset(mod_list, var == "time_to_sunrise_min")
plot(Est ~ delta, data = dat)
arrows(dat[, delta], dat[, Est-1.96*SE], dat[, delta], dat[, Est+1.96*SE], code = 3, length = 0.05, angle = 90)
text(30, 0.6, "Time to sunrise")

dat = subset(mod_list, var == "time_to_sunset_min")
plot(Est ~ delta, data = dat)
arrows(dat[, delta], dat[, Est-1.96*SE], dat[, delta], dat[, Est+1.96*SE], code = 3, length = 0.05, angle = 90)
text(30, 0.55, "Time to sunset")


hh[, keep := which(hh[, yid])]
hh = subset(hh, )
