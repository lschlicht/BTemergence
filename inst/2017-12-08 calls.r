#1. Gamm: Selection of k ####
{
  #check EDF and Smoothness selection criterion
  #males, sunrise #####
  tmp = copy(x)
  br10.1 = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp',k = 10, by = sex2), random = ~(1|ID), data = tmp)
  br20.1 = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp',k = 20, by = sex2), random = ~(1|ID), data = tmp)
  br40.1 = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp',k = 40, by = sex2), random = ~(1|ID), data = tmp)
  br80.1 = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp',k = 80, by = sex2), random = ~(1|ID), data = tmp)
  summary(br10.1$gam)
  summary(br20.1$gam)
  summary(br40.1$gam) #use for males
  summary(br80.1$gam)

  br40.1 -> msr
  summary(msr$mer)


  #females, sunrise #####
  tmp = subset(x, sex == 2)
  br10.2 = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp',k = 10) + temperature_dawn + precipitation_dawn + humidity_dawn, random = ~(1|ID), data = tmp)
  br20.2 = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp',k = 20) + temperature_dawn + precipitation_dawn + humidity_dawn, random = ~(1|ID), data = tmp)
  br40.2 = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp',k = 40) + temperature_dawn + precipitation_dawn + humidity_dawn, random = ~(1|ID), data = tmp)
  br80.2 = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp',k = 80) + temperature_dawn + precipitation_dawn + humidity_dawn, random = ~(1|ID), data = tmp)
  summary(br10.2$gam)
  summary(br20.2$gam)
  summary(br40.2$gam) #use for females
  summary(br80.2$gam)

  br40.2 -> fsr
  summary(fsr$mer)

  #males, sunset #####
  tmp = subset(x, sex == 1)
  br10.3 = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp',k = 10) + temperature_dusk + precipitation_dusk + humidity_dusk, random = ~(1|ID), data = tmp)
  br20.3 = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp',k = 20) + temperature_dusk + precipitation_dusk + humidity_dusk, random = ~(1|ID), data = tmp)
  br40.3 = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp',k = 40) + temperature_dusk + precipitation_dusk + humidity_dusk, random = ~(1|ID), data = tmp)
  br80.3 = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp',k = 40) + temperature_dusk + precipitation_dusk + humidity_dusk, random = ~(1|ID), data = tmp)
  summary(br10.3$gam)
  summary(br20.3$gam)
  summary(br40.3$gam) #use for males
  summary(br80.3$gam)

  br40.3 -> mss
  summary(mss$mer)

  #females, sunset #####
  tmp = subset(x, sex == 2)
  br10.4 = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp',k = 10) + temperature_dusk + precipitation_dusk + humidity_dusk, random = ~(1|ID), data = tmp)
  br20.4 = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp',k = 20) + temperature_dusk + precipitation_dusk + humidity_dusk, random = ~(1|ID), data = tmp)
  br40.4 = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp',k = 40) + temperature_dusk + precipitation_dusk + humidity_dusk, random = ~(1|ID), data = tmp)
  br80.4 = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp',k = 40) + temperature_dusk + precipitation_dusk + humidity_dusk, random = ~(1|ID), data = tmp)
  summary(br10.4$gam)
  summary(br20.4$gam)
  summary(br40.4$gam) #use for females
  summary(br80.4$gam)

  br40.4 -> fss
  summary(fss$mer)





  #males, prop time used #####
  tmp = subset(x, sex == 1)
  br10.5 = gamm4(prop_daylength_used ~ s(rel_day, bs = 'tp',k = 10) + avgTemperature + avgRainfall + avgHumidity, random = ~(1|ID), data = tmp)
  br20.5 = gamm4(prop_daylength_used ~ s(rel_day, bs = 'tp',k = 20) + avgTemperature + avgRainfall + avgHumidity, random = ~(1|ID), data = tmp)
  br40.5 = gamm4(prop_daylength_used ~ s(rel_day, bs = 'tp',k = 40) + avgTemperature + avgRainfall + avgHumidity, random = ~(1|ID), data = tmp)
  br80.5 = gamm4(prop_daylength_used ~ s(rel_day, bs = 'tp',k = 80) + avgTemperature + avgRainfall + avgHumidity, random = ~(1|ID), data = tmp)
  summary(br10.5$gam)
  summary(br20.5$gam)
  summary(br40.5$gam) #use for males
  summary(br80.5$gam)

  br40.5 -> mact
  summary(mact$mer)

  #females, prop time spent active #####
  tmp = subset(x, sex == 2)
  br10.6 = gamm4(prop_daylength_used ~ s(rel_day, bs = 'tp',k = 10) + avgTemperature + avgRainfall + avgHumidity, random = ~(1|ID), data = tmp)
  br20.6 = gamm4(prop_daylength_used ~ s(rel_day, bs = 'tp',k = 20) + avgTemperature + avgRainfall + avgHumidity, random = ~(1|ID), data = tmp)
  br40.6 = gamm4(prop_daylength_used ~ s(rel_day, bs = 'tp',k = 40) + avgTemperature + avgRainfall + avgHumidity, random = ~(1|ID), data = tmp)
  br80.6 = gamm4(prop_daylength_used ~ s(rel_day, bs = 'tp',k = 80) + avgTemperature + avgRainfall + avgHumidity, random = ~(1|ID), data = tmp)
  summary(br10.6$gam)
  summary(br20.6$gam)
  summary(br40.6$gam) #use for females
  summary(br80.6$gam)

  br40.6 -> fact
  summary(fact$mer)


}

#2. Plot ####
#setup data #####
dp_all = copy(x)
dp_all = dp_all[, data.table(matrix(boxplot.stats(time_to_sunrise_min)$stats, nrow = 1)), by = list(rel_day, sex)]
SHIFT1_sr = mean(dp_all[sex == 1, V3]) - mean(plot(msr$gam)[[1]]$fit)
SHIFT2_sr = mean(dp_all[sex == 2, V3]) - mean(plot(fsr$gam)[[1]]$fit)
sr = copy(dp_all)

dp_all = copy(x)
dp_all = dp_all[, data.table(matrix(boxplot.stats(time_to_sunset_min)$stats, nrow = 1)), by = list(rel_day, sex)]
SHIFT1_ss = mean(dp_all[sex == 1, V3]) - mean(plot(mss$gam)[[1]]$fit)
SHIFT2_ss = mean(dp_all[sex == 2, V3]) - mean(plot(fss$gam)[[1]]$fit)
ss = copy(dp_all)

dp_all = copy(x)
dp_all = dp_all[, data.table(matrix(boxplot.stats(prop_daylength_used)$stats, nrow = 1)), by = list(rel_day, sex)]
SHIFT1_act = mean(dp_all[sex == 1, V3]) - mean(plot(mact$gam)[[1]]$fit)
SHIFT2_act = mean(dp_all[sex == 2, V3]) - mean(plot(fact$gam)[[1]]$fit)
act = copy(dp_all)

ENV = dcast(x, rel_day ~ ., value.var = c("temperature_dawn", "temperature_dusk", "avgTemperature", "precipitation_dawn", "precipitation_dusk", "avgRainfall", "humidity_dawn", "humidity_dusk", "avgHumidity"), fun = mean)

#setup plot #####
jpeg(file = "inst/figures/Seasonal patterns.jpg", width = 480, height = 720)
layout(c(1,2,3, 4, 5, 6), heights = c(1,1,1.2))

#sunset #####
par(mar = c(0.7,4.1,0.5,0.5))
XLIM = c(-200, 50)
YLIM = c(-60, 20)
plot(mss$gam, xlim = XLIM, ylim = YLIM, shift = SHIFT1_ss, xlab = "Day to first egg", ylab = "Time to sunset", col = 'blue', rug = FALSE, xaxt = 'n')
axis(1, labels = FALSE, tick = TRUE)
par(new = TRUE)
plot(fss$gam, xlim = XLIM, ylim = YLIM, shift = SHIFT2_ss, xlab = "Day to first egg", ylab = "Time to sunset", col = 'red', rug = FALSE, xaxt = 'n')
abline(h = 0, lty = 3); abline(v = 0, lty = 3)


#sunrise #####
par(mar = c(0.7,4.1,0.5,0.5))
XLIM = c(-200, 50)
YLIM = c(-30, 20)
plot(msr$gam, xlim = XLIM, ylim = YLIM, shift = SHIFT1_sr, xlab = "Day to first egg", ylab = "Time to sunrise", col = 'blue', rug = FALSE, xaxt = 'n')
axis(1, labels = FALSE, tick = TRUE)
par(new = TRUE)
plot(fsr$gam, xlim = XLIM, ylim = YLIM, shift = SHIFT2_sr, xlab = "Day to first egg", ylab = "Time to sunrise", col = 'red', rug = FALSE, xaxt = 'n')
abline(h = 0, lty = 3); abline(v = 0, lty = 3)

#active time #####
par(mar = c(2.5,4.1,0.5,0.5))
XLIM = c(-200, 50)
YLIM = c(0.90, 1.10)
plot(mact$gam, xlim = XLIM, ylim = YLIM, shift = SHIFT1_act, xlab = "Day to first egg", ylab = "Prop. time active", col = 'blue', rug = FALSE, xaxt = 'n')
axis(1, labels = TRUE, tick = TRUE)
par(new = TRUE)
plot(fact$gam, xlim = XLIM, ylim = YLIM, shift = SHIFT2_act, xlab = "Day to first egg", ylab = "", col = 'red', rug = FALSE, xaxt = 'n')
abline(h = 1, lty = 3); abline(v = 0, lty = 3)




#rain etc
plot(x = ENV[, rel_day], y = ENV[, avgTemperature], type = 'l')
plot(x = ENV[, rel_day], y = ENV[, avgHumidity], type = 'l', col = 'red')
plot(x = ENV[, rel_day], y = ENV[, avgRainfall], type = 'l', col = 'blue')



#setdown plot #####
dev.off()



#3. "boxplots" ####
#DODO

#4. Environment ####

z_ss = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp',k = 40, by = sex2) + z_temperature_dusk*sex2 + z_precipitation_dusk*sex2 + z_humidity_dusk*sex2, random = ~(1|ID), data = x)
z_sr = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp',k = 40, by = sex2) + z_temperature_dawn*sex2 + z_precipitation_dawn*sex2 + z_humidity_dawn*sex2, random = ~(1|ID), data = x)
z_act = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp',k = 40, by = sex2) + z_avgTemperature*sex2 + z_avgRainfall*sex2 + z_avgHumidity*sex2, random = ~(1|ID), data = x)





z_mss = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp',k = 40) + z_temperature_dusk + z_precipitation_dusk + z_humidity_dusk, random = ~(1|ID), data = subset(x, sex == 1))
z_fss = gamm4(time_to_sunset_min ~ s(rel_day, bs = 'tp',k = 40) + z_temperature_dusk + z_precipitation_dusk + z_humidity_dusk, random = ~(1|ID), data = subset(x, sex == 2))

z_msr = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp',k = 40) + z_temperature_dawn + z_precipitation_dawn + z_humidity_dawn, random = ~(1|ID), data = subset(x, sex == 1))
z_fsr = gamm4(time_to_sunrise_min ~ s(rel_day, bs = 'tp',k = 40) + z_temperature_dawn + z_precipitation_dawn + z_humidity_dawn, random = ~(1|ID), data = subset(x, sex == 2))

z_mact = gamm4(prop_daylength_used ~ s(rel_day, bs = 'tp',k = 40) + z_avgTemperature + z_avgRainfall + z_avgHumidity, random = ~(1|ID), data = subset(x, sex == 1))
z_fact = gamm4(prop_daylength_used ~ s(rel_day, bs = 'tp',k = 40) + z_avgTemperature + z_avgRainfall + z_avgHumidity, random = ~(1|ID), data = subset(x, sex == 2))

MSS = summary(z_mss$mer)$coefficients
FSS = summary(z_fss$mer)$coefficients
MSR = summary(z_msr$mer)$coefficients
FSR = summary(z_fsr$mer)$coefficients
MACT = summary(z_mact$mer)$coefficients
FACT = summary(z_fact$mer)$coefficients

male_col = 'blue'
female_col = 'red'
male_pch = 16
female_pch = 1
male_loc = (3:1)-0.05
female_loc = (3:1)+0.05
par(mfrow = c(1,3))
layout(mat = matrix(c(1,2,3), ncol = 3), widths = c(1.5, 1, 1))
layout.show(3)
par(mar = c(5.1, 4.1, 4.1, 0.2))

plot(c(-2, 2), c(0.5,3.5), type = 'n', yaxt = 'n', ylab = '', xlab = "Estimate ± CI")
points(MSR[2:4,1],male_loc, col = male_col, pch = male_pch)
arrows(MSR[2:4,1] + MSR[2:4,2]*1.96, male_loc, MSR[2:4,1] - MSR[2:4,2]*1.96, male_loc, code = 3, length = 0.05, angle = 90, col = male_col)
points(FSR[2:4,1],female_loc, col = female_col, pch = female_pch)
arrows(FSR[2:4,1] + FSR[2:4,2]*1.96, female_loc, FSR[2:4,1] - FSR[2:4,2]*1.96, female_loc, code = 3, length = 0.05, angle = 90, col = female_col)
abline(v = 0, lty = 3)
axis(2, labels = c("Humidity", "Precipitation", "Temperature"), at = c(1,2,3))

par(mar = c(5.1, 0.2, 4.1, 0.2))
plot(c(-2, 2), c(0.5,3.5), type = 'n', yaxt = 'n', ylab = '', xlab = "Estimate ± CI")
points(MSS[2:4,1],male_loc, col = male_col, pch = male_pch)
arrows(MSS[2:4,1] + MSS[2:4,2]*1.96, male_loc, MSS[2:4,1] - MSS[2:4,2]*1.96, male_loc, code = 3, length = 0.05, angle = 90, col = male_col)
points(FSS[2:4,1],female_loc, col = female_col, pch = female_pch)
arrows(FSS[2:4,1] + FSS[2:4,2]*1.96, female_loc, FSS[2:4,1] - FSS[2:4,2]*1.96, female_loc, code = 3, length = 0.05, angle = 90, col = female_col)
abline(v = 0, lty = 3)


plot(c(-0.01, 0.01), c(0.5,3.5), type = 'n', yaxt = 'n', ylab = '', xlab = "Estimate ± CI")
points(MACT[2:4,1],male_loc, col = male_col, pch = male_pch)
arrows(MACT[2:4,1] + MACT[2:4,2]*1.96, male_loc, MACT[2:4,1] - MACT[2:4,2]*1.96, male_loc, code = 3, length = 0.05, angle = 90, col = male_col)
points(FACT[2:4,1],female_loc, col = female_col, pch = female_pch)
arrows(FACT[2:4,1] + FACT[2:4,2]*1.96, female_loc, FACT[2:4,1] - FACT[2:4,2]*1.96, female_loc, code = 3, length = 0.05, angle = 90, col = female_col)
abline(v = 0, lty = 3)


