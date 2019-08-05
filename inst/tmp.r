dat = copy(x2)
dat[, rainYN := ifelse(precipitation_dawn > 0, ifelse(precipitation_dawn > 1.1, 2, 1), 0)]
dat[avgTemperature < 2.23, quT := as.numeric(-1.5)]
dat[avgTemperature >= 2.23 & avgTemperature < 6.36, quT := as.numeric(-0.5)]
dat[avgTemperature >= 6.36 & avgTemperature < 9.86, quT := as.numeric(0.5)]
dat[avgTemperature > 9.86, quT := as.numeric(1.5)]

m = lmer(time_to_sunrise_min ~ 0+z_precipitation_dawn + z_avgTemperature*factor(quT) + (1|yid), data = dat)
hist(resid(m))
qqnorm(resid(m))
plot(m)
summary(m)
