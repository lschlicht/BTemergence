require(gamm4)


#a. setup data
dp_all = copy(x)
dp_all = dp_all[, data.table(matrix(boxplot.stats(time_to_sunrise_min)$stats, nrow = 1)), by = list(rel_day, sex)]
SHIFT1 = mean(dp_all[sex == 1, V3]) - mean(plot(msr$gam)[[1]]$fit)
SHIFT2 = mean(dp_all[sex == 2, V3]) - mean(plot(fsr$gam)[[1]]$fit)
sr = copy(dp_all)

dp_all = copy(x)
dp_all = dp_all[, data.table(matrix(boxplot.stats(time_to_sunset_min)$stats, nrow = 1)), by = list(rel_day, sex)]
SHIFT1 = mean(dp_all[sex == 1, V3]) - mean(plot(mss$gam)[[1]]$fit)
SHIFT2 = mean(dp_all[sex == 2, V3]) - mean(plot(fss$gam)[[1]]$fit)
ss = copy(dp_all)
###


setnames(dp_all, "rel_day", "day")

jpeg('tmp2.jpg', width = 1000, height = 1000)


par(mfrow = c(2,1))

SEX = 2
#plot(br2$gam, ylim = c(-65, 60), shift = SHIFT2, xlab = "Day to first egg", ylab = "Time to sunrise")
plot(br4$gam, ylim = c(-120, 30), shift = SHIFT2, xlab = "Day to first egg", ylab = "Time to sunset")
points(dp_all[sex == SEX,day], dp_all[sex == SEX, V3], pch = 16, cex = 0.5, col = 'black')
arrows(dp_all[sex == SEX,day], dp_all[sex == SEX, V1], dp_all[sex == SEX,day], dp_all[sex == SEX,V2], code = 3, length = 0)
arrows(dp_all[sex == SEX,day], dp_all[sex == SEX, V4], dp_all[sex == SEX,day], dp_all[sex == SEX,V5], code = 3, length = 0)


SEX = 1
#plot(br1$gam, ylim = c(-65, 25), shift = SHIFT1, xlab = "Day to first egg", ylab = "Time to sunrise")
plot(br3$gam, ylim = c(-120, 30), shift = SHIFT1, xlab = "Day to first egg", ylab = "Time to sunset")
points(dp_all[sex == SEX,day], dp_all[sex == SEX, V3], pch = 16, cex = 0.5, col = 'black')
arrows(dp_all[sex == SEX,day], dp_all[sex == SEX, V1], dp_all[sex == SEX,day], dp_all[sex == SEX,V2], code = 3, length = 0)
arrows(dp_all[sex == SEX,day], dp_all[sex == SEX, V4], dp_all[sex == SEX,day], dp_all[sex == SEX,V5], code = 3, length = 0)



dev.off()
