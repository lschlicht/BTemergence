#plot for Bart
tmp = subset(x2, date_ > (as.IDate(firstEgg)+clutch) & date_ < hatchDate & sex == 2, select = c('breeding_season', "ID", "time_to_sunrise_min", "firstEgg", "date_", "rel_day", "clutch"))
tmp[, yid := paste(breeding_season, ID)]
tmp[, inc_day := as.IDate(firstEgg)+clutch]
tmp = unique(tmp)
tmp[, julFirstEgg := yday(as.IDate(firstEgg))]

m = lmer(time_to_sunrise_min ~ julFirstEgg+inc_day+(1), data = tmp)
plot(m)
summary(m)

plot(time_to_sunrise_min ~ julFirstEgg, data = tmp, xlab = "Day of first egg (0 = 1st January)", ylab = "Average emergence time during incubation")
abline(coef(m)[1:2])
text(x = 110, y = 60, "Estimate±SE: 0.26±0.12")
