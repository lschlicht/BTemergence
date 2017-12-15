bla = function(x) {
dat = subset(x, sex == 1 & yday(date_) > 75 & yday(date_) < 120)
tmp = unique(subset(dat, select = c('ID', 'year_', 'firstEgg')))
tmp[, pop_firstEgg := median(as.IDate(firstEgg), na.rm=  TRUE), by = 'year_']
tmp = unique(subset(tmp, select = c('year_', 'pop_firstEgg')))
dat = merge(dat, tmp, by = 'year_', all.x = TRUE)
dat[, pop_rel_day := as.numeric(date_ - pop_firstEgg)]


dat[, var := dat[,time_to_sunrise_min], with = TRUE]

hist(dat$var)
st = boxplot.stats(dat$var)$stats
n = nrow(dat)
dat = subset(dat, var > st[1] & var <= st[5])
hist(dat$var)
nrow(dat)/n

m1 = lmer(var ~ abs(rel_day) + abs(pop_rel_day) + sunrise_time + (1|ID) + (1|year_), data = dat)
qqnorm(residuals(m1))
hist(resid(m1))
plot(resid(m1) ~ fitted(m1))

summary(m1)
m2 = lmer(var ~ abs(pop_rel_day) + sunrise_time + (1|ID) + (1|year_), data = dat)
m3 = lmer(var ~ abs(rel_day) + sunrise_time + (1|ID) + (1|year_), data = dat)
}
