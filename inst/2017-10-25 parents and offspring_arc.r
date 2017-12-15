#rel_day prev and next year

xx = copy(x)
xx = merge(xx, xx, by = c("ID", "rel_day"), suffixes = c('.1', '.2'))
xx = subset(xx, breeding_season.2 > breeding_season.1)

xx_male = subset(xx, sex.1 == 1)
xx_female = subset(xx, sex.1 == 2)

jpeg(file = "parents_parents.jpg", width = 720, height = 720)
dev.off()

par(mfrow = c(6,3))

XX = list(xx_male, xx_female)
VARS = 1:3
M = list()
for(i in 1 : length(XX)) {
  for(j in VARS) {
    { dat = copy(XX[[i]])
    if(VARS[j] == 1) { dat[, var1 := time_to_sunrise_min.1]; dat[, var2 := time_to_sunrise_min.2]; dat[, var3 := sunrise_time.2] }
    if(VARS[j] == 2) { dat[, var1 := time_to_sunset_min.1]; dat[, var2 := time_to_sunset_min.2]; dat[, var3 := sunset_time.2] }
    if(VARS[j] == 3) {dat[, var1 := daylength_min.1]; dat[, var2 := daylength_min.2]; dat[, var3 := actualDaylength.2]}
    }
      {
        dat = unique(dat)
        nn = nrow(dat)
        #hist(dat$var2)
        st = boxplot.stats(dat$var2)$stats
        dat = subset(dat, var2 > st[1] & var2 <= st[5])
        #hist(dat$var2)
        m2 = lmer(var2 ~ var1 + rel_day + (1|ID), data = dat)
        #hist(resid(m2))
        #qqnorm(residuals(m2))
        #plot(resid(m2) ~ fitted(m2))
        M[[length(M)+1]] = data.table(Est = summary(m2)$coefficients[2,1], SE = summary(m2)$coefficients[2,2], t = summary(m2)$coefficients[2,3], sex = dat[1,sex.1], var = VARS[j], N = nrow(dat), Nind = length(unique(dat$ID)))
      }
  }
}

MM = rbindlist(M)

MM





#who is who's son/daughter

con = dbcon(user = "lschlicht")
sd = dbq(con, "SELECT ID, father, mother FROM BTatWESTERHOLZ.PATERNITY")

xx = copy(x)
xx = merge(xx, sd, by = "ID")
xx = xx[(sex == 1 & (father %in% ID | ID %in% father)) |
          (sex == 2 & (mother %in% ID | ID %in% mother))]

xx_male = merge(xx, xx, by.x = c("ID", "rel_day"), by.y = c("father", "rel_day"), suffixes = c('.offspring', '.parent'))
xx_female = merge(xx, xx, by.x = c("ID", "rel_day"), by.y = c("mother", "rel_day"), suffixes = c('.offspring', '.parent'))

#jpeg(file = "parents_offspring.jpg", width = 720, height = 720)

par(mfrow = c(2,3))

XX = list(xx_male, xx_female)
VARS = 1:3
M = list()
for(i in 1 : length(XX)) {
  for(j in VARS) {
    { dat = copy(XX[[i]])
    if(VARS[j] == 1) { dat[, var1 := time_to_sunrise_min.parent]; dat[, var2 := time_to_sunrise_min.offspring]; dat[, var3 := sunrise_time.offspring] }
    if(VARS[j] == 2) { dat[, var1 := time_to_sunset_min.parent]; dat[, var2 := time_to_sunset_min.offspring]; dat[, var3 := sunset_time.offspring] }
    if(VARS[j] == 3) {dat[, var1 := daylength_min.parent]; dat[, var2 := daylength_min.offspring]; dat[, var3 := actualDaylength.offspring]}
    }

{
nn = nrow(dat)
#hist(dat$var2)
st = boxplot.stats(dat$var2)$stats
dat = subset(dat, var2 > st[1] & var2 <= st[5])
#hist(dat$var2)
m1 = lmer(var2 ~ var1 + (1|ID), data = dat)
m2 = lmer(var2 ~ var1 + rel_day + (1|ID), data = dat)
#hist(resid(m2))
#qqnorm(residuals(m2))
#plot(resid(m2) ~ fitted(m2))
M[[length(M)+1]] = c(Est = summary(m2)$coefficients[2,1], SE = summary(m2)$coefficients[2,2], t = summary(m2)$coefficients[2,3], N = nrow(dat), Nind = length(unique(dat$ID)))
}


    plot(dat$var2, dat$var1)
    abline(coef = summary(m2)$coefficients[1:2,1], lwd = 2)

  }
}

MM = do.call(rbind,M)

dev.off()

