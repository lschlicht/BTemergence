bla = function(x){
#across years

{
  MODELOUTPUT = list()
  runthrough = expand.grid(c(1,2), unique(x$season), c("time_to_sunrise_min", "time_to_sunset_min", "daylength_min"), stringsAsFactors = FALSE)
  names(runthrough) = c('sex', 'season', 'var')
  runthrough = as.data.table(runthrough)
  runthrough[var == "time_to_sunrise_min", var2 := "sunrise_time"]
  runthrough[var == "time_to_sunset_min", var2 := "sunset_time"]
  runthrough[var == "daylength_min", var2 := "actualDaylength_min"]

  for(i in 1:nrow(runthrough)) {
    print(i)

    dat = subset(x, sex == runthrough[i, sex]  & season == runthrough[i, season])
    dat[, var := dat[[(runthrough[i,var])]], with = TRUE]
    dat[, var2 := dat[[(runthrough[i,var2])]], with = TRUE]
    hist(dat$var)
    st = boxplot.stats(dat$var)$stats
    dat = subset(dat, var > st[1] & var <= st[5])
    hist(dat$var)

    m =  lmer(var ~ SumRainfall_dawn + Temperature2m_dawn + var2 + (1|ID), data = dat)


    hist(resid(m))
    plot(resid(m) ~ fitted(m))
    qqnorm(residuals(m))
    tmp = data.table(coefficients(summary(m)))
    tmp[, season := runthrough[i, 'season']]
    tmp[, sex := runthrough[i, 'sex']]
    tmp[, variable := c("Int.", "Rain", "T", "var2")]
    tmp[, fixed := runthrough[i, 'var']]
    MODELOUTPUT[[length(MODELOUTPUT)+1]] = tmp

  }
}

M = rbindlist(MODELOUTPUT) #rank deficient in spring 2011 for males


M[season == "0_spring", ord := "3"]
M[season == "1_preBreeding", ord := "2"]
M[season == "2_winter", ord := "1"]

M[sex == 1, ord := paste(ord, "m", sep = "_")]
M[sex == 2, ord := paste(ord, "f", sep = "_")]

M[fixed == "time_to_sunrise_min", ord := paste(ord, "1M", sep = "_")]
M[fixed == "time_to_sunset_min", ord := paste(ord, "2N", sep = "_")]
M[fixed == "daylength_min", ord := paste(ord, "3D", sep = "_")]



par(mfrow = c(1,2))
M2 = subset(M, variable == "Rain")
setkey(M2, ord)
plot(c(0.5, 18.5), c(-20, 10), type = 'n')
points(1:18, M2$Estimate, pch = 16)
arrows(1:18, M2$Estimate + (1.96*M2$"Std. Error"), 1:18, M2$Estimate - (1.96*M2$"Std. Error"), code = 3, length = 0.05, angle = 90)
abline(h = 0, lty = 3)
abline(v = c(6.5, 12.5))

M2 = subset(M, variable == "T")
setkey(M2, ord)
plot(c(0.5, 18.5), c(-2.5, 2.5), type = 'n')
points(1:18, M2$Estimate, pch = 16)
arrows(1:18, M2$Estimate + (1.96*M2$"Std. Error"), 1:18, M2$Estimate - (1.96*M2$"Std. Error"), code = 3, length = 0.05, angle = 90)
abline(h = 0, lty = 3)
abline(v = c(6.5, 12.5))


#each year


{
  MODELOUTPUT = list()
  runthrough = expand.grid(unique(x$year_), c(1,2), unique(x$season), c("time_to_sunrise_min", "time_to_sunset_min", "daylength_min"), stringsAsFactors = FALSE)
  names(runthrough) = c('year_', 'sex', 'season', 'var')
  for(i in 1:nrow(runthrough)) {
  print(i)

  dat = subset(x, sex == runthrough[i, 'sex']  & season == runthrough[i, 'season'] & year_ == runthrough[i, 'year_'])
  dat[, var := dat[[(runthrough[i,'var'])]], with = TRUE]
  hist(dat$var)
  st = boxplot.stats(dat$var)$stats
  dat = subset(dat, var > st[1] & var <= st[5])
  hist(dat$var)

  m =  lmer(var ~ SumRainfall_dawn + Temperature2m_dawn + sunrise_time + (1|ID), data = dat)


  hist(resid(m))
  plot(resid(m) ~ fitted(m))
  qqnorm(residuals(m))
  tmp = data.table(coefficients(summary(m)))
  tmp[, year_ := runthrough[i, 'year_']]
  tmp[, season := runthrough[i, 'season']]
  tmp[, sex := runthrough[i, 'sex']]
  tmp[, variable := c("Int.", "Rain", "T", "sunrise1")]
  tmp[, fixed := runthrough[i, 'var']]
  MODELOUTPUT[[length(MODELOUTPUT)+1]] = tmp

  }



}

M = rbindlist(MODELOUTPUT) #rank deficient in spring 2011 for males

M[season == "0_spring", ord := "3"]
M[season == "1_preBreeding", ord := "2"]
M[season == "2_winter", ord := "1"]

M[sex == 1, ord := paste(ord, "m", sep = "_")]
M[sex == 2, ord := paste(ord, "f", sep = "_")]

M[fixed == "time_to_sunrise_min", ord := paste(ord, "1M", sep = "_")]
M[fixed == "time_to_sunset_min", ord := paste(ord, "2N", sep = "_")]
M[fixed == "daylength_min", ord := paste(ord, "3D", sep = "_")]

M2 = subset(M, variable == "Rain")
boxplot(M2$Estimate ~ M2$ord, ylim = c(-30, 15))
boxplot(M2$Estimate ~ M2$ord)
abline(h = 0, lty = 3)
abline(v = c(6.5, 12.5))

M2 = subset(M, variable == "T")
boxplot(M2$Estimate ~ M2$ord, ylim = c(-30, 15))
boxplot(M2$Estimate ~ M2$ord)
abline(h = 0, lty = 3)
abline(v = c(6.5, 12.6))





  dat = subset(x, sex == 2 & season == '2_winter')
  hist(dat$time_to_sunrise_min)
  st = boxplot.stats(dat$time_to_sunrise_min)$stats
  dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])
  hist(dat$time_to_sunrise_min)
  fw = lmer(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(fw))
  plot(resid(fw) ~ fitted(fw))
  qqnorm(residuals(fw))
  summary(fw)
  fw_dawn = fw


  dat = subset(x, sex == 1 & season == '1_preBreeding')
  hist(dat$time_to_sunrise_min)
  st = boxplot.stats(dat$time_to_sunrise_min)$stats
  dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])
  hist(dat$time_to_sunrise_min)
  mp = lmer(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(mp))
  plot(resid(mp) ~ fitted(mp))
  qqnorm(residuals(mp))
  summary(mp)
  mp -> mp_dawn

  dat = subset(x, sex == 2 & season == '1_preBreeding')
  hist(dat$time_to_sunrise_min)
  st = boxplot.stats(dat$time_to_sunrise_min)$stats
  dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])
  hist(dat$time_to_sunrise_min)
  fp = lmer(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(fp))
  plot(resid(fp) ~ fitted(fp))
  qqnorm(residuals(fp))
  summary(fp)
  fp_dawn = fp


  dat = subset(x, sex == 1 & season == '0_spring')
  hist(dat$time_to_sunrise_min)
  st = boxplot.stats(dat$time_to_sunrise_min)$stats
  dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])
  hist(dat$time_to_sunrise_min)
  ms = lmer(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(ms))
  plot(resid(ms) ~ fitted(ms))
  qqnorm(residuals(ms))
  summary(ms)
  ms -> ms_dawn

  dat = subset(x, sex == 2 & season == '0_spring')
  hist(dat$time_to_sunrise_min)
  st = boxplot.stats(dat$time_to_sunrise_min)$stats
  dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])
  hist(dat$time_to_sunrise_min)
  fs = lmer(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(fs))
  plot(resid(fs) ~ fitted(fs))
  qqnorm(residuals(fs))
  summary(fs)
  fs -> fs_dawn



#DUSK
{
  dat = subset(x, sex == 1 & season == '2_winter')
  hist(dat$time_to_sunset_min)
  st = boxplot.stats(dat$time_to_sunset_min)$stats
  dat = subset(dat, time_to_sunset_min > st[1] & time_to_sunset_min <= st[5])
  hist(dat$time_to_sunset_min)

  mw = lmer(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(mw))
  plot(resid(mw) ~ fitted(mw))
  qqnorm(residuals(mw))
  summary(mw)
  mw -> mw_dusk
  Rmw_dusk = rpt(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|ID) + (1|year_ID) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rmw_dusk2 = rpt(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rmw_dusk



  dat = subset(x, sex == 2 & season == '2_winter')
  hist(dat$time_to_sunset_min)
  st = boxplot.stats(dat$time_to_sunset_min)$stats
  dat = subset(dat, time_to_sunset_min > st[1] & time_to_sunset_min <= st[5])
  hist(dat$time_to_sunset_min)
  fw = lmer(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(fw))
  plot(resid(fw) ~ fitted(fw))
  qqnorm(residuals(fw))
  summary(fw)
  fw -> fw_dusk

  Rfw_dusk = rpt(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|ID) + (1|year_ID) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rfw_dusk2 = rpt(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rfw_dusk

  dat = subset(x, sex == 1 & season == '1_preBreeding')
  hist(dat$time_to_sunset_min)
  st = boxplot.stats(dat$time_to_sunset_min)$stats
  dat = subset(dat, time_to_sunset_min > st[1] & time_to_sunset_min <= st[5])
  hist(dat$time_to_sunset_min)
  mp = lmer(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(mp))
  plot(resid(mp) ~ fitted(mp))
  qqnorm(residuals(mp))
  summary(mp)
  mp -> mp_dusk

  Rmp_dusk = rpt(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rmp_dusk2 = rpt(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rmp_dusk


  dat = subset(x, sex == 2 & season == '1_preBreeding')
  hist(dat$time_to_sunset_min)
  st = boxplot.stats(dat$time_to_sunset_min)$stats
  dat = subset(dat, time_to_sunset_min > st[1] & time_to_sunset_min <= st[5])
  hist(dat$time_to_sunset_min)
  fp = lmer(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(fp))
  plot(resid(fp) ~ fitted(fp))
  qqnorm(residuals(fp))
  summary(fp)
  fp -> fp_dusk

  Rfp_dusk = rpt(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rfp_dusk2 = rpt(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rfp_dusk


  dat = subset(x, sex == 1 & season == '0_spring')
  hist(dat$time_to_sunset_min)
  st = boxplot.stats(dat$time_to_sunset_min)$stats
  dat = subset(dat, time_to_sunset_min > st[1] & time_to_sunset_min <= st[5])
  hist(dat$time_to_sunset_min)
  ms = lmer(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(ms))
  plot(resid(ms) ~ fitted(ms))
  qqnorm(residuals(ms))
  summary(ms)
  ms -> ms_dusk
  Rms_dusk = rpt(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rms_dusk2 = rpt(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)

  Rms_dusk

  dat = subset(x, sex == 2 & season == '0_spring')
  hist(dat$time_to_sunset_min)
  st = boxplot.stats(dat$time_to_sunset_min)$stats
  dat = subset(dat, time_to_sunset_min > st[1] & time_to_sunset_min <= st[5])
  hist(dat$time_to_sunset_min)
  fs = lmer(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(fs))
  plot(resid(fs) ~ fitted(fs))
  qqnorm(residuals(fs))
  summary(fs)
  fs -> fs_dusk
  Rfs_dusk = rpt(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rfs_dusk2 = rpt(time_to_sunset_min ~ SumRainfall_dusk + Temperature2m_dusk + poly(sunset_time,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rfs_dusk

}

#DAYLENGTH
{
  dat = subset(x, sex == 1 & season == '2_winter')
  hist(dat$daylength)
  st = boxplot.stats(dat$daylength_min)$stats
  nrow(dat)
  dat = subset(dat, daylength_min > st[1] & daylength_min <= st[5])
  nrow(dat)
  hist(dat$daylength_min)

  mw = lmer(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(mw))
  plot(resid(mw) ~ fitted(mw))
  qqnorm(residuals(mw))
  summary(mw)
  mw -> mw_day
  Rmw_daylength_min = rpt(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rmw_daylength_min2 = rpt(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rmw_daylength_min
  Rmw_daylength_min2

  dat = subset(x, sex == 2 & season == '2_winter')
  hist(dat$daylength_min)
  st = boxplot.stats(dat$daylength_min)$stats
  dat = subset(dat, daylength_min > st[1] & daylength_min <= st[5])
  hist(dat$daylength_min)
  fw = lmer(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(fw))
  plot(resid(fw) ~ fitted(fw))
  qqnorm(residuals(fw))
  summary(fw)
  fw -> fw_day

  Rfw_daylength_min = rpt(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rfw_daylength_min2 = rpt(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)

  Rfw_daylength_min

  dat = subset(x, sex == 1 & season == '1_preBreeding')
  hist(dat$daylength_min)
  st = boxplot.stats(dat$daylength_min)$stats
  dat = subset(dat, daylength_min > st[1] & daylength_min <= st[5])
  hist(dat$daylength_min)
  mp = lmer(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(mp))
  plot(resid(mp) ~ fitted(mp))
  qqnorm(residuals(mp))
  summary(mp)
  mp -> mp_day

  Rmp_daylength_min = rpt(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rmp_daylength_min2 = rpt(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|ID) + (1|yid) + (1|year_), grname = c("year_", "yid", "ID"), data = dat)
  Rmp_daylength_min


  dat = subset(x, sex == 2 & season == '1_preBreeding')
  hist(dat$daylength_min)
  st = boxplot.stats(dat$daylength_min)$stats
  dat = subset(dat, daylength_min > st[1] & daylength_min <= st[5])
  hist(dat$daylength_min)
  fp = lmer(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(fp))
  plot(resid(fp) ~ fitted(fp))
  qqnorm(residuals(fp))
  summary(fp)
  fp -> fp_day

  Rfp_daylength_min = rpt(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rfp_daylength_min2 = rpt(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rfp_daylength_min


  dat = subset(x, sex == 1 & season == '0_spring')
  hist(dat$daylength_min)
  st = boxplot.stats(dat$daylength_min)$stats
  dat = subset(dat, daylength_min > st[1] & daylength_min <= st[5])
  hist(dat$daylength_min)
  ms = lmer(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(ms))
  plot(resid(ms) ~ fitted(ms))
  qqnorm(residuals(ms))
  summary(ms)
  ms -> ms_day
  Rms_daylength_min = rpt(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rms_daylength_min2 = rpt(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rms_daylength_min

  dat = subset(x, sex == 2 & season == '0_spring')
  hist(dat$daylength_min)
  st = boxplot.stats(dat$daylength_min)$stats
  dat = subset(dat, daylength_min > st[1] & daylength_min <= st[5])
  hist(dat$daylength_min)
  fs = lmer(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(fs))
  plot(resid(fs) ~ fitted(fs))
  qqnorm(residuals(fs))
  summary(fs)
  fs -> fs_day
  Rfs_daylength_min = rpt(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rfs_daylength_min2 = rpt(daylength_min ~ SumRainfall + Temperature2m + poly(actualDaylength_min,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rfs_daylength_min

}


}
