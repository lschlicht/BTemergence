#0a. fetch packages
{
  require(lme4)
  require(ggplot2)
  require(rptR)
}

#0b. fetch data
{
  x = runFetchData()
  w = fetchWeatherData()
  w[, yearDay := yday(date_)]
  w[, SumRainfall := as.numeric(sub(",", ".", SumRainfall))]
  w[, Temperature2m := as.numeric(sub(",", ".", Temperature2m))]

  x[, daylength := 24 - as.numeric(difftime(out_, in_, units = "hours"))]
  x[, SumRainfall_dusk := as.numeric(sub(",", ".", SumRainfall_dusk))]
  x[, SumRainfall_dawn := as.numeric(sub(",", ".", SumRainfall_dawn))]
  x[, Temperature2m_dusk := as.numeric(sub(",", ".", Temperature2m_dusk))]
  x[, Temperature2m_dawn := as.numeric(sub(",", ".", Temperature2m_dawn))]
  x[, rainYN_dusk := ifelse(as.numeric(SumRainfall_dusk) > 0, 1, as.numeric(SumRainfall_dusk))]
  x[, rainYN_dawn := ifelse(as.numeric(SumRainfall_dawn) > 0, 1, as.numeric(SumRainfall_dawn))]
  x[, rainYN := ifelse(as.numeric(SumRainfall) > 0, 1, as.numeric(SumRainfall))]
  x[, prop_daylength_used := daylength / (24 - as.numeric(difftime(sunrise, sunset, unit = 'hours')))]
  x[, Week := rel_day %/% 7]
  x[, season := '0_spring']
  x[rel_day < -30 | rel_day >= 200, season := "2_winter"]
  x[rel_day >= -30 & rel_day < 0 , season := "1_preBreeding"]
  x[, yearDay_sunturn := as.numeric(yday(date_))]
  x[yearDay_sunturn > 200, yearDay_sunturn := -366 + yearDay_sunturn]
  x[, yearDay_sunturn := yearDay_sunturn - 10]
  x[, yid := paste(substring(firstEgg, 1, 4), ID, sep = '_')]
  x[, sunrise_time := as.numeric(as.ITime(sunrise))/60/60]
  x[, sunset_time := as.numeric(as.ITime(sunrise))/60/60]
  x[, actualDaylength := 24 - as.numeric(difftime(sunrise, sunset, unit = 'hours'))]
  x[, daylength_min := daylength * 60]
  x[, actualDaylength_min := actualDaylength * 60]
  x[, age := as.factor(age)]

  copy(x) -> backup


  #remove potential replacement broods
  hist(x[year_ == 2016, c_firstEgg], 30)
  egg_range = subset(x, select = c('year_', 'firstEgg', 'c_firstEgg', 'ID'))
  egg_range = unique(egg_range, by = names(egg_range))
  egg_range[, max_ := (-1)*min(c_firstEgg), by = year_]
  egg_range = subset(egg_range, c_firstEgg < max_)
  x = subset(x, firstEgg %in% egg_range$firstEgg)
  hist(x[,c_firstEgg])


}

#1. descriptive part
{
  #a. plot overview
  {
    #run each plot twice: for males and for females; the first will be plotted on the left, the second on the right.
    jpeg(file = "weeklyAcrossYear_rel_day.jpg", width = 1440, height = 720)
    dev.off()
    jpeg(file = "dailyAcrossYear.jpg", width = 1440, height = 720)
    dev.off()
    jpeg(file = "weeklyAcrossYear_date.jpg", width = 1440, height = 720)
    dev.off()


    CEX = 0.8; PCH1 = 16; COL1 = 'black'; COL2 = 'black'; COL3 = 'black'
    par(mar = c(4.5, 4.5, 0.5, 0.5))
    par(mfcol = c(3,2))
    SEX = 2
    SEX = 1


    #Figure 1: weekly plot across year (rel_day)
    {
      dp_all = subset(x, sex == SEX)
      dp_all[, Week := rel_day %/% 7]

      dp_all[, ':=' (mean_time_to_sunrise_min = mean(time_to_sunrise_min, na.rm = TRUE), mean_time_to_sunset_min = mean(time_to_sunset_min, na.rm = TRUE), mean_daylength_hour = mean(prop_daylength_used, na.rm = TRUE), se_time_to_sunrise_min = sd(time_to_sunrise_min, na.rm = TRUE)/sqrt(length(na.omit(time_to_sunrise_min))), se_time_to_sunset_min = sd(time_to_sunset_min, na.rm = TRUE)/sqrt(length(na.omit(time_to_sunset_min))), se_daylength_hour = sd(prop_daylength_used, na.rm = TRUE)/sqrt(length(na.omit(prop_daylength_used))), N = length(na.omit(time_to_sunset_min))), by = Week]
      dp_all[, ':=' (conf_int_time_to_sunrise_min = se_time_to_sunrise_min*1.96, conf_int_time_to_sunset_min = se_time_to_sunset_min*1.96, conf_int_daylength_hour = se_daylength_hour*1.96), by = Week]
      dp_all = subset(dp_all, select = c('Week', 'mean_time_to_sunrise_min', 'mean_time_to_sunset_min', 'mean_daylength_hour', 'se_time_to_sunrise_min', 'se_time_to_sunset_min', 'se_daylength_hour', 'conf_int_time_to_sunrise_min', 'conf_int_time_to_sunset_min', 'conf_int_daylength_hour', 'N'))
      dp_all = unique(dp_all, by = names(dp_all))
      setkey(dp_all, Week)

      plot(c(-20, 39), c(-70, 10), type = 'n', xlab = '', ylab = list("Sleep onset (minutes to sunset)", cex = 1.9), cex.axis = 1.4, las = 1)
      rect(-2, -70, 7, -68, border = NA, col = 'black')
      abline(h = c(-60, -40, -20), col = 'light grey', lty = 3); abline(v = c(-10:15)*5, col = 'light grey', lty = 3); abline(h = 0, col = 'black', lty = 3)
      text(-22, 6.5, labels = paste("\u263C", ifelse(SEX == 2, "\u2640", "\u2642"), sep = ''), cex = 4, adj = 0)
      box()
      points(dp_all$Week, dp_all$mean_time_to_sunset_min, cex = CEX, pch = PCH1, col = COL2)
      arrows(dp_all$Week, dp_all$mean_time_to_sunset_min+dp_all$conf_int_time_to_sunset_min, dp_all$Week, dp_all$mean_time_to_sunset_min-dp_all$conf_int_time_to_sunset_min, code = 3, length = 0, col = COL2)

      plot(c(-20, 39), c(-40, 40), type = 'n', xlab = '', ylab = list("Emergence time (minutes to sunrise)", cex = 1.9), cex.axis = 1.4, las = 1)
      rect(-2, -40, 7, -38, border = NA, col = 'black')
      abline(h = c(-20, 20, 40), col = 'light grey', lty = 3); abline(v = c(-10:15)*5, col = 'light grey', lty = 3); abline(h = 0, lty = 3)
      text(-22.5, 37, labels = paste("\u263D", ifelse(SEX == 2, "\u2640", "\u2642"), sep = ''), cex = 4, adj = 0)
      box()
      points(dp_all$Week, dp_all$mean_time_to_sunrise_min, cex = CEX, pch = PCH1, col = COL1)
      arrows(dp_all$Week, dp_all$mean_time_to_sunrise_min+dp_all$conf_int_time_to_sunrise_min, dp_all$Week, dp_all$mean_time_to_sunrise_min-dp_all$conf_int_time_to_sunrise_min, code = 3, length = 0, col = COL1)
      text(x = dp_all$Week, y = 25, labels = dp_all$N, adj = 0.5, srt = 90, cex = 1.45)
      text(-19, 25, "N", adj = 0.5, cex = 2)

      plot(c(-20, 39), c(0.88, 1.06), type = 'n', xlab = list("Week to first egg", cex = 1.9), ylab = list("Proportion of time spent active", cex = 1.9), cex.axis = 1.4, las = 1)
      rect(-2, 0.88, 7, 0.885, border = NA, col = 'black')
      abline(h = c(0.9, 0.95, 1.05), col = 'light grey', lty = 3); abline(v = c(-10:15)*5, col = 'light grey', lty = 3); abline(h = 1, lty = 3)
      text(-22, 1.053, labels = paste("\u2666", ifelse(SEX == 2, "\u2640", "\u2642"), sep = ''), cex = 4, adj = 0)
      box()
      points(dp_all$Week, dp_all$mean_daylength_hour, cex = CEX, pch = PCH1, col = COL3)
      arrows(dp_all$Week, dp_all$mean_daylength_hour+dp_all$conf_int_daylength_hour, dp_all$Week, dp_all$mean_daylength_hour-dp_all$conf_int_daylength_hour, code = 3, length = 0, col = COL3)

    }

    #Figure S1: daily plot across year
    {
    dp_all = subset(x, sex == SEX)
      dp_all[, ':=' (mean_time_to_sunrise_min = mean(time_to_sunrise_min, na.rm = TRUE), mean_time_to_sunset_min = mean(time_to_sunset_min, na.rm = TRUE), mean_daylength_hour = mean(prop_daylength_used, na.rm = TRUE), se_time_to_sunrise_min = sd(time_to_sunrise_min, na.rm = TRUE)/sqrt(length(na.omit(time_to_sunrise_min))), se_time_to_sunset_min = sd(time_to_sunset_min, na.rm = TRUE)/sqrt(length(na.omit(time_to_sunset_min))), se_daylength_hour = sd(prop_daylength_used, na.rm = TRUE)/sqrt(length(na.omit(prop_daylength_used))), N = length(na.omit(time_to_sunset_min))), by = yday(date_)]
      dp_all[, ':=' (conf_int_time_to_sunrise_min = se_time_to_sunrise_min*1.96, conf_int_time_to_sunset_min = se_time_to_sunset_min*1.96, conf_int_daylength_hour = se_daylength_hour*1.96), by = yday(date_)]

    dp_all[, yearDay := yday(date_)]
    dp_all = subset(dp_all, select = c('yearDay', 'mean_time_to_sunrise_min', 'mean_time_to_sunset_min', 'mean_daylength_hour', 'se_time_to_sunrise_min', 'se_time_to_sunset_min', 'se_daylength_hour', 'conf_int_time_to_sunrise_min', 'conf_int_time_to_sunset_min', 'conf_int_daylength_hour', 'N'))
    dp_all = unique(dp_all, by = names(dp_all))
    setkey(dp_all, yearDay)


    plot(c(0, 365), c(-70, 10), type = 'n', xlab = '', ylab = list("Entering time (in minutes to sunset)", cex = 1.5))
    rect(90, -70, 181, -68, border = NA, col = 'black')
    abline(h = c(-60, -40, -20), col = 'light grey', lty = 3); abline(v = c(-10:15)*25, col = 'light grey', lty = 3); abline(h = 0, col = 'black', lty = 3)
    text(-5, -5, labels = "\u2600", cex = 3, adj = 0.5)
    text(-5, 5, labels = "\u263E", cex = 3, adj = 0.3)
    points(dp_all$yearDay, dp_all$mean_time_to_sunset_min, cex = CEX, pch = PCH1, col = COL2)
    arrows(dp_all$yearDay, dp_all$mean_time_to_sunset_min+dp_all$conf_int_time_to_sunset_min, dp_all$yearDay, dp_all$mean_time_to_sunset_min-dp_all$conf_int_time_to_sunset_min, code = 3, length = 0, col = COL2)

    plot(c(0, 365), c(-40, 40), type = 'n', xlab = '', ylab = list("Emergence time (in minutes to sunrise)", cex = 1.5))
    rect(90, -40, 181, -38, border = NA, col = 'black')
    abline(h = c(-20, 20, 40), col = 'light grey', lty = 3); abline(v = c(-10:15)*25, col = 'light grey', lty = 3); abline(h = 0, lty = 3)
    text(-5, 5, labels = "\u2600", cex = 3, adj = 0.5)
    text(-5, -5, labels = "\u263E", cex = 3, adj = 0.3)
    points(dp_all$yearDay, dp_all$mean_time_to_sunrise_min, cex = CEX, pch = PCH1, col = COL1)
    arrows(dp_all$yearDay, dp_all$mean_time_to_sunrise_min+dp_all$conf_int_time_to_sunrise_min, dp_all$yearDay, dp_all$mean_time_to_sunrise_min-dp_all$conf_int_time_to_sunrise_min, code = 3, length = 0, col = COL1)

    text(x = dp_all$yearDay[which((1:nrow(dp_all))%%3 == 1)], y = 25, labels = dp_all$N[which((1:nrow(dp_all))%%3 == 1)], adj = 0.5, srt = 90, cex = 0.7)
    text(x = dp_all$yearDay[which((1:nrow(dp_all))%%3 == 2)], y = 30, labels = dp_all$N[which((1:nrow(dp_all))%%3 == 2)], adj = 0.5, srt = 90, cex = 0.7)
    text(x = dp_all$yearDay[which((1:nrow(dp_all))%%3 == 0)], y = 35, labels = dp_all$N[which((1:nrow(dp_all))%%3 == 0)], adj = 0.5, srt = 90, cex = 0.7)
    text(-5, 30, "N", adj = 0.5, cex = 1.5)

    plot(c(0, 365), c(0.88, 1.07), type = 'n', xlab = list("Day of the year", cex = 1.5), ylab = list("Time of activity / Time from sunrise to sunset", cex = 1.5))
    rect(90, 0.88, 181, 0.885, border = NA, col = 'black')
    abline(h = c(0.9, 0.95, 1.05), col = 'light grey', lty = 3); abline(v = c(0:15)*25, col = 'light grey', lty = 3); abline(h = 1, lty = 3)
    points(dp_all$yearDay, dp_all$mean_daylength_hour, cex = CEX, pch = PCH1, col = COL3)
    arrows(dp_all$yearDay, dp_all$mean_daylength_hour+dp_all$se_daylength_hour, dp_all$yearDay, dp_all$mean_daylength_hour-dp_all$se_daylength_hour, code = 3, length = 0, col = COL3)

    }

    #Figure 1: weekly plot across year (yearDay)
    {
      dp_all = subset(x, sex == SEX)
      dp_all[, Week := week(date_)]
      dp_all[, ':=' (mean_time_to_sunrise_min = mean(time_to_sunrise_min, na.rm = TRUE), mean_time_to_sunset_min = mean(time_to_sunset_min, na.rm = TRUE), mean_daylength_hour = mean(prop_daylength_used, na.rm = TRUE), se_time_to_sunrise_min = sd(time_to_sunrise_min, na.rm = TRUE)/sqrt(length(na.omit(time_to_sunrise_min))), se_time_to_sunset_min = sd(time_to_sunset_min, na.rm = TRUE)/sqrt(length(na.omit(time_to_sunset_min))), se_daylength_hour = sd(prop_daylength_used, na.rm = TRUE)/sqrt(length(na.omit(prop_daylength_used))), N = length(na.omit(time_to_sunset_min))), by = Week]
      dp_all[, ':=' (conf_int_time_to_sunrise_min = se_time_to_sunrise_min*1.96, conf_int_time_to_sunset_min = se_time_to_sunset_min*1.96, conf_int_daylength_hour = se_daylength_hour*1.96), by = Week]
      dp_all = subset(dp_all, select = c('Week', 'mean_time_to_sunrise_min', 'mean_time_to_sunset_min', 'mean_daylength_hour', 'se_time_to_sunrise_min', 'se_time_to_sunset_min', 'se_daylength_hour', 'conf_int_time_to_sunrise_min', 'conf_int_time_to_sunset_min', 'conf_int_daylength_hour', 'N'))
      dp_all = unique(dp_all, by = names(dp_all))
      setkey(dp_all, Week)


      plot(c(0, 53), c(-70, 10), type = 'n', xlab = '', ylab = list("Entering time (in minutes to sunset)", cex = 1.5))
      rect(14, -70, 28, -68, border = NA, col = 'black')
      abline(h = c(-60, -40, -20), col = 'light grey', lty = 3); abline(v = c(-10:15)*5, col = 'light grey', lty = 3); abline(h = 0, col = 'black', lty = 3)
      text(-1, -5, labels = "\u2600", cex = 3, adj = 0.5)
      text(-1, 5, labels = "\u263E", cex = 3, adj = 0.3)
      points(dp_all$Week, dp_all$mean_time_to_sunset_min, cex = CEX, pch = PCH1, col = COL2)
      arrows(dp_all$Week, dp_all$mean_time_to_sunset_min+dp_all$conf_int_time_to_sunset_min, dp_all$Week, dp_all$mean_time_to_sunset_min-dp_all$conf_int_time_to_sunset_min, code = 3, length = 0, col = COL2)

      plot(c(0, 53), c(-40, 40), type = 'n', xlab = '', ylab = list("Emergence time (in minutes to sunrise)", cex = 1.5))
      rect(14, -40, 28, -38, border = NA, col = 'black')
      abline(h = c(-20, 20, 40), col = 'light grey', lty = 3); abline(v = c(-10:15)*5, col = 'light grey', lty = 3); abline(h = 0, lty = 3)
      text(-1, 5, labels = "\u2600", cex = 3, adj = 0.5)
      text(-1, -5, labels = "\u263E", cex = 3, adj = 0.3)
      points(dp_all$Week, dp_all$mean_time_to_sunrise_min, cex = CEX, pch = PCH1, col = COL1)
      arrows(dp_all$Week, dp_all$mean_time_to_sunrise_min+dp_all$conf_int_time_to_sunrise_min, dp_all$Week, dp_all$mean_time_to_sunrise_min-dp_all$conf_int_time_to_sunrise_min, code = 3, length = 0, col = COL1)
      text(x = dp_all$Week, y = 30, labels = dp_all$N, adj = 0.5, srt = 90)
      text(0, 30, "N", adj = 0.5, cex = 1.5)

      plot(c(0, 53), c(0.88, 1.06), type = 'n', xlab = list("Week of the year", cex = 1.5), ylab = list("Time of activity / Time from sunrise to sunset", cex = 1.5))
      rect(14, 0.88, 28, 0.885, border = NA, col = 'black')
      abline(h = c(0.9, 0.95, 1.05), col = 'light grey', lty = 3); abline(v = c(0:15)*5, col = 'light grey', lty = 3); abline(h = 1, lty = 3)
      points(dp_all$Week, dp_all$mean_daylength_hour, cex = CEX, pch = PCH1, col = COL3)
      arrows(dp_all$Week, dp_all$mean_daylength_hour+dp_all$conf_int_daylength_hour, dp_all$Week, dp_all$mean_daylength_hour-dp_all$conf_int_daylength_hour, code = 3, length = 0, col = COL3)

    }
  }

  #b. models rain and temperature ###TAKE A WHILE TO RUN BECAUSE OF REPEATABILITIES!###
  #DAWN
  {
  dat = subset(x, sex == 1 & season == '2_winter')
  hist(dat$time_to_sunrise_min)
  st = boxplot.stats(dat$time_to_sunrise_min)$stats
  dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])
  hist(dat$time_to_sunrise_min)

  mw_dawn = lmer(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|ID) + (1|yid) + (1|year_), data = dat)
  hist(resid(mw_dawn))
  plot(resid(mw_dawn) ~ fitted(mw_dawn))
  qqnorm(residuals(mw_dawn))
  summary(mw_dawn)
  Rmw_dawn = rpt(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rmw_dawn2 = rpt(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rmw_dawn2



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

  Rfw_dawn = rpt(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|ID) + (1|year_ID) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rfw_dawn2 = rpt(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)

  Rfw_dawn2

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

  Rmp_dawn = rpt(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rmp_dawn2 = rpt(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rmp_dawn2


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
  Rfp_dawn = rpt(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|ID) + (1|year_ID) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rfp_dawn2 = rpt(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rfp_dawn2


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
  Rms_dawn = rpt(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rms_dawn2 = rpt(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rms_dawn2

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
  Rfs_dawn = rpt(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|ID) + (1|yid) + (1|year_), grname = c("ID", "yid", "year_"), data = dat)
  Rfs_dawn2 = rpt(time_to_sunrise_min ~ SumRainfall_dawn + Temperature2m_dawn + poly(sunrise_time,2) + (1|year_) + (1|yid) + (1|ID), grname = c("year_", "yid", "ID"), data = dat)
  Rfs_dawn2

}

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


  #Figure 2: Rainfall and T (based on the models above)
  jpeg(file = "RainfallTemperature.jpg", width = 720, height = 720)
  {
  POINTS = as.data.table(rbind(
    summary(mw_dawn)$coefficients[2:3,1:2],
    summary(mw_dusk)$coefficients[2:3,1:2],
    summary(mw_day)$coefficients[2:3,1:2],
    summary(fw_dawn)$coefficients[2:3,1:2],
    summary(fw_dusk)$coefficients[2:3,1:2],
    summary(fw_day)$coefficients[2:3,1:2],
    summary(mp_dawn)$coefficients[2:3,1:2],
    summary(mp_dusk)$coefficients[2:3,1:2],
    summary(mp_day)$coefficients[2:3,1:2],
    summary(fp_dawn)$coefficients[2:3,1:2],
    summary(fp_dusk)$coefficients[2:3,1:2],
    summary(fp_day)$coefficients[2:3,1:2],
    summary(ms_dawn)$coefficients[2:3,1:2],
    summary(ms_dusk)$coefficients[2:3,1:2],
    summary(ms_day)$coefficients[2:3,1:2],
    summary(fs_dawn)$coefficients[2:3,1:2],
    summary(fs_dusk)$coefficients[2:3,1:2],
    summary(fs_day)$coefficients[2:3,1:2]))
  POINTS[, var := rep(c('Rain', 'T'), 18)]
  POINTS[, season := rep(c('0winter', '1preBreeding', '2spring'), each = 12)]
  POINTS[, timing := rep(rep(c("dawn", "dusk", "day"), each = 2), 6)]
  POINTS[, sex := rep(rep(c('male', 'female'), each = 6), 3)]
  POINTS[, conf_int := 1.96*POINTS$'Std. Error']
  setkey(POINTS, var, season, sex)
  POINTS[, XX := as.numeric(rep(rep(1:6, each = 3), 2))]
  POINTS[timing == "dawn", XX := XX-0.2]
  POINTS[timing == "day", XX := XX+0.2]
  POINTS[timing == "dusk", LTY := 1]
  POINTS[timing == "dawn", LTY := 1]
  POINTS[timing == "day", LTY := 1]
  POINTS[timing == "dawn", PCH := "\u263C"]
  POINTS[timing == "dusk", PCH := "\u263D"]
  POINTS[timing == "day", PCH := "\u2666"]


  par(mar = c(7, 4.1, 1.1, 1.1))
  par(mfrow = c(1,2))
  jump = 0.3
  plot(c(0.5, 6.5), c(-16, 10), type = 'n', xlab = '', ylab = list("Estimate ± Confidence Interval", cex = 1.4), xaxt = 'n')
  points(POINTS$XX[1:18], POINTS$Estimate[1:18], pch = POINTS$PCH[1:18])
  arrows(POINTS$XX[1:18], POINTS$Estimate[1:18] - POINTS$conf_int[1:18], POINTS$XX[1:18], POINTS$Estimate[1:18] - jump, angle = 90, code = 1, length = 0.02, lty = POINTS$LTY[1:18])
  arrows(POINTS$XX[1:18], POINTS$Estimate[1:18] + POINTS$conf_int[1:18], POINTS$XX[1:18], POINTS$Estimate[1:18] + jump, angle = 90, code = 1, length = 0.02, lty = POINTS$LTY[1:18])
  axis(1, at = c(3.5), labels = c('Rainfall'), line = 3, tick = FALSE, cex.axis = 1.8)
  axis(1, at = c(1.5, 3.5, 5.5), labels = c("\u2744", "\u266B", "\u2B2F"), line = 1, tick = FALSE, cex.axis = 3)
  axis(1, at = c(1:6), labels = rep(c("\u2640", "\u2642"), 3), line = -3, tick = FALSE, cex.axis = 1.2)
  abline(v = c(2.5, 4.5), lty = 3)
  abline(h = 0, lty = 3)


  jump = 0.05

  plot(c(0.5, 6.5), c(-1, 2.5), type = 'n', xlab = '', ylab = list("Estimate ± Confidence Interval", cex = 1.4), xaxt = 'n')
  points(POINTS$XX[19:36], POINTS$Estimate[19:36], pch = POINTS$PCH[19:36])
  arrows(POINTS$XX[19:36], POINTS$Estimate[19:36] - POINTS$conf_int[19:36], POINTS$XX[19:36], POINTS$Estimate[19:36] - jump, angle = 90, code = 1, length = 0.02, lty = POINTS$LTY[19:36])
  arrows(POINTS$XX[19:36], POINTS$Estimate[19:36] + POINTS$conf_int[19:36], POINTS$XX[19:36], POINTS$Estimate[19:36] + jump, angle = 90, code = 1, length = 0.02, lty = POINTS$LTY[19:36])
  axis(1, at = c(3.5), labels = c('Temperature'), line = 3, tick = FALSE, cex.axis = 1.8)
  axis(1, at = c(1.5, 3.5, 5.5), labels = c("\u2744", "\u266B", "\u2B2F"), line = 1, tick = FALSE, cex.axis = 3)
  axis(1, at = c(1:6), labels = rep(c("\u2640", "\u2642"), 3), line = -3, tick = FALSE, cex.axis = 1.2)
  abline(v = c(2.5, 4.5), lty = 3)
  abline(h = 0, lty = 3)
  }
  dev.off()


}

#2. 'pure' replication
{
  #Great tit and Junco data
  WWJU <- read.csv("DataGrahamEtAl/JuncoRData.csv")
  names(WWJU) = make.names(names(WWJU))
  GRTI <- read.csv("DataGrahamEtAl/GreatTitRData.csv")
  names(GRTI) = make.names(names(GRTI))
  GRTI$poly1 = poly(GRTI$apr.date,2)[,1]
  GRTI$poly2 = poly(GRTI$apr.date,2)[,2]



  GRTIModel <-lmer(wakeup ~ egg1 + inc.day2 + apr.date + (1|nestid), data=GRTI)
  summary(GRTIModel)
  #problem: inc.day2, apr.date and egg1 are highly collinear; females are recorded either early, in the middle or late during incubation --> inc.day and egg1 one are strongly negatively correlated. Including inc.day with this  data therefore does not make much sense.
  GRTI_REP = rpt(wakeup ~ inc.day + (1|nestid), grname = "nestid", data = GRTI, datatype = "Gaussian")
  #repeatabilities can be replicated

  WWJUModel <-lmer(wakeup ~ egg1 + incdate2 + date + (1|nestid) + (1|Year), data=WWJU)
  summary(WWJUModel)
  #strong collinearities, but less problematic than with great tits.

  #Great tits our model; use apr.date not inc day because of strong colliearities.
  GRTIModel <-lmer(wakeup ~ scale(egg1, scale = FALSE, center = TRUE) + (1|nestid), data=GRTI)
  summary(GRTIModel)

  hist(resid(GRTIModel))
  qqnorm(residuals(GRTIModel))
  #model fit ok, no effect of egg1 remaining

  #calculating repeatabilities while correcting for april date (and first egg; doesn't change much)
  GRTI_REP2 = rpt(wakeup ~ (1|nestid), grname = "nestid", data = GRTI, datatype = "Gaussian")


  #Juncos our model
  WWJUModel <-lmer(wakeup ~ scale(egg1, scale = FALSE, center = TRUE) + (1|nestid) + (1|Year), data=WWJU)
  summary(WWJUModel)
  hist(resid(WWJUModel))
  qqnorm(residuals(WWJUModel))
  #model fit ok, effect of egg1 remaining
  #year effects and effects of first egg almost indistinguishable  --> use only first egg

  WWJU_REP2 = rpt(wakeup ~ (1|nestid), grname = "nestid", data = WWJU, datatype = "Gaussian")

  #blue tits our model
  dat = subset(x, sex == 2 & rel_day > (clutch-1) & date_ < hatchDate)
  hist(dat$time_to_sunrise_min)
  st = boxplot.stats(dat$time_to_sunrise_min)$stats
  dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])
  hist(dat$time_to_sunrise_min)
  dat[, inc_day := rel_day + clutch - 1]

  BTmodel <- lmer(time_to_sunrise_min ~ c_firstEgg + (1|ID) + (1|year_), data = dat)
  hist(resid(BTmodel))
  qqnorm(residuals(BTmodel))


  summary(BTmodel)
  #collinearity small, effect of first egg present.

  BT_REP2 = rpt(time_to_sunrise_min ~ (1|ID) + (1|year_), grname = "ID", data = dat)

  }

#3. full model set: all combinations of periods (winter, pre-breeding, incubation) and response variables (daylength, emergence, sleep onset) together with the explanatory variables first egg, female age, and poly(sunrise time/sunset time/daylength, 2) respectively.
{
  #female incubation, emergence
  {
    dat = subset(x, sex == 2 & rel_day >= (clutch - 1) & date_ < hatchDate)
    nn = nrow(dat)
    hist(dat$time_to_sunrise_min)
    st = boxplot.stats(dat$time_to_sunrise_min)$stats
    dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])
    hist(dat$time_to_sunrise_min)
    nrow(dat)/nn #0.969

    m <- lmer(time_to_sunrise_min ~ c_firstEgg*age + poly(sunrise_time,2) + (1|ID) + (1|year_), data = dat)
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))

    f_inc_m = summary(m)
    f_inc_m

    m <- lmer(time_to_sunrise_min ~ c_firstEgg + poly(sunrise_time,2) + (1|ID) + (1|year_), data = subset(dat, age == 1))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))

    f_inc_m_age1 = summary(m)

    m <- lmer(time_to_sunrise_min ~ c_firstEgg + poly(sunrise_time,2) + (1|ID) + (1|year_), data = subset(dat, age == 2))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))

    f_inc_m_age2 = summary(m)

    R_f_inc_m = rpt2(time_to_sunrise_min ~ poly(sunrise_time,2) + age + (1|ID) + (1|yid) + (1|year_), grname = c("ID", 'yid', 'year_'), data = dat)
  }

  #female preBreeding, emergence
  {
  dat = subset(x, sex == 2 & rel_day >= -30 & rel_day < 0)
  nn = nrow(dat)
  hist(dat$time_to_sunrise_min)
  st = boxplot.stats(dat$time_to_sunrise_min)$stats
  dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])
  hist(dat$time_to_sunrise_min)
  nrow(dat)/nn

  m <- lmer(time_to_sunrise_min ~ c_firstEgg*age + poly(sunrise_time,2) + (1|ID) + (1|year_), data = dat)
  hist(resid(m))
  qqnorm(residuals(m))
  plot(resid(m) ~ fitted(m))
  f_pb_m = summary(m)
  f_pb_m

  f_pb_mc = confint(m)

  m <- lmer(time_to_sunrise_min ~ c_firstEgg + poly(sunrise_time,2) + (1|ID) + (1|year_), data = subset(dat, age == 1))
  hist(resid(m))
  qqnorm(residuals(m))
  plot(resid(m) ~ fitted(m))
  f_pb_m_age1 = summary(m)

  m <- lmer(time_to_sunrise_min ~ c_firstEgg + poly(sunrise_time,2) + (1|ID) + (1|year_), data = subset(dat, age == 2))
  hist(resid(m))
  qqnorm(residuals(m))
  plot(resid(m) ~ fitted(m))
  f_pb_m_age2 = summary(m)


  R_f_pb_m = rpt2(time_to_sunrise_min ~ poly(sunrise_time,2) + age + (1|ID) + (1|yid) + (1|year_), grname = c("ID", 'yid', 'year_'), data = dat)
}

  #female winter, emergence
  {
  dat = subset(x, sex == 2 & rel_day >= 200 | rel_day < -3)
  nrow(dat) -> nn
  hist(dat$time_to_sunrise_min)
  st = boxplot.stats(dat$time_to_sunrise_min)$stats
  dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])
  hist(dat$time_to_sunrise_min)
  nrow(dat)/nn

  m <- lmer(time_to_sunrise_min ~ c_firstEgg*age + poly(sunrise_time,2) + (1|ID) + (1|year_), data = dat)
  hist(resid(m))
  qqnorm(residuals(m))
  plot(resid(m) ~ fitted(m))
  f_w_m = summary(m)
  f_w_m

  f_w_mc = confint(m)

  m <- lmer(time_to_sunrise_min ~ c_firstEgg + poly(sunrise_time,2) + (1|ID) + (1|year_), data = subset(dat, age == 1))
  hist(resid(m))
  qqnorm(residuals(m))
  plot(resid(m) ~ fitted(m))
  f_w_m_age1 = summary(m)

  m <- lmer(time_to_sunrise_min ~ c_firstEgg + poly(sunrise_time,2) + (1|ID) + (1|year_), data = subset(dat, age == 2))
  hist(resid(m))
  qqnorm(residuals(m))
  plot(resid(m) ~ fitted(m))
  f_w_m_age2 = summary(m)

  R_f_w_m = rpt2(time_to_sunrise_min ~ poly(sunrise_time,2) + age + (1|ID) + (1|yid) + (1|year_), grname = c("ID", 'yid', 'year_'), data = dat)

  }


  #female incubation, sleep onset
  {
    dat = subset(x, sex == 2 & rel_day >= (clutch - 1) & date_ < hatchDate)
    nn = nrow(dat)
    hist(dat$time_to_sunset_min)
    st = boxplot.stats(dat$time_to_sunset_min)$stats
    dat = subset(dat, time_to_sunset_min > st[1] & time_to_sunset_min <= st[5])
    hist(dat$time_to_sunset_min)
    nrow(dat)/nn

    m <- lmer(time_to_sunset_min ~ c_firstEgg*age + poly(sunset_time,2) + (1|ID) + (1|year_), data = dat)
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_inc_n = summary(m)
    f_inc_n

    f_inc_nc = confint(m)

    m <- lmer(time_to_sunset_min ~ c_firstEgg + poly(sunset_time,2) + (1|ID) + (1|year_), data = subset(dat, age == 1))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_inc_n_age1 = summary(m)

    m <- lmer(time_to_sunset_min ~ c_firstEgg + poly(sunset_time,2) + (1|ID) + (1|year_), data = subset(dat, age == 2))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_inc_n_age2 = summary(m)

     R_f_inc_n = rpt2(time_to_sunset_min ~ poly(sunset_time,2) + age + (1|ID) + (1|yid) + (1|year_), grname = c("ID", 'yid', 'year_'), data = dat)
  }

  #female prebreeding, sleep onset
  {
    dat = subset(x, sex == 2 & rel_day >= -30 & rel_day < 0)
    nn = nrow(dat)
    hist(dat$time_to_sunset_min)
    st = boxplot.stats(dat$time_to_sunset_min)$stats
    dat = subset(dat, time_to_sunset_min > st[1] & time_to_sunset_min <= st[5])
    hist(dat$time_to_sunset_min)
    nrow(dat)/nn

    m <- lmer(time_to_sunset_min ~ c_firstEgg*age + poly(sunset_time,2) + (1|ID) + (1|year_), data = dat)
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_pb_n = summary(m)
    f_pb_n

    f_pb_nc = confint(m)


    m <- lmer(time_to_sunset_min ~ c_firstEgg + poly(sunset_time,2) + (1|ID) + (1|year_), data = subset(dat, age == 1))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_pb_n_age1 = summary(m)

    m <- lmer(time_to_sunset_min ~ c_firstEgg + poly(sunset_time,2) + (1|ID) + (1|year_), data = subset(dat, age == 2))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_pb_n_age2 = summary(m)

    R_f_pb_n = rpt2(time_to_sunset_min ~ poly(sunset_time,2) + age + (1|ID) + (1|yid) + (1|year_), grname = c("ID", 'yid', 'year_'), data = dat)
  }

  #female winter, sleep onset
  {
    dat = subset(x, sex == 2 & rel_day >= 200 | rel_day < -3)
    nn = nrow(dat)
    hist(dat$time_to_sunset_min)
    st = boxplot.stats(dat$time_to_sunset_min)$stats
    dat = subset(dat, time_to_sunset_min > st[1] & time_to_sunset_min <= st[5])
    hist(dat$time_to_sunset_min)
    nrow(dat)/nn

    m <- lmer(time_to_sunset_min ~ c_firstEgg*age + poly(sunset_time,2) + (1|ID) + (1|year_), data = dat)
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_w_n = summary(m)
    f_w_n

    f_w_nc = confint(m)

    m <- lmer(time_to_sunset_min ~ c_firstEgg + poly(sunset_time,2) + (1|ID) + (1|year_), data = subset(dat, age == 1))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_w_n_age1 = summary(m)

    m <- lmer(time_to_sunset_min ~ c_firstEgg + poly(sunset_time,2) + (1|ID) + (1|year_), data = subset(dat, age == 2))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_w_n_age2 = summary(m)

    R_f_w_n = rpt2(time_to_sunset_min ~ poly(sunset_time,2) + age + (1|ID) + (1|yid) + (1|year_), grname = c("ID", 'yid', 'year_'), data = dat)
  }


  #female incubation, daylength
  {
    dat = subset(x, sex == 2 & rel_day >= (clutch - 1) & date_ < hatchDate)
    nn = nrow(dat)
    hist(dat$daylength_min)
    st = boxplot.stats(dat$daylength_min)$stats
    dat = subset(dat, daylength_min > st[1] & daylength_min <= st[5])
    hist(dat$daylength_min)
    nrow(dat)/nn

    m <- lmer(daylength_min ~ c_firstEgg*age + poly(actualDaylength,2) + (1|ID) + (1|year_), data = dat)
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_inc_d = summary(m)
    f_inc_d

    f_inc_dc = confint(m)

    m <- lmer(daylength_min ~ c_firstEgg + poly(actualDaylength,2) + (1|ID) + (1|year_), data = subset(dat, age == 1))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_inc_d_age1 = summary(m)


    m <- lmer(daylength_min ~ c_firstEgg + poly(actualDaylength,2) + (1|ID) + (1|year_), data = subset(dat, age == 2))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_inc_d_age2 = summary(m)

    R_f_inc_d = rpt2(daylength_min ~ poly(actualDaylength) + age + (1|ID) + (1|yid) + (1|year_), grname = c("ID", 'yid', 'year_'), data = dat)
  }

  #female pre-breeding, daylength
  {
    dat = subset(x, sex == 2 & rel_day >= -30 & rel_day < 0)
    nn = nrow(dat)
    hist(dat$daylength_min)
    st = boxplot.stats(dat$daylength_min)$stats
    dat = subset(dat, daylength_min > st[1] & daylength_min <= st[5])
    hist(dat$daylength_min)
    nrow(dat)/nn

    m <- lmer(daylength_min ~ c_firstEgg*age + poly(actualDaylength,2) + (1|ID) + (1|year_), data = dat)
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_pb_d = summary(m)
    f_pb_d

    f_pb_dc = confint(m)

    m <- lmer(daylength_min ~ c_firstEgg + poly(actualDaylength,2) + (1|ID) + (1|year_), data = subset(dat, age == 1))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_pb_d_age1 = summary(m)

    m <- lmer(daylength_min ~ c_firstEgg + poly(actualDaylength,2) + (1|ID) + (1|year_), data = subset(dat, age == 2))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_pb_d_age2 = summary(m)

    R_f_pb_d = rpt2(daylength_min ~ poly(actualDaylength) + age + (1|ID) + (1|yid) + (1|year_), grname = c("ID", 'yid', 'year_'), data = dat)
  }

  #female winter, daylength
  {
    dat = subset(x, sex == 2 & rel_day >= 200 | rel_day < -3)
    nn = nrow(dat)
    hist(dat$daylength_min)
    st = boxplot.stats(dat$daylength_min)$stats
    dat = subset(dat, daylength_min > st[1] & daylength_min <= st[5])
    hist(dat$daylength_min)
    nrow(dat)/nn

    m <- lmer(daylength_min ~ c_firstEgg*age + poly(actualDaylength,2) + (1|ID) + (1|year_), data = dat)
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_w_d = summary(m)
    f_w_d

    f_w_dc = confint(m)

    m <- lmer(daylength_min ~ c_firstEgg + poly(actualDaylength,2) + (1|ID) + (1|year_), data = subset(dat, age == 1))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_w_d_age1 = summary(m)

    m <- lmer(daylength_min ~ c_firstEgg + poly(actualDaylength,2) + (1|ID) + (1|year_), data = subset(dat, age == 2))
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    f_w_d_age2 = summary(m)

       R_f_w_d = rpt2(daylength_min ~ poly(actualDaylength) + age + (1|ID) + (1|yid) + (1|year_), grname = c("ID", 'yid', 'year_'), data = dat)
  }



}

#4. plot the latter
#try 1
{
   jpeg(file = "Overview sleep patterns first egg and age.jpg", width = 720, height = 720)

par(mfrow = c(3,3))
par(mar = c(2.5, 2.5, 0.5, 0.5))

#emergence
{
plot(time_to_sunrise_min ~ c_firstEgg, data = subset(x, sex == 2 & rel_day >= (clutch - 1) & date_ < hatchDate), cex = 0.5, pch = ifelse(age == 1, 1, 16), type = 'n', ylim = c(-30, 10), xlab = '', ylab = list('Time to sunrise (in minutes)', cex = 1.3), xaxt = 'n', cex.axis = 1.5)
m = f_inc_m
abline(coef(m)[1:2,1], lwd = 1)
abline(c(coef(m)[1,1]+coef(m)[3,1], coef(m)[2,1]+coef(m)[6,1]), lwd = 2)
points(x = -6, y = 8, pch = "\u263C", cex = 4)




plot(time_to_sunrise_min ~ c_firstEgg, data = subset(x, sex == 2 & rel_day >= -30 & rel_day < 0), cex = 0.5, pch = ifelse(age == 1, 1, 16), type = 'n', ylim = c(-30, 10), xlab = '', ylab = '', xaxt = 'n', cex.axis = 1.5)
m = f_pb_m
abline(coef(m)[1:2,1], lwd = 1)
abline(c(coef(m)[1,1]+coef(m)[3,1], coef(m)[2,1]+coef(m)[6,1]), lwd = 2)
points(x = -10, y = 8, pch = "\u263C", cex = 4)

plot(time_to_sunrise_min ~ c_firstEgg, data = subset(x, sex == 2 & rel_day >= 200 | rel_day < -3), cex = 0.5, pch = ifelse(age == 1, 1, 16), type = 'n', ylim = c(-30, 10), xlab = '', ylab = '', xaxt = 'n', cex.axis = 1.5)
m = f_w_m
abline(coef(m)[1:2,1], lwd = 1)
abline(c(coef(m)[1,1]+coef(m)[3,1], coef(m)[2,1]+coef(m)[6,1]), lwd = 2)
points(x = -10, y = 8, pch = "\u263C", cex = 4)
}

#sleep onset
{
plot(time_to_sunset_min ~ c_firstEgg, data = subset(x, sex == 2 & rel_day >= (clutch - 1) & date_ < hatchDate), cex = 0.5, pch = ifelse(age == 1, 1, 16), type = 'n', ylim = c(-80, -40), xlab = '', ylab = list('Time to sunset (in minutes)', cex = 1.3), xaxt = 'n', cex.axis = 1.5)
m = f_inc_n
abline(coef(m)[1:2,1], lwd = 1)
abline(c(coef(m)[1,1]+coef(m)[3,1], coef(m)[2,1]+coef(m)[6,1]), lwd = 2)
points(x = -7, y = -42, pch = "\u263D", cex = 4)


plot(time_to_sunset_min ~ c_firstEgg, data = subset(x, sex == 2 & rel_day >= -30 & rel_day < 0), cex = 0.5, pch = ifelse(age == 1, 1, 16), type = 'n', ylim = c(-30, 10), xlab = '', ylab = '', xaxt = 'n', cex.axis = 1.5)
m = f_pb_n
abline(coef(m)[1:2,1], lwd = 1)
abline(c(coef(m)[1,1]+coef(m)[3,1], coef(m)[2,1]+coef(m)[6,1]), lwd = 2)
points(x = -11, y = 8, pch = "\u263D", cex = 4)


plot(time_to_sunset_min ~ c_firstEgg, data = subset(x, sex == 2 & rel_day >= 200 | rel_day < -3), cex = 0.5, pch = ifelse(age == 1, 1, 16), type = 'n', ylim = c(-30, 10), xlab = '', ylab = '', xaxt = 'n', cex.axis = 1.5)
m = f_w_n
abline(coef(m)[1:2,1], lwd = 1)
abline(c(coef(m)[1,1]+coef(m)[3,1], coef(m)[2,1]+coef(m)[6,1]), lwd = 2)
points(x = -11, y = 8, pch = "\u263D", cex = 4)
}

#daylength
{
plot(daylength ~ c_firstEgg, data = subset(x, sex == 2 & rel_day >= (clutch - 1) & date_ < hatchDate), cex = 0.5, pch = ifelse(age == 1, 1, 16), type = 'n', ylim = c(12.75, 13.75), xlab = '', ylab = list('Daylength (hours)', cex = 1.3), cex.axis = 1.5)
m = f_inc_d
abline(coef(m)[1:2,1]/60, lwd = 1)
abline(c((coef(m)[1,1]+coef(m)[3,1])/60, (coef(m)[2,1]+coef(m)[6,1])/60), lwd = 2)
points(x = -7, y = 13.7, pch = "\u2666", cex = 4)


plot(daylength ~ c_firstEgg, data = subset(x, sex == 2 & rel_day >= -30 & rel_day < 0), cex = 0.5, pch = ifelse(age == 1, 1, 16), type = 'n', ylim = c(12.5, 13.5), xlab = list('first egg (centered within year)', cex = 1.3), ylab = '', cex.axis = 1.5)
m = f_pb_d
abline(coef(m)[1:2,1]/60, lwd = 1)
abline(c((coef(m)[1,1]+coef(m)[3,1])/60, (coef(m)[2,1]+coef(m)[6,1])/60), lwd = 2)
points(x = -10, y = 13.45, pch = "\u2666", cex = 4)


plot(daylength ~ c_firstEgg, data = subset(x, sex == 2 & rel_day >= 200 | rel_day < -3), cex = 0.5, pch = ifelse(age == 1, 1, 16), type = 'n', ylim = c(10.75, 11.75), xlab = '', ylab = '', cex.axis = 1.5)
m = f_w_d
abline(coef(m)[1:2,1]/60, lwd = 1)
abline(c((coef(m)[1,1]+coef(m)[3,1])/60, (coef(m)[2,1]+coef(m)[6,1])/60), lwd = 2)
points(x = -10, y = 11.7, pch = "\u2666", cex = 4)

dev.off()

 }
}
#try 2
{
  modellist1 = list(f_w_m_age1, f_pb_m_age1, f_inc_m_age1, f_w_n_age1, f_pb_n_age1, f_inc_n_age1, f_w_d_age1, f_pb_d_age1, f_inc_d_age1)
  modellist2 = list(f_w_m_age2, f_pb_m_age2, f_inc_m_age2, f_w_n_age2, f_pb_n_age2, f_inc_n_age2, f_w_d_age2, f_pb_d_age2, f_inc_d_age2)
  symbollist = expand.grid(c("\u2744", "\u266B", "\u2B2F"), c("\u263C","\u263D","\u2666"), stringsAsFactors = FALSE)


  jpeg(file = "Overview sleep patterns first egg and age - 2.jpg", width = 720, height = 720)

  par(mfrow = c(3,3))
  for(i in 1 : length(modellist1)) {
  plot(c(0.5, 2.5), c(-2.5, 2.5), type = 'n', xaxt = 'n', xlab = '', ylab = list("effect size", cex = 1.3))
  points(1,coefficients(modellist1[[i]])[2,1], pch = 16)
  arrows(1, coefficients(modellist1[[i]])[2,1]+1.96*coefficients(modellist1[[i]])[2,2], 1, coefficients(modellist1[[i]])[2,1]-1.96*coefficients(modellist1[[i]])[2,2], code = 3, length = 0.05, angle = 90)

  points(2,coefficients(modellist2[[i]])[2,1], pch = 16)
  arrows(2, coefficients(modellist2[[i]])[2,1]+1.96*coefficients(modellist2[[i]])[2,2], 2, coefficients(modellist2[[i]])[2,1]-1.96*coefficients(modellist2[[i]])[2,2], code = 3, length = 0.05, angle = 90)

  abline(h = 0, lty = 3)

  points(0.6, 2.25, pch = symbollist[i,1], cex = 4)
  points(0.95, 2.25, pch = symbollist[i,2], cex = 4)
  axis(1, at = c(1,2), labels = c("yearling", "older"), cex.axis = 1.3)

  }
  dev.off()
  }


#5. chronotype
#a. correlations of medians
{

  plotACF2 = function (x, alpha = 0, xlab = "Lag", ylab = "Autocorrelation")
  {
    object <- x
    ylim <- range(object$ACF)
    assign("stdv", qnorm(1 - alpha/2)/sqrt(attr(object, "n.used")))
    stMax <- max(stdv)
    ylim <- c(min(c(-stMax, ylim[1])), max(c(ylim[2], stMax)))
    assign("alpha", as.logical(alpha))
    plot(ACF ~ lag, object, ylim = ylim, type = 'h', ylab = list("Autocorrelation", cex = 1.4), lwd = 2.5, cex.axis = 1.4, xlab = list("lag", cex = 1.4))
    lines(object$lag, stdv, lty = 2, )
    lines(object$lag, -stdv, lty = 2)

  }

  require(nlme)

  dat = copy(x)
  dat[, median_time_to_sunrise_min := median(time_to_sunrise_min, na.rm = TRUE), by = list(year_, ID, season)]
  dat[, median_time_to_sunset_min := median(time_to_sunset_min, na.rm = TRUE), by = list(year_, ID, season)]
  dat[, median_daylength := median(daylength, na.rm = TRUE), by = list(year_, ID, season)]
  dat = subset(dat, select = c('ID', 'year_', 'median_time_to_sunrise_min', 'median_time_to_sunset_min', 'median_daylength', 'season'))
  dat = unique(dat, by = names(dat))
  setkey(dat, "ID", "year_", "season")
  tmp = subset(dat, select = c('ID', 'year_'))
  tmp[, dup := ifelse(duplicated(tmp), 1, 0)]
  tmp[, dup := sum(dup), by = list(ID, year_)]
  tmp = unique(tmp)
  tmp = subset(tmp, dup == 2)
  setkey(tmp, ID, year_)
  tmp[, dup := ifelse(duplicated(ID) == TRUE,1,0)]
  tmp[, dup := max(dup), by = ID]
  tmp = subset(tmp, dup == 1)
  tmp = paste(tmp$year_, tmp$ID, sep = '_')
  dat[, year_ID := paste(year_, ID, sep = '_')]
  dat = subset(dat, year_ID %in% tmp)
  dat[season == "0_spring", season2 := "3_spring"]
  dat[season == "1_preBreeding", season2 := "2_preBreeding"]
  dat[season == "2_winter", season2 := "1_winter"]
  setkey(dat, ID, year_, season2)


  #across the data (everythird ACF!)
  m = lme(median_time_to_sunrise_min ~ year_+season, random = ~1|ID, data = dat)
  m1 = ACF(m, maxLag = 5)

  m = lme(median_time_to_sunset_min ~ year_+season, random = ~1|ID, data = dat)
  m2 = ACF(m, maxLag = 5)

  m = lme(median_daylength ~ year_+season, random = ~1|ID, data = dat)
  m3 = ACF(m, maxLag = 5)
  m4 = acf(residuals(m), lag.max = 5)

  jpeg(file = "Overview ACF_across seasons.jpg", width = 240, height = 720)
  par(mfrow = c(3,1))
  plotACF2(m1, alpha = 0.05)
  points(x = 0.5, y = 0.9, pch = "\u263C", cex = 4)
  plotACF2(m2, alpha = 0.05)
  points(x = 0.5, y = 0.9, pch = "\u263D", cex = 4)
  plotACF2(m3, alpha = 0.05)
  points(x = 0.5, y = 0.9, pch = "\u2666", cex = 4)

  dev.off()

  #across preBreeding
 { #data

  m = lme(median_time_to_sunrise_min ~ year_, random = ~1|ID, data = subset(dat, season == "1_preBreeding"))
  m11 = ACF(m, maxLag = 2)

  m = lme(median_time_to_sunrise_min ~ year_, random = ~1|ID, data = subset(dat, season == "2_winter"))
  m12 = ACF(m, maxLag = 2)

  m = lme(median_time_to_sunrise_min ~ year_, random = ~1|ID, data = subset(dat, season == "0_spring"))
  m13 = ACF(m, maxLag = 2)

  m = lme(median_time_to_sunset_min ~ year_, random = ~1|ID, data = subset(dat, season == "1_preBreeding"))
  m21 = ACF(m, maxLag = 2)

  m = lme(median_time_to_sunset_min ~ year_, random = ~1|ID, data = subset(dat, season == "2_winter"))
  m22 = ACF(m, maxLag = 2)

  m = lme(median_time_to_sunset_min ~ year_, random = ~1|ID, data = subset(dat, season == "0_spring"))
  m23 = ACF(m, maxLag = 2)

  m = lme(median_daylength ~ year_, random = ~1|ID, data = subset(dat, season == "1_preBreeding"))
  m31 = ACF(m, maxLag = 2)

  m = lme(median_daylength ~ year_, random = ~1|ID, data = subset(dat, season == "2_winter"))
  m32 = ACF(m, maxLag = 2)


  m = lme(median_daylength ~ year_, random = ~1|ID, data = subset(dat, season == "0_spring"))
  m33 = ACF(m, maxLag = 2)
}

  jpeg(file = "Overview ACF_all combinations.jpg", width = 720, height = 720)

  par(mfrow = c(3,3))
  #sunrise
  plotACF2(m11, alpha = 0.05)
  points(x = 1.5, y = 0.9, pch = "\u263C", cex = 4)
  points(x = 1.9, y = 0.9, pch = "\u2744", cex = 4)
  plotACF2(m12, alpha = 0.05)
  points(x = 1.5, y = 0.9, pch = "\u263C", cex = 4)
  points(x = 1.9, y = 0.9, pch = "\u266B", cex = 4)
  plotACF2(m13, alpha = 0.05)
  points(x = 1.5, y = 0.9, pch = "\u263C", cex = 4)
  points(x = 1.9, y = 0.9, pch = "\u2B2F", cex = 4)

  #sunset
  plotACF2(m21, alpha = 0.05)
  points(x = 1.5, y = 0.9, pch = "\u263D", cex = 4)
  points(x = 1.9, y = 0.9, pch = "\u2744", cex = 4)
  plotACF2(m22, alpha = 0.05)
  points(x = 1.5, y = 0.9, pch = "\u263D", cex = 4)
  points(x = 1.9, y = 0.9, pch = "\u266B", cex = 4)
  plotACF2(m23, alpha = 0.05)
  points(x = 1.5, y = 0.9, pch = "\u263D", cex = 4)
  points(x = 1.9, y = 0.9, pch = "\u2B2F", cex = 4)

  #daylength
  plotACF2(m31, alpha = 0.05)
  points(x = 1.5, y = 0.9, pch = "\u2666", cex = 4)
  points(x = 1.9, y = 0.9, pch = "\u2744", cex = 4)
  plotACF2(m32, alpha = 0.05)
  points(x = 1.5, y = 0.9, pch = "\u2666", cex = 4)
  points(x = 1.9, y = 0.9, pch = "\u266B", cex = 4)
  plotACF2(m33, alpha = 0.05)
  points(x = 1.5, y = 0.9, pch = "\u2666", cex = 4)
  points(x = 1.9, y = 0.9, pch = "\u2B2F", cex = 4)

  dev.off()
  ########
}


#b. variability in behaviour
{
dat = subset(x, rel_day >= 3 & rel_day <= 10)
Range_m = max(dat$time_to_sunrise_min, na.rm = TRUE) - min(dat$time_to_sunrise_min, na.rm = TRUE)
Range_n = max(dat$time_to_sunset_min, na.rm = TRUE) - min(dat$time_to_sunset_min, na.rm = TRUE)
Range_d = max(dat$daylength_min, na.rm = TRUE) - min(dat$daylength_min, na.rm = TRUE)



dat[, min_ID_m := min(time_to_sunrise_min, na.rm = TRUE), by = list(ID, year_)]
dat[, max_ID_m := max(time_to_sunrise_min, na.rm = TRUE), by = list(ID, year_)]
dat[, min_ID_n := min(time_to_sunset_min, na.rm = TRUE), by = list(ID, year_)]
dat[, max_ID_n := max(time_to_sunset_min, na.rm = TRUE), by = list(ID, year_)]
dat[, min_ID_d := min(daylength_min, na.rm = TRUE), by = list(ID, year_)]
dat[, max_ID_d := max(daylength_min, na.rm = TRUE), by = list(ID, year_)]
dat[, N := length(time_to_sunset_min), by = list(ID, year_)]
dat = subset(dat, N > 1)
dat[, range_ID_m := max_ID_m - min_ID_m]
dat[, range_ID_n := max_ID_n - min_ID_n]
dat[, range_ID_d := max_ID_d - min_ID_d]
dat = subset(dat, select = c('range_ID_m', 'range_ID_n', 'range_ID_d', 'yid'))
dat = unique(dat, by = names(dat))
Mm = mean(dat$range_ID_m)
Sm = (sd(dat$range_ID_m)/sqrt(nrow(dat)))*1.96
Mn = mean(dat$range_ID_n)
Sn = (sd(dat$range_ID_n)/sqrt(nrow(dat)))*1.96
Md = mean(dat$range_ID_d)
Sd = (sd(dat$range_ID_d)/sqrt(nrow(dat)))*1.96

jpeg(file = "variability in sleep patterns.jpg", width = 720, height = 720)

boxplot(dat$range_ID_m, dat$range_ID_n, dat$range_ID_d, ylim = c(0, 140), ylab = list("Behavioural range (day 3 - day 10) in minutes", cex = 1.3))
axis(1, labels = c("\u263C","\u263D","\u2666"), at = c(1,2,3), cex.axis = 4, line = 2, tick = FALSE)

dev.off()

}
