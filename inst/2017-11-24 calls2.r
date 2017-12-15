#0a. fetch packages
{
  require(lme4)
  require(ggplot2)
  require(rptR)
  require(plotrix)
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
  setnames(x, "season", "breeding_season")
  x[, season := '0_breeding']
  x[rel_day < -30, season := "2_winter"]
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
  hist(x[,c_firstEgg], 30)
  #manually
  con = dbcon(user = "lschlicht")
  fe = dbq(con, "SELECT DATE(firstEgg) as firstEgg, breeding_season, box FROM BTatWESTERHOLZ.BREEDING")
  closeCon(con)
  fe[, c_firstEgg := scale(as.IDate(firstEgg), center = TRUE, scale = FALSE), by = breeding_season]
  fe[, min_ := boxplot.stats(c_firstEgg)$stats[1], by = breeding_season]
  fe[, max_ := boxplot.stats(c_firstEgg)$stats[5], by = breeding_season]
  subset(fe, c_firstEgg > max_ & breeding_season >= 2011)
                                    #automatic exclusion; manual exclusion
  table(fe[breeding_season == 2016,firstEgg]) #>26.04.; > 26.04. #identical

  table(fe[breeding_season == 2015,firstEgg]) #>25.04. ; > 26.04. #manually 2 nests less excluded in total dataset

  table(fe[breeding_season == 2014,firstEgg]) #>13.04. ; > 13.04. #identical

  table(fe[breeding_season == 2013,firstEgg]) #>30.04. ; > 30.04. #identical

  table(fe[breeding_season == 2012,firstEgg]) #>03.05. ; > 03.05. #identical

  table(fe[breeding_season == 2011,firstEgg]) #>16.04. ; > 18.04. #manually 2 nests less excluded in total dataset

  x[breeding_season == 2011, ':=' (auto = "2011-04-16", manual = "2011-04-18")]
  x[breeding_season == 2012, ':=' (auto = "2012-05-03", manual = "2012-05-03")]
  x[breeding_season == 2013, ':=' (auto = "2013-04-30", manual = "2013-04-30")]
  x[breeding_season == 2014, ':=' (auto = "2014-04-13", manual = "2014-04-13")]
  x[breeding_season == 2015, ':=' (auto = "2015-04-25", manual = "2015-04-26")]
  x[breeding_season == 2016, ':=' (auto = "2016-04-26", manual = "2016-04-26")]

  nrow(unique(subset(x, select = c('breedingAttempt')))) #621
  nrow(unique(subset(x, firstEgg <= auto, select = c('breedingAttempt')))) #599
  nrow(unique(subset(x, firstEgg <= manual, select = c('breedingAttempt')))) #603

  #manual exclusion (used for final analyses): 18 nests
  #automatic exclusion: 22 nests
  x = subset(x, firstEgg <= manual) #224 datapoints (< 1 % of data) removed


}

#1. descriptive part
{
  #a. plot overview
  {
    #run each plot twice: for males and for females; the first will be plotted on the left, the second on the right.
    VAR = 'rel_day'; XLAB = 'Day to first egg'
    VAR = 'date'; XLAB = 'Day of the year'

    #create dataset
    {
      dp_all = copy(x)
      if( VAR == "rel_day") {
      dp_all[, ':=' (mean_time_to_sunrise_min = mean(time_to_sunrise_min, na.rm = TRUE), mean_time_to_sunset_min = mean(time_to_sunset_min, na.rm = TRUE), mean_daylength_hour = mean(prop_daylength_used, na.rm = TRUE), se_time_to_sunrise_min = sd(time_to_sunrise_min, na.rm = TRUE)/sqrt(length(na.omit(time_to_sunrise_min))), se_time_to_sunset_min = sd(time_to_sunset_min, na.rm = TRUE)/sqrt(length(na.omit(time_to_sunset_min))), se_daylength_hour = sd(prop_daylength_used, na.rm = TRUE)/sqrt(length(na.omit(prop_daylength_used))), N = length(na.omit(time_to_sunset_min))), by = list(rel_day, sex)];
        dp_all[, ':=' (conf_int_time_to_sunrise_min = se_time_to_sunrise_min*1.96, conf_int_time_to_sunset_min = se_time_to_sunset_min*1.96, conf_int_daylength_hour = se_daylength_hour*1.96), by = yday(date_)];
        dp_all[, day := rel_day]

      } else {
        dp_all[, ':=' (mean_time_to_sunrise_min = mean(time_to_sunrise_min, na.rm = TRUE), mean_time_to_sunset_min = mean(time_to_sunset_min, na.rm = TRUE), mean_daylength_hour = mean(prop_daylength_used, na.rm = TRUE), se_time_to_sunrise_min = sd(time_to_sunrise_min, na.rm = TRUE)/sqrt(length(na.omit(time_to_sunrise_min))), se_time_to_sunset_min = sd(time_to_sunset_min, na.rm = TRUE)/sqrt(length(na.omit(time_to_sunset_min))), se_daylength_hour = sd(prop_daylength_used, na.rm = TRUE)/sqrt(length(na.omit(prop_daylength_used))), N = length(na.omit(time_to_sunset_min))), by = list(yday(date_), sex)] ;
        dp_all[, ':=' (conf_int_time_to_sunrise_min = se_time_to_sunrise_min*1.96, conf_int_time_to_sunset_min = se_time_to_sunset_min*1.96, conf_int_daylength_hour = se_daylength_hour*1.96), by = yday(date_)];
        dp_all[, day := yday(date_)]
    }


    dp_all = subset(dp_all, select = c('day', 'mean_time_to_sunrise_min', 'mean_time_to_sunset_min', 'mean_daylength_hour', 'se_time_to_sunrise_min', 'se_time_to_sunset_min', 'se_daylength_hour', 'conf_int_time_to_sunrise_min', 'conf_int_time_to_sunset_min', 'conf_int_daylength_hour', 'N', 'sex'))
    dp_all = unique(dp_all, by = names(dp_all))
    setkey(dp_all, day)
    }

    #sample sizes
    {
    jpeg(file = "S2.jpg", width = 720, height = 720)
    par(mfrow = c(2,2))
    par(mar = c(5.1, 4.1, 1.1, 1.1))

    plot(N ~ day, data = data.table(N = dp_all[sex == 2, N], day = dp_all[sex == 2, day]), type = 'h', ylim = c(0, 250), xlab = list(XLAB, cex = 1.4), ylab = list("Sample size", cex = 1.4))
    text(-200, 240, "\u2640", adj = 0, cex = 2)
    plot(N ~ day, data = data.table(N = dp_all[sex == 1, N], day = dp_all[sex == 1, day]), type = 'h', ylim = c(0, 250), xlab = list(XLAB, cex = 1.4), ylab = list("Sample size", cex = 1.4))
    text(-200, 240, "\u2642", adj = 0, cex = 2)

    dev.off()
    }

    #Figure 1: set VAR (above) to "rel_day"
    {
      #1a: Emergence/date
      jpeg(file = "1a.jpg", width = 1080, height = 1440)

      plot_overview(data = dp_all, mean_variable = "mean_time_to_sunrise_min", conf_int_variable = "conf_int_time_to_sunrise_min",
                    XLAB = list("Day in relation to the first egg (day 0 = first egg date)", cex = 2), YLAB = list("Emergence time (in minutes to sunrise)", cex = 2),
                    XLIM = c(-205, 62), YLIM = c(-65, 85),
                    CEX = 0.5,
                    rect1 = -30, rect3 = 44,
                    SYMBOL = c("\u2600", "\u263E", paste("\u2600", "\u2192", "\u263E", sep = ''))[1], symbol1 = -195, symbol2 = 80, PCH = 16,
                    sexSymbol1 = -180, sexSymbol2 = 80,
                    BREAK = c(100, 160),
                    ARROWS1 = c(0, -55, 0, -45),
                    ARROWS2 = c(0, -35, 0, -25)
      )


      dev.off()


      #1b: Sleep onset/date
      jpeg(file = "1b.jpg", width = 1080, height = 1440)
      plot_overview(data = dp_all, mean_variable = "mean_time_to_sunset_min", conf_int_variable = "conf_int_time_to_sunset_min",
                    XLAB = list("Day in relation to the first egg (day 0 = first egg date)", cex = 2), YLAB = list("Sleep onset (in minutes to sunset)", cex = 2),
                    XLIM = c(-205, 62), YLIM = c(-100, 20),
                    CEX = 0.5,
                    rect1 = -30, rect3 = 44,
                    SYMBOL = c("\u2600", "\u263E", paste("\u2600", "\u2192", "\u263E", sep = ''))[2], symbol1 = -195, symbol2 = 16, PCH = 16,
                    sexSymbol1 = -180, sexSymbol2 = 16,
                    BREAK = c(182, 261),
                    ARROWS1 = c(0, 11, 0, 1),
                    ARROWS2 = c(0, -1, 0, -11)
      )

      dev.off()

      #1c: Daylength/date
      jpeg(file = "1c.jpg", width = 1080, height = 1440)

      plot_overview(mean_variable = "mean_daylength_hour", conf_int_variable = "conf_int_daylength_hour",
                    XLAB = list("Day in relation to the first egg (day 0 = first egg date)", cex = 2), YLAB = list("Proportion of lighted day spent active", cex = 2),
                    XLIM = c(-205, 62), YLIM = c(0.83, 1.09),
                    CEX = 0.5,
                    rect1 = -30, rect2 = 0.83, rect3 = 44, rect4 = 0.835,
                    SYMBOL = c("\u2600", "\u263E", paste("\u2600", "\u2192", "\u263E", sep = ''))[3], symbol1 = -195, symbol2 = 1.075, PCH = 16,
                    sexSymbol1 = -150, sexSymbol2 = 1.075,
                    BREAK = c(182, 261),
                    ARROWS1 = c(0, 1.07, 0, 1.05),
                    ARROWS2 = c(0, 1.03, 0, 1.01)
      )

      dev.off()
    }

    #Figure S1: set VAR (above) to "date"
    {
    #S1a: Emergence/date
    jpeg(file = "S1a.jpg", width = 1080, height = 1440)
    plot_overview(mean_variable = "mean_time_to_sunrise_min", conf_int_variable = "conf_int_time_to_sunrise_min",
                             XLAB = list("Day of the year", cex = 2), YLAB = list("Emergence time (in minutes to sunrise)", cex = 2),
                             XLIM = c(-0.5, 295), YLIM = c(-50, 50),
                             CEX = 0.5,
                             rect1 = 90, rect3 = 166,
                             SYMBOL = c("\u2600", "\u263E", paste("\u2600", "\u2192", "\u263E", sep = ''))[1], symbol1 = 7, symbol2 = 45, PCH = 16,
                             sexSymbol1 = 22, sexSymbol2 = 45,
                             BREAK = c(182, 261)
    )
    dev.off()


    #S1b: Sleep onset/date
    jpeg(file = "S1b.jpg", width = 1080, height = 1440)
    plot_overview(mean_variable = "mean_time_to_sunset_min", conf_int_variable = "conf_int_time_to_sunset_min",
                  XLAB = list("Day of the year", cex = 2), YLAB = list("Sleep onset (in minutes to sunset)", cex = 2),
                  XLIM = c(-0.5, 295), YLIM = c(-120, 20),
                  CEX = 0.5,
                  rect1 = 90, rect3 = 166,
                  SYMBOL = c("\u2600", "\u263E", paste("\u2600", "\u2192", "\u263E", sep = ''))[2], symbol1 = 7, symbol2 = 16, PCH = 16,
                  sexSymbol1 = 22, sexSymbol2 = 16,
                  BREAK = c(182, 261)
    )

    dev.off()

    #S1c: Daylength/date
    jpeg(file = "S1c.jpg", width = 1080, height = 1440)

    plot_overview(mean_variable = "mean_daylength_hour", conf_int_variable = "conf_int_daylength_hour",
                  XLAB = list("Day of the year", cex = 2), YLAB = list("Daylength (prop. of daylight spent active)", cex = 2),
                  XLIM = c(-0.5, 295), YLIM = c(0.85, 1.1),
                  CEX = 0.5,
                  rect1 = 90, rect2 = 0.85, rect3 = 166, rect4 = 0.855,
                  SYMBOL = c("\u2600", "\u263E", paste("\u2600", "\u2192", "\u263E", sep = ''))[3], symbol1 = 7, symbol2 = 1.09, PCH = 16,
                  sexSymbol1 = 52, sexSymbol2 = 1.09,
                  BREAK = c(182, 261)
    )

    dev.off()
}





   }

  #b. models rain and temperature ###TAKE A WHILE TO RUN BECAUSE OF REPEATABILITIES!###
  runRepeatabilities = FALSE

  {
    runthrough = data.table(expand.grid(sex = c(1,2), season = unique(x$season), var = c('time_to_sunrise_min', 'time_to_sunset_min', 'daylength_min'), stringsAsFactors =FALSE))

  #models
    ENV_MODELS = list()
  for(i in 1 : nrow(runthrough)) {
    print(i)
    dat = subset(x, sex == runthrough[i,sex] & season == runthrough[i,season])
    if(runthrough$var[i] == "time_to_sunrise_min") {
      dat[, ':=' (var = time_to_sunrise_min, var2 = SumRainfall_dawn, var3 = Temperature2m_dawn, var4 = sunrise_time)] }
    if(runthrough$var[i] == "time_to_sunset_min") {
      dat[, ':=' (var = time_to_sunset_min, var2 = SumRainfall_dusk, var3 = Temperature2m_dusk, var4 = sunset_time)] }
    if(runthrough$var[i] == "daylength_min") {
      dat[, ':=' (var = daylength_min, var2 = SumRainfall, var3 = Temperature2m, var4 = actualDaylength_min)]}


    hist(dat$var)
    st = boxplot.stats(dat$var)$stats
    dat = subset(dat, var > st[1] & var <= st[5])
    hist(dat$var)

    if(runthrough$season[i] == '2_winter') {
      m = lmer(var ~ var2 + var3 + var4 + (1|ID) + (1|yid) + (1|breeding_season), data = dat)
    } else {
      m = lmer(var ~ var2 + var3 + rel_day + (1|ID) + (1|yid) + (1|breeding_season), data = dat)
    }
    hist(resid(m))
    plot(resid(m) ~ fitted(m))
    qqnorm(residuals(m))
    ENV_MODELS[[i]] = list(summary(m))

    if(runRepeatabilities == TRUE) {
      Rmw_dawn = rpt2(var ~ var2 + var3 + (1|breeding_season) + (1|yid) + (1|ID), grname = c("breeding_season", "yid", "ID"), data = dat)
      ENV_MODELS[[i]][[2]] = Rmw_dawn
    }

    names(ENV_MODELS[[i]]) = paste(runthrough$sex[i], runthrough$season[i], runthrough$var[i])

}

  #repeatabilities
    RR = list()
    for(i in 1 : length(ENV_MODELS)) {
      RR[[length(RR)+1]] = ENV_MODELS[[i]][2]
      names(RR[[length(RR)]]) = names(ENV_MODELS[[i]][1])
    }

  #models for table
    MM = list()
    for(i in 1 : length(ENV_MODELS)) {
      MM[[length(MM)+1]] = list(ENV_MODELS[[i]][[1]]$coefficients)
      names(MM)[length(MM)] = base::names(ENV_MODELS[[i]][1])
      MM[[length(MM)]][[2]] = ENV_MODELS[[i]][[1]]$ngrps
      MM[[length(MM)]][[3]] = ENV_MODELS[[i]][[1]]$devcomp$dims[1]
    }



  #Figure 2: Rainfall and T (based on the models above)
    {
  {
    #data
    {
    L = list()
    for(i in 1 : length(ENV_MODELS)) {
      tmp = as.data.table(summary(ENV_MODELS[[i]][[1]])$coefficients[2:3, 1:2])
      tmp[, dataset := names(ENV_MODELS[[i]])]
      L[[i]] = tmp
    }

      POINTS = rbindlist(L)

      POINTS[, var := rep(c('Rain', 'T'), 18)]
      POINTS[, season := substring(dataset, 3, 7)]
      POINTS[, timing := rep(c("2dawn", "1dusk", "0day"), each = 12)]
      POINTS[, sex := substring(dataset, 1,1)]
      POINTS[, conf_int := 1.96*POINTS$'Std. Error']
      setkey(POINTS, season, var, timing, sex)
      POINTS[sex == 1, PCH := "♂"]
      POINTS[sex == 2, PCH := "♀"]

      POINTS[, YY :=
               c(rep(as.numeric(rep(1:3, each = 2)),2),
               rep(as.numeric(rep(4:6, each = 2)),2),
               rep(as.numeric(rep(7:9, each = 2)),2))
             ]
      POINTS[sex == 1, YY := YY-0.12]
      POINTS[sex == 2, YY := YY+0.12]

      TTT = split(POINTS, POINTS$var)
    }

  jpeg(file = "2.jpg", width = 720, height = 720)
  par(mar = c(4.1, 9.6, 1.1, 1.1))
  par(mfrow = c(2,1))

  for(i in 1 : length(TTT)) {
    TT = TTT[[i]]

    if(TT[1,var] == "T") {
      plot(c(-1, 2.5), c(0.5, 9.5), type = 'n', xlab = list("Estimate ± Confidence Interval", cex = 1.3), ylab = "", yaxt = 'n')
      text(2.5, 9, "Temperature", adj = 1, cex = 2)
      distance = 0.03

    } else {
      plot(c(-14, 10), c(0.5, 9.5), type = 'n', xlab = list("Estimate ± Confidence Interval", cex = 1.3), ylab = "", yaxt = 'n')
      text(8, 9, "Rain", adj = 1, cex = 2)
      distance = 0.2

      }



    points(TT$Estimate[1:18], TT$YY[1:18], pch = TT$PCH)
    arrows(TT$Estimate[1:18] - TT$conf_int[1:18], TT$YY[1:18], TT$Estimate[1:18] - distance, TT$YY[1:18], angle = 90, code = 1, length = 0.02, lty = POINTS$LTY[1:18])
    arrows(TT$Estimate[1:18] + TT$conf_int[1:18], TT$YY[1:18], TT$Estimate[1:18] + distance, TT$YY[1:18], angle = 90, code = 1, length = 0.02, lty = POINTS$LTY[1:18])
    abline(v = 0, lty = 3)
    abline(h = c(3.5, 6.5), lty = 3)
    axis(2, at = c(2,5,8), labels = c('Breeding', 'Pre-breeding', 'Winter'), las = 1, line = 2, tick = FALSE, cex.axis = 1.3)
    axis(2, at = 1:9, labels = rep(c(paste("\u2600", "\u2192", "\u263E", sep = ''), "\u263E", "\u2600"), 3), las = 1)
    }

  dev.off()
  }

  #Figure S4: Rainfall and T per year
    #models
    {
      MODELOUTPUT = list()
      runthrough = expand.grid(unique(x$breeding_season), c(1,2), unique(x$season), c("time_to_sunrise_min", "time_to_sunset_min", "daylength_min"), stringsAsFactors = FALSE)
      names(runthrough) = c('breeding_season', 'sex', 'season', 'var')
      for(i in 1:nrow(runthrough)) {
        print(i)
        dat = subset(x, sex == runthrough[i, 'sex']  & season == runthrough[i, 'season'] & breeding_season == runthrough[i, 'breeding_season'])
        dat[, var := dat[[(runthrough[i,'var'])]], with = TRUE]

        if(runthrough[i,'var'] == 'time_to_sunrise_min') {
          dat[, var2 := SumRainfall_dawn]
          dat[, var3 := Temperature2m_dawn]
          dat[, var4 := sunrise_time]
        }
        if(runthrough[i,'var'] == 'time_to_sunset_min') {
          dat[, var2 := SumRainfall_dusk]
          dat[, var3 := Temperature2m_dusk]
          dat[, var4 := sunset_time]
        }
        if(runthrough[i,'var'] == 'daylength_min') {
          dat[, var2 := SumRainfall]
          dat[, var3 := Temperature2m]
          dat[, var4 := actualDaylength_min]
        }

        hist(dat$var)
        st = boxplot.stats(dat$var)$stats
        dat = subset(dat, var > st[1] & var <= st[5])
        hist(dat$var)

        if(runthrough$season[i] == "2_winter") {
        m =  lmer(var ~ var2 + var3 + var4 + (1|ID), data = dat)
        } else {
        m =  lmer(var ~ var2 + var3 + rel_day + (1|ID), data = dat)
        }


        hist(resid(m))
        plot(resid(m) ~ fitted(m))
        qqnorm(residuals(m))
        tmp = data.table(coefficients(summary(m)))
        tmp[, breeding_season := runthrough[i, 'breeding_season']]
        tmp[, season := runthrough[i, 'season']]
        tmp[, sex := runthrough[i, 'sex']]
        tmp[, variable := c("Int.", "Rain", "T", "sun")]
        tmp[, fixed := runthrough[i, 'var']]
        tmp[, N := nrow(dat)]
        tmp[, N_ind := length(unique(dat$ID))]
        MODELOUTPUT[[length(MODELOUTPUT)+1]] = tmp

      }



    }

    #dataset
    {
      M = rbindlist(MODELOUTPUT) #rank deficient in spring 2011 for males
      setkey(M, season, variable, fixed, sex)

      M[season == '2_winter' & fixed == 'time_to_sunrise_min', YY := as.numeric(9)]
      M[season == '2_winter' & fixed == 'time_to_sunset_min', YY := 8]
      M[season == '2_winter' & fixed == 'daylength_min', YY := 7]

      M[season == '1_preBreeding' & fixed == 'time_to_sunrise_min', YY := 6]
      M[season == '1_preBreeding' & fixed == 'time_to_sunset_min', YY := 5]
      M[season == '1_preBreeding' & fixed == 'daylength_min', YY := 4]

      M[season == '0_breeding' & fixed == 'time_to_sunrise_min', YY := 3]
      M[season == '0_breeding' & fixed == 'time_to_sunset_min', YY := 2]
      M[season == '0_breeding' & fixed == 'daylength_min', YY := 1]

      M[sex == 1, YY := YY-0.2]
      M[sex == 2, YY := YY+0.2]

      M[breeding_season == 2011, pch_bg := 'gold1']
      M[breeding_season == 2012, pch_bg := 'white']
      M[breeding_season == 2013, pch_bg := 'green4']
      M[breeding_season == 2014, pch_bg := 'navy']
      M[breeding_season == 2015, pch_bg := 'red3']
      M[breeding_season == 2016, pch_bg := 'black']

      M[,cex := (N/max(N)+0.5)*1.5, by = season]
      M = split(M, M$variable)

    }


    #figure (2011: yellow, 2012: white, 2013: green, 2014: blue, 2015: red, 2016: black)
    jpeg(file = "S4.jpg", width = 720, height = 720)
    par(mar = c(4.1, 9.6, 1.1, 1.1))
    par(mfrow = c(2,1))
    for(i in c('Rain', 'T')) {
      TT = M[[i]]

      if(TT[1,variable] == "T") {
        plot(c(-4, 5), c(0.5, 9.5), type = 'n', xlab = list("Estimate ± Confidence Interval", cex = 1.3), ylab = "", yaxt = 'n')
        text(5, 9, "Temperature", adj = 1, cex = 2)
        text(-4, 2.8, "←", col = 'gold1', cex = 3, adj = 0)
             } else {
        plot(c(-40, 25), c(0.5, 9.5), type = 'n', xlab = list("Estimate ± Confidence Interval", cex = 1.3), ylab = "", yaxt = 'n')
        text(25, 9, "Rain", adj = 1, cex = 2)
        text(-40, 0.8, "←", col = 'gold1', cex = 3, adj = 0)
        text(-40, 1.8, "←", col = 'gold1', cex = 3, adj = 0)
        text(25, 5.2, "→", col = 'gold1', cex = 3, adj = 1)
        text(25, 5.8, "→", col = 'gold1', cex = 3, adj = 1)

      }

      points(TT$Estimate, TT$YY, pch = 21, col = 'black', bg = alpha(TT$pch_bg, 0.5), cex = TT$cex)


      if(TT[1,variable] == "T") abline(v = c(-1, 2.5), lty = 3)
      if(TT[1,variable] == "Rain") abline(v = c(-13, 10), lty = 3)
      abline(v = 0, lty = 1)
      abline(h = c(1.5, 2.5, 4.5, 5.5, 7.5, 8.5), lty = 3)
      abline(h = c(3.5, 6.5), lty = 3, lwd = 3)

      axis(2, at = c(2,5,8), labels = c('Breeding', 'Pre-breeding', 'Winter'), las = 1, line = 2, tick = FALSE, cex.axis = 1.3)
      axis(2, at = 1:9, labels = rep(c(paste("\u2600", "\u2192", "\u263E", sep = ''), "\u263E", "\u2600"), 3), las = 1)
    }

    dev.off()


    }
}
}

#2. 'pure' replication
{
  #GRAHAM et al. 2017 models
  {
    #Great tit and Junco data
  WWJU <- read.csv("DataGrahamEtAl/JuncoRData.csv")
  names(WWJU) = make.names(names(WWJU))
  GRTI <- read.csv("DataGrahamEtAl/GreatTitRData.csv")
  names(GRTI) = make.names(names(GRTI))

  GRTIModel <-lmer(wakeup ~ egg1 + inc.day2 + apr.date + (1|nestid), data=GRTI)
  summary(GRTIModel)
  #problem: inc.day2, apr.date and egg1 are highly collinear; females are recorded either early, in the middle or late during incubation --> inc.day and egg1 one are strongly negatively correlated. Including inc.day with this  data therefore does not make much sense.
  GRTI_REP = rpt(wakeup ~ inc.day + (1|nestid), grname = "nestid", data = GRTI, datatype = "Gaussian")
  #repeatabilities can be replicated

  WWJUModel <-lmer(wakeup ~ egg1 + incdate2 + date + (1|nestid) + (1|Year), data=WWJU)
  summary(WWJUModel)
  #strong collinearities, but less problematic than with great tits.
}

  #OUR MODELS
  {
  #Juncos our model
  WWJUModel <-lmer(wakeup ~ egg1 + incdate + (1|nestid) + (1|Year), data=WWJU)
  summary(WWJUModel)
  hist(resid(WWJUModel))
  qqnorm(residuals(WWJUModel))
  #model fit ok, effect of egg1 remaining
  #check: effect of first egg remains
  WWJUModel <-lmer(wakeup ~ egg1 + (1|nestid) + (1|Year), data=WWJU); summary(WWJUModel)

  #Great tits our model
  GRTIModel <-lmer(wakeup ~ egg1 + inc.day + (1|nestid), data=GRTI)
  summary(GRTIModel)

  hist(resid(GRTIModel))
  qqnorm(residuals(GRTIModel))
  #still strong collinearity
  #check: effect of first egg disappears
  GRTIModel <-lmer(wakeup ~ egg1 + (1|nestid), data=GRTI); summary(GRTIModel)

  #blue tits our model
  dat = subset(x, sex == 2 & rel_day > (clutch-1) & date_ < hatchDate)
  hist(dat$time_to_sunrise_min)
  st = boxplot.stats(dat$time_to_sunrise_min)$stats
  nrow(dat) #2833
  dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])
  nrow(dat) #2745
  2745/2833 #0.9689
  hist(dat$time_to_sunrise_min)
  dat[, inc_day := rel_day + clutch - 1]

  BTmodel <- lmer(time_to_sunrise_min ~ c_firstEgg + inc_day + (1|ID) + (1|breeding_season), data = dat)
  hist(resid(BTmodel))
  qqnorm(residuals(BTmodel))
  summary(BTmodel)
  #collinearity small, effect of first egg present.
  #check: effect of first egg disappears
  BTmodel <- lmer(time_to_sunrise_min ~ c_firstEgg + (1|ID) + (1|breeding_season), data = dat); summary(BTmodel)
  }

  #repeatabilities
  {
  #calculating repeatabilities while correcting for april date (and first egg; doesn't change much)
  GRTI_REP2 = rpt(wakeup ~ inc.day + (1|nestid), grname = "nestid", data = GRTI, datatype = "Gaussian")
  GRTI_REP2

  WWJU_REP2 = rpt(wakeup ~ incdate + (1|nestid), grname = "nestid", data = WWJU, datatype = "Gaussian")
  WWJU_REP2

  BT_REP2 = rpt(time_to_sunrise_min ~ inc_day + (1|ID) + (1|breeding_season), grname = "ID", data = dat, datatype = "Gaussian")
  BT_REP2
}
}

#3. full model set: all combinations of periods (winter, pre-breeding, incubation) and response variables (daylength, emergence, sleep onset) together with the explanatory variables first egg, female age, and poly(sunrise time/sunset time/daylength, 2) respectively.
{
  SEX = 2


  #models
  {
    x[rel_day >= (clutch - 1) & date_ < hatchDate, replication_season := "incubation"]
    x[rel_day >= -30 & rel_day < 0, replication_season := "preLaying"]
    x[rel_day < -30, replication_season := "winter"]
    dat1 = subset(x, sex == SEX & !is.na(replication_season))
    runthrough = data.table(expand.grid(unique(dat1$replication_season), c('time_to_sunrise_min', 'time_to_sunset_min', 'daylength_min'), stringsAsFactors = FALSE))
    setnames(runthrough, names(runthrough), c("season", "var"))

    LIST = list()
   for(i in 1 : nrow(runthrough)) {

    dat = subset(dat1, replication_season == runthrough[i, season])
    dat[, var := dat[[runthrough[i,var]]]]
    if(runthrough[i,var] == 'time_to_sunrise_min') dat[, var2 := sunrise_time]
    if(runthrough[i,var] == 'time_to_sunset_min') dat[, var2 := sunset_time]
    if(runthrough[i,var] == 'daylength_min') dat[, var2 := actualDaylength_min]
    if(runthrough[i,season] != 'winter') dat[,var2 := rel_day]
    nn = nrow(dat)
    hist(dat$var)
    st = boxplot.stats(dat$var)$stats
    dat = subset(dat, var > st[1] & var <= st[5])
    hist(dat$var)


    m <- lmer(var ~ c_firstEgg + age + var2 + (1|ID) + (1|breeding_season), data = dat)
    hist(resid(m))
    qqnorm(residuals(m))
    plot(resid(m) ~ fitted(m))
    summary(m)

    m0 = summary(m)$coefficients[2,]
    m = summary(m)$coefficients[3,]


    m1 <- lmer(var ~ c_firstEgg + var2  + (1|ID) + (1|breeding_season), data = subset(dat, age == 1))
    hist(resid(m1))
    qqnorm(residuals(m1))
    plot(resid(m1) ~ fitted(m1))
    m1 = summary(m1)$coefficients[2,]

    m2 <- lmer(var ~ c_firstEgg + var2  + (1|ID) + (1|breeding_season), data = subset(dat, age == 2))
    hist(resid(m2))
    qqnorm(residuals(m2))
    plot(resid(m2) ~ fitted(m2))
    m2 = summary(m2)$coefficients[2,]

    m3 <- lmer(var ~ c_firstEgg + var2 + (1|ID) + (1|breeding_season), data = dat)
    hist(resid(m3))
    qqnorm(residuals(m3))
    plot(resid(m3) ~ fitted(m3))
    m3 = summary(m3)$coefficients[2,]

    tmp = data.table(rbind(m, m0, m1, m2, m3))
    tmp[, fixed := c('age', 'combind_corrAge', 'yearling', 'older', 'combined')]
    tmp[, var := runthrough[i, var]]
    tmp[, season := runthrough[i, season]]
    tmp[, N := nrow(dat)]
    tmp[, N_yearling := length(unique(subset(dat, age == 1)$ID))]
    tmp[, N_older := length(unique(subset(dat, age == 2)$ID))]
    tmp[, prop_used := nrow(dat)/nn ]

    LIST[[length(LIST)+1]] = tmp
  }

    L = rbindlist(LIST)
    setnames(L, names(L), make.names(names(L)))

  }

  #data for figure
  {
    LL = copy(L)

    LL = subset(LL, fixed == 'combind_corrAge')
    #LL = subset(LL, fixed == 'combined')
    #LL[, YY := c(8.9, 9.1, 5.9, 6.1, 2.9, 3.1, 7.9, 8.1, 4.9, 5.1, 1.9, 2.1, 6.9, 7.1, 3.9, 4.1, 0.9, 1.1)]
    LL[, YY := c(9, 6, 3, 8, 5, 2, 7, 4, 1)]
    LL[, conf_int := 1.95*Std..Error]

    LL[, PCH := 16]
    LL[fixed == 'yearling', PCH := 0]
    LL[fixed == 'older', PCH := 2]

  }

  #plot: yearlings: squares; triangles: older; sample sizes: winter 53 (y)/77 (o), prebreeding 101, 128, incubation 103, 138
  {

    jpeg(file = "3.jpg", width = 720, height = 360)
    par(mar = c(4.1, 9.6, 1.1, 1.1))

    plot(c(-5.5, 5), c(0.5, 9.5), type = 'n', xlab = list("Estimate ± Confidence Interval", cex = 1.3), ylab = "", yaxt = 'n')

      points(LL$Estimate, LL$YY, pch = LL$PCH)
      arrows(LL$Estimate - LL$conf_int, LL$YY, LL$Estimate + LL$conf_int, LL$YY, angle = 90, code = 3, length = 0.02, lty = 1)
      abline(v = 0, lty = 3)
      abline(h = c(3.5, 6.5), lty = 3)
      axis(2, at = c(2,5,8), labels = c('Incubation', 'Pre-breeding', 'Winter'), las = 1, line = 2, tick = FALSE, cex.axis = 1.3)
      axis(2, at = 1:9, labels = rep(c(paste("\u2600", "\u2192", "\u263E", sep = ''), "\u263E", "\u2600"), 3), las = 1)

    dev.off()
  }


  }


#4. chronotype
{
#a. correlations of medians
{

   require(nlme)

  #create dataset
  {
    dat = copy(x)
    dat[, median_time_to_sunrise_min := median(time_to_sunrise_min, na.rm = TRUE), by = list(breeding_season, ID, season)]
    dat[, median_time_to_sunset_min := median(time_to_sunset_min, na.rm = TRUE), by = list(breeding_season, ID, season)]
    dat[, median_daylength := median(daylength, na.rm = TRUE), by = list(breeding_season, ID, season)]
    dat = subset(dat, select = c('ID', 'breeding_season', 'median_time_to_sunrise_min', 'median_time_to_sunset_min', 'median_daylength', 'season', 'sex'))
    dat = unique(dat, by = names(dat))
    setkey(dat, "ID", "breeding_season", "season")

    #only keep birds that were present in all three "seasons" within a year
    tmp = subset(dat, select = c('ID', 'breeding_season', 'sex'))
    tmp[, dup := ifelse(duplicated(tmp), 1, 0)]
    tmp[, dup := sum(dup), by = list(ID, breeding_season)]
    tmp = unique(tmp)
    tmp = subset(tmp, dup == 2)
    setkey(tmp, ID, breeding_season)

    #only keep birds that appear at least in two years
    tmp[, dup := ifelse(duplicated(ID) == TRUE,1,0)]
    tmp[, dup := max(dup), by = ID]
    tmp = subset(tmp, dup == 1)

    #remove birds that don't occur in consequtive seasons
    tmp[, dup1 := shift(breeding_season, n = 1, type = 'lead') - breeding_season, by = ID]
    tmp[, dup2 := breeding_season - shift(breeding_season, n = 1, type = 'lag'), by = ID]
    tmp = subset(tmp, dup1 == 1 | dup2 == 1)

    tmp = paste(tmp$breeding_season, tmp$ID, sep = '_')
    dat[, breeding_seasonID := paste(breeding_season, ID, sep = '_')]
    dat = subset(dat, breeding_seasonID %in% tmp)
    dat[season == "0_breeding", season2 := "3_breeding"]
    dat[season == "1_preBreeding", season2 := "2_preBreeding"]
    dat[season == "2_winter", season2 := "1_winter"]
    setkey(dat, ID, breeding_season, season2)
  }

  SEX = 2
  #models
  {
  #across the data (everythird ACF!)
  m = lme(median_time_to_sunrise_min ~ season2, random = ~1|ID, data = subset(dat, sex == SEX))
  m1 = ACF(m, maxLag = 5)

  m = lme(median_time_to_sunset_min ~ season2, random = ~1|ID, data = subset(dat, sex == SEX))
  m2 = ACF(m, maxLag = 5)

  m = lme(median_daylength ~ season2, random = ~1|ID, data = subset(dat, sex == SEX))
  m3 = ACF(m, maxLag = 5)

  m = lme(median_time_to_sunrise_min ~ 1, random = ~1|ID, data = subset(dat, season == "1_preBreeding" & sex == SEX))
  m11 = ACF(m, maxLag = 1)

  m = lme(median_time_to_sunrise_min ~ 1, random = ~1|ID, data = subset(dat, season == "2_winter" & sex == SEX))
  m12 = ACF(m, maxLag = 1)

  m = lme(median_time_to_sunrise_min ~ 1, random = ~1|ID, data = subset(dat, season == "0_breeding" & sex == SEX))
  m13 = ACF(m, maxLag = 1)

  m = lme(median_time_to_sunset_min ~ 1, random = ~1|ID, data = subset(dat, season == "1_preBreeding" & sex == SEX))
  m21 = ACF(m, maxLag = 1)

  m = lme(median_time_to_sunset_min ~ 1, random = ~1|ID, data = subset(dat, season == "2_winter" & sex == SEX))
  m22 = ACF(m, maxLag = 1)

  m = lme(median_time_to_sunset_min ~ 1, random = ~1|ID, data = subset(dat, season == "0_breeding" & sex == SEX))
  m23 = ACF(m, maxLag = 1)

  m = lme(median_daylength ~ 1, random = ~1|ID, data = subset(dat, season == "1_preBreeding" & sex == SEX))
  m31 = ACF(m, maxLag = 1)

  m = lme(median_daylength ~ 1, random = ~1|ID, data = subset(dat, season == "2_winter" & sex == SEX))
  m32 = ACF(m, maxLag = 1)


  m = lme(median_daylength ~ 1, random = ~1|ID, data = subset(dat, season == "0_breeding" & sex == SEX))
  m33 = ACF(m, maxLag = 1)
  }

  #plot
  jpeg(file = "S7a_male.jpg", width = 720, height = 720)
  jpeg(file = "S7b_female.jpg", width = 720, height = 720)

    {
  par(mar = c(2.1, 2.6, 1.1, 1.1))

  par(mfcol = c(3,4))
  plotACF2(m1, uselags = c(2, 6), alpha = 0.05)
  abline(h = 0, lty = 3)
  plotACF2(m2, uselags = c(2, 6), alpha = 0.05)
  abline(h = 0, lty = 3)
  plotACF2(m3, uselags = c(2, 6), alpha = 0.05)
  abline(h = 0, lty = 3)
  plotACF2(m11, uselags = c(2, 2), alpha = 0.05)
  abline(h = 0, lty = 3)
  plotACF2(m12, uselags = c(2, 2), alpha = 0.05)
  abline(h = 0, lty = 3)
  plotACF2(m13, uselags = c(2, 2), alpha = 0.05)
  abline(h = 0, lty = 3)
  plotACF2(m21, uselags = c(2, 2), alpha = 0.05)
  abline(h = 0, lty = 3)
  plotACF2(m22, uselags = c(2, 2), alpha = 0.05)
  abline(h = 0, lty = 3)
  plotACF2(m23, uselags = c(2, 2), alpha = 0.05)
  abline(h = 0, lty = 3)
  plotACF2(m31, uselags = c(2, 2), alpha = 0.05)
  abline(h = 0, lty = 3)
  plotACF2(m32, uselags = c(2, 2), alpha = 0.05)
  abline(h = 0, lty = 3)
  plotACF2(m33, uselags = c(2, 2), alpha = 0.05)
  abline(h = 0, lty = 3)

  dev.off()
  }

}


#b. variability in behaviour
{
#find birds with the most data points
  dat = copy(x)
  dat[, N := length(date_), by = list(ID, season, breeding_season)]
  dat = subset(dat, select = c('ID', 'season', 'breeding_season', 'N', 'sex'))
  dat = unique(dat, by = names(dat))
  dat = subset(dat, (sex == 1 & N > 19) | (sex == 2 & N > 24))
  dat[, all_three_seasons := length(season), by = list(ID, breeding_season)]
  dat = subset(dat, all_three_seasons == 3)
  length(unique(paste(dat$ID, dat$breeding_season)))

  dat
  dat = unique(subset(dat, select = c('ID', 'breeding_season', 'sex')))
  setkey(dat, sex, breeding_season)

  jpeg(file = "S6.jpg", width = 720, height = 720)
  par(mfrow = c(2,2))
  par(mar = c(2.6, 2.6, 0.1, 0.1))
  for(i in 1 : nrow(dat)) {
    dat1 = subset(x, ID == dat$ID[i] & breeding_season == dat$breeding_season[i])
    dat1[season == "2_winter", COL := 'black']
    dat1[season == "1_preBreeding", COL := 'grey']
    dat1[season == "0_breeding", COL := 'white']

    if(dat1$ID[1] == 'B3Y6556') dat1 = subset(dat1, rel_day != -3) #double entry
    plot(dat1$time_to_sunrise_min ~ dat1$rel_day, pch = 21, bg = dat1$COL, xlim = c(-170, 55), ylim = c(-60, 120), xlab = '', ylab = '')
    abline(v = 0, lty = 3)
    lo = loess(dat1$time_to_sunrise_min ~ dat1$rel_day, control = loess.control(trace.hat = 'exact'), span = 0.5)
    lines(sort(unique(dat1$rel_day)), predict(lo, data.frame(rel_day = sort(unique(dat1$rel_day)))), col='red', lwd=2)
    text(-150, 110, paste(ifelse(dat1$sex[1] == 1, "\u2642", "\u2640"), dat1$breeding_season[1], sep = ' '), cex = 2, adj = 0)
  }
  dev.off()


  }
}


#5. relationship between emergence and sleep onset
{
  x[, season_sex := paste(season, sex, sep = '_')]
  x[, sex_inv := as.factor(-1*as.numeric(sex))]




  #1. check for interaction among sexes
  runthrough = data.table(expand.grid(season = unique(x$season), stringsAsFactors = FALSE))

  MMM = list()

  for(i in 1 : nrow(runthrough)) {
    dat = subset(x, season == runthrough[i, season])
    nn = nrow(dat)
    #hist(dat$time_to_sunrise_min)
    st = boxplot.stats(dat$time_to_sunrise_min)$stats
    dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])
    #hist(dat$time_to_sunrise_min)

    m = lmer(time_to_sunrise_min ~ time_to_sunset_min*sex+actualDaylength+(1|ID)+(time_to_sunset_min|date_)+(1|rel_day), data = dat)
    MMM[[length(MMM) + 1]] = data.table(cbind(summary(m)$coefficients, season = runthrough[i,season]))

  }
  #all interactions significant
  MMM


  #2. run for the sexes separately
  runthrough = data.table(expand.grid(sex = unique(x$sex), season = unique(x$season), stringsAsFactors = FALSE))

  MMM = list()

  for(i in 1 : nrow(runthrough)) {
    #if(runthrough[i, season] == '2_winter') {
    #  dat = subset(x, season == runthrough[i, season])
    #} else {
      dat = subset(x, season == runthrough[i, season] & sex == runthrough[i,sex])
    #}
    nn = nrow(dat)
    #hist(dat$time_to_sunrise_min)
    st = boxplot.stats(dat$time_to_sunset_min)$stats
    dat = subset(dat, time_to_sunset_min > st[1] & time_to_sunset_min <= st[5])
    #hist(dat$time_to_sunrise_min)

    m = lmer( time_to_sunset_min ~ time_to_sunrise_min +actualDaylength_min+(1|ID)+(time_to_sunrise_min|date_)+(1|rel_day), data = dat)
    MMM[[length(MMM) + 1]] = data.table(cbind(summary(m)$coefficients, season = runthrough[i,season], sex = runthrough[i,sex]))

  }
  MMM = rbindlist(MMM)

  MMM[(1:6)*3-1,]



#by date: run this to check

  LLL = list()

  for( SEX in c(1,2)) {
    for(i in min(x$date_):max(x$date_)) {
      dat = subset(x, date_ == i & sex == SEX)

      #nn = nrow(dat)
      #hist(dat$time_to_sunrise_min)
      #st = boxplot.stats(dat$time_to_sunrise_min)$stats
      #dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])

      if(nrow(dat) < 10) next;
#      hist(dat$time_to_sunrise_min)
      mm = lmer(time_to_sunset_min ~ time_to_sunrise_min+(1|rel_day), data = dat)
      LLL[[length(LLL)+1]] = c(Estimate = summary(mm)$coefficients[2,1], t = summary(mm)$coefficients[2,3], rel_day = i, N = nrow(dat), season = dat[1,season], sex = dat[1,sex])

    }
  }

  LLL = data.table(do.call(rbind,LLL))
  plot(LLL$Estimate ~ yday(as.Date(as.numeric(LLL$rel_day), origin = "1970-01-01")), col = LLL$sex)
  LLL$season2 = factor(LLL$season)

  jpeg(file = "4.jpg", width = 720, height = 720)

  boxplot(as.numeric(LLL$Estimate) ~ paste(LLL$season, LLL$sex), varwidth = TRUE, names = rep(c("\u2642", "\u2640"), 3), cex.axis = 1.5, ylab = list("Estimate", cex = 1.4))
  axis(1, at = c(1.5, 3.5, 5.5), labels = c("breeding", "pre-breeding", "winter"), line = 1.5, cex.axis = 1.3, tick = FALSE)
  abline(h = 0, lty = 3)
  abline(v = c(2.5, 4.5), lty = 3)
  text(c(1:6), rep(1.85, 6), boxplot(as.numeric(LLL$Estimate) ~ paste(LLL$season, LLL$sex), plot = FALSE)$n, cex = 1.5)
  dev.off()

  LLL3 = subset(LLL, sex == 2)
  LLL3[, Estimate := as.numeric(Estimate)]
  hist(LLL3$Estimate)
  st = boxplot.stats(LLL3$Estimate)$stats
  LLL3 = subset(LLL3, Estimate > st[1] & Estimate <= st[5])
  hist(LLL3$Estimate)

  mmm = lm(Estimate ~ 0+season2, data = LLL3)

  summary(mmm)

  plot(LLL2$Estimate ~ LLL2$rel_day)



  plot(time_to_sunrise_min ~ time_to_sunset_min, data = x, col = as.factor(season), cex = 0.1)



}


{ #stats for results section
cor(x$yearDay, x$rel_day) #0.996
cor(x$sunrise_time, x$rel_day) #-0.88
cor(x$sunset_time, x$rel_day) #-0.88
cor(x$actualDaylength, x$rel_day) #0.91

#differences between the sexes
dat = copy(x)
nn = nrow(dat)
st = boxplot.stats(dat$time_to_sunrise_min)$stats
dat = subset(dat, time_to_sunrise_min > st[1] & time_to_sunrise_min <= st[5])
mm = lmer(time_to_sunrise_min ~ sex + sunrise_time + season + (1|ID) + (1|breeding_season), data = dat)
summary(mm)

dat = copy(x)
nn = nrow(dat)
st = boxplot.stats(dat$time_to_sunset_min)$stats
dat = subset(dat, time_to_sunset_min > st[1] & time_to_sunset_min <= st[5])
mm = lmer(time_to_sunset_min ~ sex + sunset_time + season + (1|ID) + (1|breeding_season), data = dat)
summary(mm)

dat = copy(x)
nn = nrow(dat)
st = boxplot.stats(dat$daylength_min)$stats
dat = subset(dat, daylength_min > st[1] & daylength_min <= st[5])
mm = lmer(daylength_min ~ sex + actualDaylength_min + season + (1|ID) + (1|breeding_season), data = dat)
summary(mm)

#rain and T distributions
hist(x$SumRainfall, 100)
hist(x$SumRainfall_dawn, 100)
hist(x$SumRainfall_dusk, 100)

hist(x$Temperature2m, 100)
hist(x$Temperature2m_dawn, 100)
hist(x$Temperature2m_dusk, 100)

mean(x[season == '2_winter', SumRainfall], na.rm = TRUE)
sd(x[season == '2_winter', SumRainfall], na.rm = TRUE)/sqrt(nrow(x[season == '2_winter']))
min(x[season == '2_winter', SumRainfall], na.rm = TRUE)
max(x[season == '2_winter', SumRainfall], na.rm = TRUE)

mean(x[season == '1_preBreeding', SumRainfall], na.rm = TRUE)
sd(x[season == '1_preBreeding', SumRainfall], na.rm = TRUE)/sqrt(nrow(x[season == '1_preBreeding']))
min(x[season == '1_preBreeding', SumRainfall], na.rm = TRUE)
max(x[season == '1_preBreeding', SumRainfall], na.rm = TRUE)

mean(x[season == '0_breeding', SumRainfall], na.rm = TRUE)
sd(x[season == '0_breeding', SumRainfall], na.rm = TRUE)/sqrt(nrow(x[season == '0_breeding']))
min(x[season == '0_breeding', SumRainfall], na.rm = TRUE)
max(x[season == '0_breeding', SumRainfall], na.rm = TRUE)


ENV = copy(x)
{
ENV[, mean_T_dawn := mean(Temperature2m_dawn, na.rm = TRUE), by = season]
ENV[, mean_T_dusk := mean(Temperature2m_dusk, na.rm = TRUE), by = season]
ENV[, mean_T := mean(Temperature2m, na.rm = TRUE), by = season]
ENV[, min_T_dawn := min(Temperature2m_dawn, na.rm = TRUE), by = season]
ENV[, min_T_dusk := min(Temperature2m_dusk, na.rm = TRUE), by = season]
ENV[, min_T := min(Temperature2m, na.rm = TRUE), by = season]
ENV[, max_T_dawn := max(Temperature2m_dawn, na.rm = TRUE), by = season]
ENV[, max_T_dusk := max(Temperature2m_dusk, na.rm = TRUE), by = season]
ENV[, max_T := max(Temperature2m, na.rm = TRUE), by = season]
ENV[, se_T_dawn := sd(Temperature2m_dawn, na.rm = TRUE)/sqrt(length(na.omit(Temperature2m_dawn))), by = season]
ENV[, se_T_dusk := sd(Temperature2m_dusk, na.rm = TRUE)/sqrt(length(na.omit(Temperature2m_dusk))), by = season]
ENV[, se_T := sd(Temperature2m, na.rm = TRUE)/sqrt(length(na.omit(Temperature2m))), by = season]

ENV[, mean_R_dawn := mean(SumRainfall_dawn, na.rm = TRUE), by = season]
ENV[, mean_R_dusk := mean(SumRainfall_dusk, na.rm = TRUE), by = season]
ENV[, mean_R := mean(SumRainfall, na.rm = TRUE), by = season]
ENV[, min_R_dawn := min(SumRainfall_dawn, na.rm = TRUE), by = season]
ENV[, min_R_dusk := min(SumRainfall_dusk, na.rm = TRUE), by = season]
ENV[, min_R := min(SumRainfall, na.rm = TRUE), by = season]
ENV[, max_R_dawn := max(SumRainfall_dawn, na.rm = TRUE), by = season]
ENV[, max_R_dusk := max(SumRainfall_dusk, na.rm = TRUE), by = season]
ENV[, max_R := max(SumRainfall, na.rm = TRUE), by = season]
ENV[, se_R_dawn := sd(SumRainfall_dawn, na.rm = TRUE)/sqrt(length(na.omit(SumRainfall_dawn))), by = season]
ENV[, se_R_dusk := sd(SumRainfall_dusk, na.rm = TRUE)/sqrt(length(na.omit(SumRainfall_dusk))), by = season]
ENV[, se_R := sd(SumRainfall, na.rm = TRUE)/sqrt(length(na.omit(SumRainfall))), by = season]
}
ENV = subset(ENV, select = c('season', names(ENV)[87:110]))
ENV = unique(ENV)



}



