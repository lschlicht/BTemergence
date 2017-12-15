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
  fe = dbq(con, "SELECT DATE(firstEgg) as firstEgg, year_ as breeding_season, box FROM BTatWESTERHOLZ.BREEDING")
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
        dp_all = dp_all[, data.table(matrix(boxplot.stats(time_to_sunrise_min)$stats, nrow = 1)), by = list(rel_day, sex)]
        setnames(dp_all, "rel_day", "day")

      } else {
        dp_all[, ':=' (mean_time_to_sunrise_min = median(time_to_sunrise_min, na.rm = TRUE), median_time_to_sunset_min = mean(time_to_sunset_min, na.rm = TRUE), median_daylength_hour = mean(prop_daylength_used, na.rm = TRUE), se_time_to_sunrise_min = sd(time_to_sunrise_min, na.rm = TRUE)/sqrt(length(na.omit(time_to_sunrise_min))), se_time_to_sunset_min = sd(time_to_sunset_min, na.rm = TRUE)/sqrt(length(na.omit(time_to_sunset_min))), se_daylength_hour = sd(prop_daylength_used, na.rm = TRUE)/sqrt(length(na.omit(prop_daylength_used))), N = length(na.omit(time_to_sunset_min))), by = list(yday(date_), sex)] ;
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
