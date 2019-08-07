## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>",
  tidy.opts=list(width.cutoff=80),
  tidy=TRUE,
  eval=FALSE
)

## ----setup---------------------------------------------------------------
#  library(BTemergence)

## ------------------------------------------------------------------------
#  LAS = 1
#  MALE_COLOUR = "#5000FFFF"
#  FEMALE_COLOUR = "#FF758AFF"
#  ADULT_COLOUR = "#000033FF"
#  YEARLING_COLOUR = "#FF9C63FF"

## ------------------------------------------------------------------------
#  ###date_ dataset
#  {
#    dd0 = subset(x2, yday(date_) <  90 | yday(date_) > 274, select = c('time_to_sunrise_min', 'time_to_sunset_min', 'date_', 'sex', 'YDAY'))
#  
#    runthrough = data.table(expand.grid(sex = c(1,2), var = c('time_to_sunrise_min', 'time_to_sunset_min')))
#    D = list()
#    OUT = list()
#    for(j in 1 : nrow(runthrough)) {
#      dd = subset(dd0, sex == runthrough[j, sex])
#      dd[, var := dd[, which(names(dd) == runthrough[j,var]), with = FALSE]]
#      if(runthrough[j,var] == "time_to_sunset_min") dd[, YDAY := YDAY - 1] #because evening entry is the day before morning exit
#      d = data.table(YDAY = sort(unique(dd[, YDAY])))
#      out = data.table(YDAY = numeric(), out = numeric())
#      for(i in 1 : nrow(d)) {
#        d[i, med := boxplot.stats(dd[YDAY == d[i, YDAY], var])$stats[3]]
#        d[i, box_min := boxplot.stats(dd[YDAY == d[i, YDAY], var])$stats[2]]
#        d[i, box_max := boxplot.stats(dd[YDAY == d[i, YDAY], var])$stats[4]]
#        d[i, w_min := boxplot.stats(dd[YDAY == d[i, YDAY], var])$stats[1]]
#        d[i, w_max := boxplot.stats(dd[YDAY == d[i, YDAY], var])$stats[5]]
#        d[i, N := boxplot.stats(dd[YDAY == d[i, YDAY], var])$n]
#        tmp = boxplot.stats(dd[YDAY == d[i, YDAY], var])$out
#        if(length(tmp) > 0) { tmp = data.table(YDAY = rep(d[i, YDAY], length(tmp)), out = tmp); out = rbind(out, tmp) }
#      }
#      d[, sex := runthrough[j,sex]]
#      d[, var := runthrough[j,var]]
#      out[, sex := runthrough[j,sex]]
#      out[, var := runthrough[j,var]]
#      D[[length(D)+1]] = d
#      OUT[[length(OUT)+1]] = out
#  
#    }
#  }
#  ###rel_day dataset
#  {
#    dd0 = subset(x2, rel_day >= -21, select = c('time_to_sunrise_min', 'time_to_sunset_min', 'rel_day', 'sex'))
#  
#    runthrough = data.table(expand.grid(sex = c(1,2), var = c('time_to_sunrise_min', 'time_to_sunset_min')))
#    DR = list()
#    OUTR = list()
#    for(j in 1 : nrow(runthrough)) {
#      dd = subset(dd0, sex == runthrough[j, sex])
#      dd[, var := dd[, which(names(dd) == runthrough[j,var]), with = FALSE]]
#      if(runthrough[j,var] == "time_to_sunset_min") dd[, rel_day := rel_day - 1] #because evening entry is the day before morning exit
#      d = data.table(rel_day = sort(unique(dd[, rel_day])))
#      out = data.table(rel_day = numeric(), out = numeric())
#      for(i in 1 : nrow(d)) {
#        d[i, med := boxplot.stats(dd[rel_day == d[i, rel_day], var])$stats[3]]
#        d[i, box_min := boxplot.stats(dd[rel_day == d[i, rel_day], var])$stats[2]]
#        d[i, box_max := boxplot.stats(dd[rel_day == d[i, rel_day], var])$stats[4]]
#        d[i, w_min := boxplot.stats(dd[rel_day == d[i, rel_day], var])$stats[1]]
#        d[i, w_max := boxplot.stats(dd[rel_day == d[i, rel_day], var])$stats[5]]
#        d[i, N := boxplot.stats(dd[rel_day == d[i, rel_day], var])$n]
#        tmp = boxplot.stats(dd[rel_day == d[i, rel_day], var])$out
#        if(length(tmp) > 0) { tmp = data.table(rel_day = rep(d[i, rel_day], length(tmp)), out = tmp); out = rbind(out, tmp) }
#      }
#      d[, sex := runthrough[j,sex]]
#      d[, var := runthrough[j,var]]
#      out[, sex := runthrough[j,sex]]
#      out[, var := runthrough[j,var]]
#      setnames(d, 'rel_day', 'YDAY')
#      setnames(out, 'rel_day', 'YDAY')
#      DR[[length(DR)+1]] = d
#      OUTR[[length(OUTR)+1]] = out
#  
#    }
#  }

## ------------------------------------------------------------------------
#  data(var_day.sex_F)

## ------------------------------------------------------------------------
#  jpeg(file = paste0(getwd(), "/figures/F1_Dawn behaviour.jpg"), width = 1800, height = 1800)
#  plot_descriptive_figure(modellist_yday = list(var_day.sex[[1]], var_day.sex[[3]]),                        modellist_rel_day = list(var_day.sex[[2]], var_day.sex[[4]]),                        d_list = list(D[[1]], DR[[1]], D[[2]], DR[[2]]), out_list = list(OUT[[1]], OUTR[[1]], OUT[[2]], OUTR[[2]]), YLAB = "Onset of activity", VAR = "time_to_sunrise_min", ylim_boxplot = c(-60, 70), ylim_gammplot = c(-40, 20))
#  dev.off()

## ------------------------------------------------------------------------
#  jpeg(file = paste0(getwd(), "/figures/F2_Dusk behaviour.jpg"), width = 1800, height = 1800)
#  plot_descriptive_figure(modellist_yday = list(var_day.sex[[5]], var_day.sex[[7]]),
#                          modellist_rel_day = list(var_day.sex[[6]], var_day.sex[[8]]),
#                          d_list = list(D[[3]], DR[[3]], D[[4]], DR[[4]]),
#                          out_list = list(OUT[[3]], OUTR[[3]], OUT[[4]], OUTR[[4]]),
#                          YLAB = "Cessation of activity",
#                          VAR = "time_to_sunset_min",
#                          ylim_boxplot = c(-150, 30), ylim_gammplot = c(-65, 10))
#  

## ------------------------------------------------------------------------
#  jpeg(file = paste0(getwd(), "/figures/F3_Day behaviour.jpg"), width = 1000, height = 300)
#  
#  #data actual sunlight
#  actYDAY = dcast(subset(x2, YDAY < 90), YDAY ~ ., fun = mean, value.var = "actualDaylength")
#  setnames(actYDAY, '.', "actualDaylength")
#  actRELDAY = dcast(subset(x2, rel_day >= -21), rel_day ~ ., fun = mean, value.var = "actualDaylength")
#  setnames(actRELDAY, '.', "actualDaylength")
#  
#  cex.stage = 1.5
#  par(cex.axis=4/3, cex.lab=5/3, lwd = 2, las = 2, mgp = c(3.5,1.3,0))
#  line.stage = -1
#  
#  
#  
#  layout(matrix(1:2, ncol = 2, byrow = TRUE), widths = c(0.55, 0.45))
#  par(las = 2)
#  par(mar = c(5.1, 6.1, 2.1, 0.1))
#  create_gamm_plot(var = "daylength", data = subset(x2, YDAY < 90), modelset = list(var_day.sex[[9]], var_day.sex[[11]]) , ylab = list("Hours of activity", cex = 2), COL = c(MALE_COLOUR, FEMALE_COLOUR), xlim = c(-97, 90), xlab = "Date (1 = 1 January)", ylim = c(8, 16), se = 1.96)
#  axis(1, las = 0)
#  lines(x = actYDAY[, YDAY], y = actYDAY[, actualDaylength])
#  axis(1, at = c(-10:10)*10, labels = NA, tcl = 0.5)
#  mtext(text = c('O', 'N', 'D', 'J', 'F', 'M'), side = 3, line = -2, at = c(-77.5, -46, -15, 16, 46, 76.5), las = 0, cex = par("cex.lab"))
#  
#  par(mar = c(5.1, 0.1, 2.1, 6.1))
#  create_gamm_plot(var = "daylength", data = subset(x2, rel_day > -21), modelset = list(var_day.sex[[10]], var_day.sex[[12]]) , ylab = "", COL = c(MALE_COLOUR, FEMALE_COLOUR), xlim = c(-21, 60), xlab = "Days to first egg", yaxt = 'n', ylim = c(8, 16), se = 1.96)
#  
#  axis(1, las = 0)
#  axis(3, at = c(0, 10, 24, 44), labels = rep('', 4), tcl = -2)
#  axis(3, at = c(0, 10, 24, 44), labels = rep('', 4), tcl = 0.5)
#  axis(3, at = c(-10, 5, 17, 34, 55), labels = c('Nest', "Egg", "Inc.", "Young", "Post-Br."), las = 0, tick = FALSE, line = line.stage, cex.axis = cex.stage)
#  lines(x = actRELDAY[, rel_day], y = actRELDAY[, actualDaylength])
#  axis(1, at = c(-4:17)*5, labels = NA, tcl = 0.5)
#  
#  dev.off()
#  

## ------------------------------------------------------------------------
#  data(var_day.sex.age_F.RData)
#  dat1 = data.table(coefficients(summary(unlist(var_day.sex.age_F, recursive = FALSE)[[1]]$mer))[1:4,])
#  dat1[, sex := c(1,1,2,2)]; dat1[, age := c(1,2,1,2)]; dat1[, var := "Times to sunrise"];  dat1[, dayvar := "YDAY"]
#  dat2 = data.table(coefficients(summary(unlist(var_day.sex.age_F, recursive = FALSE)[[2]]$mer))[1:4,])
#  dat2[, sex := c(1,1,2,2)]; dat2[, age := c(1,2,1,2)]; dat2[, var := "Times to sunset"];  dat2[, dayvar := "YDAY"]
#  dat3 = data.table(coefficients(summary(unlist(var_day.sex.age_F, recursive = FALSE)[[3]]$mer))[1:4,])
#  dat3[, sex := c(1,1,2,2)]; dat3[, age := c(1,2,1,2)]; dat3[, var := "Times to sunrise"];  dat3[, dayvar := "rel_day"]
#  dat4 = data.table(coefficients(summary(unlist(var_day.sex.age_F, recursive = FALSE)[[4]]$mer))[1:4,])
#  dat4[, sex := c(1,1,2,2)]; dat4[, age := c(1,2,1,2)]; dat4[, var := "Times to sunset"];  dat4[, dayvar := "rel_day"]
#  
#  datFig = rbind.data.frame(dat1, dat2, dat3, dat4)
#  setnames(datFig, names(datFig), c('Est', 'SE', 't', 'sex', 'age', 'var', 'dayvar'))
#  datFig[, lowerLSD2 := Est - (1.96*SE/2)]
#  datFig[, upperLSD2 := Est + (1.96*SE/2)]
#  datFig[, YY := rep(1:4, 4)]
#  datFig[, COL_POINT := ifelse(sex == 1, MALE_COLOUR, FEMALE_COLOUR)]
#  datFig[, COL_LINE := ifelse(sex == 1, MALE_COLOUR, FEMALE_COLOUR)]
#  datFig[, PCH := rep(c(21,16,24,17),4)]
#  setkey(datFig, var, dayvar, YY)
#  
#  {
#  jpeg(file = paste0(getwd(), "/figures/F4_Sex and age.jpg"), width = 1800, height = 1800)
#  YLIM = c(0.5, 4.5)
#  
#  par(mfrow = c(2,2))
#  par(las = 0)
#  par(cex = 3, cex.lab = 3, lwd = 6)
#  plot_labels = c("(A)", "(B)", "(C)", "(D)")
#  #XLIM_LIST = list(c(-32, -8.5), c(-19, 5.5),
#  
#  #XLIM = c(-34, 5.5)
#  
#  for(i in c(0, 4, 8, 12)) {
#    j = 1:4 + i
#    par(mgp = c(1.8, 0.8,0))
#    if(i == 0) par(mar = c(3.5, 2.1, 1.5, 1.1))
#    if(i == 4) par(mar = c(3.5, 1.1, 1.5, 2.1))
#    if(i == 8) par(mar = c(4, 2.1, 1, 1.1))
#    if(i == 12) par(mar = c(4, 1.1, 1, 2.1))
#    XLIM = c(min(datFig[j, lowerLSD2])-0.1, max(datFig[j, upperLSD2])+0.1)
#    XLAB = "Estimate±CI"
#    plot(1:2, 1:2, type = 'n', xlim = XLIM, ylim = YLIM, yaxt = 'n', ylab = '', xlab = list('', cex = 1.2))
#    arrows(rep(datFig[j,Est],2), rep(datFig[j,YY],2), c(datFig[j,lowerLSD2], datFig[j,upperLSD2]), rep(datFig[j, YY],2), length = 0.15, angle = 90, col = datFig[j, COL_LINE])
#    points(datFig[j,Est], datFig[j,YY], col = datFig[j, COL_POINT], pch = datFig[j, PCH], cex = 1.4, bg = 'white')
#    if(i == 0) legend(-23.5, 4.2, pch = c(17, 24, 16, 21), pt.bg = 'white', legend = c("adult females", "yearling females", "adult males", "yearling males"), bty = 'n', col = c(FEMALE_COLOUR, FEMALE_COLOUR, MALE_COLOUR, MALE_COLOUR))
#    text(XLIM[1]-0.1, 4.4, plot_labels[(i/4)+1], adj = 0, cex = 1.4)
#    axis(1, at = -100:100, labels = NA, tcl = -0.3)
#  }
#  #mtext(1, at = c(2.5), labels = c("Times to sunrise (Estimate±CI)"), cex.axis = 1.2, tick = FALSE)
#  #mtext(1, at = c(2.5), labels = c("Times to sunset (Estimate±CI)"), cex.axis = 1.2, tick = FALSE)
#  
#  mtext("Non-Breeding", outer = TRUE, side=3, line = -1, at = 0.25, cex = 3.5, adj = 0.5)
#  mtext("Breeding", outer = TRUE, side=3, line = -1, at = 0.75, cex = 3.5, adj = 0.5)
#  mtext("Time to sunrise (Estimate±CI)", outer = TRUE, side = 3, line = -20.5, cex = 3.5, adj = 0.5, at = 0.5)
#  mtext("Time to sunset (Estimate±CI)", outer = TRUE, side = 3, line = -41, cex = 3.5, adj = 0.5, at = 0.5)
#  dev.off()
#  

## ------------------------------------------------------------------------
#  df = data.table(expand.grid(env = c("precipitation_dawn", "rainfall", "temperature"), variable = c("time_to_sunrise_min", "time_to_sunset_min"), varday = c("YDAY", "rel_day")))
#  
#  models = unlist(z_var_day.sex.env_males, recursive = FALSE)
#  
#  df[, est := unlist(lapply( models, FUN = function(x) { summary(x$gam)$p.table[1,1]}))]
#  df[, se := unlist(lapply( models, FUN = function(x) { summary(x$gam)$p.table[1,2]}))]
#  df[, sex := 1]
#  
#  
#  models = unlist(z_var_day.sex.env_females, recursive = FALSE)
#  
#  df2 = data.table(expand.grid(env = c("precipitation_dawn", "rainfall", "temperature"), variable = c("time_to_sunrise_min", "time_to_sunset_min"), varday = c("YDAY", "rel_day")))
#  df2[, est := unlist(lapply( models, FUN = function(x) { summary(x$gam)$p.table[1,1]}))]
#  df2[, se := unlist(lapply( models, FUN = function(x) { summary(x$gam)$p.table[1,2]}))]
#  df2[, sex := 2]
#  df = rbind(df, df2)
#  df = subset(df, env != "rainfall")
#  
#  df[, low := est - 1.96*se]
#  df[, up := est + 1.96*se]
#  setkey(df, variable, env, sex)
#  df[, YY := as.numeric(rep(rep(c(1:2), each = 4), 2))]
#  df[sex == 1, YY := YY-0.2]
#  df[sex == 2, YY := YY+0.2]
#  df[sex == 1, COL := MALE_COLOUR]
#  df[sex == 2, COL := FEMALE_COLOUR]
#  
#  df[, SHIFT := ifelse(varday == "YDAY", 0.1, -0.1)]
#  df[, YY := YY + SHIFT]
#  df[sex == 1, PCH := ifelse(varday == "YDAY", 16,21)]
#  df[sex == 2, PCH := ifelse(varday == "YDAY", 17,24)]
#  {
#  jpeg(file = paste0(getwd(), "/figures/F5_Environment.jpg"), width = 1800, height = 1800)
#  
#  layout(c(1,2), heights = c(1,1))
#  
#  par(cex = 3.5, cex.axis = 1, cex.lab = 1, lwd = 4, mgp = c(2,1,0))
#  par(mar = c(4.1, 8.1, 0.1, 0.1))
#  plot_env(df = df, VARIABLE = "time_to_sunrise_min", xlim = c(-0.15, 0.25), ylim = c(0.5, 2.5), xlab = "Times to sunrise (Estimate±CI)")
#  axis(1, at = c(-10:10)/40, labels = NA, tcl = -0.3)
#  legend("topright", c("non-breeding female", "breeding female", "non-breeding male", "breeding male"), lwd = 2, col = rep(c(FEMALE_COLOUR, MALE_COLOUR), each  = 2), pch = c(17, 24, 16, 21), bty = 'n', pt.bg = 'white')
#  par(mar = c(4.1, 8.1, 0.1, 0.1))
#  plot_env(df= df, VARIABLE = "time_to_sunset_min", xlim = c(-0.15, 0.25), ylim = c(0.5, 2.5), xlab = "Times to sunset (Estimate±CI)")
#  axis(1, at = c(-10:10)/40, labels = NA, tcl = -0.3)
#  
#  dev.off()
#  

