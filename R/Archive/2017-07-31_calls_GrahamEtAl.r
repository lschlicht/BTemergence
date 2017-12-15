# Multiple plot function from the R Cookbook
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



calls2 = function(working_directory = getwd()) {
  require(sdb)
  require(SNB)
  #fetch data (ca. 1.9 minutes on Linux 16 cores, 4.5 minutes on Windows4(?) cores)
  setwd(working_directory)
  con = dbcon(user = "lschlicht")
  input = whichData(years = 2011:2017, boxes = 1:277, from = "-01-01", to = "-12-31")
  x = queryEventsAll(con, input)
  x = addBreeding(con, x)
  x = addWeather(con, x)
  x = addIndVar(con, x)
  x = addSunriseSunset(x)
  x = addSleep(x)
  x = addNestStage(con, x)
  x = addBreedingStage(x)
  x[, breeder := ifelse(is.na(box.breeding), "no", "yes")]
  x[, breeder_sex := paste(breeder, sex)]
  x[, breedingAttempt := paste(year_, box.breeding, sep = '_')]
  snb = copy(x)
  x = subset(x, sleep == 1)
  x = subset(x, breeder == "yes")
  x = subset(x, !is.na(rel_day))
  x[, fac_breeding_stage := factor(breeding_stage, levels =
                                     c('nonBreeding', 'territoryEstablishment', 'nestBuilding', 'nestCompleted', 'preLaying', 'laying', 'incubation', 'earlyProvisioning', 'lateProvisioning', 'postFleding'))]

  fe = subset(x, select = c('year_', 'box.breeding', 'firstEgg'))
  fe = unique(fe, by = names(fe))
  fe[, z_firstEgg := scale(as.IDate(firstEgg)), by = 'year_']
  fe[, c_firstEgg := scale(as.IDate(firstEgg), scale = FALSE), by = 'year_']
  x = merge(x, fe, by.x = c('year_', 'box.breeding', 'firstEgg'), by.y = c('year_', 'box.breeding', 'firstEgg'))
  setkey(x, year_, box.breeding, ID, date_)
  x[, lag1_rel_day := shift(rel_day), by = list(year_, box.breeding, ID)]
  x[, lag1_rel_day_emergence_min := shift(time_to_sunrise_min), by = list(year_, box.breeding, ID)]
  x2 = subset(x, sex == 2)
  closeCon(con)
  #end fetch data

  #remove potential replacement broods
  x3 = subset(x2, z_firstEgg <= median(subset(x2, breeding_stage == 'incubation')$z_firstEgg)+abs(min(subset(x2, breeding_stage == 'incubation')$z_firstEgg)))
  xinc = subset(x3, breeding_stage == 'incubation')
  hist(x2$c_firstEgg, 20)
  hist(x3$c_firstEgg, 20)

  require(MCMCglmm)
  require(lme4)



{ #dawn
  #######their model
  {
  m = lmer(time_to_sunrise_min ~ yday(firstEgg) + I((rel_day+clutch-1)^2) + yearDay + (1|year_) + (1|ID), data = xinc)
  plot(m)
  hist(resid(m))
  qqnorm(residuals(m))
  summary(m)
  #strong collinearities among all three explanatory variables; use poly... to avoid that
  #fit not that good, but ok...

  #my model
  m = lmer(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = xinc)
  plot(m)
  hist(resid(m))
  qqnorm(residuals(m))
  summary(m)
  #collinearities are small now
  #fit not that good, but ok...

  #repeatability for individual ID
  vc = VarCorr(m)
  residual_var = attr(vc,'sc')^2
  ID_var = attr(vc$ID,'stddev')[1]^2
  year_var = attr(vc$year_,'stddev')[1]^2
  R1 = ID_var/(ID_var+year_var+residual_var) #28.18%
   CF = confint(m); CF
  CF[1,1]^2/sum(CF[1,1]^2, CF[2,1]^2, CF[3,1]^2) #25.42%
  CF[1,2]^2/sum(CF[1,2]^2, CF[2,2]^2, CF[3,2]^2) #29.51%

  #get p-values and confidence intervals with MCMCglmm
  m = MCMCglmm(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2), random = ~ID, data = xinc)
  summary(m)
  #all effects positive, but square is only a trend
  #plot to get interpretable effect sizes?
  }
  #######


  #######expand across season
  #pre-breeding
  {
  m0 = lmer(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = subset(x3, rel_day >= -30 & rel_day < 0))
  plot(m0)
  hist(resid(m0))
  qqnorm(residuals(m0))
  m = lmer(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = subset(x3, rel_day >= -30 & rel_day < 0)[which(abs(resid(m0)) < 60)])
  m = lmer(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = subset(x3, rel_day >= -30 & rel_day < 0)[which(abs(resid(m0)) < 60)])
  plot(m)
  hist(resid(m))
  qqnorm(residuals(m))
  summary(m)
  #collinearities are small now
  #fit not that good, but ok...

  #repeatability for individual ID
  vc = VarCorr(m)
  residual_var = attr(vc,'sc')^2
  ID_var = attr(vc$ID,'stddev')[1]^2
  year_var = attr(vc$year_,'stddev')[1]^2
  R1 = ID_var/(ID_var+year_var+residual_var) #24.07
  CF = confint(m); CF
  CF[1,1]^2/sum(CF[1,1]^2, CF[2,1]^2, CF[3,1]^2) #20.99%
  CF[1,2]^2/sum(CF[1,2]^2, CF[2,2]^2, CF[3,2]^2) #24.21%

  #get p-values and confidence intervals with MCMCglmm
  m = MCMCglmm(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2), random = ~ID, data = subset(x3, rel_day >= -30 & rel_day < 0)[which(abs(resid(m0)) < 60)])
  summary(m)
  }

  #winter
  {
    m0 = lmer(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = subset(x3, rel_day >= 300 | rel_day < -30)) #which(fitted(m) > 20); which(resid(m) < -50), outlier

    plot(m0)
    hist(resid(m0))
    qqnorm(residuals(m0))
    m = lmer(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = subset(x3, rel_day >= 300 | rel_day < -30)[unique(c(-987, -(26:42), -which(abs(resid(m0)) > 60)))])#[c(-987, -(26:42)))])
    plot(m)
    hist(resid(m))
    qqnorm(residuals(m))
    summary(m)
    #collinearities are small now
    #fit not that good, but ok...

     #repeatability for individual ID
    vc = VarCorr(m)
    residual_var = attr(vc,'sc')^2
    ID_var = attr(vc$ID,'stddev')[1]^2
    year_var = attr(vc$year_,'stddev')[1]^2
    R1 = ID_var/(ID_var+year_var+residual_var);R1 #18.53%
    CF = confint(m); CF
    CF[1,1]^2/sum(CF[1,1]^2, CF[2,1]^2, CF[3,1]^2) #14.76%
    CF[1,2]^2/sum(CF[1,2]^2, CF[2,2]^2, CF[3,2]^2) #20.73%

    #get p-values and confidence intervals with MCMCglmm
    m = MCMCglmm(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2), random = ~ID, data = subset(x3, rel_day >= 300 | rel_day < -30)[c(-987, -(26:42))])
    summary(m)
  }

  #######

  #######repeatabilities across the season
 { #1. repeatability of emergence times across the season
  #a. winter
  m = lmer(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = subset(x3, yearDay > 300 | yearDay < 57))
  plot(m)
  hist(resid(m))
  qqnorm(residuals(m))
  vc = VarCorr(m)
  residual_var = attr(vc,'sc')^2
  ID_var = attr(vc$ID,'stddev')[1]^2
  year_var = attr(vc$year_,'stddev')[1]^2
  R1 = ID_var/(ID_var+year_var+residual_var)
  CF = confint(m)
  CF1_low = CF[1,1]^2/sum(CF[1,1]^2, CF[2,1]^2, CF[3,1]^2) #25.42%
  CF1_up = CF[1,2]^2/sum(CF[1,2]^2, CF[2,2]^2, CF[3,2]^2) #29.51%

  #b. preLaying (-30 to -1)
  m = lmer(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = subset(x3, rel_day >= -30 & rel_day < 0))
  plot(m)
  hist(resid(m))
  qqnorm(residuals(m))
  vc = VarCorr(m)
  residual_var = attr(vc,'sc')^2
  ID_var = attr(vc$ID,'stddev')[1]^2
  year_var = attr(vc$year_,'stddev')[1]^2
  R2 = ID_var/(ID_var+year_var+residual_var)
  CF = confint(m)
  CF2_low = CF[1,1]^2/sum(CF[1,1]^2, CF[2,1]^2, CF[3,1]^2) #25.42%
  CF2_up = CF[1,2]^2/sum(CF[1,2]^2, CF[2,2]^2, CF[3,2]^2) #29.51%


  #c. Laying (0 - last egg)
  m = lmer(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = subset(x3, rel_day >= 0 & rel_day < as.Date(firstEgg) + clutch - 1))
  plot(m)
  hist(resid(m))
  qqnorm(residuals(m))
  vc = VarCorr(m)
  residual_var = attr(vc,'sc')^2
  ID_var = attr(vc$ID,'stddev')[1]^2
  year_var = attr(vc$year_,'stddev')[1]^2
  R3 = ID_var/(ID_var+year_var+residual_var)
  CF = confint(m)
  CF3_low = CF[1,1]^2/sum(CF[1,1]^2, CF[2,1]^2, CF[3,1]^2) #25.42%
  CF3_up = CF[1,2]^2/sum(CF[1,2]^2, CF[2,2]^2, CF[3,2]^2) #29.51%


  #d. incubation (lastegg + 1 to hatch-1)
  m = lmer(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = subset(x3, rel_day < as.Date(firstEgg) + clutch - 1 & date_ < hatchDate))
  plot(m)
  hist(resid(m))
  qqnorm(residuals(m))
  vc = VarCorr(m)
  residual_var = attr(vc,'sc')^2
  ID_var = attr(vc$ID,'stddev')[1]^2
  year_var = attr(vc$year_,'stddev')[1]^2
  R4 = ID_var/(ID_var+year_var+residual_var)
  CF = confint(m)
  CF4_low = CF[1,1]^2/sum(CF[1,1]^2, CF[2,1]^2, CF[3,1]^2) #25.42%
  CF4_up = CF[1,2]^2/sum(CF[1,2]^2, CF[2,2]^2, CF[3,2]^2) #29.51%

  #e. early provisioning
  m = lmer(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = subset(x3, date_ >= hatchDate & date_-8 < hatchDate))
  plot(m)
  hist(resid(m))
  qqnorm(residuals(m))
  vc = VarCorr(m)
  residual_var = attr(vc,'sc')^2
  ID_var = attr(vc$ID,'stddev')[1]^2
  year_var = attr(vc$year_,'stddev')[1]^2
  R5 = ID_var/(ID_var+year_var+residual_var)
  CF = confint(m)
  CF5_low = CF[1,1]^2/sum(CF[1,1]^2, CF[2,1]^2, CF[3,1]^2) #25.42%
  CF5_up = CF[1,2]^2/sum(CF[1,2]^2, CF[2,2]^2, CF[3,2]^2) #29.51%


  #f. late provisioning
  m = lmer(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = subset(x3, date_-8 > hatchDate & date_ < fledgeDate))
  plot(m)
  hist(resid(m))
  qqnorm(residuals(m))
  vc = VarCorr(m)
  residual_var = attr(vc,'sc')^2
  ID_var = attr(vc$ID,'stddev')[1]^2
  year_var = attr(vc$year_,'stddev')[1]^2
  R6 = ID_var/(ID_var+year_var+residual_var)
  CF = confint(m)
  CF6_low = CF[1,1]^2/sum(CF[1,1]^2, CF[2,1]^2, CF[3,1]^2) #25.42%
  CF6_up = CF[1,2]^2/sum(CF[1,2]^2, CF[2,2]^2, CF[3,2]^2) #29.51%


  #g. across the all year
  m = lmer(time_to_sunrise_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = x3)
  plot(m)
  hist(resid(m))
  qqnorm(residuals(m))
  vc = VarCorr(m)
  residual_var = attr(vc,'sc')^2
  ID_var = attr(vc$ID,'stddev')[1]^2
  year_var = attr(vc$year_,'stddev')[1]^2
  R7 = ID_var/(ID_var+year_var+residual_var)
  CF = confint(m)
  CF7_low = CF[1,1]^2/sum(CF[1,1]^2, CF[2,1]^2, CF[3,1]^2) #25.42%
  CF7_up = CF[1,2]^2/sum(CF[1,2]^2, CF[2,2]^2, CF[3,2]^2) #29.51%

  #h. during breeding from on season to the next
  #subset to females that breed in more than one year
  tmp = subset(x3, select = c(year_, ID)); tmp = unique(tmp, by = names(tmp))
  tmp = subset(tmp, duplicated(tmp$ID))
  tmp = subset(x, ID %in% tmp$ID)
  tmp[, sq_rel_day := rel_day^2]
  tmp = subset(tmp, !is.na(time_to_sunrise_min) & !is.na(rel_day) & !is.na(year_) & !is.na(ID) & !is.na(date_) & !is.na(fledgeDate) & !is.na(rel_day^2))
  tmp = subset(tmp, rel_day >= -30 & date_ < fledgeDate)
  mod = lm(time_to_sunrise_min ~ (rel_day)^2, data = tmp)

  tmp[, resid := resid(mod)]
  tmp[, median_resid := median(resid), by = list(ID, year_)]
  tmp = subset(tmp, select = c('year_', 'ID', 'median_resid'))
  tmp = unique(tmp, by = names(tmp))

  m = lmer(median_resid ~ (1|ID) + (1|year_), data = tmp)
  vc = VarCorr(m)
  residual_var = attr(vc,'sc')^2
  ID_var = attr(vc$ID,'stddev')[1]^2
  year_var = attr(vc$year_,'stddev')[1]^2
  Racross = ID_var/(ID_var+year_var+residual_var) #95 Individuals
  CF = confint(m)
  CFacross_low = CF[1,1]^2/sum(CF[1,1]^2, CF[2,1]^2, CF[3,1]^2) #25.42%
  CFacross_up = CF[1,2]^2/sum(CF[1,2]^2, CF[2,2]^2, CF[3,2]^2) #29.51%
  }
  c(R1, R2, R3, R4, R5, R6, R7, Racross)
  c(CF1_up, CF2_up, CF3_up, CF4_up, CF5_up, CF6_up, CF7_up, CFacross_up)
  c(CF1_low, CF2_low, CF3_low, CF4_low, CF5_low, CF6_low, CF7_low, CFacross_low)


  #######plot
  {
  require(gglot2)
  p1 = ggplot(data = xinc, aes(x = time_to_sunrise_min, y = yday(firstEgg))) +
    geom_point() +
    stat_smooth(method = 'loess') +
    stat_smooth(method = 'lm', colour = 'red')

  p2 = ggplot(data = xinc, aes(x = (rel_day+clutch-1), y = time_to_sunrise_min)) +
    geom_point() +
    stat_smooth(method = 'loess')

  p3 = ggplot(data = subset(x3, rel_day >= -30 & rel_day < 0), aes(x = time_to_sunrise_min, y = yday(firstEgg))) +
    geom_point() +
    stat_smooth(method = 'loess') +
    stat_smooth(method = 'lm', colour = 'red')
  p4 = ggplot(data = subset(x3, rel_day >= -30 & rel_day < 0), aes(x = rel_day, y = time_to_sunrise_min)) +
    geom_point() +
    stat_smooth(method = 'loess')

  p5 = ggplot(data = subset(x3, rel_day >= 300 | rel_day < -30)[c(-987, -(26:42))], aes(x = time_to_sunrise_min, y = yday(firstEgg))) +
    geom_point() +
    stat_smooth(method = 'loess') +
    stat_smooth(method = 'lm', colour = 'red')

  p6 = ggplot(data = subset(x3, rel_day >= 300 | rel_day < -30)[c(-987, -(26:42))], aes(x = rel_day, y = time_to_sunrise_min)) +
    geom_point() +
    stat_smooth(method = 'loess')

  jpeg("plot_emergence_firstEgg_Grahametal2017.jpeg", width = 720, height = 720)
  multiplot(p1, p3 ,p5, p2, p4, p6, cols = 2)
  dev.off()


  }
  #######

  #3. include age
  m = MCMCglmm(time_to_sunrise_min ~ minAge + yday(firstEgg) + poly((rel_day+clutch-1),2), random = ~ID, data = xinc)
  summary(m)
  m = MCMCglmm(time_to_sunrise_min ~ minAge + yday(firstEgg) + poly((rel_day+clutch-1),2), random = ~ID, data = subset(x3, rel_day >= -30 & rel_day < 0))
  summary(m)
  m = MCMCglmm(time_to_sunrise_min ~ minAge + yday(firstEgg) + poly((rel_day+clutch-1),2), random = ~ID, data = subset(x3, rel_day >= 300 | rel_day < -30)[c(-987, -(26:42))])
  summary(m)

  m = MCMCglmm(time_to_sunrise_min ~ minAge + poly((rel_day+clutch-1),2), random = ~ID, data = xinc)
  summary(m)
  m = MCMCglmm(time_to_sunrise_min ~ minAge + poly((rel_day+clutch-1),2), random = ~ID, data = subset(x3, rel_day >= -30 & rel_day < 0))
  summary(m)
  m = MCMCglmm(time_to_sunrise_min ~ minAge + poly((rel_day+clutch-1),2), random = ~ID, data = subset(x3, rel_day >= 300 | rel_day < -30)[c(-987, -(26:42))])
  summary(m)


  #b. within individual: does an individual emerge earlier during incubation as an adult than as a yearling?
  #subset to individuals that were recorded as yearling and adult
  tmp =   subset(x3, select = c('ID', 'year_'))
  tmp = unique(tmp, by = names(tmp))
  tmp = tmp[duplicated(tmp$ID)]
  tmp = subset(x3, ID %in% tmp$ID)
  tmp2 = unique(subset(tmp, age == 1)$ID)
  tmp = subset(tmp, ID %in% tmp2)


  tmp = subset(tmp, breeding_stage %in% c('incubation') )
  tmp[, median_emergence := median(time_to_sunrise_min), by = list(year_, ID)]
  tmp = subset(tmp, select = c('median_emergence', 'year_', 'ID', 'minAge', 'firstEgg'))
  tmp = unique(tmp, by = names(tmp))
  boxplot(median_emergence ~ minAge, data = tmp, varwidth = TRUE)
  m = lmer(median_emergence ~ minAge + yday(firstEgg) + (1|ID), data = tmp)
  hist(resid(m))
  plot(m)
  qqnorm(resid(m))
  summary(m)
  m = MCMCglmm(median_emergence ~ minAge + yday(firstEgg), random = ~ID, data = tmp)
  summary(m)


  m = lmer(median_emergence ~ minAge + (1|ID), data = tmp)
  hist(resid(m))
  plot(m)
  qqnorm(resid(m))
  summary(m)
  m = lmer(median_emergence ~ yday(firstEgg) + (1|ID), data = tmp)
  hist(resid(m))
  plot(m)
  qqnorm(resid(m))
  summary(m)
  #no evidence.

  p1 = ggplot(data = xinc, aes(x = yday(firstEgg), y = time_to_sunrise_min, group = factor(minAge), colour = factor(minAge))) +
    stat_smooth(method = 'lm', se = FALSE)
  jpeg("plot_emergence_firstEgg_age_Grahametal2017.jpeg", width = 720, height = 720)
  p1
  dev.off()

}

  { #dusk
    #######their model
    {
      #my model
      m = lmer(time_to_sunset_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = xinc)
      plot(m)
      hist(resid(m))
      qqnorm(residuals(m))
      summary(m)
      #collinearities are small now
      #fit perfect

      #repeatability for individual ID
      vc = VarCorr(m)
      residual_var = attr(vc,'sc')^2
      ID_var = attr(vc$ID,'stddev')[1]^2
      year_var = attr(vc$year_,'stddev')[1]^2
      R1 = ID_var/(ID_var+year_var+residual_var);R1 #27.59%
      CF = confint(m); CF
      (CF[1,1]^2)/sum(CF[1,1]^2, CF[2,2]^2, CF[3,2]^2) #27.20%
      CF[1,2]^2/sum(CF[1,2]^2, CF[2,1]^2, CF[3,1]^2) #23.77%

      #get p-values and confidence intervals with MCMCglmm
      m = MCMCglmm(time_to_sunset_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2), random = ~ID, data = xinc)
      summary(m)
      #first Egg and poly1 negative, poly2 positive
      #plot to get interpretable effect sizes?
    }
    #######


    #######expand across season
    #pre-breeding
    {
      m = lmer(time_to_sunset_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = subset(x3, rel_day >= -30 & rel_day < 0))
      plot(m)
      hist(resid(m))
      qqnorm(residuals(m))
      summary(m)
      #collinearities are small now
      #fit not that good, but ok...

      #repeatability for individual ID
      vc = VarCorr(m)
      residual_var = attr(vc,'sc')^2
      ID_var = attr(vc$ID,'stddev')[1]^2
      year_var = attr(vc$year_,'stddev')[1]^2
      R1 = ID_var/(ID_var+year_var+residual_var) #21.98%
      CF = confint(m); CF
      CF[1,1]^2/sum(CF[1,1]^2, CF[2,2]^2, CF[3,2]^2) #18.19%
      CF[1,2]^2/sum(CF[1,2]^2, CF[2,1]^2, CF[3,1]^2) #29.03%

      #get p-values and confidence intervals with MCMCglmm
      m = MCMCglmm(time_to_sunset_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2), random = ~ID, data = subset(x3, rel_day >= -30 & rel_day < 0))
      summary(m)
    }

    #winter
    {
      m = lmer(time_to_sunset_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|year_) + (1|ID), data = subset(x3, rel_day >= 300 | rel_day < -30)[c(-987, -(26:42))]) #which(fitted(m) > 20); which(resid(m) < -50), outlier

      plot(m)
      hist(resid(m))
      qqnorm(residuals(m))
      summary(m)
      #collinearities are small now
      #fit not that good, but ok...

      #repeatability for individual ID
      vc = VarCorr(m)
      residual_var = attr(vc,'sc')^2
      ID_var = attr(vc$ID,'stddev')[1]^2
      year_var = attr(vc$year_,'stddev')[1]^2
      R1 = ID_var/(ID_var+year_var+residual_var);R1 #23.43%
      CF = confint(m); CF
      CF[1,1]^2/sum(CF[1,1]^2, CF[2,2]^2, CF[3,2]^2) #13.50%
      CF[1,2]^2/sum(CF[1,2]^2, CF[2,1]^2, CF[3,1]^2) #33.91%

      #get p-values and confidence intervals with MCMCglmm
      m = MCMCglmm(time_to_sunset_min ~ yday(firstEgg) + poly((rel_day+clutch-1),2), random = ~ID, data = subset(x3, time_to_sunset_min < 25 & (rel_day >= 300 | rel_day < -30))[c(-987, -(26:42))])
      summary(m)
    }

    #######


    #######plot
    {
      require(gglot2)
      p1 = ggplot(data = xinc, aes(x = time_to_sunset_min, y = yday(firstEgg))) +
        geom_point() +
        stat_smooth(method = 'loess') +
        stat_smooth(method = 'lm', colour = 'red')

      p2 = ggplot(data = xinc, aes(x = rel_day, y = time_to_sunset_min)) +
        geom_point() +
        stat_smooth(method = 'loess')

      p3 = ggplot(data = subset(x3, rel_day >= -30 & rel_day < 0), aes(x = time_to_sunset_min, y = yday(firstEgg))) +
        geom_point() +
        stat_smooth(method = 'loess') +
        stat_smooth(method = 'lm', colour = 'red')
      p4 = ggplot(data = subset(x3, rel_day >= -30 & rel_day < 0), aes(x = rel_day, y = time_to_sunset_min)) +
        geom_point() +
        stat_smooth(method = 'loess')

      p5 = ggplot(data = subset(x3, rel_day >= 300 | rel_day < -30)[c(-987, -(26:42))], aes(x = time_to_sunset_min, y = yday(firstEgg))) +
        geom_point() +
        stat_smooth(method = 'loess') +
        stat_smooth(method = 'lm', colour = 'red')

      p6 = ggplot(data = subset(x3, rel_day >= 300 | rel_day < -30)[c(-987, -(26:42))], aes(x = rel_day, y = time_to_sunset_min)) +
        geom_point() +
        stat_smooth(method = 'loess')

      jpeg("plot_sleepOnset_firstEgg_Grahametal2017.jpeg", width = 720, height = 720)
      multiplot(p1, p3 ,p5, p2, p4, p6, cols = 2)
      dev.off()


    }
    #######

    #3. include age
    m = lmer(time_to_sunrise_min ~ minAge + yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|ID) + (1|year_), data = xinc)
    summary(m)
    m = lmer(time_to_sunrise_min ~ minAge + yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|ID) + (1|year_), data = subset(x3, rel_day >= -30 & rel_day < 0))
    summary(m)
    m = lmer(time_to_sunrise_min ~ minAge + yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|ID) + (1|year_), data = subset(x3, rel_day < -30 | rel_day > 300))
    summary(m)
    m = lmer(time_to_sunrise_min ~ age + yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|ID) + (1|year_), data = xinc)
    summary(m)
    m = lmer(time_to_sunrise_min ~ age + yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|ID) + (1|year_), data = subset(x3, rel_day >= -30 & rel_day < 0))
    summary(m)
    m = lmer(time_to_sunrise_min ~ age + yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|ID) + (1|year_), data = subset(x3, rel_day < -30 | rel_day > 300))
    summary(m)
    m = lmer(time_to_sunset_min ~ minAge + yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|ID) + (1|year_), data = xinc)
    summary(m)
    m = lmer(time_to_sunset_min ~ minAge + yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|ID) + (1|year_), data = subset(x3, rel_day >= -30 & rel_day < 0))
    summary(m)
    m = lmer(time_to_sunset_min ~ minAge + yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|ID) + (1|year_), data = subset(x3, rel_day < -30 | rel_day > 300))
    summary(m)
    m = lmer(time_to_sunset_min ~ age + yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|ID) + (1|year_), data = xinc)
    summary(m)
    m = lmer(time_to_sunset_min ~ age + yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|ID) + (1|year_), data = subset(x3, rel_day >= -30 & rel_day < 0))
    summary(m)
    m = lmer(time_to_sunset_min ~ age + yday(firstEgg) + poly((rel_day+clutch-1),2) + (1|ID) + (1|year_), data = subset(x3, rel_day < -30 | rel_day > 300))
    summary(m)



    m = MCMCglmm(time_to_sunrise_min ~ minAge + yday(firstEgg) + poly((rel_day+clutch-1),2), random = ~ID, data = xinc)
    summary(m)
    m = MCMCglmm(time_to_sunrise_min ~ minAge + yday(firstEgg) + poly((rel_day+clutch-1),2), random = ~ID, data = subset(x3, rel_day >= -30 & rel_day < 0))
    summary(m)
    m = MCMCglmm(time_to_sunset_min ~ minAge + yday(firstEgg) + poly((rel_day+clutch-1),2), random = ~ID, data = subset(x3, rel_day >= 300 | rel_day < -30)[c(-987, -(26:42))])
    summary(m)

    m = MCMCglmm(time_to_sunset_min ~ minAge + poly((rel_day+clutch-1),2), random = ~ID, data = xinc)
    summary(m)
    m = MCMCglmm(time_to_sunset_min ~ minAge + poly((rel_day+clutch-1),2), random = ~ID, data = subset(x3, rel_day >= -30 & rel_day < 0))
    summary(m)
    m = MCMCglmm(time_to_sunset_min ~ minAge + poly((rel_day+clutch-1),2), random = ~ID, data = subset(x3, rel_day >= 300 | rel_day < -30)[c(-987, -(26:42))])
    summary(m)


    #b. within individual: does an individual emerge earlier during incubation as an adult than as a yearling?
    #subset to individuals that were recorded as yearling and adult
    tmp =   subset(x3, select = c('ID', 'year_'))
    tmp = unique(tmp, by = names(tmp))
    tmp = tmp[duplicated(tmp$ID)]
    tmp = subset(x3, ID %in% tmp$ID)
    tmp2 = unique(subset(tmp, age == 1)$ID)
    tmp = subset(tmp, ID %in% tmp2)


    tmp = subset(tmp, breeding_stage %in% c('incubation') )
    tmp[, median_sleepOnset := median(time_to_sunset_min), by = list(year_, ID)]
    tmp[, median_emergence := median(time_to_sunrise_min), by = list(year_, ID)]
    tmp = subset(tmp, select = c('median_sleepOnset', 'year_', 'ID', 'minAge', 'firstEgg', 'age', 'median_emergence'))
    tmp = unique(tmp, by = names(tmp))
    boxplot(median_sleepOnset ~ minAge, data = tmp, varwidth = TRUE)
    m = lmer(median_sleepOnset ~ age + yday(firstEgg) + (1|ID), data = tmp)
    m = lmer(median_sleepOnset ~ minAge + yday(firstEgg) + (1|ID), data = tmp)
    m = lmer(median_emergence ~ age + yday(firstEgg) + (1|ID), data = tmp)
    m = lmer(median_emergence ~ minAge + yday(firstEgg) + (1|ID), data = tmp)
    hist(resid(m))
    plot(m)
    qqnorm(resid(m))
    summary(m)
    m = MCMCglmm(median_sleepOnset ~ minAge + yday(firstEgg), random = ~ID, data = tmp)
    summary(m)


    m = lmer(median_sleepOnset ~ minAge + (1|ID), data = tmp)
    hist(resid(m))
    plot(m)
    qqnorm(resid(m))
    summary(m)
    m = lmer(median_sleepOnset ~ yday(firstEgg) + (1|ID), data = tmp)
    hist(resid(m))
    plot(m)
    qqnorm(resid(m))
    summary(m)
    #no evidence.

    p1 = ggplot(data = xinc, aes(x = yday(firstEgg), y = time_to_sunset_min, group = factor(minAge), colour = factor(minAge))) +
      stat_smooth(method = 'lm', se = FALSE)
    jpeg("plot_sleepOnset_firstEgg_age_Grahametal2017.jpeg", width = 720, height = 720)
    p1
    dev.off()

    p1 = ggplot(data = subset(x3, rel_day >= 300 | rel_day < -30)[c(-987, -(26:42))], aes(x = yday(firstEgg), y = time_to_sunset_min, group = factor(minAge), colour = factor(minAge))) +
      stat_smooth(method = 'lm', se = FALSE)
    jpeg("plot_sleepOnset_firstEgg_age_winter_Grahametal2017.jpeg", width = 720, height = 720)
    p1
    dev.off()


  }


  return(list(x, x2))
 }



