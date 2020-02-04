#load data
data("BTemergenceData")
#note that the models have to be run first, and they take long to compute --> run them once and save them in a convenient location!
#set working directory to the folder you need.
setwd("/ds/grpkempenaers/Lotte/R Studio Projects/Data for package BTemergence/figures")
require(data.table)
LAS = 1
MALE_COLOUR = "#5000FFFF"
FEMALE_COLOUR = "#FF758AFF"
ADULT_COLOUR = "#000033FF"
YEARLING_COLOUR = "#FF9C63FF"

#Figure 1-3: Seasonal patterns split up by sex, variable, and day(te) ####
#repeatabilities:
#order:
#sunrise+ YDAY    + male    : 0.67
#sunset + YDAY    + male    : 0.79
#sunrise+ YDAY    + female  : 0.46
#sunset + YDAY    + female  : 0.37
#sunrise+ rel_day + male    : 0.49
#sunset + rel_day + male    : 0.58
#sunrise+ rel_day + female  : 0.39
#sunset + rel_day + female  : 0.29
###datasets: ####
###date_ dataset
{
  dd0 = subset(x2, yday(date_) <  90 | yday(date_) > 274, select = c('time_to_sunrise_min', 'time_to_sunset_min', 'date_', 'sex', 'YDAY'))

  runthrough = data.table(expand.grid(sex = c(1,2), var = c('time_to_sunrise_min', 'time_to_sunset_min')))
  D = list()
  OUT = list()
  for(j in 1 : nrow(runthrough)) {
    dd = subset(dd0, sex == runthrough[j, sex])
    dd[, var := dd[, which(names(dd) == runthrough[j,var]), with = FALSE]]
    if(runthrough[j,var] == "time_to_sunset_min") dd[, YDAY := YDAY - 1] #because evening entry is the day before morning exit
    d = data.table(YDAY = sort(unique(dd[, YDAY])))
    out = data.table(YDAY = numeric(), out = numeric())
    for(i in 1 : nrow(d)) {
      d[i, med := boxplot.stats(dd[YDAY == d[i, YDAY], var])$stats[3]]
      d[i, box_min := boxplot.stats(dd[YDAY == d[i, YDAY], var])$stats[2]]
      d[i, box_max := boxplot.stats(dd[YDAY == d[i, YDAY], var])$stats[4]]
      d[i, w_min := boxplot.stats(dd[YDAY == d[i, YDAY], var])$stats[1]]
      d[i, w_max := boxplot.stats(dd[YDAY == d[i, YDAY], var])$stats[5]]
      d[i, N := boxplot.stats(dd[YDAY == d[i, YDAY], var])$n]
      tmp = boxplot.stats(dd[YDAY == d[i, YDAY], var])$out
      if(length(tmp) > 0) { tmp = data.table(YDAY = rep(d[i, YDAY], length(tmp)), out = tmp); out = rbind(out, tmp) }
    }
    d[, sex := runthrough[j,sex]]
    d[, var := runthrough[j,var]]
    out[, sex := runthrough[j,sex]]
    out[, var := runthrough[j,var]]
    D[[length(D)+1]] = d
    OUT[[length(OUT)+1]] = out

  }
}
###rel_day dataset
{
  dd0 = subset(x2, rel_day >= -21, select = c('time_to_sunrise_min', 'time_to_sunset_min', 'rel_day', 'sex'))

  runthrough = data.table(expand.grid(sex = c(1,2), var = c('time_to_sunrise_min', 'time_to_sunset_min')))
  DR = list()
  OUTR = list()
  for(j in 1 : nrow(runthrough)) {
    dd = subset(dd0, sex == runthrough[j, sex])
    dd[, var := dd[, which(names(dd) == runthrough[j,var]), with = FALSE]]
    if(runthrough[j,var] == "time_to_sunset_min") dd[, rel_day := rel_day - 1] #because evening entry is the day before morning exit
    d = data.table(rel_day = sort(unique(dd[, rel_day])))
    out = data.table(rel_day = numeric(), out = numeric())
    for(i in 1 : nrow(d)) {
      d[i, med := boxplot.stats(dd[rel_day == d[i, rel_day], var])$stats[3]]
      d[i, box_min := boxplot.stats(dd[rel_day == d[i, rel_day], var])$stats[2]]
      d[i, box_max := boxplot.stats(dd[rel_day == d[i, rel_day], var])$stats[4]]
      d[i, w_min := boxplot.stats(dd[rel_day == d[i, rel_day], var])$stats[1]]
      d[i, w_max := boxplot.stats(dd[rel_day == d[i, rel_day], var])$stats[5]]
      d[i, N := boxplot.stats(dd[rel_day == d[i, rel_day], var])$n]
      tmp = boxplot.stats(dd[rel_day == d[i, rel_day], var])$out
      if(length(tmp) > 0) { tmp = data.table(rel_day = rep(d[i, rel_day], length(tmp)), out = tmp); out = rbind(out, tmp) }
    }
    d[, sex := runthrough[j,sex]]
    d[, var := runthrough[j,var]]
    out[, sex := runthrough[j,sex]]
    out[, var := runthrough[j,var]]
    setnames(d, 'rel_day', 'YDAY')
    setnames(out, 'rel_day', 'YDAY')
    DR[[length(DR)+1]] = d
    OUTR[[length(OUTR)+1]] = out

  }
}
#load model var_day.sex
###Figure 1####
{pdf(file = paste0(getwd(), "/F1_Dawn behaviour.pdf"), width = 30, height = 30, onefile = TRUE, paper = "special")
plot_descriptive_figure(modellist_yday = list(var_day.sex[[1]], var_day.sex[[3]]),
                        modellist_rel_day = list(var_day.sex[[2]], var_day.sex[[4]]),
                        d_list = list(D[[1]], DR[[1]], D[[2]], DR[[2]]),
                        out_list = list(OUT[[1]], OUTR[[1]], OUT[[2]], OUTR[[2]]),
                        YLAB = "Start of activity",
                        VAR = "time_to_sunrise_min",
                        ylim_boxplot = c(-60, 70), ylim_gammplot = c(-40, 20))
mtext('R = 0.67', adj = 0, side = 3, line = 132, at = -47, cex = 3.4, las = 1)
mtext('R = 0.49', adj = 0, side = 3, line = 132, at = 40, cex = 3.4, las = 1)
mtext('R = 0.46', adj = 0, side = 3, line = 62, at = -47, cex = 3.4, las = 1)
mtext('R = 0.39', adj = 0, side = 3, line = 62, at = 40, cex = 3.4, las = 1)
dev.off()
}


  ###Figure 2####
{pdf(file = paste0(getwd(), "/F2_Dusk behaviour.pdf"), width = 30, height = 30, onefile = TRUE, paper = "special")
plot_descriptive_figure(modellist_yday = list(var_day.sex[[5]], var_day.sex[[7]]),
                        modellist_rel_day = list(var_day.sex[[6]], var_day.sex[[8]]),
                        d_list = list(D[[3]], DR[[3]], D[[4]], DR[[4]]),
                        out_list = list(OUT[[3]], OUTR[[3]], OUT[[4]], OUTR[[4]]),
                        YLAB = "End of activity",
                        VAR = "time_to_sunset_min",
                        ylim_boxplot = c(-150, 30), ylim_gammplot = c(-65, 10))
mtext('Males', adj = 0, side = 3, line = 80, at = -130, cex = 3.4, las = 1)
mtext('Females', adj = 0, side = 3, line = 10, at = -130, cex = 3.4, las = 1)
mtext('R = 0.79', adj = 0, side = 3, line = 80, at = -47, cex = 3.4, las = 1)
mtext('R = 0.58', adj = 0, side = 3, line = 80, at = 40, cex = 3.4, las = 1)
mtext('R = 0.37', adj = 0, side = 3, line = 10, at = -47, cex = 3.4, las = 1)
mtext('R = 0.29', adj = 0, side = 3, line = 10, at = 40, cex = 3.4, las = 1)

dev.off()
}
###Figure 3####
pdf(file = paste0(getwd(), "/F3_Day behaviour.pdf"), width = 15, height = 4.5, onefile = TRUE, paper = "special")

#data actual sunlight
actYDAY = dcast(subset(x2, YDAY < 90), YDAY ~ ., fun = mean, value.var = "actualDaylength")
setnames(actYDAY, '.', "actualDaylength")
actRELDAY = dcast(subset(x2, rel_day >= -21), rel_day ~ ., fun = mean, value.var = "actualDaylength")
setnames(actRELDAY, '.', "actualDaylength")

cex.stage = 1.5
par(cex.axis=4/3, cex.lab=5/3, lwd = 2, las = 2, mgp = c(3.5,1.3,0))
line.stage = -1

layout(matrix(1:2, ncol = 2, byrow = TRUE), widths = c(0.55, 0.45))
par(las = 2)
par(mar = c(5.1, 6.1, 2.1, 0.1))
create_gamm_plot(var = "daylength", data = subset(x2, YDAY < 90), modelset = list(var_day.sex[[9]], var_day.sex[[11]]) , ylab = list("Hours of activity", cex = 2), COL = c(MALE_COLOUR, FEMALE_COLOUR), xlim = c(-97, 90), xlab = "Date (1 = 1 January)", ylim = c(8, 16), se = 1.96)
axis(1, las = 0)
lines(x = actYDAY[, YDAY], y = actYDAY[, actualDaylength])
axis(1, at = c(-10:10)*10, labels = NA, tcl = 0.5)
mtext(text = c('O', 'N', 'D', 'J', 'F', 'M'), side = 3, line = -2, at = c(-77.5, -46, -15, 16, 46, 76.5), las = 0, cex = par("cex.lab"))


par(mar = c(5.1, 0.1, 2.1, 6.1))
create_gamm_plot(var = "daylength", data = subset(x2, rel_day > -21), modelset = list(var_day.sex[[10]], var_day.sex[[12]]) , ylab = "", COL = c(MALE_COLOUR, FEMALE_COLOUR), xlim = c(-21, 60), xlab = "Days to first egg", yaxt = 'n', ylim = c(8, 16), se = 1.96)

axis(1, las = 0)
axis(3, at = c(0, 10, 24, 44), labels = rep('', 4), tcl = -2)
axis(3, at = c(0, 10, 24, 44), labels = rep('', 4), tcl = 0.5)
axis(3, at = c(-10, 5, 17, 34, 55), labels = c('Nest', "Egg", "Inc.", "Young", "Post-Br."), las = 0, tick = FALSE, line = line.stage, cex.axis = cex.stage)
lines(x = actRELDAY[, rel_day], y = actRELDAY[, actualDaylength])
axis(1, at = c(-4:17)*5, labels = NA, tcl = 0.5)

dev.off()









#Figure 4: Seasonal patterns and sex and age ####
#load model var_day.sex.age_F.RData
dat1 = data.table(coefficients(summary(unlist(var_day.sex.age_F, recursive = FALSE)[[1]]$mer))[1:4,])
dat1[, sex := c(1,1,2,2)]; dat1[, age := c(1,2,1,2)]; dat1[, var := "Times to sunrise"];  dat1[, dayvar := "YDAY"]
dat2 = data.table(coefficients(summary(unlist(var_day.sex.age_F, recursive = FALSE)[[2]]$mer))[1:4,])
dat2[, sex := c(1,1,2,2)]; dat2[, age := c(1,2,1,2)]; dat2[, var := "Times to sunset"];  dat2[, dayvar := "YDAY"]
dat3 = data.table(coefficients(summary(unlist(var_day.sex.age_F, recursive = FALSE)[[3]]$mer))[1:4,])
dat3[, sex := c(1,1,2,2)]; dat3[, age := c(1,2,1,2)]; dat3[, var := "Times to sunrise"];  dat3[, dayvar := "rel_day"]
dat4 = data.table(coefficients(summary(unlist(var_day.sex.age_F, recursive = FALSE)[[4]]$mer))[1:4,])
dat4[, sex := c(1,1,2,2)]; dat4[, age := c(1,2,1,2)]; dat4[, var := "Times to sunset"];  dat4[, dayvar := "rel_day"]

datFig = rbind.data.frame(dat1, dat2, dat3, dat4)
setnames(datFig, names(datFig), c('Est', 'SE', 't', 'sex', 'age', 'var', 'dayvar'))
datFig[, lowerLSD2 := Est - (1.96*SE/2)]
datFig[, upperLSD2 := Est + (1.96*SE/2)]
datFig[, YY := rep(1:4, 4)]
datFig[, COL_POINT := ifelse(sex == 1, MALE_COLOUR, FEMALE_COLOUR)]
datFig[, COL_LINE := ifelse(sex == 1, MALE_COLOUR, FEMALE_COLOUR)]
datFig[, PCH := rep(c(21,16,24,17),4)]
setkey(datFig, var, dayvar, YY)

{
pdf(file = paste0(getwd(), "/F4_Sex and age.pdf"), width = 30, height = 30, onefile = TRUE, paper = "special")
YLIM = c(0.5, 4.5)

par(mfrow = c(2,2))
par(las = 0)
par(cex = 3, cex.lab = 3, lwd = 6)
plot_labels = c("(A)", "(B)", "(C)", "(D)")

for(i in c(0, 4, 8, 12)) {
  j = 1:4 + i
  par(mgp = c(1.8, 0.8,0))
  if(i == 0) par(mar = c(3.5, 2.1, 1.5, 1.1))
  if(i == 4) par(mar = c(3.5, 1.1, 1.5, 2.1))
  if(i == 8) par(mar = c(4, 2.1, 1, 1.1))
  if(i == 12) par(mar = c(4, 1.1, 1, 2.1))
  XLIM = c(min(datFig[j, lowerLSD2])-0.1, max(datFig[j, upperLSD2])+0.1)
  XLAB = "Estimate±CI"
  plot(1:2, 1:2, type = 'n', xlim = XLIM, ylim = YLIM, yaxt = 'n', ylab = '', xlab = list('', cex = 1.2))
  arrows(rep(datFig[j,Est],2), rep(datFig[j,YY],2), c(datFig[j,lowerLSD2], datFig[j,upperLSD2]), rep(datFig[j, YY],2), length = 0.15, angle = 90, col = datFig[j, COL_LINE])
  points(datFig[j,Est], datFig[j,YY], col = datFig[j, COL_POINT], pch = datFig[j, PCH], cex = 1.4, bg = 'white')
  if(i == 0) legend(-23.5, 4.2, pch = c(17, 24, 16, 21), pt.bg = 'white', legend = c("adult females", "yearling females", "adult males", "yearling males"), bty = 'n', col = c(FEMALE_COLOUR, FEMALE_COLOUR, MALE_COLOUR, MALE_COLOUR))
  text(XLIM[1]-0.1, 4.4, plot_labels[(i/4)+1], adj = 0, cex = 1.4)
  axis(1, at = -100:100, labels = NA, tcl = -0.3)
}

mtext("Non-Breeding", outer = TRUE, side=3, line = -1, at = 0.25, cex = 3.5, adj = 0.5)
mtext("Breeding", outer = TRUE, side=3, line = -1, at = 0.75, cex = 3.5, adj = 0.5)
mtext("Start of activity (Estimate±CI)", outer = TRUE, side = 3, line = -24.5, cex = 3.5, adj = 0.5, at = 0.5)
mtext("End of activity (Estimate±CI)", outer = TRUE, side = 3, line = -49, cex = 3.5, adj = 0.5, at = 0.5)
dev.off()
}





#Figure 5: Effects of the environment ####
#load models z_var_day.sex.env_females_F and z_var_day.sex.env_males_F
df = data.table(expand.grid(env = c("precipitation_dawn", "rainfall", "temperature"), variable = c("time_to_sunrise_min", "time_to_sunset_min"), varday = c("YDAY", "rel_day")))

models = unlist(z_var_day.sex.env_males, recursive = FALSE)

df[, est := unlist(lapply( models, FUN = function(x) { summary(x$gam)$p.table[1,1]}))]
df[, se := unlist(lapply( models, FUN = function(x) { summary(x$gam)$p.table[1,2]}))]
df[, sex := 1]


models = unlist(z_var_day.sex.env_females, recursive = FALSE)

df2 = data.table(expand.grid(env = c("precipitation_dawn", "rainfall", "temperature"), variable = c("time_to_sunrise_min", "time_to_sunset_min"), varday = c("YDAY", "rel_day")))
df2[, est := unlist(lapply( models, FUN = function(x) { summary(x$gam)$p.table[1,1]}))]
df2[, se := unlist(lapply( models, FUN = function(x) { summary(x$gam)$p.table[1,2]}))]
df2[, sex := 2]
df = rbind(df, df2)
df = subset(df, env != "rainfall")

df[, low := est - 1.96*se]
df[, up := est + 1.96*se]
setkey(df, variable, env, sex)
df[, YY := as.numeric(rep(rep(c(1:2), each = 4), 2))]
df[sex == 1, YY := YY-0.2]
df[sex == 2, YY := YY+0.2]
df[sex == 1, COL := MALE_COLOUR]
df[sex == 2, COL := FEMALE_COLOUR]

df[, SHIFT := ifelse(varday == "YDAY", 0.1, -0.1)]
df[, YY := YY + SHIFT]
df[sex == 1, PCH := ifelse(varday == "YDAY", 16,21)]
df[sex == 2, PCH := ifelse(varday == "YDAY", 17,24)]
{
pdf(file = paste0(getwd(), "/F5_Environment.pdf"), width = 30, height = 30, onefile = TRUE, paper = "special")
layout(c(1,2), heights = c(1,1))

par(cex = 3.5, cex.axis = 1, cex.lab = 1, lwd = 4, mgp = c(2,1,0))
par(mar = c(4.1, 8.1, 0.1, 0.1))
plot_env(df = df, VARIABLE = "time_to_sunrise_min", xlim = c(-0.15, 0.25), ylim = c(0.5, 2.5), xlab = "Start of activity (Estimate±CI)")
axis(1, at = c(-10:10)/40, labels = NA, tcl = -0.3)
legend("topright", c("non-breeding female", "breeding female", "non-breeding male", "breeding male"), lwd = 2, col = rep(c(FEMALE_COLOUR, MALE_COLOUR), each  = 2), pch = c(17, 24, 16, 21), bty = 'n', pt.bg = 'white')
par(mar = c(4.1, 8.1, 0.1, 0.1))
plot_env(df= df, VARIABLE = "time_to_sunset_min", xlim = c(-0.15, 0.25), ylim = c(0.5, 2.5), xlab = "End of activity (Estimate±CI)")
axis(1, at = c(-10:10)/40, labels = NA, tcl = -0.3)

dev.off()
}


#Figure S1: Environment ####
#plot environment over the years
ENV = subset(x2, select = c("date_", 'YDAY', 'avgTemperature', 'precipitation_dawn', 'precipitation_dusk'))
ENV = unique(ENV)
ENV = dcast(ENV, YDAY ~ ., value.var = c("avgTemperature", "precipitation_dawn", "precipitation_dusk"), fun = list(mean, min, max, median), na.rm = TRUE)

{
jpeg(file = paste0(getwd(), "/S1_Rainfall and temperature.jpg"), width = 1800, height = 1800)
YLIM = c(-16, 27)
par(cex.lab = 6, cex.axis = 4, mgp = c(10,4,0), las = LAS, lwd = 4, tcl = -2)
par(mfrow = c(3,1))

par(mar = c(0.1,16.9,12.9,0.5))
plot(x = ENV[, YDAY], y = ENV[, avgTemperature_mean], type = 'l', xlim = c(-97, 150), xaxt = "n", xlab = '', ylab = "", yaxt = 'n', ylim = YLIM)
polygon(c(ENV[,YDAY], rev(ENV[,YDAY])), c(ENV[,avgTemperature_min], rev(ENV[,avgTemperature_max])), col = "light grey", border = NA)
par(new = TRUE)
plot(x = ENV[, YDAY], y = ENV[, avgTemperature_median], type = 'l', xlim = c(-97, 150), xaxt = "n", xlab = '', ylab = "", yaxt = 'n', lty = 1, ylim = YLIM, col = 'red', lwd = 4)
par(new = TRUE)
plot(x = ENV[, YDAY], y = ENV[, avgTemperature_mean], type = 'l', xlim = c(-97, 150), xaxt = "n", xlab = '', ylab = "Temperature (°C)", yaxt = 'n', ylim = YLIM, lwd = 6)
text(-100, 25, "(A)", cex = 5)
axis(1, at = (-10:15)*10, labels = FALSE)

axis(1, labels = FALSE, lwd = 5)
axis(2, lwd = 5)

YLIM = c(0, 8.5)
par(mar = c(6.5,16.9,6.5,0.5))
plot(x = ENV[, YDAY], y = ENV[, precipitation_dawn_mean], type = 'l', xlim = c(-97, 150), xaxt = 'n', ylab = "", xlab = '', yaxt = 'n', ylim = YLIM)
polygon(c(ENV[,YDAY], rev(ENV[,YDAY])), c(ENV[,precipitation_dawn_min], rev(ENV[,precipitation_dawn_max])), col = "light grey", border = NA)
par(new = TRUE)
plot(x = ENV[, YDAY], y = ENV[, precipitation_dawn_median], type = 'l', xlim = c(-97, 150), xaxt = 'n', ylab = "", xlab = '', yaxt = 'n', lty = 1, ylim = YLIM, col = 'red', lwd = 4)
par(new = TRUE)
plot(x = ENV[, YDAY], y = ENV[, precipitation_dawn_mean], type = 'l', xlim = c(-97, 150), xaxt = 'n', ylab = "Rain at dawn (mm/h)", xlab = '', yaxt = 'n', ylim = YLIM, lwd = 6)
axis(1, labels = FALSE, lwd = 5)
axis(2, lwd = 5)
text(-100, 8, "(B)", cex = 5)
axis(1, at = (-10:15)*10, labels = FALSE)

par(mar = c(12.9,16.9,0.1, 0.5))
plot(x = ENV[, YDAY], y = ENV[, precipitation_dusk_mean], type = 'l', xlim = c(-97, 150), ylab = "", xlab = '', xaxt = 'n', yaxt = 'n', ylim = YLIM)
polygon(c(ENV[,YDAY], rev(ENV[,YDAY])), c(ENV[,precipitation_dusk_min], rev(ENV[,precipitation_dusk_max])), col = "light grey", border = NA)
par(new = TRUE)
plot(x = ENV[, YDAY], y = ENV[, precipitation_dusk_median], type = 'l', xlim = c(-97, 150), ylab = "", xlab = '', xaxt = 'n', yaxt = 'n', lty = 1, ylim = YLIM, col = 'red', lwd = 4)
par(new = TRUE)
plot(x = ENV[, YDAY], y = ENV[, precipitation_dusk_mean], type = 'l', xlim = c(-97, 150), ylab = "Rain at dusk (mm/h)", xlab = 'Date (1=1st Jan)', xaxt = 'n', yaxt = 'n', ylim = YLIM, lwd = 6)
text(-100, 8, "(C)", cex = 5)
axis(1, lwd = 5)
axis(2, lwd = 5)
axis(1, at = (-10:15)*10, labels = FALSE)
dev.off()
}



#Figure S2: sample sizes YDAY ####
{
jpeg(file = paste0(getwd(), "/S2_Sample sizes_YDAY.jpg"), width = 480, height = 480)

ss = subset(x2, YDAY < 90, select = c('ID', 'breeding_season', 'sex', 'age', 'YDAY'))
ym = table(ss[age == 1 & sex == 1,YDAY])
am = table(ss[age == 2 & sex == 1,YDAY])
yf = table(ss[age == 1 & sex == 2,YDAY])
af = table(ss[age == 2 & sex == 2,YDAY])

YLIM = c(0, max(c(ym, am, yf, af)))
XLIM = c(-100, 90)
par(mfrow = c(2,2))
par(las = 1)
  par(mar = c(0.1, 4.1, 5.1, 0.1))
  plot(ym, type = "h", ylim = YLIM, xlim = XLIM, xaxt = 'n', ylab = list("Males", cex= 1.3), col = MALE_COLOUR)
  mtext(text = 'Yearlings', line = 1, cex = 1)

  par(mar = c(0.1, 0.1, 5.1, 4.1))
  plot(am, type = "h", ylim = YLIM, xlim = XLIM, xaxt = 'n', yaxt = 'n', col = MALE_COLOUR)
  mtext(text = 'Adults', line = 1, cex = 1)


  par(mar = c(5.1, 4.1, 0.1, 0.1))
  plot(yf, type = "h", ylim = YLIM, xlim = XLIM, xaxt = 'n', ylab = list("Females", cex = 1.3), col = FEMALE_COLOUR, xlab = list("", cex = 1.2))
  axis(1, at = c(-200, -150, -100, -50, 0, 50), labels = c(-200, -150, -100, -50, 0, 50))
  par(mar = c(5.1, 0.1, 0.1, 4.1))
  plot(af, type = "h", ylim = YLIM, xlim = XLIM, xaxt = 'n', yaxt = 'n', col = FEMALE_COLOUR, xlab = list("", cex = 1.2))
  axis(1, at = c(-200, -150, -100, -50, 0, 50), labels = c(-200, -150, -100, -50, 0, 50))
  mtext("Date (1 = 1st Jan)", side = 1, line = -2, outer = TRUE, adj = 0.5, at = 0.5, cex = 1)
dev.off()
}
#Figure S3: sample sizes rel_day####
{

jpeg(file = paste0(getwd(), "/S3_Sample sizes_rel_day.jpg"), width = 480, height = 480)

  ss = unique(subset(x2, rel_day > -21, select = c('ID', 'sex', 'age', 'rel_day')))
  ym = table(ss[age == 1 & sex == 1,rel_day])
  am = table(ss[age == 2 & sex == 1,rel_day])
  yf = table(ss[age == 1 & sex == 2,rel_day])
  af = table(ss[age == 2 & sex == 2,rel_day])

  YLIM = c(0, max(c(ym, am, yf, af)))
  XLIM = c(-21, 50)
  par(mfrow = c(2,2))
  par(las = 1)
  par(mar = c(0.1, 4.1, 5.1, 0.1))
  plot(ym, type = "h", ylim = YLIM, xlim = XLIM, xaxt = 'n', ylab = list("Males", cex= 1.3), col = MALE_COLOUR)
  mtext(text = 'Yearlings', line = 1, cex = 1)

  par(mar = c(0.1, 0.1, 5.1, 4.1))
  plot(am, type = "h", ylim = YLIM, xlim = XLIM, xaxt = 'n', yaxt = 'n', col = MALE_COLOUR)
  mtext(text = 'Adults', line = 1, cex = 1)


  par(mar = c(5.1, 4.1, 0.1, 0.1))
  plot(yf, type = "h", ylim = YLIM, xlim = XLIM, xaxt = 'n', ylab = list("Females", cex = 1.3), col = FEMALE_COLOUR, xlab = list("", cex = 1.2))
  axis(1, at = c(-10:10)*10, labels = c(-10:10)*10)
  par(mar = c(5.1, 0.1, 0.1, 4.1))
  plot(af, type = "h", ylim = YLIM, xlim = XLIM, xaxt = 'n', yaxt = 'n', col = FEMALE_COLOUR, xlab = list("", cex = 1.2))
  axis(1, at = c(-10:10)*10, labels = c(-10:10)*10)
  mtext("Days to first egg", side = 1, line = -2, outer = TRUE, adj = 0.5, at = 0.5, cex = 1)

  dev.off()
}

#Figure S4-5: Examples ####
#b. prepare data set: fetch individuals with most data points around day -50 to 50 and data on days -2, -1, and 0######
year_ID = subset(x2, rel_day %in% -2:0, select = c('breeding_season', 'ID', 'sex', 'age', 'yid'))
year_ID[, count := length(sex), by = list(ID, breeding_season)]
year_ID = unique(year_ID)
year_ID = subset(year_ID, count == 3)


year_ID = subset(x2, rel_day >= -50 & rel_day < 50 & yid %in% year_ID[,yid], select = c('breeding_season', 'ID', 'sex', 'age'))
year_ID[, count := length(sex), by = list(ID, breeding_season)]
year_ID = unique(year_ID)
setkey(year_ID, count)
#take the yearling male, yearling female, adult male, and adult female with the most data points
year_ID[, take := max(count), by = list(sex, age)]
year_ID = subset(year_ID, take == count)

#c. plot morning ######
{
jpeg(file = paste0(getwd(), "/S4_Examples_morning.jpg"), width = 1800, height = 1800)


par(mfrow = c(4,1))
par(cex.axis=4, cex.lab=5, lwd = 2, las = 2, mgp = c(8,4,0), las = LAS, cex = 1, tcl = -1.8)
XLAB = list("Days to first egg")
XLIM = c(-200, 50)
YLIM = c(-55, 60)
LTY = 3
VAR = "time_to_sunrise_min"
mar.left = 16.1
mar.bottom = 10.1
cex.legend = 3
PLOT.LINES = TRUE

par(mar = c(0.1, mar.left, mar.bottom, 0.2))
plot_individuals(x2, i = 1, year_ID = year_ID, var = VAR, xaxt = 'n', ylab = '', xlab= '', xlim = XLIM, ylim = YLIM, lty = LTY, cex = 2, plot.lines = PLOT.LINES)
legend(-218, 70, "yearling male (2016)", bty = 'n', cex = cex.legend)

par(mar = c(mar.bottom/3, mar.left, 2*(mar.bottom/3), 0.2))
plot_individuals(x2, i = 2, year_ID = year_ID, var = VAR, xaxt = 'n', ylab = '', xlab= '', xlim = XLIM, ylim = YLIM, lty = LTY, cex = 2, plot.lines = PLOT.LINES)
legend(-218, 70, "adult male (2015)", bty = 'n', cex = cex.legend)

par(mar = c(2*(mar.bottom)/3, mar.left, mar.bottom/3, 0.2))
plot_individuals(x2, i = 3, year_ID = year_ID, var = VAR, xaxt = 'n', ylab = '', xlab = '', xlim = XLIM, ylim = YLIM, lty = LTY, cex = 2, plot.lines = PLOT.LINES)
legend(-218, 70, "yearling female (2015)", bty = 'n', cex = cex.legend)

par(mar = c(mar.bottom, mar.left, 0.2, 0.2))
plot_individuals(x2, i = 4, year_ID = year_ID, var = VAR, ylab = '', xlab = XLAB, xlim = XLIM, ylim = YLIM, lty = LTY, cex = 2, plot.lines = PLOT.LINES)
legend(-218, 70, "adult female (2017)", bty = 'n', cex = cex.legend)
mtext("Times to sunrise (minutes)", side = 2, las = 0, line = -5, outer = TRUE, cex = par("cex.lab"))

dev.off()

}

#d. plot evening ######
{
jpeg(file = paste0(getwd(), "/S5_Examples_evening.jpg"), width = 1800, height = 1800)
par(mfrow = c(4,1))
par(cex.axis=4, cex.lab=5, lwd = 2, las = 2, mgp = c(8,4,0), las = LAS, cex = 1, tcl = -1.8, las = LAS)
XLAB = list("Days to first egg")
XLIM = c(-200, 50)
YLIM = c(-92, 20)
LTY = 3
VAR = "time_to_sunset_min"
mar.left = 16.1
mar.bottom = 10.1
cex.legend = 3
PLOT.LINES = TRUE

par(mar = c(0.1, mar.left, mar.bottom, 0.2))
plot_individuals(x2, i = 1, year_ID = year_ID, var = VAR, xaxt = 'n', xlab = '', ylab = '', xlim = XLIM, ylim = YLIM, lty = LTY, cex = 2, plot.lines = PLOT.LINES)
legend(-218, 70, "yearling male (2016)", bty = 'n', cex = cex.legend)

par(mar = c(mar.bottom/3, mar.left, 2*(mar.bottom/3), 0.2))
plot_individuals(x2, i = 2, year_ID = year_ID, var = VAR, xaxt = 'n', xlab = '', ylab = '', xlim = XLIM, ylim = YLIM, lty = LTY, cex = 2, plot.lines = PLOT.LINES)
legend(-218, 70, "adult male (2015)", bty = 'n', cex = cex.legend)

par(mar = c(2*(mar.bottom)/3, mar.left, mar.bottom/3, 0.2))
plot_individuals(x2, i = 3, year_ID = year_ID, var = VAR, xaxt = 'n', xlab = '', ylab = '', xlim = XLIM, ylim = YLIM, lty = LTY, cex = 2, plot.lines = PLOT.LINES)
legend(-218, 70, "yearling female (2015)", bty = 'n', cex = cex.legend)

par(mar = c(mar.bottom, mar.left, 0.2, 0.2))
plot_individuals(x2, i = 4, year_ID = year_ID, var = VAR, xlab = XLAB, ylab = '', xlim = XLIM, ylim = YLIM, lty = LTY, cex = 2, plot.lines = PLOT.LINES)
legend(-218, 70, "adult female (2017)", bty = 'n', cex = cex.legend)

mtext("Times to sunset (minutes)", side = 2, las = 0, line = -5, outer = TRUE, cex = par("cex.lab"))

dev.off()

}



#Figure S6-9: plot estimates across season####################
dat = copy(x2)
dat[, MONTH := month(date_)]
dat = subset(dat, MONTH != 6 & MONTH != 5)
dat[MONTH > 6, MONTH := -12+MONTH]
dat[, STAGE := factor("non", levels = c("non", "pre", "E", "I", "Y"))]
dat[rel_day >= -7 & rel_day < 0, STAGE := "pre"]
dat[rel_day >= 0 & rel_day < clutch, STAGE := "E"]
dat[rel_day >= clutch & date_ < hatchDate, STAGE := "I"]
dat[date_ >= hatchDate & date_ <= fledgeDate, STAGE := "I"]
###YDAY data ################
L = list()
for(i in unique(dat$MONTH)) {
  print(i)
  df = data.table(Est = numeric(), SE = numeric(), t = numeric(), dayvar = character(), envvar = character(), var = character(), sex = numeric())

  tmp = subset(dat, MONTH == i & sex == 1)
  if(nrow(tmp) >= 20 &
     length(na.omit(unique(tmp[, z_precipitation_dusk]))) > 1 &
     length(na.omit(unique(tmp[, z_precipitation_dawn]))) > 1) {
    m = lmer(z_time_to_sunrise_min ~ YDAY + z_precipitation_dawn + (1|yid), data = tmp)
    df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "YDAY", envvar = "z_precipitation_dawn", var = "time_to_sunrise_min", sex = 1))
    m = lmer(z_time_to_sunrise_min ~ YDAY + z_avgTemperature + (1|yid), data = tmp)
    df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "YDAY", envvar = "z_avgTemperature", var = "time_to_sunrise_min", sex = 1))
  }

  tmp = subset(dat, MONTH == i & sex == 2)
  if(nrow(tmp) >= 20 &
     length(na.omit(unique(tmp[, z_precipitation_dusk]))) > 1 &
     length(na.omit(unique(tmp[, z_precipitation_dawn]))) > 1) {
    m = lmer(z_time_to_sunrise_min ~ YDAY + z_precipitation_dawn + (1|yid), data = tmp)
    df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "YDAY", envvar = "z_precipitation_dawn", var = "time_to_sunrise_min", sex = 2))
    m = lmer(z_time_to_sunrise_min ~ YDAY + z_avgTemperature + (1|yid), data = tmp)
    df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "YDAY", envvar = "z_avgTemperature", var = "time_to_sunrise_min", sex = 2))
  }

  tmp = subset(dat, MONTH == i & sex == 1)
  if(nrow(tmp) >= 20 &
     length(na.omit(unique(tmp[, z_precipitation_dusk]))) > 1 &
     length(na.omit(unique(tmp[, z_precipitation_dawn]))) > 1) {
    m = lmer(z_time_to_sunset_min ~ YDAY + z_precipitation_dusk + (1|yid), data = tmp)
    df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "YDAY", envvar = "z_precipitation_dusk", var = "time_to_sunset_min", sex = 1))
    m = lmer(z_time_to_sunset_min ~ YDAY + z_avgTemperature + (1|yid), data = tmp)
    df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "YDAY", envvar = "z_avgTemperature", var = "time_to_sunset_min", sex = 1))
  }

  tmp = subset(dat, MONTH == i & sex == 2)
  if(nrow(tmp) >= 20 &
     length(na.omit(unique(tmp[, z_precipitation_dusk]))) > 1 &
     length(na.omit(unique(tmp[, z_precipitation_dawn]))) > 1) {
    m = lmer(z_time_to_sunset_min ~ YDAY + z_precipitation_dusk + (1|yid), data = tmp)
    df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "YDAY", envvar = "z_precipitation_dusk", var = "time_to_sunset_min", sex = 2))
    m = lmer(z_time_to_sunset_min ~ YDAY + z_avgTemperature + (1|yid), data = tmp)
    df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "YDAY", envvar = "z_avgTemperature", var = "time_to_sunset_min", sex = 2))
  }

  if(nrow(df) > 0) {
    df[, MONTH := i]
    df -> L[[length(L)+1]]
  }
}
LL = rbindlist(L)
LL[, upLSD := Est + 1.96*SE]
LL[, lowLSD := Est - 1.96*SE]

tmp = split(LL, paste(LL[, envvar], LL[, var], LL[ ,sex]))
tmp -> tmp1

###rel_day data ########
L = list()
for(i in levels(dat[, STAGE])[c(-1, -5)]) {
  print(i)
  tmp = subset(dat, STAGE == i & sex == 1)
  m = lmer(z_time_to_sunrise_min ~ rel_day + z_precipitation_dawn + (1|yid), data = tmp)
  df = data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "rel_day", envvar = "z_precipitation_dawn", var = "time_to_sunrise_min", sex = 1)
  m = lmer(z_time_to_sunrise_min ~ rel_day + z_avgTemperature + (1|yid), data = tmp)
  df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "rel_day", envvar = "z_avgTemperature", var = "time_to_sunrise_min", sex = 1))

  tmp = subset(dat, STAGE == i & sex == 2)
  m = lmer(z_time_to_sunrise_min ~ rel_day + z_precipitation_dawn + (1|yid), data = tmp)
  df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "rel_day", envvar = "z_precipitation_dawn", var = "time_to_sunrise_min", sex = 2))
  m = lmer(z_time_to_sunrise_min ~ rel_day + z_avgTemperature + (1|yid), data = tmp)
  df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "rel_day", envvar = "z_avgTemperature", var = "time_to_sunrise_min", sex = 2))



  tmp = subset(dat, STAGE == i & sex == 1)
  m = lmer(z_time_to_sunset_min ~ rel_day + z_precipitation_dusk + (1|yid), data = tmp)
  df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "rel_day", envvar = "z_precipitation_dusk", var = "time_to_sunset_min", sex = 1))
  m = lmer(z_time_to_sunset_min ~ rel_day + z_avgTemperature + (1|yid), data = tmp)
  df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "rel_day", envvar = "z_avgTemperature", var = "time_to_sunset_min", sex = 1))

  tmp = subset(dat, STAGE == i & sex == 2)
  m = lmer(z_time_to_sunset_min ~ rel_day + z_precipitation_dusk + (1|yid), data = tmp)
  df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "rel_day", envvar = "z_precipitation_dusk", var = "time_to_sunset_min", sex = 2))
  m = lmer(z_time_to_sunset_min ~ rel_day + z_avgTemperature + (1|yid), data = tmp)
  df = rbind(df, data.table(Est = summary(m)$coef[3,1], SE = summary(m)$coef[3,2], t = summary(m)$coef[3,3], dayvar = "rel_day", envvar = "z_avgTemperature", var = "time_to_sunset_min", sex = 2))


  df[, STAGE := i]
  df -> L[[length(L)+1]]

}
LL = rbindlist(L)
LL[, upLSD := Est + 1.96*SE]
LL[, lowLSD := Est - 1.96*SE]
tmp = split(LL, paste(LL[, envvar], LL[, var], LL[ ,sex]))
tmp -> tmp2

###plot ########

jpeg(file = paste0(getwd(), "/S6_Temperature and onset of activity across year.jpg"), width = 900, height = 900)
plot_seasonal_progress(tmp1 = list(tmp1[[1]], tmp1[[2]]), tmp2 = list(tmp2[[1]], tmp2[[2]]), YLIM = c(-0.3, 0.2), XLAB = "", YLAB = "Effect of temperature\r\n")
mtext("Start of activity", side = 3, line = -1.5, outer = TRUE, cex = 2.5, adj = 0.6)
dev.off()

jpeg(file = paste0(getwd(), "/S7_Temperature and cessation of activity across year.jpg"), width = 900, height = 900)
plot_seasonal_progress(tmp1 = list(tmp1[[3]], tmp1[[4]]), tmp2 = list(tmp2[[3]], tmp2[[4]]), YLIM = c(-0.05, 0.45), XLAB = "", YLAB = "Effect of temperature\r\n")
mtext("End of activity", side = 3, line = -1.5, outer = TRUE, cex = 2.5, adj = 0.6)
dev.off()


jpeg(file = paste0(getwd(), "/S8_Rainfall and onset of activity across year.jpg"), width = 900, height = 900)
plot_seasonal_progress(tmp1 = list(tmp1[[5]], tmp1[[6]]), tmp2 = list(tmp2[[5]], tmp2[[6]]), YLIM = c(-0.05, 0.45), XLAB = "", YLAB = " Effect of rainfall at dawn\r\n")
mtext("Start of activity", side = 3, line = -1.5, outer = TRUE, cex = 2.5, adj = 0.6)
dev.off()

jpeg(file = paste0(getwd(), "/S9_Rainfall and cessation of activity across year.jpg"), width = 900, height = 900)
plot_seasonal_progress(tmp1 = list(tmp1[[7]], tmp1[[8]]), tmp2 = list(tmp2[[7]], tmp2[[8]]), YLIM = c(-0.3, 0.2), XLAB = "Cessation of activity", YLAB = "Effect of rainfall at dusk\r\n")
mtext("End of activity", side = 3, line = -1.5, outer = TRUE, cex = 2.5, adj = 0.6)
dev.off()





