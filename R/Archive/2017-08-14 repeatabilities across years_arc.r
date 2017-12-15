bla = function(){
#repeatabilities across years
tmp = subset(x, select = c("ID", "year_", "sex"))
tmp = unique(tmp, by = names(tmp))
setkey(tmp, ID, year_)
tmp[, next_year := shift(year_), by = ID]
tmp = na.omit(tmp)
tmp[, next_year := year_ - next_year]
table(tmp$sex, tmp$next_year)
#    1   2   4
#1 111  10   1
#2 141   8   0
tmp = subset(tmp, next_year == 1)
tmp = subset(tmp, select = c('ID' , 'year_'))
tmp1 = copy(tmp)
tmp1[ , year_ := year_ - 1]
tmp = rbind(tmp, tmp1)
tmp = unique(tmp, by = names(tmp))

tmp = subset(x, paste(ID, year_) %in% paste(tmp$ID, tmp$year_))

runthrough = subset(tmp, select = c('sex', 'breeding_stage'))
runthrough = na.omit(unique(runthrough, by = names(runthrough)))
require(rptR)
RR = apply(runthrough, MARGIN = 1, FUN = function(a, tmp){
  dd = subset(tmp, sex == a[1] & breeding_stage == a[2])
  ddtmp = subset(dd, select = c('ID', 'year_'))
  ddtmp = unique(ddtmp, by = names(ddtmp))
  dd = subset(dd, ID %in% ddtmp$ID[which(duplicated(ddtmp$ID))])
  if(a[2] == "preLaying") {
    tt = try((R = rpt(time_to_sunrise_min ~ rel_day + (1|breedingAttempt) + (1|ID), grname = c("ID", "breedingAttempt"), datatype = "Gaussian", data = dd, nboot = 200, npermut = 0)), silent = TRUE)
  } else {
    tt = try((R = rpt(time_to_sunrise_min ~ poly(rel_day,2) + (1|breedingAttempt) + (1|ID), grname = c("ID", "breedingAttempt"), datatype = "Gaussian", data = dd, nboot = 200, npermut = 0)), silent = TRUE)
  }
  if(class(tt) != "try-error") {
    return(c(IDs = unname(summary(R)$R[1]), IDlow = unname(summary(R)$CI_emp[1,1]), IDup = unname(summary(R)$CI_emp[1,2]), nest = unname(summary(R)$R[2]), nestlow = unname(summary(R)$CI_emp[2,1]), nestup = unname(summary(R)$CI_emp[2,2]), sex = unname(a[1]), breeding_stage = unname(a[2]), summary(R)$ngroups, N = nrow(dd))) } else {
      return(list(IDs = NA, IDlow = NA, IDup = NA, nest = NA, nestlow = NA, nestup = NA, sex = unname(a[1]), breeding_stage = unname(a[2]), ID = NA, breedingAttempt = NA, N = NA))
    }
}, tmp)


RR = rbindlist(RR)
RR[breeding_stage == 'nonBreeding', sorting := 1]
RR[breeding_stage == 'territoryEstablishment', sorting := 2]
RR[breeding_stage == 'nestBuilding', sorting := 3]
RR[breeding_stage == 'nestCompleted', sorting := 4]
RR[breeding_stage == 'preLaying', sorting := 5]
RR[breeding_stage == 'laying', sorting := 6]
RR[breeding_stage == 'incubation', sorting := 7]
RR[breeding_stage == 'earlyProvisioning', sorting := 8]
RR[breeding_stage == 'lateProvisioning', sorting := 9]
RR[breeding_stage == 'postFledging', sorting := 10]
RR[sex == 1 & ID > 15, COLOUR := "red"]
RR[sex == 1 & ID <= 15, COLOUR := "pink"]
RR[sex == 2 & ID > 15, COLOUR := "blue"]
RR[sex == 2 & ID <= 15, COLOUR := "light blue"]
setkey(RR, sorting, sex)


par(mar = c(11, 4.1, 4.1, 2.1))
plot(c(0,21), c(0, 1), type = 'n', xlab = '', xaxt = 'n', ylab = "Repeatability")
points((1:20)-0.1, RR$IDs, col = RR$COLOUR)
arrows((1:20)-0.1, RR$IDlow,(1:20)-0.1, RR$IDup, length = 0.05, angle = 90, code = 3, col = RR$COLOUR, lty = 2)
points((1:20)+0.1, RR$nest, col = RR$COLOUR, pch = 16)
arrows((1:20)+0.1, RR$nestlow, (1:20)+0.1, RR$nestup, length = 0.05, angle = 90, code = 3, col = RR$COLOUR, lwd = 2)
axis(1, line = 2, col = 'white', at = (1:10)*2-0.5, labels = unique(RR$breeding_stage), las = 2)
axis(1, at = 1:20, padj = 0.5, labels = paste(RR$N, RR$ID, RR$breedingAttempt, sep = '\n'), las = 1, cex.axis =0.7)
axis(1, at = 0, padj = 0.5, labels = "N:\nIDs:\nnests:", cex.axis = 0.7)
abline(v = (1:11)*2-1.5, lty = 3, col = 'grey')
abline(h = (1:5)*0.2, lty = 3, col = 'grey')
par(mar = c(5.1, 4.1, 4.1, 2.1))

#males all data
m0 = lmer(time_to_sunrise_min ~ breeding_stage + (1|breedingAttempt) + (1|ID) + (1|year_), data = subset(tmp, sex == 1))
plot(m0)
qqnorm(residuals(m0))
hist(resid(m0), 100)
hist(tmp[sex == 1, median_time_to_sunrise_min], 100)
m = lmer(time_to_sunrise_min ~ breeding_stage + (1|ID/breedingAttempt) + (1|year_), data = subset(tmp, sex == 1 & time_to_sunrise_min < 25))
plot(m)
hist(resid(m))
qqnorm(residuals(m))
vc = VarCorr(m)
residual_var = attr(vc,'sc')^2
ID_var = attr(vc$ID,'stddev')[1]^2
year_var = attr(vc$year_,'stddev')[1]^2
nest_var = attr(vc$breedingAttempt,'stddev')[1]^2

#compute the unadjusted repeatability
R = ID_var/(ID_var+residual_var+year_var+nest_var)
Rn = nest_var/(ID_var+residual_var+year_var+nest_var)
R #0.14
Rn #0.27

summary(m) #N = 188/78/6
}
