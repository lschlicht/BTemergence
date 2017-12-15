bla = function(){
require(lme4)
require(MCMCglmm)
require(ggplot2)

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


text_results = function(tx, nvar = 1:3) {
  tx[, 1:4] = round(tx[, 1:4], digits = 2)
  L = lapply(nvar, FUN = function(i) {
    paste0(rownames(tx)[i], ": ", tx[i,1], " [", tx[i,2], ";", tx[i,3], "]; p = ", tx[i,5])
  })
  return(do.call(paste, args = list(L, collapse = '\n')))
  }

SEX = 2
#model sunrise: day -10 to -2: poly 2 pops up; day -2 to 0 is linear
{
  m0 = lmer(time_to_sunrise_min ~ poly(rel_day, 3) + (1|ID), data = subset(x, sex == SEX & rel_day >= -10 & rel_day < -1))
  hist(resid(m0))
  length(which(resid(m0) >= 50)) #18
  m = lmer(time_to_sunrise_min ~ poly(rel_day, 3) + (1|ID), data = subset(x, sex == SEX & rel_day >= -10 & rel_day < -1)[which(resid(m0) < 50)])
  hist(resid(m))
  plot(m)
  qqnorm(residuals(m))
  summary(m)
  mP = MCMCglmm(time_to_sunrise_min ~ poly(rel_day, 3), random = ~ID, data = subset(x, sex == SEX & rel_day >= -10 & rel_day < -1)[which(resid(m0) < 50)])
  summary(m)

  m = MCMCglmm(time_to_sunrise_min ~ rel_day + I(rel_day^2), random = ~ID, data = subset(x, sex == SEX & rel_day >= -10 & rel_day < -1)[which(resid(m0) < 50)])
  summary(m)

  g0 = lmer(time_to_sunrise_min ~ poly(rel_day, 2) + (1|ID), data = subset(x, sex == SEX & rel_day >= -2 & rel_day <= 0))
  hist(resid(g0))
  length(which(resid(g0) >= 50)) #3
  g = lmer(time_to_sunrise_min ~ poly(rel_day, 2) + (1|ID), data = subset(x, sex == SEX & rel_day >= -2 & rel_day <= 0)[which(resid(g0) < 50)])
  hist(resid(g))
  plot(g)
  qqnorm(residuals(g))
  summary(g)
  gP = MCMCglmm(time_to_sunrise_min ~ poly(rel_day, 2), random = ~ID, data = subset(x, sex == SEX & rel_day >= -2 & rel_day <= 0)[which(resid(g0) < 50)])
  summary(g)

  g = MCMCglmm(time_to_sunrise_min ~ rel_day, random = ~ID, data = subset(x, sex == SEX & rel_day >= -2 & rel_day <= 0)[which(resid(g0) < 50)])
  summary(g)

}

#model sunset: day -10 to -2: rel_day doesn't predict; day -2 to 0 is poly 2
{
  sm0 = lmer(time_to_sunset_min ~ poly(rel_day, 3) + (1|ID), data = subset(x, sex == SEX & rel_day >= -10 & rel_day < -1))
  hist(resid(sm0))
  sm = lmer(((time_to_sunset_min+120)^2) ~ poly(rel_day, 3) + (1|ID), data = subset(x, sex == SEX & rel_day >= -10 & rel_day < -1))
  hist(resid(sm))
  plot(sm)
  qqnorm(residuals(sm))
  summary(sm)

  smP = MCMCglmm(((time_to_sunset_min+120)^2) ~ poly(rel_day, 3), random = ~ID, data = subset(x, sex == SEX & rel_day >= -10 & rel_day < -1))
  summary(sm)

  sm = MCMCglmm(time_to_sunset_min ~ 1, random = ~ID, data = subset(x, sex == SEX & rel_day >= -10 & rel_day < -1))
  summary(sm)

  sg0 = lmer(time_to_sunset_min ~ poly(rel_day, 2) + (1|ID), data = subset(x, sex == SEX & rel_day >= -2 & rel_day <= 0))
  hist(resid(sg0))
  plot(sg0)
  qqnorm(residuals(sg0))

  sg = sg0
  summary(sg)

  sgP = MCMCglmm(time_to_sunset_min ~ poly(rel_day, 2), random = ~ID, data = subset(x, sex == SEX & rel_day >= -2 & rel_day <= 0))
  summary(sg)

  sg = MCMCglmm(time_to_sunset_min ~ rel_day + I(rel_day^2), random = ~ID, data = subset(x, sex == SEX & rel_day >= -2 & rel_day <= 0))
  summary(sg)

}
}
bla = function() {
#plot
p1 =  ggplot(subset(x, rel_day >= -10 & rel_day <= 0)[which(resid(m0) < 50)],
       aes(x = rel_day, y = time_to_sunrise_min, group = factor(sex), colour = factor(sex))) +
  geom_point() +
  geom_smooth() +
  stat_function(fun = function(x) {return(summary(m)$solutions[1,1] + summary(m)$solutions[2,1]*x + summary(m)$solutions[3,1] * (x^2))}, xlim = c(-10, -2), colour = 'red', size = 1.5) +
  stat_function(fun = function(x) {return(summary(g)$solutions[1,1] + summary(g)$solutions[2,1]*x)}, xlim = c(-2, 0), colour = 'red', size = 1.5) +
  ylim(-75, 60) +
  annotate(geom="text", x=-8, y=50, label=text_results(summary(mP)$solutions, nvar = 1:4),
           color="black") +
  annotate(geom="text", x=-2, y=50, label=text_results(summary(gP)$solutions),
           color="black")


p2 =  ggplot(subset(x, rel_day >= -10 & rel_day <= 0),
             aes(x = rel_day, y = time_to_sunset_min, group = factor(sex), colour = factor(sex))) +
  geom_point() +
  geom_smooth() +
  stat_function(fun = function(x) {return(summary(sm)$solutions[1,1])}, xlim = c(-10, -2), colour = 'red', size = 1.5) +
  stat_function(fun = function(x) {return(summary(sg)$solutions[1,1] + summary(sg)$solutions[2,1]*x  + summary(sg)$solutions[3,1]*(x^2))}, xlim = c(-2, 0), colour = 'red', size = 1.5) +
  ylim(-70, 20) +
  annotate(geom="text", x=-8, y=15, label=text_results(summary(smP)$solutions, nvar = 1:4),
           color="black") +
  annotate(geom="text", x=-2, y=15, label=text_results(summary(sgP)$solutions),
           color="black")




jpeg(filename = "modelPreLayingDip.jpg", width = 720, height = 720)
multiplot(p2, p1, cols = 1)
dev.off()

#night length for males and females
#total
x[, daylength := as.numeric(difftime(out_, in_, units = 'hours'))]
ggplot(subset(x, rel_day >= -30 & rel_day < 30), aes(x = factor(rel_day), y = daylength)) +
  geom_boxplot(aes(fill = sex)) +
  ylim(8.5, 13)

x[, daylength_to_sun := time_to_sunset_min - time_to_sunrise_min]
#small value = early to bed and/or up late; large value: late to bed and/or up early

#run this first!!!!
m0 = lmer(daylength_to_sun ~ poly(rel_day, 3) + (1|ID), data = subset(x,sex == 1 &  rel_day >= -10 & rel_day <= 10))
hist(resid(m0))
length(which(resid(m0) < -75 | resid(m0) > 75)) #9


m = lmer(daylength_to_sun ~ poly(rel_day,3, raw = TRUE) + (1|ID), data = subset(x, sex == 1 & rel_day >= -10 & rel_day <= 10)[-which(resid(m0) < -75 | resid(m0) > 75)])


f = summary(m)$coefficients[,1]
ff = function(x) return(f[1] + f[2]*x + f[3]*x^2 + f[4]*x^3)


m = lmer(daylength_to_sun ~ rel_day+
           I(rel_day^2) + I(rel_day^3) + (1|ID), data = subset(x, sex == 1 & rel_day >= -10 & rel_day <= 10)[-which(resid(m0) < -75 | resid(m0) > 75)])

f = summary(m)$coefficients[,1]
ff = function(x) return(f[1] + f[2]*x + f[3]*x^2 + f[4]*x^3)
optimize(ff, interval=c(-10, 10), maximum=TRUE)


m = lmer(daylength_to_sun ~ rel_day+
               I(rel_day^2) + I(rel_day^3)
             + I(rel_day^4) + I(rel_day^5)
             + I(rel_day^6) + I(rel_day^7)
             + I(rel_day^8) + I(rel_day^9)
             + I(rel_day^10) + (1|ID), data = subset(x, sex == 2 & rel_day >= -10 & rel_day <= 10)[-which(resid(m0) < -75 | resid(m0) > 75)])

f = summary(m)$coefficients[,1]
ff = function(x) return(f[1] + f[2]*x + f[3]*x^2 + f[4]*x^3 + f[5]*x^4 + f[6]*x^5+f[7]*x^6 + f[8]*x^7 + f[9]*x^8  + f[10]*x^9  + f[11]*x^10 )

optimize(ff, interval=c(-10, 10), maximum=TRUE)


m = lmer(daylength_to_sun ~ rel_day+
               I(rel_day^2) + I(rel_day^3)
             + I(rel_day^4) + I(rel_day^5)
             + I(rel_day^6) + I(rel_day^7)
             + I(rel_day^8) + I(rel_day^9)
             + I(rel_day^10) + I(rel_day^11)
             + I(rel_day^12) + I(rel_day^13)
             + I(rel_day^14) + I(rel_day^15)
             + (1|ID),
         data = subset(x, sex == 2 & rel_day >= -10 & rel_day <= 10)[-which(resid(m0) < -75 | resid(m0) > 75)])

f = summary(m)$coefficients[,1]
ff = function(x) return(f[1] + f[2]*x + f[3]*x^2 + f[4]*x^3 + f[5]*x^4 + f[6]*x^5+f[7]*x^6 + f[8]*x^7 + f[9]*x^8  + f[10]*x^9  + f[11]*x^10 + f[12]*x^11 + f[13]*x^12 + f[14]*x^13 + f[15]*x^14 + f[16]*x^15)
optimize(ff, interval=c(-10, 10), maximum=TRUE)

newdata <- with(subset(x, sex == 2 & rel_day >= -10 & rel_day <= 10)[-which(resid(m0) < -75 | resid(m0) > 75)], expand.grid(rel_day=unique(rel_day), ID=unique(ID)))
newdata$pred = predict(m, newdata = newdata)
newdata = data.table(newdata)
newdata[,med := median(pred), by = rel_day]
newdata[, pred := NULL]
newdata = unique(newdata, by = names(newdata))
newdata[, rel_day := rel_day + 11]


newdata -> newdata_m
newdata -> newdata_f
#females: max at -3.96(p7)/-2.38(p10)/-6.11(p3)/-0.48(p18)
#males: max at -3.26(p7)/-3.32(p10)/-4.34(p3)/-0.68(p18)
jpeg(filename = "modelRelDaylength.jpg", width = 720, height = 720)

ggplot(subset(x, rel_day >= -10 & rel_day < 10), aes(x = rel_day, y = daylength_to_sun)) +
  geom_boxplot(aes(x = factor(rel_day), fill = sex, col = sex)) +
  #geom_point(aes(colour = sex)) +
  geom_vline(xintercept=11, lty = 3) +
  geom_line(data = newdata_m, aes(x = rel_day, y = med)) +
  geom_point(aes(x = -3.77+11, y = 25.7), shape = 8, size = 2)+
  geom_point(aes(x = -2.77+11, y = -1.1), shape = 8, size = 2)+
  geom_line(data = newdata_f, aes(x = rel_day, y = med)) +
  ylim(-125, 55)
dev.off()


#nest completion and emergence
m0 = lmer(time_to_sunrise_min ~ rel_day+breeding_stage + (1|ID), data = subset(x, sex == 2 & rel_day >= -10 & breeding_stage %in% c('nestBuilding', 'nestCompleted')))
hist(resid(m0))
m = lmer(time_to_sunrise_min ~ rel_day+breeding_stage + (1|ID), data = subset(x, sex == 2 & rel_day >= -10 & breeding_stage %in% c('nestBuilding', 'nestCompleted'))[which(resid(m0) < 50)])
hist(resid(m))
plot(m)
qqnorm(residuals(m))
m = MCMCglmm(time_to_sunrise_min ~ rel_day+breeding_stage, random = ~ID, data = subset(x, sex == 2 & rel_day >= -10 & breeding_stage %in% c('nestBuilding', 'nestCompleted'))[which(resid(m0) < 50)])
summary(m)
#no effect of nest stage
jpeg(filename = "nestCompletion.jpg", width = 720, height = 720)

ggplot(subset(x, sex == 2 & rel_day > -10 & breeding_stage %in% c('nestBuilding', 'nestCompleted')), aes(x = rel_day, y = time_to_sunrise_min)) +
  geom_boxplot(aes(x = factor(rel_day), fill = breeding_stage)) + ylim(-50, 25)
dev.off()

}
