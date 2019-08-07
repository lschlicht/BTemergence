
#' Wrapper function for gamm4()
#'
#' @param response Response variable
#' @param by_gamm "By" for the smoother
#' @param k_gamm "k" that si supplied to gamm4
#' @param explanatory explanatory variable
#' @param data dataset
#' @param rm.intercept Should the intercet be removed when calculating the model (0+ added to the model formula on the right side)
#' @param smooth_over Which variable to smooth over (here date or days to first egg)
#' @return The output of a gamm4.
#' @import data.table
#' @import stats
#' @import gamm4
#' @export
#'
gamm4.2 = function(response, by_gamm, k_gamm = 40, explanatory = c(NULL), data, rm.intercept = FALSE, smooth_over = 'rel_day') {
  if(rm.intercept == TRUE & is.null(explanatory)) print("If rm.intercept == TRUE then explanatory variables must be given.")

  if(is.null(explanatory)) form = paste(response, paste0("s(", smooth_over, ", bs = 'tp',k = ", k_gamm, ", by = ", by_gamm, ")"), sep = " ~ ") else { if(rm.intercept == FALSE) form = paste(response, paste0("s(", smooth_over, ", bs = 'tp',k = ", k_gamm, ", by = ", by_gamm, ") + ", paste(explanatory, collapse = " + ")), sep = " ~ ") else form = paste(response, paste0("0 + s(", smooth_over, ", bs = 'tp',k = ", k_gamm, ", by = ", by_gamm, ") + ", paste(explanatory, collapse = " + ")), sep = " ~ ")
  }

  model_output = gamm4(as.formula(form), random = ~(1|ID), data = data)
  return(model_output)
}



#' Make Figures 1-3 (smoothed figures): Subfunction
#'
#' @param var variable to plot, e.g. time_to_sunset_min
#' @param data dataset
#' @param include_age Should age be included?
#' @param modelset Model for smooth line
#' @param COL Colors for males and females
#' @param plot_sex obsolete?
#' @param plot_which obsolete?
#' @param ... passed on to plot.
#' @import stats
#' @return A panel of figures 1-3
#' @export
#'
create_gamm_plot = function(var, data, include_age = FALSE, modelset = list(), COL = c("red", "blue"), plot_sex = NULL, plot_which = NULL, ...) {

  SHIFT1 = summary(modelset[[1]]$gam)$p.table[1,1]
  SHIFT2 = summary(modelset[[2]]$gam)$p.table[1,1]

  #plot
  plot(modelset[[1]]$gam, shift = SHIFT1, col = COL[1], select = 1, rug = FALSE, xaxt = 'n', ...)
  axis(1, labels = FALSE, tick = TRUE)
  par(new = TRUE)
  plot(modelset[[2]]$gam, shift = SHIFT2, col = COL[2], select = 1, rug = FALSE, xaxt = 'n', ...)
}



#' Plot environmental variables (Figure S1)
#'
#' @param df Data
#' @param VARIABLE variable to plot.
#' @param ... further parameters
#' @return A figure
#' @import graphics
#' @export
#'
plot_env = function(df, VARIABLE, ...) {
  plot(c(-2, 3.0), c(0.5, 2.5), type = "n", yaxt = "n", ylab = "", ...)
  arrows(df[variable == VARIABLE, low], df[variable == VARIABLE, YY], df[variable == VARIABLE, up], df[variable == VARIABLE, YY], code = 3, angle = 90, length = 0.15, col = df[variable == VARIABLE, COL])
  points(df[variable == VARIABLE, est], df[variable == VARIABLE, YY], pch = df[variable == VARIABLE, PCH], col = df[variable == VARIABLE, COL], bg = "white")
  abline(v = 0, lty = 3)

  if(VARIABLE == "time_to_sunrise_min") {
      LABEL = "";
      axis(2, at = 1:2, labels = c("Rainfall", "Temperature"), las = 2)
  }
  if(VARIABLE == "time_to_sunset_min") {
      LABEL = "";
      axis(2, at = 1:2, labels = c("Rainfall", "Temperature"), las = 2)
  }

  axis(2, at = 1.5, labels = LABEL, line = 5, cex.axis = 1.5, tick = FALSE)

}

#' Helper function for Fig. 1-3
#'
#' @param d1 First list
#' @param d2 Second list
#' @param ... further arguments
#' @import graphics
#' @export
#'
boxplot_emergence = function(d1, d2, ...) {
  plot(c(-100, 90), c(-60, 70), type = 'n', ...)
  points(d1$YDAY, d1$med, pch = 16, ...)
  arrows(d1$YDAY, d1$box_min, d1$YDAY, d1$w_min, length = 0, ...)
  arrows(d1$YDAY, d1$box_max, d1$YDAY, d1$w_max, length = 0, ...)
  points(d2$YDAY, d2$out, ...)
}


#' Pipeline function for Figure 1-3
#' @details Uses the functions boxplot_emergence and create_gamm_plot.
#' @param modellist_yday Modellist for "Day of year"
#' @param modellist_rel_day Modellist for "Days to first egg"
#' @param d_list Boxplot variables
#' @param out_list Boxplot variables
#' @param YLAB ylab
#' @param VAR which variable is plotted?
#' @param ylim_boxplot ylim of boxplot
#' @param ylim_gammplot yoim of gamm-plot
#' @return Figure like Figures 1-3
#' @import graphics
#' @export
plot_descriptive_figure = function(modellist_yday, modellist_rel_day, d_list, out_list, YLAB, VAR,ylim_boxplot = c(-60, 70), ylim_gammplot = c(-40, 20)) {
    #figure settings
    cex.pt = 2
    cex.stage = 3.5
    cex.sex = 5
    par(cex.axis=4, cex.lab=5, lwd = 2, las = 2, mgp = c(9,2.8,0))
    mar.left = 20.1
    mar.bottom = 12.1
    line.stage = 0.5
    line.y.lab = -5.2
    tcl.stage = -3
    tcl.stage2 = 2
    stage.labels = c('Nest', "Egg", "Inc.", "Young", "Post-Br.")
    layout(matrix(1:6, ncol = 2, byrow = TRUE), widths = c(0.55, 0.45))
    par(mar = c(0.1, mar.left, mar.bottom, 0.1))

    #plot
    boxplot_emergence(d1 = d_list[[1]], d2 = out_list[[1]], xaxt = 'n', xlab = '', ylab = list('', cex = cex.sex), col = MALE_COLOUR, xlim = c(-100, 90), cex = cex.pt, ylim = ylim_boxplot)
    text(-97, 60, "Males",  bty = 'n', cex = cex.sex, adj = 0)
    par(mar = c(0.1, 0.1, mar.bottom, mar.left))
    boxplot_emergence(d1 = d_list[[2]], d2 = out_list[[2]], xlim = c(-21, 60), xaxt = 'n', yaxt = 'n', xlab = '', col = MALE_COLOUR, cex = cex.pt, ylim = ylim_boxplot)
    axis(1, at = c(0, 10, 24, 44), labels = rep('', 4), tcl = tcl.stage2)
    axis(1, at = c(0, 10, 24, 44), labels = rep('', 4), tcl = tcl.stage)
    axis(1, at = c(-10, 5, 17, 34, 55), labels = stage.labels, las = 0, tick = FALSE, line = line.stage, cex.axis = cex.stage)

    par(mar = c(mar.bottom/2, mar.left, mar.bottom/2, 0.1))
    boxplot_emergence(d1 = d_list[[3]], d2 = out_list[[3]], xaxt = 'n', xlab = '', ylab = list('', cex = cex.sex), col = FEMALE_COLOUR, xlim = c(-100, 90), cex = cex.pt, ylim = ylim_boxplot)
    text(-97, 60, "Females",  bty = 'n', cex = cex.sex, adj = 0)
    par(mar = c(mar.bottom/2, 0.1, mar.bottom/2, mar.left))
    boxplot_emergence(d1 = d_list[[4]], d2 = out_list[[4]], xlim = c(-21, 60), yaxt = 'n', xaxt = 'n', xlab = '', col = FEMALE_COLOUR, cex = cex.pt, ylim = ylim_boxplot)
    axis(1, at = c(0, 10, 24, 44), labels = rep('', 4), tcl = tcl.stage2)
    axis(1, at = c(0, 10, 24, 44), labels = rep('', 4), tcl = tcl.stage)
    axis(3, at = c(0, 10, 24, 44), labels = rep('', 4), tcl = tcl.stage2)
    axis(3, at = c(0, 10, 24, 44), labels = rep('', 4), tcl = tcl.stage)
    axis(1, at = c(-10, 5, 17, 34, 55), labels = stage.labels, las = 0, tick = FALSE, line = line.stage, cex.axis = cex.stage)

    mtext(YLAB, side = 2, line = line.y.lab, outer = TRUE, las = 0, cex = par("cex.lab"))
    par(mar = c(mar.bottom, mar.left, 0.1, 0.1))
    create_gamm_plot(var = VAR, modelset = modellist_yday , ylab = list(""), COL = c(MALE_COLOUR, FEMALE_COLOUR), xlim = c(-92, 90), xlab = list("Date (1 = 1 January)"), ylim = ylim_gammplot, se = 1.96)
    mtext(text = c('O', 'N', 'D', 'J', 'F', 'M'), side = 1, line = -4, at = c(-77.5, -46, -15, 16, 46, 76.5), las = 0, cex = par("cex.lab")/2)
    axis(1, las = 0)
    axis(1, at = c(-10:10)*10, labels = NA, tcl = 1)

    par(mar = c(mar.bottom, 0.1, 0.1, mar.left))
    create_gamm_plot(var = VAR, modelset = modellist_rel_day , ylab = list(""), COL = c(MALE_COLOUR, FEMALE_COLOUR), xlim = c(-21, 60), xlab = list("Days to first egg"), yaxt = 'n', ylim = ylim_gammplot, se = 1.96)
    axis(1, las = 0)
    axis(3, at = c(0, 10, 24, 44), labels = rep('', 4), tcl = tcl.stage2)
    axis(3, at = c(0, 10, 24, 44), labels = rep('', 4), tcl = tcl.stage)
    axis(1, at = c(-4:17)*5, labels = NA, tcl = 1)

}



#' Plot individuals
#'
#' @param data Dataset
#' @param i Number of the individual within data that should be plotted
#' @param year_ID year_ and ID pasted together
#' @param var variable to plot (e.g. time_to_sunset_min)
#' @param MALE_COLOUR colour of males
#' @param FEMALE_COLOUR colour of females
#' @param plot.lines logical. Should points within a week be connected by lines?
#' @param ... Further arguments passed on to plot
#' @import graphics
#' @return A figure such as S3-S4
#' @export
plot_individuals = function(data, i = 1, year_ID, var = "time_to_sunrise_min", MALE_COLOUR = "#5000FFFF", FEMALE_COLOUR = "#FF758AFF", plot.lines = FALSE, ...) {

  N = subset(data, ID == year_ID[i, ID] & breeding_season == year_ID[i, breeding_season])
  N[, var := N[,which(names(N) == var), with = FALSE]]
  if(var == "time_to_sunset_min") N[, rel_day := rel_day - 1] #because individuals entered the nestbox in the evening before emergence
  setkey(N, rel_day)
  COL_POINTS = ifelse(N[1,sex == 1], MALE_COLOUR, FEMALE_COLOUR)

  plot(N[, var] ~ N[, rel_day], pch=16, col = COL_POINTS, ...)
  abline(h = 0, v = 0, ...)
  #add lines
  if(plot.lines) {
    N[, rel_day.previous := shift(rel_day)]
    N[, var.previous := shift(var)] #ifelse(, 1, 0) == 1
    LINES = subset(N, !is.na(var.previous) & shift(rel_day) - rel_day >= -7, select = c(var, var.previous, rel_day, rel_day.previous))
    for(i in 1 : nrow(LINES)) {
      lines(c(LINES[i,rel_day], LINES[i,rel_day.previous]), c(LINES[i,var], LINES[i,var.previous]), type = "c", col = COL_POINTS)
    }
  }

}

#' Helper function to check the fit of the model assumptions
#'
#' @param mod gamm model to plot
#' @return A figure
#' @import graphics
#' @import stats
#' @export
plot_gamm_models = function(mod) {
  par(mfrow = c(2,2))
  qqnorm(resid(mod$mer))
  plot(resid(mod$mer) ~ fitted(mod$mer))
  hist(resid(mod$mer))
  plot(0:1, 0:1, type = 'n')
}

#' Plots how the effect sizes vary with season (Figs. S7-10)
#'
#' @param tmp1 The data for day of year
#' @param tmp2 The data for relative day
#' @param YLIM ylim
#' @param XLAB xlab
#' @param YLAB ylab
#' @return A figure such as Figs. S7-10
#' @import graphics
#' @export
plot_seasonal_progress = function(tmp1, tmp2, YLIM = c(-10, 5), XLAB = "Onset of activity", YLAB = NULL) {
  if(is.null(YLAB)) YLAB = paste0("average Temperature\r\n", ifelse(i == 1, "males", "females"))
  par(mfrow = c(2,1))
  par(cex = 2, lwd = 2, cex.axis = 0.8)
  for(i in 1 : 2) {
    YLAB2 = paste0(YLAB, ifelse(i == 1, "males", "females"))
    if(i == 1) par(mar = c(2.6, 5.1, 2.6, 0.1)) else par(mar = c(5.1, 5.1, 0.1, 0.1))
    plot(c(-4, 0) ~ c(1,3), type = 'n', ylim = YLIM, xlim = c(-2.5, 7.5), ylab = YLAB2, xaxt = 'n', xlab = ifelse(i == 1, "", XLAB))
    axis(1, at = c(-2:7), labels = c("Oct.", "Nov.", "Dec.", "Jan.", "Feb.", "Mar.", "Apr.", "Wk.-1", "Egg", "Inc."))
    arrows(tmp1[[i]][,MONTH], tmp1[[i]][, Est]+1.96*tmp1[[i]][,SE], tmp1[[i]][,MONTH], tmp1[[i]][, Est]-1.96*tmp1[[i]][,SE], angle = 90, length = 0.05, code = 3)
    points(tmp1[[i]][,MONTH], tmp1[[i]][, Est], pch = 21, bg = "white", cex = 0.5)
    arrows(5:7, tmp2[[i]][, Est]+1.96*tmp2[[i]][,SE], 5:7, tmp2[[i]][, Est]-1.96*tmp2[[i]][,SE], angle = 90, length = 0.05, code = 3)
    points(5:7, tmp2[[i]][, Est], pch = 21, bg = "white", cex = 0.5)
    abline(h = 0, lty = 3)
    abline(v = 4.5, lty = 2)
  }

}
