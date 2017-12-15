
plot_overview = function(data = dp_all, mean_variable = "mean_time_to_sunrise_min", conf_int_variable = "conf_int_time_to_sunrise_min",
  XLAB = list("Day of the year", cex = 1.3), YLAB = list("Entering time (in minutes to sunset)", cex = 1.3),
  XLIM = c(0, 295), YLIM = c(-50, 50),
  CEX = 0.5,
  rect1 = 90, rect2 = NULL, rect3 = 181, rect4 = NULL,
  SYMBOL = c("\u2600", "\u263E", paste("\u2600", "\u2192", "\u263E", sep = ''))[1], symbol1 = 7, symbol2 = 45, PCH = 16,
  sexSymbol1 = 22, sexSymbol2 = 45,
  BREAK = c(182, 261),
  ARROWS1 = NULL, ARROWS2 = NULL
) {
  par(las = 0)
  if(is.null(rect2)) rect2 = YLIM[1]
  if(is.null(rect4)) rect4 = YLIM[1]+2
  layout(mat = c(1,2), heights = c(5,6))

  for(SEX in c(2,1)) {
    tmp = subset(data, sex == SEX)
    tmp[, var1 := tmp[[mean_variable]]]
    tmp[, var2 := tmp[[conf_int_variable]]]
    tmp[, x := day]
    tmp[x > BREAK[2], x := as.integer(x-(BREAK[2] - BREAK[1]))]

    if(SEX == 2) {
      par(mar = c(0.6, 5.1, 0.1, 0.1))
      plot(c(0, 1), c(0, 1), type = 'n', xlab = '', ylab = YLAB, xlim = XLIM, ylim = YLIM, xaxt = 'n', xaxs = 'i')
      axis(1, at = c(-50:50)*10, labels = FALSE, tck = -0.015)
      text(sexSymbol1, sexSymbol2, labels = "\u2640", cex = 3, adj = 0)
      if(!is.null(ARROWS2)) arrows(ARROWS2[1], ARROWS2[2], ARROWS2[3], ARROWS2[4], lwd = 2, length = 0.15)


    } else {
      par(mar = c(5.1, 5.1, 0.1, 0.1))
      plot(c(0, 1), c(0, 1), type = 'n', xlab = XLAB, ylab = YLAB, xlim = XLIM, ylim = YLIM, xaxt = 'n', xaxs = 'i')
      LABELS = c(-20:20)*50
      LABELS = LABELS[-which(LABELS > BREAK[1] & LABELS < BREAK[2])]
      AT = c(-20:20)*50
      AT = AT[1:length(LABELS)]
      axis(1, at = c(-50:50)*10, labels = FALSE, tck = -0.015)
      axis(1, at = AT, labels = LABELS)
      text(sexSymbol1, sexSymbol2, labels = "\u2642", cex =  3, adj = 0)
      if(!is.null(ARROWS2)) arrows(ARROWS1[1], ARROWS1[2], ARROWS1[3], ARROWS1[4], lwd = 2, length = 0.15)

    }
    grid()
    axis.break(axis=1,breakpos=BREAK[1]+0.5,style="slash")

    rect(rect1, rect2, rect3, rect4, border = NA, col = 'black')
    abline(h = 0, col = 'black', lty = 3)
    text(symbol1, symbol2, labels = SYMBOL, cex = 3, adj = 0)


    points(tmp$x, tmp$var1, cex = CEX, pch = PCH)
    arrows(tmp$x, tmp$var1+tmp$var2, tmp$x, tmp$var1-tmp$var2, code = 3, length = 0)

  }

}




plotACF2 = function (x, uselags = c(2,3), alpha = 0.05, xlab = "Lag", ylab = "Autocorrelation")
{
  object <- x
  ylim <- range(object$ACF[uselags[1]:uselags[2]])
  assign("stdv", qnorm(1 - alpha/2)/sqrt(attr(object, "n.used")[uselags[1]:uselags[2]]))
  stMax <- max(stdv)
  ylim <- c(min(c(-stMax, ylim[1])), max(c(ylim[2], stMax)))
  assign("alpha", as.logical(alpha))
  plot(ACF ~ lag, object[uselags[1]: uselags[2],], ylim = ylim, type = 'h', ylab = list("Autocorrelation", cex = 1.4), lwd = 2.5, cex.axis = 1.4, xlab = list("lag", cex = 1.4), xlim = c(uselags[1]-1.2, uselags[2]-0.8), xaxt = 'n')
  axis(1, at = (uselags[1]:uselags[2])-1, labels = (uselags[1]:uselags[2])-1, cex.axis = 1.4)
  lines(object$lag[uselags[1]:uselags[2]], stdv, lty = 2, )
  lines(object$lag[uselags[1]:uselags[2]], -stdv, lty = 2)


}

