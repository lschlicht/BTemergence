bla = function(){
plotEmergence = function(x, xx = "rel_day", yy = "time_to_sunrise_min", colour_group = "sex", xlabel = "Day to first egg", ylabel = "Time in minutes to sunrise", yylim = NULL) {
  copy(x) -> X

  NTmp = data.table(table(X[[colour_group]], X[[xx]]))
  NTmp[,Freq := (N/5)-150]
  NTmp[,V2 := as.integer(V2)]

  X[, XX := X[[xx]]]
  X[, YY := X[[yy]]]
  X[, GG := X[[colour_group]]]
  if(sign(min(X[[xx]], na.rm = TRUE)) != sign(max(X[[xx]], na.rm = TRUE))) {
    LABELS = c(rep('', abs(min(X[[xx]], na.rm = TRUE))), 0, rep('',max(X[[xx]], na.rm = TRUE)))
  } else {
    LABELS = rep('',max(X[[xx]], na.rm = TRUE)-min(X[[xx]], na.rm = TRUE)+1)
    }
  LABELS = min(X[[xx]], na.rm = TRUE):max(X[[xx]], na.rm = TRUE)
  LABELS[which(LABELS%%10 != 0)] = ''

  ggplot(X, aes(factor(XX), YY, colour = factor(GG))) +
    geom_boxplot() +
    scale_x_discrete(xlabel, breaks=factor(min(X[[xx]], na.rm = TRUE):max(X[[xx]], na.rm = TRUE)), drop=FALSE, labels = LABELS) +
    geom_line(data = NTmp, aes(as.numeric(as.factor(V2)), Freq, colour = factor(V1)), inherit.aes = FALSE) +
    scale_y_continuous(ylabel, breaks = c((-15:-11)*10, (-2:1)*100), labels = c((0:4)*50, (-2:1)*100), limits = yylim)

}

plotEmergenceNS = function(x, xx = "breeding_stage", yy = "time_to_sunrise_min", colour_group = "sex", xlabel = "Breeding stage", ylabel = "Time in minutes to sunrise") {
  copy(x) -> X

  X[, XX := factor(X[[xx]], levels = c('nonBreeding', 'territoryEstabilshment', 'nestBuilding', 'nestCompleted', 'preLaying', 'laying', 'incubation', 'earlyProvisioning', 'lateProvisioning', 'postFledging'))]

  X[, YY := X[[yy]]]
  X[, GG := X[[colour_group]]]
  ggplot(X, aes(factor(XX), YY, colour = factor(GG))) +
    geom_boxplot() +
    scale_x_discrete(xlabel) +
    scale_y_continuous(ylabel)

}
}
