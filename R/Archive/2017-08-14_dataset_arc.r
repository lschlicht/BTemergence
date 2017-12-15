
whichData = function(years = 2011:2016, boxes = 1:277, from = "-01-01", to = "-12-31") {
  f = data.table(expand.grid(years, boxes))
  setnames(f, names(f), c('year_', 'box'))
  f[, from := paste0(year_, from)]
  f[, to := paste0(year_, to)]
  return(f)
}

queryEventsAll = function(con, input) {

  require(foreach)
  require(tools)
  require(doParallel)

  cl = makePSOCKcluster(detectCores())
  registerDoParallel(cl)


  output = foreach(i = 1 : nrow(input)) %dopar% {
       require(BTemergence)
       require(sdb)
       require(SNB)
    con = dbcon(user = "lschlicht")
    on.exit(closeCon(con))
    x = queryEvents(con = con, box = input[i,box], from = input[i,from], to = input[i,to], includeFIELD = TRUE)
    if(nrow(x) > 0) { x = queryResidents(con = con, x = x)

    return(x) }

  }
  output = rbindlist(output)

  stopCluster(cl)
  stopImplicitCluster()


  #copy(output) -> backupbackup
  #copy(backupbackup) -> output
  output = subset(output, !is.na(ID))
  output[, date_ := as.IDate(ifelse(!is.na(out_), as.IDate(out_), as.IDate(in_)), origin = "1970-01-01")]
  output[, yearDay := yday(date_)]
  output[yearDay > 200, yearDay := as.integer(yearDay-365)]
  output = unique(output, by = c('ID', 'year_', 'transp', 'box', 'in_', 'out_'))

  #force timezone
  output[, in_time := as.ITime(in_)]
  output[, out_time := as.ITime(out_)+out_duration]

  return(output)
}

addBreeding = function(con, x) {
  copy(x) -> X
  males = dbq(con, "SELECT year_, box, IDmale as ID, DATE(firstEgg) as firstEgg, DATE(hatchDate) as hatchDate, DATE(lastHatchDate) as lastHatchDate, DATE(fledgeDate) as fledgeDate, clutch, hatched, fledged, laying_gap FROM BTatWESTERHOLZ.BREEDING where IDmale is not NULL")
  females = dbq(con, "SELECT year_, box, IDfemale as ID, DATE(firstEgg) as firstEgg, DATE(hatchDate) as hatchDate, DATE(lastHatchDate) as lastHatchDate, DATE(fledgeDate) as fledgeDate, clutch, hatched, fledged, laying_gap FROM BTatWESTERHOLZ.BREEDING where IDfemale is not NULL")
  br = rbind(males, females)
  br[, noSocialNests := ifelse(!is.na(ID), length(.I), as.integer(NA)), by = list(year_, ID)]
  br[noSocialNests > 1, ':=' (firstEgg = NA, hatchDate = NA, lastHatchDate = NA, fledgeDate = NA, clutch = NA, hatched = NA, fledged = NA, laying_gap = NA)]
  br[noSocialNests > 1, box := as.integer(NA), by = list(year_, ID)]
  br[, noSocialNests := NULL]
  br = unique(br, by = names(br))
  X = merge(X, br, by = c('year_', 'ID'), all.x = TRUE, suffixes = c('.sleep', '.breeding'))
  X[, rel_day := as.numeric(date_ - as.IDate(firstEgg))]
  return(X)
}

addWeather = function(con, x) {
  copy(x) -> X
  w = dbq(con, "SELECT *, DATE(datetime_) as date_, HOUR(datetime_) as hour_ FROM LOGGERSatWESTERHOLZ.ENVIRONMENTAL")
  w[, datetime_ := NULL]
  w[, pk := NULL]
  w[, date_ := as.Date(date_, origin = '1970-01-01')]
  X[, hour_ := ifelse(!is.na(in_), round(as.numeric(as.ITime(in_))/60/60), round(as.numeric(as.ITime(out_))/60/60))]
  X = merge(X, w, by = c('date_', 'hour_'), all.x = TRUE, all.y = FALSE)
  X[, hour_ := NULL]
  return(X)
}

addIndVar = function(con, x) {
  copy(x) -> X
  ind1 = dbq(con,  "SELECT season as capture_season, ID, tarsus, age FROM BTatWESTERHOLZ.ADULTS")
  ind2 = dbq(con,  "SELECT year_ as capture_season, ID, tarsus FROM BTatWESTERHOLZ.CHICKS")
  ind2[, age := 0]

  ind = rbind(ind1, ind2)
  ind[ , ageAtFirstCapture := min(age, na.rm = TRUE), by = ID]
  ind[, seasonOfFirstCapture := capture_season[which.min(age)[1]], by = ID]
  ind[ageAtFirstCapture == 'Inf', ':=' (seasonOfFirstCapture = min(capture_season), ageAtFirstCapture = NA)]

  ind[,tarsus := mean(tarsus, na.rm = TRUE), by = ID]
  ind[, capture_season := NULL]
  ind[, age := NULL]
  ind = unique(ind, by = names(ind))
  X = merge(X, ind, by = 'ID', all.x  = TRUE)
  X[, minAge := year_ - seasonOfFirstCapture + ageAtFirstCapture]
  X[, age := ifelse(minAge > 1, 2, 1)]
  X[, ageAtFirstCapture := NULL]
  X[, seasonOfFirstCapture := NULL]
  return(X)
}

addSunriseSunset = function(x) {
  copy(x) -> X

  crds <- matrix(c(10.88, 48.13), nrow=1)
  dates <- as.POSIXct("2011-03-01", tz="Etc/GMT-2")
  crds_seq <- seq(from=dates, length.out=90000, by="days")

  #sunrise
  up <- as.data.table(sunriset(crds, crds_seq, direction="sunrise", POSIXct.out=TRUE))
  up[, date_ := as.Date(as.POSIXct(up$time))]
  up = up[, c('time', 'date_'), with = FALSE]
  names(up) = c('sunrise', 'date_')

  #sunset
  down <- as.data.table(sunriset(crds, crds_seq, direction="sunset", POSIXct.out=TRUE))
  down[, date_ := as.Date(as.POSIXct(down$time))]
  down = down[, c('time', 'date_'), with = FALSE]
  names(down) = c('sunset', 'date_')
  down[, date_ := date_+1]

  updown = merge(up, down, by = 'date_')

  X = merge(X, updown, by = 'date_')
  X[, time_to_sunrise := (out_time+out_duration)-as.numeric(as.ITime(sunrise))]
  X[, time_to_sunset := in_time-as.numeric(as.ITime(sunset))]
  X[, time_to_sunrise_min := as.numeric(time_to_sunrise)/60]
  X[, time_to_sunset_min := as.numeric(time_to_sunset)/60]

  return(X)
}

addNestStage = function(con, x) {
  copy(x) -> X
  nests = dbq(con, "SELECT * FROM BTatWESTERHOLZ.NESTS")
  nests[, nest_start := as.IDate(min(c(date_B, date_C, date_LIN), na.rm = TRUE)), by = npk]
  nests[, nest_completed := as.IDate(date_LIN)]
  nests = subset(nests, select = c('box', 'year_', 'nest_start', 'nest_completed'))
  X = merge(X, nests, by.x = c('year_', 'box.breeding'), by.y = c('year_', 'box'), all.x = TRUE)
  X[is.na(nest_start) & is.na(nest_completed), ':=' (nest_start = as.IDate(firstEgg), nest_completed = as.IDate(firstEgg))]
  X[!is.na(nest_start) & is.na(nest_completed), ':=' (nest_completed = as.IDate(firstEgg))]
  return(X)
}

addBreedingStage = function(x) {
  copy(x) -> X
  X[month(date_) < 3 | month(date_) >= 8, breeding_stage := 'nonBreeding']
  X[month(date_) >= 3 & month(date_) < 8 & date_ < nest_start, breeding_stage := 'territoryEstablishment']
  X[date_ >= nest_start & date_ < nest_completed & rel_day < -2, breeding_stage := "nestBuilding"]
  X[date_ >= nest_start & date_ >= nest_completed & rel_day < -2, breeding_stage := "nestCompleted"]
  X[rel_day >= -2 & rel_day < 0, breeding_stage := "preLaying"]
  X[rel_day >= 0 & rel_day < clutch-1, breeding_stage := "laying"]
  X[rel_day >= clutch-1 & date_ < hatchDate, breeding_stage := "incubation"]
  X[date_ >= hatchDate & date_ < as.IDate(hatchDate)+8, breeding_stage := "earlyProvisioning"]
  X[date_ >= as.IDate(hatchDate)+8 & date_ < fledgeDate, breeding_stage := "lateProvisioning"]
  X[date_ >= fledgeDate & month(date_) < 8, breeding_stage := "postFledging"]


  return(X)
}

addSleep = function(x) {
  copy(x) -> X
  X[, sleep := 0]
  X[abs(time_to_sunset_min) < 120 & abs(time_to_sunrise_min) < 120 & !is.na(in_) & !is.na(out_) & (as.Date(in_)+1) == as.Date(out_) , sleep := 1]
  return(X)
}


addEPP = function(con, x) {
  dbq(con, "USE BTatWESTERHOLZ")
  eppGain = dbq(con, " select * from (
 (SELECT DISTINCT year_, sum(epy) AS EPP_gain, father AS IDmale
	FROM
		(SELECT year_, epy, father FROM BTatWESTERHOLZ.PATERNITY P
		   WHERE father IS NOT NULL) AS p1

	GROUP BY year_, father HAVING  sum(epy) IS NOT NULL) AS Y
		LEFT OUTER JOIN

	--  NO OF EP FEMALES
	( SELECT DISTINCT year_, count(DISTINCT mother) AS EP_females, father AS IDmale
	FROM
		(SELECT year_, epy, mother, father FROM BTatWESTERHOLZ.PATERNITY P
		   WHERE  father IS NOT NULL) AS p2

	WHERE father IS NOT NULL AND epy = 1
	GROUP BY year_, father ) AS F
ON
Y.IDmale = F.IDmale AND Y.year_ = F.year_ ) ; ")
  eppGain = eppGain[, -6, with  =FALSE]
  eppGain = eppGain[, -4, with  =FALSE]
  setnames(eppGain, "IDmale", "ID")

  eppLossM = dbq(con, "SELECT year_, father as ID, sum(epy) as epy FROM PATERNITY WHERE father is not NULL and epy is not NULL group by year_, father")
  eppLossM
  eppLossF = dbq(con, "SELECT year_, mother as ID, sum(epy) as epy FROM PATERNITY WHERE mother is not NULL and epy is not NULL group by year_, mother")
  eppLossF

  eppLoss = rbind(eppLossM, eppLossF)

  x = merge(x, eppGain, by = c('year_', 'ID'), all.x = TRUE)
  x = merge(x, eppLoss, by = c('year_', 'ID'), all.x = TRUE)
  setnames(x, "epy", "EPP_loss")
  x[is.na(EP_females) & sex == 1, EP_females := 0]
  return(x)
}
