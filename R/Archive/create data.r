runFetchData = function(){
con = dbcon(user = "lschlicht")
input = whichData(years = 2011:2016, boxes = 1:277, from = "-01-01", to = "-12-31")
x = queryEventsAll(con, input)
x = addBreeding(con, x)
x = addWeather(con, x)
x = addIndVar(con, x)
x = addSunriseSunset(x)
x = addSleep(x)
x = addNestStage(con, x)
x[, breeder := ifelse(is.na(box.breeding), "no", "yes")]
x[, breeder_sex := paste(breeder, sex)]
x[, breedingAttempt := paste(year_, box.breeding, sep = '_')]
snb = copy(x)
x = subset(x, sleep == 1)
x = subset(x, breeder == "yes")
x = subset(x, !is.na(rel_day))

fe = subset(x, select = c('season', 'box.breeding', 'firstEgg'))
fe = unique(fe, by = names(fe))
fe[, z_firstEgg := scale(as.IDate(firstEgg)), by = 'season']
fe[, c_firstEgg := scale(as.IDate(firstEgg), scale = FALSE), by = 'season']

x = merge(x, fe, by.x = c('season', 'box.breeding', 'firstEgg'), by.y = c('season', 'box.breeding', 'firstEgg'))
setkey(x, year_, box.breeding, ID, date_)
x[, lag1_rel_day := shift(rel_day), by = list(season, box.breeding, ID)]
x[, lag1_rel_day_emergence_min := shift(time_to_sunrise_min), by = list(season, box.breeding, ID)]

#add moon phase
library(oce)
par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0)) # tighten margins
t <- as.POSIXct("2011-03-1", tz="UTC") + seq(0, 3000*24*3600, 3600*24)
f <- moonAngle(t=t, longitude=10.88,
               latitude=48.13)$illuminatedFraction
moon = data.table(date_ = as.IDate(t), moon = f)
x = merge(x, moon, by = "date_")

#add EPP
x = addEPP(con, x)
x[, EPP_lossYN := ifelse(EPP_loss > 0, 1, EPP_loss)]
x[, EPP_gainYN := ifelse(EPP_gain > 0, 1, EPP_gain)]


closeCon(con)
#end fetch data
return(x)
}

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
  output[, in_time := ifelse(is.na(in_), NA, as.ITime(in_))]
  output[, out_time := ifelse(is.na(out_), NA, as.ITime(out_)+out_duration)]

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
  X[, season := ifelse(yday(date_) > 200 | year(date_) != year_, year_ + 1, year_)]

  X = merge(X, br, by.x = c('season', 'ID'), by.y = c('year_', 'ID'), all.x = TRUE, suffixes = c('.sleep', '.breeding'))
  X[, rel_day := as.numeric(date_ - as.IDate(firstEgg))]
  X = subset(X, rel_day > -300)
  return(X)
}

fetchWeatherData = function() {
  tmp = list.files("/ds/raw_data_kemp/FIELD/Westerholz/ENVIRONMENTAL_DATA/in database/", full.names = FALSE)
  tmp = list.files("/ds/raw_data_kemp/FIELD/Westerholz/ENVIRONMENTAL_DATA/in database/", full.names = TRUE)[which(substring(tmp, 1, 6) == "Stunde")]
  tmp = lapply(tmp, FUN = function(x) data.table(read.csv(x, sep = ";", stringsAsFactors = FALSE)))
  tmp = rbindlist(tmp, fill = TRUE)

  #w = dbq(con, "SELECT *, DATE(datetime_) as date_, HOUR(datetime_) as hour_ FROM LOGGERSatWESTERHOLZ.ENVIRONMENTAL")
  #w[, datetime_ := NULL]
  #w[, pk := NULL]
  #w[, date_ := as.Date(date_, origin = '1970-01-01')]
  w = copy(tmp)
  w[nchar(Tag) > 8, date_ := as.IDate(Tag, format = "%d.%m.%Y")]
  w[nchar(Tag) == 8, date_ := as.IDate(Tag, format = "%Y%m%d")]
  w[, Tag := NULL]
  setnames(w, names(w), c("hour_", "humidity", "windspeed", "SumRainfall", "GlobalRadiation", "Temperature2m", 'date_'))
  return(w)
}

addWeather = function(con, x) {
  copy(x) -> X
  w = fetchWeatherData()

  #dawn
  X[, hour_ := ifelse(!is.na(out_), round(as.numeric(as.ITime(out_))/60/60), round(as.numeric(as.ITime(in_))/60/60))]
  X = merge(X, w, by = c('date_', 'hour_'), all.x = TRUE, all.y = FALSE)
  X[, hour_ := NULL]

  #dusk
  X[, hour_ := ifelse(!is.na(in_), round(as.numeric(as.ITime(in_))/60/60), round(as.numeric(as.ITime(out_))/60/60))]
  X = merge(X, w, by = c('date_', 'hour_'), all.x = TRUE, all.y = FALSE, suffixes = c('_dawn', '_dusk'))
  X[, hour_ := NULL]


  #daylength (daily averages)
  w[, SumRainfall := as.numeric(sub(",", ".", SumRainfall))]
  w[, Temperature2m := as.numeric(sub(",", ".", Temperature2m))]
  w[, SumRainfall := mean(SumRainfall, na.rm = TRUE), by = date_]
  w[, Temperature2m := mean(Temperature2m, na.rm = TRUE), by = date_]
  w = subset(w, select = c('SumRainfall', 'Temperature2m', 'date_'))
  w = unique(w, by = names(w))
  X = merge(X, w, by = 'date_', all.x = TRUE, all.y = FALSE, suffixes = c('', '_daymean'))

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
  X = merge(X, nests, by.x = c('season', 'box.breeding'), by.y = c('year_', 'box'), all.x = TRUE)
  X[is.na(nest_start) & is.na(nest_completed), ':=' (nest_start = as.IDate(firstEgg), nest_completed = as.IDate(firstEgg))]
  X[!is.na(nest_start) & is.na(nest_completed), ':=' (nest_completed = as.IDate(firstEgg))]
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
  setnames(eppGain, "year_", "season")

  eppLossM = dbq(con, "SELECT year_ as season, father as ID, sum(epy) as epy FROM PATERNITY WHERE father is not NULL and epy is not NULL group by year_, father")
  eppLossF = dbq(con, "SELECT year_ as season, mother as ID, sum(epy) as epy FROM PATERNITY WHERE mother is not NULL and epy is not NULL group by year_, mother")
  eppLoss = rbind(eppLossM, eppLossF)

  x = merge(x, eppGain, by = c('season', 'ID'), all.x = TRUE)
  x = merge(x, eppLoss, by = c('season', 'ID'), all.x = TRUE)
  setnames(x, "epy", "EPP_loss")
  x[is.na(EP_females) & sex == 1, EP_females := 0]
  return(x)
}
