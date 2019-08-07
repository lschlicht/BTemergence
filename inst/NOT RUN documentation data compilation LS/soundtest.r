#create data again (including experimental boxes)
setwd("/ds/grpkempenaers/Lotte/R Studio projects/Data for package BTemergence")

require(BTemergenceData)
USER = 'lschlicht'


#fetch data - (2 min) 21 minutes: data save in folder "data" ####
a = Sys.time()
YEARS = 2012:2013
x = runFetchData(con = dbcon(user = USER), YEARS = YEARS)
Sys.time()-a
x = copy(x[[1]])

#remove potential replacements broods (verified replacements removed previously)
x = subset(x, replacementClutchOutlier == 0)

#remove female in 2012 sleeping in box 41, because of time drift in box 41
x = subset(x, !(breeding_season == 2012 & ID == "B2X7402" & box.sleep == 41)) #N = 17

#add day-of-year column
x[, YDAY := yday(date_)]
x[YDAY > 250, YDAY := as.integer(YDAY - 366)]
x[, age2 := as.factor(age)]


#manually check and remove all instances where one individual slept in more than one box in a single night
#how many?
length(which(duplicated(subset(x, select = c('ID', 'year_', 'YDAY'))) == TRUE)) #183
length(which(duplicated(subset(x, select = c('ID', 'year_', 'YDAY'))) == TRUE))/nrow(x) #0.5%

REMOVE = c()

#check manually
dup = which(duplicated(subset(x, select = c('ID', 'year_', 'YDAY'))) == TRUE)
which(dup %in% REMOVE)
for(i in sort(dup, decreasing = TRUE)) {
  tmp = subset(x, breeding_season == x[i,breeding_season] & ID == x[i, ID] & YDAY == x[i,YDAY], select = c('box.breeding', 'box.sleep', 'in_', 'out_'))

  if(!(i %in% REMOVE) & nrow(tmp) > 1) {
    remove = NA
    if(tmp[1, in_] <= tmp[2,in_] & tmp[1, out_] >= tmp[2, out_]) remove = 1 else if(tmp[2, in_] <= tmp[1,in_] & tmp[2, out_] >= tmp[1, out_]) remove = 2
    print(tmp)
    print(remove)
    if(is.na(remove)) {
      remove = readline(prompt = "Which rows should be removed?")
      remove = as.numeric(unlist(strsplit(remove, ' ')))
    }
    if(length(remove) > 0) { #save entries to remove
      for(l in remove) {
        REMOVE[length(REMOVE)+1] = which(x[,breeding_season == x[i,breeding_season]] & x[,ID == x[i, ID]] & x[,YDAY == x[i,YDAY]])[l]
      }
    }
  }
}

length(REMOVE) #184
x = x[-REMOVE,]

#save dataset
save(x, file = paste0(getwd(), "/data/", Sys.Date(), "_SoundTestDataset.RData"))




#comparison with sound data
#fetch sound data
###2013 #####
sleep13 = read.csv("/ds/grpkempenaers/Lotte/R Studio projects/Data for package BTemergence/data/Sound data_2013-08-28b_data.txt", sep = '\t', stringsAsFactors = FALSE)
sleep13 = sleep13[,c(2, 3, 8, 9)]
names(sleep13) = c('file', 'time_', 'type', 'box')
sleep13 = as.data.table(sleep13)
sleep13$date_ = substring(sleep13$file, nchar(sleep13$file)-18, nchar(sleep13$file)-11)
sleep13$start_ = substring(sleep13$file, nchar(sleep13$file)-9, nchar(sleep13$file)-4)
sleep13$date_ = as.IDate(sleep13$date_, format = "%Y%m%d")
sleep13[, time_ := as.ITime(time_)]
sleep13 = sleep13[,c('date_', 'time_', 'type', 'box', 'start_')]
sleep13[, start_ := as.ITime(start_, format = "%H%M%OS")]
sleep13[, time_ := time_ + start_]
sleep13 = sleep13[ type %in% c("sleep", "no sleep", "emergence")]
sleep13[type == 'sleep', time_ := NA]
sleep13[type == 'emergence', type := "sleep"]
sleep13 = subset(sleep13, !(date_ == "2013-04-24" & box == 89) & !(date_ == "2013-04-22" & box == 30) & !(date_ == "2013-04-22" & box == 38)) #errors in data: two emergence times recorded => remove
sleep13[, time_ := sort(time_, na.last = TRUE)[1], by = list(date_, box)]
sleep13 = unique(sleep13)
sleep13[, sleep := ifelse(type == "sleep", as.character(1), as.character(0))]
sleep13 = subset(sleep13, select = c('date_', 'sleep', 'box', 'time_'))
#remove experimentally treated boxes (not controls)
con = dbcon('lschlicht')
exp13 = dbq(con, "SELECT box, DATE(Installed) as Installed, DATE(turnedOff) turnedOff FROM EXTRA_BTatWESTERHOLZ.2013_LS_LEDs where ExpOrControl = 'exp'")
closeCon(con)
exp13[, box := as.character(box)]
sleep13 = merge(sleep13, exp13, by = 'box', all.x = TRUE)
sleep13[is.na(Installed), keep := 1]
sleep13[!is.na(Installed), keep := ifelse(date_ > as.IDate(Installed) & date_ <= as.IDate(turnedOff), 0, 1)]
sleep13 = subset(sleep13, keep == 1, select = c('box', 'date_','sleep', 'time_'))
setnames(sleep13, c('box', 'time_'), c('box.sleep', 'emergence'))
sleep13[, breeding_season := 2013]


###2012 #####
sleep12 = read.csv("/ds/grpkempenaers/Lotte/R Studio projects/Data for package BTemergence/data/emergence 2012.csv", sep = ';', stringsAsFactors = FALSE)
tmp = read.csv("/ds/grpkempenaers/Lotte/R Studio projects/Data for package BTemergence/data/dates and recording times2012.csv", sep = ';', stringsAsFactors = FALSE)
sleep12 = sleep12[,c(1, 2, 4, 8)]
names(tmp) = c('date_', 'start_')
names(sleep12) = c('sleep', 'date_', 'box', 'emergence')
sleep12 = subset(sleep12, date_ != '' & date_ != 'date')
sleep12 = merge(sleep12, tmp)
sleep12 = as.data.table(sleep12)
sleep12[, date_ := as.IDate(date_, format = "%d.%m.%Y")]
sleep12[, emergence := as.ITime(emergence)]
sleep12[, start_ := as.ITime(start_)]
sleep12[, emergence := start_ + emergence]
sleep12[, start_ := NULL]
sleep12 = subset(sleep12, sleep %in% c(0,1))


#remove experimentally treated boxes (not controls)
con = dbcon('lschlicht')
exp12 = dbq(con, "SELECT box, DATE(Installed) as Installed, DATE(turnedOff) turnedOff FROM EXTRA_BTatWESTERHOLZ.2012_LS_LEDs where ExpOrControl = 'exp'")
closeCon(con)
exp12[, box := as.character(box)]
sleep12 = merge(sleep12, exp12, by = 'box', all.x = TRUE)
sleep12[is.na(Installed), keep := 1]
sleep12[!is.na(Installed), keep := ifelse(date_ > as.IDate(Installed) & date_ <= as.IDate(turnedOff), 0, 1)]
sleep12 = subset(sleep12, keep == 1, select = c('box', 'date_','sleep', 'emergence'))
setnames(sleep12, "box", "box.sleep")
sleep12[, breeding_season := 2012]

### combine sleep #####
sleep = rbind(sleep12, sleep13)
sleep[, box.sleep := as.integer(box.sleep)]
table(sleep$breeding_season, sleep$sleep)

sleep2 = rbind(sleep12, sleep13)
sleep2[, box.sleep := as.integer(box.sleep)]
table(sleep2$breeding_season, sleep2$sleep)

sleep = merge(subset(x, select = c('date_', 'breeding_season', 'box.sleep', 'ID', 'out_', 'out_duration')), sleep, by = c('box.sleep', 'date_', 'breeding_season'), all.x = FALSE, all.y = TRUE)
sleep = subset(sleep, !is.na(box.sleep))    #removes the erroneous box codes (typos)
sleep[, emergence.snb := as.ITime(out_) + out_duration]
sleep[, sleep.snb := ifelse(is.na(out_), 0, 1)]
sleep = subset(sleep, select = c('box.sleep', 'date_', 'breeding_season', 'ID', 'sleep', 'sleep.snb', 'emergence', 'emergence.snb'))
#if emergence time is present assume that bird slept
sleep[!is.na(emergence), sleep := "1"]


#check whether any snb data is available on that day
ql = paste0("SELECT * FROM SNBatWESTERHOLZ.EVENTS where box = ", sleep[, box.sleep], " and date(in_) = '", sleep[, date_], "' limit 1")
con = dbcon('lschlicht')
ql = lapply(ql, FUN = function(x) { print(Sys.time()); return(dbq(con, x)) } ) #couple minutes?
closeCon(con)
ql = rbindlist(ql)
ql = subset(ql, select = c('box', 'in_'))
#write(as.matrix(ql), file = "data present.txt", ncolumns = 2)

ql[, date_ := as.IDate(in_)]
ql[, in_ := NULL]
ql[, data_present := 1]
sleep = merge(sleep, ql, by.x = c('box.sleep', 'date_'), by.y = c('box', 'date_'), all.x = TRUE, all.y = FALSE)
sleep[is.na(data_present), data_present := 0]
table(sleep$data_present)
sleep = subset(sleep, data_present == 1)

#remove birds that did not carry at transponder
con = dbcon("lschlicht")
ad = dbq(con, "SELECT ID, date(capture_date_time) as date_time FROM BTatWESTERHOLZ.ADULTS")
br = dbq(con, "SELECT box as box, IDfemale as ID, year_ as breeding_season FROM BTatWESTERHOLZ.BREEDING WHERE year_ = 2012 or year_ = 2013")
setnames(br, "box", "box.sleep")

ad[, date_time := min(date_time), by = "ID"]
ad = unique(ad)
ad = subset(ad, !is.na(ID))
br = merge(br, ad, by = 'ID')
br[, pc := ifelse(as.IDate(paste0(breeding_season, "-03-15")) - as.IDate(date_time) < 0, 0, 1)]
br[, date_time := NULL]
br[, ID := NULL]

sleep = merge(sleep, br, by = c('box.sleep', 'breeding_season'))
sleep = subset(sleep, pc == 1)


###error rate for sleep yes/no #####
a = table(sleep$sleep, sleep$sleep.snb+10)
a
#10  11
#0   3   0
#1  49 123
a[1,2]/sum(a[1,]) #0% where sleep was assigned when no bird slept
a[2,1]/sum(a[2,]) #28.49% where no sleep was assigned although birds slept; for most the entry was missed but the exit recorded

###error for emergence times ###
sleep[, dif := emergence - emergence.snb]
dif = sleep[!is.na(dif), dif]
plot(density(dif))
abline(v = -60)
abline(v = 60)
length(dif)       #119
mean(dif) #-48.15
sd(dif)/sqrt(length(dif)) #37.10
min(dif) #-3563
min(dif)/60 #-59.38 minutes
max(dif) #257
max(dif)/60 #4.28 minutes
quantile(dif, probs = c(0.05, 0.95))




