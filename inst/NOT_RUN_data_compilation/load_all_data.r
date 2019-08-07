setwd("/ds/grpkempenaers/Lotte/R Studio projects/Data for package BTemergence")
load(paste0(getwd(), "/data/2019-01-28_FullDataset.RData"))
load(paste0(getwd(), "/data/2019-05-15_ModelDataset.RData"))
#breeder and nonbreeder





#not updated
load(file = paste0(getwd(), "/models/var_day.sex_F.RData"))
sr_m1 = var_day.sex[[1]]; sr_m2 = var_day.sex[[2]]; sr_f1 = var_day.sex[[3]]; sr_f2 = var_day.sex[[4]]; ss_m1 = var_day.sex[[5]]; ss_m2 = var_day.sex[[6]]; ss_f1 = var_day.sex[[7]]; ss_f2 = var_day.sex[[8]]; act_m1 = var_day.sex[[9]]; act_m2 = var_day.sex[[10]]; act_f1 = var_day.sex[[11]]; act_f2 = var_day.sex[[12]]


load(file = paste0(getwd(), "/models/var_day.sex.age_F.RData"))

load(file = paste0(getwd(), "/models/z_var_day.sex.env_females_F.RData"))
load(file = paste0(getwd(), "/models/z_var_day.sex.env_males_F.RData"))


load(file = paste0(getwd(), "/models/var_day.sex.age_T.RData"))
load(file = paste0(getwd(), "/models/var_day.sex.env_T.RData"))

#load(file = paste0(getwd(), "/data/select.k.model.list.RData"))
#load(file = paste0(getwd(), "/data/var_day.sex.RData"))
#load(file = paste0(getwd(), "/data/var_day.sex.age.RData"))
#load(file = paste0(getwd(), "/data/var_day.sex.env.RData"))
#load(file = paste0(getwd(), "/data/var_day.sex.env_choice.of.variable.emergence.RData"))
#load(file = paste0(getwd(), "/data/var_day.sex3.env.RData"))
#load(file = paste0(getwd(), "/data/z_var_day.sex.env.RData"))


