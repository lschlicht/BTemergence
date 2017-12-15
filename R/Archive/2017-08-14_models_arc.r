modelSunrisetSex = function(x, response = "time_to_sunrise_min") {
  copy(x) -> X
  X[, RR := X[[response]]]
  X[, interact := interaction(breeding_stage, sex)]
  m = lmer(RR ~ 0 + interact + (1|ID) + (1|year_), data = subset(X, !is.na(RR) & !is.na(interact)))
  mcomp = glht(m, mcp(interact = c(
    "territoryEstablishment.2 - nonBreeding.2 = 0",
    "nestBuilding.2 - territoryEstablishment.2 = 0",
    "nestCompleted.2 - nestBuilding.2 = 0",
    "nestCompleted.2 - preLaying.2 = 0",
    "laying.2 - preLaying.2 = 0",
    "incubation.2 - laying.2 = 0",
    "earlyProvisioning.2 - incubation.2 = 0",
    "lateProvisioning.2 - earlyProvisioning.2 = 0",
    "postFledging.2 - lateProvisioning.2 = 0",

    "territoryEstablishment.1 - nonBreeding.1 = 0",
    "nestBuilding.1 - territoryEstablishment.1 = 0",
    "nestCompleted.1 - nestBuilding.1 = 0",
    "nestCompleted.1 - preLaying.1 = 0",
    "laying.1 - preLaying.1 = 0",
    "incubation.1 - laying.1 = 0",
    "earlyProvisioning.1 - incubation.1 = 0",
    "lateProvisioning.1 - earlyProvisioning.1 = 0",
    "postFledging.1 - lateProvisioning.1 = 0",

    "nonBreeding.2 - nonBreeding.1 = 0",
    "territoryEstablishment.2 - territoryEstablishment.1 = 0",
    "nestBuilding.2 - nestBuilding.1 = 0",
    "nestCompleted.2 - nestCompleted.1 = 0",
    "preLaying.2 - preLaying.1 = 0",
    "laying.2 - laying.1 = 0",
    "incubation.2 - incubation.1 = 0",
    "earlyProvisioning.2 - earlyProvisioning.1 = 0",
    "lateProvisioning.2 - lateProvisioning.1 = 0",
    "postFledging.2 - postFledging.1 = 0",

    "(territoryEstablishment.2 - nonBreeding.2) - (territoryEstablishment.1 - nonBreeding.1) = 0",
    "(nestBuilding.2 - territoryEstablishment.2) - (nestBuilding.1 - territoryEstablishment.1) = 0",
    "(nestCompleted.2 - nestBuilding.2) - (nestCompleted.1 - nestBuilding.1) = 0",
    "(nestCompleted.2 - preLaying.2) - (nestCompleted.1 - preLaying.1) = 0",
    "(laying.2 - preLaying.2) - (laying.1 - preLaying.1) = 0",
    "(incubation.2 - laying.2) - (incubation.1 - laying.1) = 0",
    "(earlyProvisioning.2 - incubation.2) - (earlyProvisioning.1 - incubation.1) = 0",
    "(lateProvisioning.2 - earlyProvisioning.2) - (lateProvisioning.1 - earlyProvisioning.1) = 0",
    "(postFledging.2 - lateProvisioning.2) - (postFledging.1 - lateProvisioning.1) = 0")))

    return(mcomp)

}


modelPreLaying = function(x, response = "time_to_sunrise_min") {
  X = subset(x, rel_day >= -5 & rel_day <= 0)
  X[, RR := X[[response]]]
  X[, rel_day2 := rel_day + 5]
  X[sex == 1, sex := 'male']
  X[sex == 2, sex := 'female']
  X[, interact := interaction(sex, rel_day2)]


  m = lmer(RR ~ 0 + interact + (1|ID) + (1|year_), data = X)
  mcomp = glht(m, mcp(interact = c(
    "female.1 - female.0 = 0",
    "female.2 - female.1 = 0",
    "female.3 - female.2 = 0",
    "female.4 - female.3 = 0",
    "female.5 - female.4 = 0",

    "male.1 - male.0 = 0",
    "male.2 - male.1 = 0",
    "male.3 - male.2 = 0",
    "male.4 - male.3 = 0",
    "male.5 - male.4 = 0",

    "female.0 - male.0 = 0",
    "female.1 - male.1 = 0",
    "female.2 - male.2 = 0",
    "female.3 - male.3 = 0",
    "female.4 - male.4 = 0",
    "female.5 - male.5 = 0",

    "(female.1 - female.0) - (male.1 - male.0) = 0",
    "(female.2 - female.1) - (male.2 - male.1) = 0",
    "(female.3 - female.2) - (male.3 - male.2) = 0",
    "(female.4 - female.3) - (male.4 - male.3) = 0",
    "(female.5 - female.4) - (male.5 - male.4) = 0"
)))
  return(mcomp)

}

