source("Data.R")
source("DataCollection.R")
source("SpatialData.R")
source("BaseFgmDataRef.R")
source("DaughterFgmDataRef.R")
source("RegressionResults.R")

system.time(original.data <- DaughterFgmData$new(ir.file = '~/Data/EDHS/2008/EGIR5AFL.DTA', br.file = '~/Data/EDHS/2008/EGBR5AFL.DTA', gps.file = '~/Data/EDHS/2008/EGGE5AFF.dbf', other.grpavg.controls = c("med.circum", "circum"))) #, skip.cleanup = T))
original.data$relevel("urban.rural", ref = "rural")
original.data$relevel("med.help.distance.fac", ref = "not big problem")
original.data$relevel("med.help.money.fac", ref = "not big problem")
original.data$relevel("med.help.transportation.fac", ref = "not big problem")

radii <- c(10, 20)

regress.data <- original.data$copy()
for (radius in radii) {
    system.time(regress.data$generate.delivery.means.spatial(radius = radius, postfix = as.character(radius)))
    system.time(regress.data$generate.delivery.means.spatial(radius = radius, postfix = paste(as.character(radius), "wt", sep = '.'), dist.wt = TRUE))
}
regress.data$rm.by.res.years(10)

for (radius in radii) { 
    system.time(regress.data$generate.reg.means.spatial(radius = radius, postfix = as.character(radius), exclude.self = TRUE))
    system.time(regress.data$generate.reg.means.spatial(radius = radius, postfix = paste(as.character(radius), "wt", sep = "."), exclude.self = TRUE, dist.wt = TRUE))
}

system.time(regress.data$generate.reg.means.spatial(radius = 20, inner.radius = 10, postfix = "10_20", exclude.self = TRUE))

#regress.data$spatial.data@data$grpavg.educ.lvl_primary_neg <- - regress.data$spatial.data@data$grpavg.educ.lvl_primary
#regress.data$spatial.data@data$grpavg.educ.lvl_secondary_neg <- - regress.data$spatial.data@data$grpavg.educ.lvl_secondary
#regress.data$spatial.data@data$grpavg.mother.circum.fac_yes_neg <- - regress.data$spatial.data@data$grpavg.mother.circum.fac_yes

regress.data.10 <- regress.data$copy()
regress.data.10$subset(grp.size.10 >= 10)

regress.data.20 <- regress.data$copy()
regress.data.20$subset(grp.size.20 >= 20)

regress.data.10_20 <- regress.data$copy()
regress.data.10_20$subset(grp.size.10_20 >= 10)

# Exogenous Effects only

pooled.exogen.only.results.10 <- regress.data.10$plm(circum ~ governorate + birth.year.fac + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + order + I(order^2) + spat.grpavg.urban.rural_urban.10 + spat.grpavg.wealth.index.2_rich.10 + spat.grpavg.educ.lvl_primary.10 + spat.grpavg.educ.lvl_secondary.10 + spat.grpavg.educ.lvl_higher.10 + spat.grpavg.marital.age.10 + spat.grpavg.mother.circum.fac_yes.10 + spat.grpavg.religion_christian.10 + spat.grpavg.hh.head.sex_female.10 + spat.grpavg.med.help.distance.fac_big_problem.10, effect = "individual", model = "pooling", index = c("hh.id", "order.fac"))

within.exogen.only.results.10 <- regress.data.10$plm(circum ~ birth.year.fac + spat.grpavg.urban.rural_urban.10 + spat.grpavg.wealth.index.2_rich.10 + spat.grpavg.educ.lvl_primary.10 + spat.grpavg.educ.lvl_secondary.10 + spat.grpavg.educ.lvl_higher.10 + spat.grpavg.marital.age.10 + spat.grpavg.mother.circum.fac_yes.10 + spat.grpavg.religion_christian.10 + spat.grpavg.hh.head.sex_female.10 + spat.grpavg.med.help.distance.fac_big_problem.10, effect = "twoways", model = "within", index = c("hh.id", "order.fac"))

#within.exogen.only.results.10.2 <- regress.data.10$plm(circum ~ spat.grpavg.urban.rural_urban.10 + spat.grpavg.wealth.index.2_rich.10 + spat.grpavg.educ.lvl_primary.10 + spat.grpavg.educ.lvl_secondary.10 + spat.grpavg.educ.lvl_higher.10 + spat.grpavg.marital.age.10 + spat.grpavg.mother.circum.fac_yes.10 + spat.grpavg.religion_christian.10 + spat.grpavg.hh.head.sex_female.10 + spat.grpavg.med.help.distance.fac_big_problem.10, effect = "twoways", model = "within", index = c("hh.id", "birth.year.fac"))

pooled.exogen.only.results.20 <- regress.data.10$plm(circum ~ governorate + birth.year.fac + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + order + I(order^2) + spat.grpavg.urban.rural_urban.20 + spat.grpavg.wealth.index.2_rich.20 + spat.grpavg.educ.lvl_primary.20 + spat.grpavg.educ.lvl_secondary.20 + spat.grpavg.educ.lvl_higher.20 + spat.grpavg.marital.age.20 + spat.grpavg.mother.circum.fac_yes.20 + spat.grpavg.religion_christian.20 + spat.grpavg.hh.head.sex_female.20 + spat.grpavg.med.help.distance.fac_big_problem.20, effect = "individual", model = "pooling", index = c("hh.id", "order.fac"))

within.exogen.only.results.20 <- regress.data.20$plm(circum ~ birth.year.fac + spat.grpavg.urban.rural_urban.20 + spat.grpavg.wealth.index.2_rich.20 + spat.grpavg.educ.lvl_primary.20 + spat.grpavg.educ.lvl_secondary.20 + spat.grpavg.educ.lvl_higher.20 + spat.grpavg.marital.age.20 + spat.grpavg.mother.circum.fac_yes.20 + spat.grpavg.religion_christian.20 + spat.grpavg.hh.head.sex_female.20 + spat.grpavg.med.help.distance.fac_big_problem.20, effect = "twoways", model = "within", index = c("hh.id", "order.fac"))

pooled.exogen.only.results.10_20 <- regress.data.20$plm(circum ~ governorate + birth.year.fac + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + order + I(order^2) + spat.grpavg.urban.rural_urban.10_20 + spat.grpavg.wealth.index.2_rich.10_20 + spat.grpavg.educ.lvl_primary.10_20 + spat.grpavg.educ.lvl_secondary.10_20 + spat.grpavg.educ.lvl_higher.10_20 + spat.grpavg.marital.age.10_20 + spat.grpavg.mother.circum.fac_yes.10_20 + spat.grpavg.religion_christian.10_20 + spat.grpavg.hh.head.sex_female.10_20 + spat.grpavg.med.help.distance.fac_big_problem.10_20, effect = "individual", model = "pooling", index = c("hh.id", "order.fac"))

within.exogen.only.results.10_20 <- regress.data.10$plm(circum ~ birth.year.fac + spat.grpavg.urban.rural_urban.10_20 + spat.grpavg.wealth.index.2_rich.10_20 + spat.grpavg.educ.lvl_primary.10_20 + spat.grpavg.educ.lvl_secondary.10_20 + spat.grpavg.educ.lvl_higher.10_20 + spat.grpavg.marital.age.10_20 + spat.grpavg.mother.circum.fac_yes.10_20 + spat.grpavg.religion_christian.10_20 + spat.grpavg.hh.head.sex_female.10_20 + spat.grpavg.med.help.distance.fac_big_problem.10_20, effect = "twoways", model = "within", index = c("hh.id", "order.fac"))

#within.exogen.only.results <- regress.data$plm(circum ~ governorate + birth.year.fac + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + order + I(order^2) + grpavg.urban.rural_urban + grpavg.wealth.index.2_rich + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.help.distance.fac_big_problem, effect = "twoways", model = "within", index = c("hh.id", "order.fac"))

