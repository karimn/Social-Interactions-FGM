source("BaseFgmDataRef.R")
source("DaughterFgmDataRef.R")

original.data <- DaughterFgmData$new(ir.file = '~/Data/EDHS/2008/EGIR5AFL.DTA', br.file = '~/Data/EDHS/2008/EGBR5AFL.DTA', gps.file = '~/Data/EDHS/2008/EGGE5AFF.dbf', other.grpavg.controls = c("med.circum", "circum"))
original.data$spdf@data$urban.rural <- relevel(original.data$spdf@data$urban.rural, ref = "rural")
original.data$spdf@data$med.help.distance.fac <- relevel(original.data$spdf@data$med.help.distance.fac, ref = "not big problem")
original.data$spdf@data$med.help.money.fac <- relevel(original.data$spdf@data$med.help.money.fac, ref = "not big problem")
original.data$spdf@data$med.help.transportation.fac <- relevel(original.data$spdf@data$med.help.transportation.fac, ref = "not big problem")
#original.data <- DaughterFgmData$new(spdf = spdf, cluster.info = clust.info, other.grpavg.controls = c("med.circum", "circum"))
#x <- DaughterFgmData$new(spdf = spdf, cluster.info = clust.info, youngest.cohort = 2000, other.grpavg.controls = "med.circum")
#x <- DaughterFgmData$new(spdf = spdf, cluster.info = clust.info, other.grpavg.controls = c("med.circum", "circum"))
#x <- DaughterFgmData$new(spdf = y$spdf, cluster.info = clust.info, other.grpavg.controls = "med.circum")
x <- original.data$copy()
y <- original.data$copy()

#y.urbanrural <- original.data$copy()
#y.religion <- original.data$copy()
#y.wealth <- original.data$copy()
#z <- original.data$copy()

y$rm.by.res.years(10)
#y$generate.reg.means(exclude.self = FALSE, other.network.reg = "urban.rural")
y$generate.reg.means(exclude.self = FALSE)
y$rm.by.grp.size(24)
y$rm.duplicate(c("governorate", "birth.year.fac"))
y$spdf@data$grpavg.educ.lvl_primary_neg <- - y$spdf@data$grpavg.educ.lvl_primary
y$spdf@data$grpavg.educ.lvl_secondary_neg <- - y$spdf@data$grpavg.educ.lvl_secondary
y$spdf@data$grpavg.mother.circum.fac_yes_neg <- - y$spdf@data$grpavg.mother.circum.fac_yes

#y.urbanrural$rm.by.res.years(10)
#y.urbanruralgenerate.reg.means(exclude.self = FALSE, other.network.reg = "urban.rural")
#y.urbanrural$generate.reg.means(exclude.self = FALSE)
#y.urbanrural$rm.by.grp.size(24)
#y.urbanrural$rm.duplicate(c("governorate", "birth.year.fac", "urban.rural"))

#y.religion$rm.by.res.years(10)
#y.religiongenerate.reg.means(exclude.self = FALSE, other.network.reg = "urban.rural")
#y.religion$generate.reg.means(exclude.self = FALSE)
#y.religion$rm.by.grp.size(24)
#y.religion$rm.duplicate(c("governorate", "birth.year.fac", "religion"))

#y.wealth$rm.by.res.years(10)
#y.wealthgenerate.reg.means(exclude.self = FALSE, other.network.reg = "urban.rural")
#y.wealth$generate.reg.means(exclude.self = FALSE)
#y.wealth$rm.by.grp.size(24)
#y.wealth$rm.duplicate(c("governorate", "birth.year.fac", "wealth.index.2"))

x$generate.delivery.means()
x$rm.by.res.years(10)
x$generate.reg.means(exclude.self = TRUE)
x$rm.by.grp.size(24)
x$spdf@data$grpavg.educ.lvl_primary_neg <- - x$spdf@data$grpavg.educ.lvl_primary
x$spdf@data$grpavg.educ.lvl_secondary_neg <- - x$spdf@data$grpavg.educ.lvl_secondary
x$spdf@data$grpavg.mother.circum.fac_yes_neg <- - x$spdf@data$grpavg.mother.circum.fac_yes

#x.urbanrural$rm.by.res.years(10)
#x.urbanrural$generate.reg.means(exclude.self = TRUE, other.network.reg = "urban.rural")
#x.urbanrural$rm.by.grp.size(24)
#x.urbanrural$spdf@data$grpavg.educ.lvl_primary_neg <- - x.urbanrural$spdf@data$grpavg.educ.lvl_primary
#x.urbanrural$spdf@data$grpavg.educ.lvl_secondary_neg <- - x.urbanrural$spdf@data$grpavg.educ.lvl_secondary
#x.urbanrural$spdf@data$grpavg.mother.circum.fac_yes_neg <- - x.urbanrural$spdf@data$grpavg.mother.circum.fac_yes
#
#x.religion$rm.by.res.years(10)
#x.religion$generate.reg.means(exclude.self = TRUE, other.network.reg = "religion")
#x.religion$rm.by.grp.size(24)
#x.religion$spdf@data$grpavg.educ.lvl_primary_neg <- - x.religion$spdf@data$grpavg.educ.lvl_primary
#x.religion$spdf@data$grpavg.educ.lvl_secondary_neg <- - x.religion$spdf@data$grpavg.educ.lvl_secondary
#x.religion$spdf@data$grpavg.mother.circum.fac_yes_neg <- - x.religion$spdf@data$grpavg.mother.circum.fac_yes
#
#x.wealth$rm.by.res.years(10)
#x.wealth$generate.reg.means(exclude.self = TRUE, other.network.reg = "wealth.index.2")
#x.wealth$rm.by.grp.size(24)
#x.wealth$spdf@data$grpavg.educ.lvl_primary_neg <- - x.wealth$spdf@data$grpavg.educ.lvl_primary
#x.wealth$spdf@data$grpavg.educ.lvl_secondary_neg <- - x.wealth$spdf@data$grpavg.educ.lvl_secondary
#x.wealth$spdf@data$grpavg.mother.circum.fac_yes_neg <- - x.wealth$spdf@data$grpavg.mother.circum.fac_yes

#r <- x$regress("has.or.intends.circum", TRUE)

#r.panel <- x$regress.panel("has.or.intends.circum")
#r.panel <- x$regress.panel("circum")
#r.panel$summary()

# To show that some grpavg don't effect grpavg.circum

r0.1 <- y$lm(grpavg.circum ~ governorate + birth.year.fac + grpavg.urban.rural_urban + grpavg.wealth.index.2_rich + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.help.distance.fac_big_problem, gen.vcov = TRUE)

r0.2 <- y$plm(grpavg.circum ~ grpavg.urban.rural_urban + grpavg.wealth.index.2_rich + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.help.distance.fac_big_problem, effect = "twoways", model = "within", index = c("governorate", "birth.year.fac"), gen.vcov = TRUE)

# pooled model 1

#r1.instr.5 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural * med.help.distance.fac + grpavg.circum + order + I(order^2) | grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg + birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural * med.help.distance.fac + grpavg.religion_christian + order + I(order^2), gen.vcov = TRUE)

r1.instr.5.pooled <- x$plm(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural * med.help.distance.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg, effect = "individual", model = "pooling", index = c("hh.id", "order.fac"), gen.vcov = TRUE)


# panel model 1

r2.instr.5 <- x$plm(circum ~ birth.year.fac + order + I(order^2) + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.6 <- x$plm(circum ~ birth.year.fac + order + I(order^2) + grpavg.circum + grpavg.circum : urban.rural | . - grpavg.circum - grpavg.circum : urban.rural + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg + grpavg.urban.rural_urban : urban.rural + grpavg.mother.circum.fac_yes : urban.rural + grpavg.educ.lvl_primary_neg : urban.rural + grpavg.educ.lvl_secondary_neg : urban.rural, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.7 <- x$plm(circum ~ birth.year.fac + order + I(order^2) + grpavg.circum + grpavg.circum : discuss.circum.fac | . - grpavg.circum - grpavg.circum : discuss.circum.fac + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg + grpavg.urban.rural_urban : discuss.circum.fac + grpavg.mother.circum.fac_yes : discuss.circum.fac + grpavg.educ.lvl_primary_neg : discuss.circum.fac + grpavg.educ.lvl_secondary_neg : discuss.circum.fac, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.8 <- x$plm(circum ~ birth.year.fac + order + I(order^2) + grpavg.circum + grpavg.circum : received.info.circum.fac | . - grpavg.circum - grpavg.circum : received.info.circum.fac + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg + grpavg.urban.rural_urban : received.info.circum.fac + grpavg.mother.circum.fac_yes : received.info.circum.fac + grpavg.educ.lvl_primary_neg : received.info.circum.fac + grpavg.educ.lvl_secondary_neg : received.info.circum.fac, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

# pooled model 2

#r3.instr.5 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural * med.help.distance.fac + grpavg.circum + grpavg.med.circum + order + I(order^2) | grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural * med.help.distance.fac + grpavg.med.circum + order + I(order^2), gen.vcov = TRUE)

r3.instr.5.pooled <- x$plm(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural * med.help.distance.fac + grpavg.circum + grpavg.med.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, effect = "individual", model = "pooling", index = c("hh.id", "order.fac"), gen.vcov = TRUE)

r3.instr.6.pooled <- x$plm(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural * med.help.distance.fac + grpavg.circum + grpavg.med.circum + grpavg.delivered.by.daya_yes + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, effect = "individual", model = "pooling", index = c("hh.id", "order.fac"), gen.vcov = TRUE)

# panel model 2

r4.instr.5 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.7 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum * urban.rural + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.8 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : med.help.distance.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

#r4.instr.9 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : urban.rural + grpavg.med.circum : med.help.distance.fac + grpavg.circum | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.10 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : urban.rural + grpavg.med.circum : med.help.distance.fac + grpavg.med.circum : urban.rural : med.help.distance.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.11 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : discuss.circum.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.12 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : urban.rural + grpavg.med.circum : discuss.circum.fac + grpavg.med.circum : urban.rural : discuss.circum.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.13 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : received.info.circum.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.14 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : urban.rural + grpavg.med.circum : received.info.circum.fac + grpavg.med.circum : urban.rural : received.info.circum.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.15 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : med.help.money.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.16 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : urban.rural + grpavg.med.circum : med.help.money.fac + grpavg.med.circum : urban.rural : med.help.money.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.17 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : med.help.transportation.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.18 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : urban.rural + grpavg.med.circum : med.help.transportation.fac + grpavg.med.circum : urban.rural : med.help.transportation.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.19 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum * wealth.index.2 + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.20 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : wealth.index.2 + grpavg.med.circum : med.help.distance.fac + grpavg.med.circum : wealth.index.2 : med.help.distance.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.21 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : wealth.index.2 + grpavg.med.circum : med.help.money.fac + grpavg.med.circum : wealth.index.2 : med.help.money.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.22 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : wealth.index.2 + grpavg.med.circum : med.help.transportation.fac + grpavg.med.circum : wealth.index.2 : med.help.transportation.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.23 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum * religion + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.24 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : religion + grpavg.med.circum : med.help.distance.fac + grpavg.med.circum : religion : med.help.distance.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.25 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : religion + grpavg.med.circum : med.help.money.fac + grpavg.med.circum : religion : med.help.money.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.26 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : religion + grpavg.med.circum : med.help.transportation.fac + grpavg.med.circum : religion : med.help.transportation.fac + grpavg.circum + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.27 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.circum + grpavg.delivered.by.daya_yes + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.28 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum * urban.rural + grpavg.circum + grpavg.delivered.by.daya_yes + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.29 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : med.help.distance.fac + grpavg.circum + grpavg.delivered.by.daya_yes + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.30 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : urban.rural + grpavg.med.circum : med.help.distance.fac + grpavg.med.circum : urban.rural : med.help.distance.fac + grpavg.circum + grpavg.delivered.by.daya_yes + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.31 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : med.help.money.fac + grpavg.circum + grpavg.delivered.by.daya_yes + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.32 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : urban.rural + grpavg.med.circum : med.help.money.fac + grpavg.med.circum : urban.rural : med.help.money.fac + grpavg.circum + grpavg.delivered.by.daya_yes + order + I(order^2) | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

# No social effects regression; direct only

#r5 <- x$lm(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural * med.help.distance.fac + order + I(order^2), gen.vcov = TRUE)

r5.pooled <- x$plm(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural * med.help.distance.fac + order + I(order^2), index = c("hh.id", "order.fac"), effect = "individual", model = "pooling", gen.vcov = TRUE)

#r6 <- x$lm(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + religion + hh.head.sex + urban.rural * med.help.distance.fac + order + I(order^2), gen.vcov = TRUE)

r6.pooled <- x$plm(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + religion + hh.head.sex + urban.rural * med.help.distance.fac + order + I(order^2), effect = "individual", model = "pooling", index = c("hh.id", "order.fac"), gen.vcov = TRUE)

r7.pooled <- x$plm(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + religion + hh.head.sex + urban.rural * med.help.distance.fac + discuss.circum.fac + received.info.circum.fac + order + I(order^2), effect = "individual", model = "pooling", index = c("hh.id", "order.fac"), gen.vcov = TRUE)

r8.pooled <- x$plm(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + religion + hh.head.sex + urban.rural * med.help.distance.fac + received.info.circum.fac + order + I(order^2), effect = "individual", model = "pooling", index = c("hh.id", "order.fac"), gen.vcov = TRUE)

spdf <- x$spdf

save(list = c(ls(pattern = "r\\d(\\.instr\\.\\d{1,2})?(\\.pooled)?$"), "x", "y", "spdf", "r0.1", "r0.2", "r0.neg"), file = "new_results.RData")
