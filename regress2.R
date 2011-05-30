source("BaseFgmDataRef.R")
source("DaughterFgmDataRef.R")

original.data <- DaughterFgmData$new(ir.file = '~/Data/EDHS/2008/EGIR5AFL.DTA', br.file = '~/Data/EDHS/2008/EGBR5AFL.DTA', gps.file = '~/Data/EDHS/2008/EGGE5AFF.dbf', other.grpavg.controls = c("med.circum", "circum"))
#original.data <- DaughterFgmData$new(spdf = spdf, cluster.info = clust.info, other.grpavg.controls = c("med.circum", "circum"))
#x <- DaughterFgmData$new(spdf = spdf, cluster.info = clust.info, youngest.cohort = 2000, other.grpavg.controls = "med.circum")
#x <- DaughterFgmData$new(spdf = spdf, cluster.info = clust.info, other.grpavg.controls = c("med.circum", "circum"))
#x <- DaughterFgmData$new(spdf = y$spdf, cluster.info = clust.info, other.grpavg.controls = "med.circum")
x <- original.data$copy()
y <- original.data$copy()

y.urbanrural <- original.data$copy()
y.religion <- original.data$copy()
y.wealth <- original.data$copy()
#z <- original.data$copy()

x.urbanrural <- original.data$copy()
x.religion <- original.data$copy()
x.wealth <- original.data$copy()

y$rm.by.res.years(10)
#y$generate.reg.means(exclude.self = FALSE, other.network.reg = "urban.rural")
y$generate.reg.means(exclude.self = FALSE)
y$rm.by.grp.size(24)
y$rm.duplicate(c("governorate", "birth.year.fac"))
y$spdf@data$grpavg.educ.lvl_primary_neg <- - y$spdf@data$grpavg.educ.lvl_primary
y$spdf@data$grpavg.educ.lvl_secondary_neg <- - y$spdf@data$grpavg.educ.lvl_secondary
y$spdf@data$grpavg.mother.circum.fac_yes_neg <- - y$spdf@data$grpavg.mother.circum.fac_yes

y.urbanrural$rm.by.res.years(10)
#y.urbanruralgenerate.reg.means(exclude.self = FALSE, other.network.reg = "urban.rural")
y.urbanrural$generate.reg.means(exclude.self = FALSE)
y.urbanrural$rm.by.grp.size(24)
y.urbanrural$rm.duplicate(c("governorate", "birth.year.fac", "urban.rural"))

y.religion$rm.by.res.years(10)
#y.religiongenerate.reg.means(exclude.self = FALSE, other.network.reg = "urban.rural")
y.religion$generate.reg.means(exclude.self = FALSE)
y.religion$rm.by.grp.size(24)
y.religion$rm.duplicate(c("governorate", "birth.year.fac", "religion"))

y.wealth$rm.by.res.years(10)
#y.wealthgenerate.reg.means(exclude.self = FALSE, other.network.reg = "urban.rural")
y.wealth$generate.reg.means(exclude.self = FALSE)
y.wealth$rm.by.grp.size(24)
y.wealth$rm.duplicate(c("governorate", "birth.year.fac", "wealth.index.2"))

x$rm.by.res.years(10)
x$generate.reg.means(exclude.self = TRUE)
x$rm.by.grp.size(24)
x$spdf@data$grpavg.educ.lvl_primary_neg <- - x$spdf@data$grpavg.educ.lvl_primary
x$spdf@data$grpavg.educ.lvl_secondary_neg <- - x$spdf@data$grpavg.educ.lvl_secondary
x$spdf@data$grpavg.mother.circum.fac_yes_neg <- - x$spdf@data$grpavg.mother.circum.fac_yes

x.urbanrural$rm.by.res.years(10)
x.urbanrural$generate.reg.means(exclude.self = TRUE, other.network.reg = "urban.rural")
x.urbanrural$rm.by.grp.size(24)
x.urbanrural$spdf@data$grpavg.educ.lvl_primary_neg <- - x.urbanrural$spdf@data$grpavg.educ.lvl_primary
x.urbanrural$spdf@data$grpavg.educ.lvl_secondary_neg <- - x.urbanrural$spdf@data$grpavg.educ.lvl_secondary
x.urbanrural$spdf@data$grpavg.mother.circum.fac_yes_neg <- - x.urbanrural$spdf@data$grpavg.mother.circum.fac_yes

x.religion$rm.by.res.years(10)
x.religion$generate.reg.means(exclude.self = TRUE, other.network.reg = "religion")
x.religion$rm.by.grp.size(24)
x.religion$spdf@data$grpavg.educ.lvl_primary_neg <- - x.religion$spdf@data$grpavg.educ.lvl_primary
x.religion$spdf@data$grpavg.educ.lvl_secondary_neg <- - x.religion$spdf@data$grpavg.educ.lvl_secondary
x.religion$spdf@data$grpavg.mother.circum.fac_yes_neg <- - x.religion$spdf@data$grpavg.mother.circum.fac_yes

x.wealth$rm.by.res.years(10)
x.wealth$generate.reg.means(exclude.self = TRUE, other.network.reg = "wealth.index.2")
x.wealth$rm.by.grp.size(24)
x.wealth$spdf@data$grpavg.educ.lvl_primary_neg <- - x.wealth$spdf@data$grpavg.educ.lvl_primary
x.wealth$spdf@data$grpavg.educ.lvl_secondary_neg <- - x.wealth$spdf@data$grpavg.educ.lvl_secondary
x.wealth$spdf@data$grpavg.mother.circum.fac_yes_neg <- - x.wealth$spdf@data$grpavg.mother.circum.fac_yes

#r <- x$regress("has.or.intends.circum", TRUE)

#r.panel <- x$regress.panel("has.or.intends.circum")
#r.panel <- x$regress.panel("circum")
#r.panel$summary()

# To show that some grpavg don't effect grpavg.circum
#r0 <- y$lm(grpavg.circum ~ governorate + birth.year.fac + grpavg.wealth.index.2_rich + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female, gen.vcov = TRUE)
r0 <- y$lm(grpavg.circum ~ governorate + birth.year.fac + grpavg.urban.rural_urban + grpavg.wealth.index.2_rich + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female, gen.vcov = TRUE)

r0.1 <- y$lm(grpavg.circum ~ governorate + birth.year.fac + grpavg.urban.rural_urban + grpavg.wealth.index.2_rich + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.help.distance.fac_big_problem, gen.vcov = TRUE)

r0.2 <- y.urbanrural$lm(grpavg.circum ~ governorate + birth.year.fac + grpavg.wealth.index.2_rich + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.help.distance.fac_big_problem, gen.vcov = TRUE)

r0.3 <- y.religion$lm(grpavg.circum ~ governorate + birth.year.fac + grpavg.urban.rural_urban + grpavg.wealth.index.2_rich + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.hh.head.sex_female + grpavg.med.help.distance.fac_big_problem, gen.vcov = TRUE)

r0.4 <- y.wealth$lm(grpavg.circum ~ governorate + birth.year.fac + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.help.distance.fac_big_problem, gen.vcov = TRUE)

r0.neg <- y$lm(grpavg.circum ~ governorate + birth.year.fac + grpavg.urban.rural_urban + grpavg.wealth.index.2_rich + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female, gen.vcov = TRUE)

# pooled model 1
r1 <- x$lm(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.wealth.index.2_rich + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female, gen.vcov = TRUE)

r1.instr.1 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.wealth.index.2_rich + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.circum | grpavg.urban.rural_urban + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.wealth.index.2_rich + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female, gen.vcov = TRUE)

r1.instr.2 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.circum | grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex, gen.vcov = TRUE)

r1.instr.3 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.circum | grpavg.urban.rural_urban + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex, gen.vcov = TRUE)

r1.instr.4 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.circum | grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex, gen.vcov = TRUE)

r1.instr.5 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.circum | grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.religion_christian, gen.vcov = TRUE)

r1.instr.6 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.circum | grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.religion_christian, gen.vcov = TRUE)

r1.instr.7 <- x.urbanrural$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.circum | grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.religion_christian, gen.vcov = TRUE)

r1.instr.8 <- x.religion$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.circum | grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.religion_christian, gen.vcov = TRUE)

r1.instr.9 <- x.wealth$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.circum | grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.religion_christian, gen.vcov = TRUE)

# panel model 1
r2 <- x$plm(circum ~ birth.year.fac + grpavg.wealth.index.2_rich + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.1 <- x$plm(circum ~ birth.year.fac + grpavg.wealth.index.2_rich +  grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.2 <- x$plm(circum ~ birth.year.fac + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.3 <- x$plm(circum ~ birth.year.fac + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.4 <- x$plm(circum ~ birth.year.fac + grpavg.circum | . - grpavg.circum + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.5 <- x$plm(circum ~ birth.year.fac + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.6 <- x$plm(circum ~ birth.year.fac + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.7 <- x.urbanrural$plm(circum ~ birth.year.fac + grpavg.circum | . - grpavg.circum + grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.8 <- x.religion$plm(circum ~ birth.year.fac + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.9 <- x.wealth$plm(circum ~ birth.year.fac + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

# pooled model 2
r3 <- x$lm(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.circum, gen.vcov = TRUE)

r3.instr.1 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.circum + grpavg.circum | birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.circum, gen.vcov = TRUE)

r3.instr.2 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.circum + grpavg.med.circum | grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.med.circum, gen.vcov = TRUE)

r3.instr.3 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.circum + grpavg.med.circum | grpavg.urban.rural_urban + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.med.circum, gen.vcov = TRUE)

r3.instr.4 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.circum + grpavg.med.circum | grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.med.circum, gen.vcov = TRUE)

r3.instr.6 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.circum + grpavg.med.circum | grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.med.circum, gen.vcov = TRUE)

r3.instr.5 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.circum + grpavg.med.circum | grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.med.circum, gen.vcov = TRUE)

r3.instr.7 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.circum + urban.rural * grpavg.med.circum | grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + urban.rural * grpavg.med.circum, gen.vcov = TRUE)

r3.instr.8 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + grpavg.circum + med.help.distance.fac * grpavg.med.circum | grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + med.help.distance.fac * grpavg.med.circum, gen.vcov = TRUE)

r3.instr.9 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + med.help.distance.fac + grpavg.circum + med.help.distance.fac : grpavg.med.circum  + urban.rural * grpavg.med.circum| grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + med.help.distance.fac +  med.help.distance.fac * grpavg.med.circum + urban.rural * grpavg.med.circum, gen.vcov = TRUE)

r3.instr.10 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + med.help.distance.fac + grpavg.circum + med.help.distance.fac : grpavg.med.circum + urban.rural * grpavg.med.circum + med.help.distance.fac : urban.rural : grpavg.med.circum | grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + med.help.distance.fac +  med.help.distance.fac * grpavg.med.circum + urban.rural * grpavg.med.circum + med.help.distance.fac : urban.rural : grpavg.med.circum, gen.vcov = TRUE)

r3.instr.11 <- x.urbanrural$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.circum + grpavg.med.circum | grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.med.circum, gen.vcov = TRUE)

r3.instr.12 <- x.urbanrural$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + grpavg.circum + urban.rural * grpavg.med.circum | grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac + urban.rural * grpavg.med.circum, gen.vcov = TRUE)

r3.instr.13 <- x.urbanrural$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + grpavg.circum + med.help.distance.fac * grpavg.med.circum | grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + med.help.distance.fac * grpavg.med.circum, gen.vcov = TRUE)

r3.instr.14 <- x.urbanrural$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + med.help.distance.fac + grpavg.circum + med.help.distance.fac : grpavg.med.circum  + urban.rural * grpavg.med.circum| grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + med.help.distance.fac +  med.help.distance.fac * grpavg.med.circum + urban.rural * grpavg.med.circum, gen.vcov = TRUE)

r3.instr.15 <- x.urbanrural$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + med.help.distance.fac + grpavg.circum + med.help.distance.fac : grpavg.med.circum + urban.rural * grpavg.med.circum + med.help.distance.fac : urban.rural : grpavg.med.circum | grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + med.help.distance.fac +  med.help.distance.fac * grpavg.med.circum + urban.rural * grpavg.med.circum + med.help.distance.fac : urban.rural : grpavg.med.circum, gen.vcov = TRUE)

# panel model 2
r4 <- x$plm(circum ~ birth.year.fac + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.circum, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.1 <- x$plm(circum ~ birth.year.fac + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.circum + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.2 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.3 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.4 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.circum | . - grpavg.circum + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.6 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.circum | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.5 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.circum | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.educ.lvl_secondary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.7 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum * urban.rural + grpavg.circum | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.8 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : med.help.distance.fac + grpavg.circum | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.9 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : urban.rural + grpavg.med.circum : med.help.distance.fac + grpavg.circum | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.10 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.med.circum : urban.rural + grpavg.med.circum : med.help.distance.fac + grpavg.med.circum : urban.rural : med.help.distance.fac + grpavg.circum | . - grpavg.circum + grpavg.educ.lvl_primary_neg + grpavg.urban.rural_urban + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

# No social effects regression; direct only

r5 <- x$lm(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + med.help.distance.fac, gen.vcov = TRUE)

r6 <- x$lm(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + religion + hh.head.sex + med.help.distance.fac, gen.vcov = TRUE)

save(list = c(ls(pattern = "r\\d(\\.instr\\.\\d{1,2})?$"), "x", "y", "z", "spdf", "r0.1", "r0.neg"), file = "new_results.RData")
