source("BaseFgmDataRef.R")
source("DaughterFgmDataRef.R")

#x <- DaughterFgmData$new(spdf = spdf, cluster.info = clust.info, youngest.cohort = 2000, other.grpavg.controls = "med.circum")
x <- DaughterFgmData$new(spdf = spdf, cluster.info = clust.info, other.grpavg.controls = c("med.circum", "circum"))
#x <- DaughterFgmData$new(spdf = y$spdf, cluster.info = clust.info, other.grpavg.controls = "med.circum")
y <- x$copy()
y$rm.by.res.years(10)
y$generate.reg.means(exclude.self = FALSE)
#y$spdf@data$grpavg.urban.rural_rural_neg <- - y$spdf@data$grpavg.urban.rural_rural
y$rm.by.grp.size(24)
y$rm.duplicate(c("governorate", "birth.year.fac"))

x$rm.by.res.years(10)
x$generate.reg.means(exclude.self = TRUE)
#x$spdf@data$grpavg.urban.rural_rural_neg <- - x$spdf@data$grpavg.urban.rural_rural
x$rm.by.grp.size(24)

#r <- x$regress("has.or.intends.circum", TRUE)

#r.panel <- x$regress.panel("has.or.intends.circum")
#r.panel <- x$regress.panel("circum")
#r.panel$summary()

# To show that some grpavg don't effect grpavg.circum
r0 <- y$lm(grpavg.circum ~ governorate + birth.year.fac + grpavg.wealth.index.2_rich + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female, gen.vcov = TRUE)

# pooled model 1
r1 <- x$lm(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.wealth.index.2_rich + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female, gen.vcov = TRUE)

r1.instr.1 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.wealth.index.2_rich + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.circum | grpavg.urban.rural_urban + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.wealth.index.2_rich + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female, gen.vcov = TRUE)

r1.instr.2 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.circum | grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex, gen.vcov = TRUE)

r1.instr.3 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.circum | grpavg.urban.rural_urban + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex, gen.vcov = TRUE)

# panel model 1
r2 <- x$plm(circum ~ birth.year.fac + grpavg.wealth.index.2_rich + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.1 <- x$plm(circum ~ birth.year.fac + grpavg.wealth.index.2_rich +  grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.2 <- x$plm(circum ~ birth.year.fac + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r2.instr.3 <- x$plm(circum ~ birth.year.fac + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

# pooled model 2
r3 <- x$lm(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.circum, gen.vcov = TRUE)

r3.instr.1 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.circum + grpavg.circum | birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.circum, gen.vcov = TRUE)

r3.instr.2 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.circum + grpavg.med.circum | grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.mother.circum.fac_yes + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.med.circum, gen.vcov = TRUE)

r3.instr.3 <- x$ivreg(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.circum + grpavg.med.circum | grpavg.urban.rural_urban + birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.med.circum, gen.vcov = TRUE)

# panel model 2
r4 <- x$plm(circum ~ birth.year.fac + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.circum, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.1 <- x$plm(circum ~ birth.year.fac + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.circum + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.2 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.mother.circum.fac_yes, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

r4.instr.3 <- x$plm(circum ~ birth.year.fac + grpavg.med.circum + grpavg.circum | . - grpavg.circum + grpavg.urban.rural_urban, index = c("hh.id", "order.fac"), model = "within", effect = "individual", gen.vcov = TRUE)

