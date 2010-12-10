source("BaseFgmDataRef.R")
source("DaughterFgmDataRef.R")

#x <- DaughterFgmData$new(spdf = spdf, cluster.info = clust.info, youngest.cohort = 2000, other.grpavg.controls = "med.circum")
#x <- DaughterFgmData$new(spdf = spdf, cluster.info = clust.info, other.grpavg.controls = c("med.circum", "circum"))
#x <- DaughterFgmData$new(spdf = y$spdf, cluster.info = clust.info, other.grpavg.controls = "med.circum")
x$generate.reg.means(0, exclude.self = TRUE)
#x$rm.by.res.years()
x$rm.by.grp.size(24)

#r <- x$regress("has.or.intends.circum", TRUE)

#r.panel <- x$regress.panel("has.or.intends.circum")
r.panel <- x$regress.panel("circum")
r.panel$summary()

# To show that some grpavg don't effect grpavg.circum
y$rm.duplicate(c("governorate", "birth.year.fac"))
r0 <- lm(grpavg.circum ~ governorate + birth.year.fac + grpavg.wealth.index.2_rich + grpavg.urban.rural_rural + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female, data = y$spdf@data)
v0 <- vcov(r0)
coeftest(r0, vcov = v0)

# pooled model 1
r1 <- lm(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.wealth.index.2_rich + grpavg.urban.rural_rural + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female, data = x$spdf@data)
v1 <- vcov(r1)
coeftest(r1, vcov = v1)

# panel model 1
r2 <- plm(circum ~ birth.year.fac + grpavg.wealth.index.2_rich + grpavg.urban.rural_rural + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female, data = x$spdf@data, index = c("hh.id", "order.fac"), model = "within", effects = "individual")
v2 <- vcov(r2)
coeftest(r2, vcov = v2)

# pooled model 1
r1 <- lm(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.hh.head.sex_female, data = x$spdf@data)
v1 <- vcov(r1)
coeftest(r1, vcov = v1)

# panel model 1
r2 <- plm(circum ~ birth.year.fac + grpavg.marital.age + grpavg.mother.circum.fac_yes, data = x$spdf@data, index = c("hh.id", "order.fac"), model = "within", effects = "individual")
v2 <- vcov(r2)
coeftest(r2, vcov = v2)

# pooled model 2
r3 <- lm(circum ~ birth.year.fac + governorate + wealth.index.2 + urban.rural + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + grpavg.urban.rural_rural + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.circum, data = x$spdf@data)
v3 <- vcov(r3)
coeftest(r3, vcov = v3)

# panel model 2
r4 <- plm(circum ~ birth.year.fac + grpavg.urban.rural_rural + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.circum, data = x$spdf@data, index = c("hh.id", "order.fac"), model = "within", effects = "individual")
v4 <- vcov(r4)
coeftest(r4, vcov = v4)
