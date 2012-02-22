source("Data.R")
source("DataCollection.R")
source("SpatialData.R")
source("BaseFgmDataRef.R")
source("DaughterFgmDataRef.R")
source("RegressionResults.R")

library(igraph)

system.time(original.data <- DaughterFgmData$new(ir.file = '~/Data/EDHS/2008/EGIR5AFL.DTA', br.file = '~/Data/EDHS/2008/EGBR5AFL.DTA', gps.file = '~/Data/EDHS/2008/EGGE5AFF.dbf', other.grpavg.controls = c("med.circum", "circum"))) #, skip.cleanup = T))
original.data$relevel("urban.rural", ref = "rural")
original.data$relevel("med.help.distance.fac", ref = "not big problem")
original.data$relevel("med.help.money.fac", ref = "not big problem")
original.data$relevel("med.help.transportation.fac", ref = "not big problem")

regress.data <- original.data$copy()

regress.data$calc.dist.matrix()
regress.data$calc.all.cohorts.dist.matrix()

del.regs <- c("delivery.location", "delivered.by.daya")
del.year.offset <- 12

# radii <- c(10, 20)
radii <- 10
coh.adj.mat <- regress.data$get.cohort.adj.matrix()

for (radius in radii) {
    print(system.time(regress.data$generate.reg.means.spatial(radius = radius, regs = del.regs, prefix = "del.spat.grpavg", postfix = as.character(radius), use.all.cohorts.data = TRUE, year.offset = del.year.offset)))
    print(system.time(regress.data$generate.reg.means.spatial(radius = radius, postfix = as.character(radius))))
    print(system.time(regress.data$generate.reg.means.spatial(radius = radius, prefix = "spat.intran.grpavg", postfix = as.character(radius), degree = 2)))

    spat.adj.mat <- regress.data$get.spatial.adj.matrix(radius = radius)

    spat.only.graph <- graph.adjacency(spat.adj.mat)
    spat.coh.graph <- graph.adjacency(spat.adj.mat * coh.adj.mat)

    regress.data$spatial.data$spat.only.graph.cluster <- clusters(spat.only.graph)$membership + 1
    regress.data$spatial.data$spat.coh.graph.cluster <- clusters(spat.coh.graph)$membership + 1
}

#regress.data$rm.by.res.years(10)

#regs.to.average <- c(regress.data$individual.controls, regress.data$other.grpavg.controls)

#system.time(regress.data$generate.reg.means.spatial(radius = 20, inner.radius = 10, postfix = "10_20", exclude.self = TRUE))

#regress.data$spatial.data@data$grpavg.educ.lvl_primary_neg <- - regress.data$spatial.data@data$grpavg.educ.lvl_primary
#regress.data$spatial.data@data$grpavg.educ.lvl_secondary_neg <- - regress.data$spatial.data@data$grpavg.educ.lvl_secondary
#regress.data$spatial.data@data$grpavg.mother.circum.fac_yes_neg <- - regress.data$spatial.data@data$grpavg.mother.circum.fac_yes

# regress.data.10 <- regress.data$copy()
# regress.data.10$subset(grp.size.10 >= 10)
# 
# regress.data.20 <- regress.data$copy()
# regress.data.20$subset(grp.size.20 >= 20)
# 
# regress.data.10_20 <- regress.data$copy()
# regress.data.10_20$subset(grp.size.10_20 >= 10)

get.grpavg.regs <- function(data, radius, weighted = FALSE, spat = TRUE, intran = FALSE) {
    reg.pattern <- sprintf("%s%sgrpavg\\.(?!med\\.circum|circum).+%d%s$", 
                           if (spat) "spat." else "",
                           if (intran) "intran." else "",
                           radius,
                           if (weighted) ".wt" else "")
    return(grep(reg.pattern, data$names, value = TRUE, perl = TRUE))
}

hh.regs <- c("governorate", "wealth.index.2", "educ.lvl", "marital.age", "mother.circum.fac", "religion", "hh.head.sex", "urban.rural", "med.help.distance.fac", "n.ord")
daughter.regs <- c("birth.year.fac", "order")

# Test the strength of the intran averages on circum averages ################################################################################ 

relv.formula <- list()
relv.results <- list()
for (radius in radii) {
    relv.formula[[radius]] <- formula(sprintf("spat.grpavg.circum.%d ~ %s", radius, get.grpavg.regs(regress.data, radius = radius, intran = TRUE)))
    relv.results[[radius]] <- regress.data$lm(relv.formula[[radius]])
}

relv.formula.wt <- list()
relv.results.wt <- list()
for (radius in radii) {
    relv.formula[[radius]] <- formula(sprintf("spat.grpavg.circum.%d.wt ~ %s", radius, get.grpavg.regs(regress.data, radius = radius, intran = TRUE, weighted = TRUE)))
    relv.results.wt[[radius]] <- regress.data$lm(relv.formula.wt[[radius]])
}

# Med influencers ################################################################################  

pooled.med.results.10 <- regress.data$plm(formula(sprintf("med.circum ~ %s", paste(c(hh.regs, daughter.regs, "I(order^2)", "spat.grp.size.10"), collapse = " + "))), effect = "individual", model = "pooling", index = c("hh.id", "order.fac"), subset = circum == 1)

relv.med.results.10 <- regress.data$lm(formula(sprintf("spat.grpavg.med.circum.10 ~ %s", paste(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), collapse = " + "))))

# Using spat.intran.grpavg.circum as another instrument
relv.med.results.10.2 <- regress.data$lm(formula(sprintf("spat.grpavg.med.circum.10 ~ %s", paste(c(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), "spat.intran.grpavg.circum.10"), collapse = " + "))))

# Effect of exogenous averages on behavioral averages ################################################################################ 

pooled.avg.results.10 <- regress.data$plm(formula(sprintf("spat.grpavg.circum.10 ~ birth.year.fac + (%s) * spat.grp.size.10", 
                                                                  paste(get.grpavg.regs(regress.data, radius = 10), collapse = " + "))),
                                                  effect = "individual", model = "pooling", index = c("hh.id", "order.fac"))

# TODO a within version of the above regression

# Direct effects only ################################################################################      

pooled.dir.only.results.10 <- regress.data$plm(formula(sprintf("circum ~ %s", paste(c(hh.regs, daughter.regs, "I(order^2)", "spat.grpavg.grp.size.10"), collapse = " + "))), effect = "individual", model = "pooling", index = c("hh.id", "order.fac"), vcov.fun = NULL)

# Exogenous effects only ################################################################################   
pooled.exogen.only.results.10 <- regress.data$plm(formula(sprintf("circum ~ %s + (%s) * spat.grp.size.10", 
                                                                  paste(c(hh.regs, daughter.regs, "I(order^2)"), collapse = " + "),
                                                                  paste(get.grpavg.regs(regress.data, radius = 10), collapse = " + "))),
                                                  effect = "individual", model = "pooling", index = c("hh.id", "order.fac"))

within.exogen.only.results.10 <- regress.data$plm(formula(sprintf("circum ~ birth.year.fac + (%s) * spat.grp.size.10", paste(get.grpavg.regs(regress.data, radius = 10), collapse = " + "))),
                                                  effect = "twoways", model = "within", index = c("hh.id", "order.fac"))

#within.exogen.only.results.10.2 <- regress.data.10$plm(circum ~ spat.grpavg.urban.rural_urban.10 + spat.grpavg.wealth.index.2_rich.10 + spat.grpavg.educ.lvl_primary.10 + spat.grpavg.educ.lvl_secondary.10 + spat.grpavg.educ.lvl_higher.10 + spat.grpavg.marital.age.10 + spat.grpavg.mother.circum.fac_yes.10 + spat.grpavg.religion_christian.10 + spat.grpavg.hh.head.sex_female.10 + spat.grpavg.med.help.distance.fac_big_problem.10, effect = "twoways", model = "within", index = c("hh.id", "birth.year.fac"))

pooled.exogen.only.results.20 <- regress.data.10$plm(circum ~ governorate + birth.year.fac + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + order + I(order^2) + spat.grpavg.urban.rural_urban.20 + spat.grpavg.wealth.index.2_rich.20 + spat.grpavg.educ.lvl_primary.20 + spat.grpavg.educ.lvl_secondary.20 + spat.grpavg.educ.lvl_higher.20 + spat.grpavg.marital.age.20 + spat.grpavg.mother.circum.fac_yes.20 + spat.grpavg.religion_christian.20 + spat.grpavg.hh.head.sex_female.20 + spat.grpavg.med.help.distance.fac_big_problem.20, effect = "individual", model = "pooling", index = c("hh.id", "order.fac"))

within.exogen.only.results.20 <- regress.data.20$plm(circum ~ birth.year.fac + spat.grpavg.urban.rural_urban.20 + spat.grpavg.wealth.index.2_rich.20 + spat.grpavg.educ.lvl_primary.20 + spat.grpavg.educ.lvl_secondary.20 + spat.grpavg.educ.lvl_higher.20 + spat.grpavg.marital.age.20 + spat.grpavg.mother.circum.fac_yes.20 + spat.grpavg.religion_christian.20 + spat.grpavg.hh.head.sex_female.20 + spat.grpavg.med.help.distance.fac_big_problem.20, effect = "twoways", model = "within", index = c("hh.id", "order.fac"))

pooled.exogen.only.results.10_20 <- regress.data.20$plm(circum ~ governorate + birth.year.fac + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + order + I(order^2) + spat.grpavg.urban.rural_urban.10_20 + spat.grpavg.wealth.index.2_rich.10_20 + spat.grpavg.educ.lvl_primary.10_20 + spat.grpavg.educ.lvl_secondary.10_20 + spat.grpavg.educ.lvl_higher.10_20 + spat.grpavg.marital.age.10_20 + spat.grpavg.mother.circum.fac_yes.10_20 + spat.grpavg.religion_christian.10_20 + spat.grpavg.hh.head.sex_female.10_20 + spat.grpavg.med.help.distance.fac_big_problem.10_20, effect = "individual", model = "pooling", index = c("hh.id", "order.fac"))

within.exogen.only.results.10_20 <- regress.data.10$plm(circum ~ birth.year.fac + spat.grpavg.urban.rural_urban.10_20 + spat.grpavg.wealth.index.2_rich.10_20 + spat.grpavg.educ.lvl_primary.10_20 + spat.grpavg.educ.lvl_secondary.10_20 + spat.grpavg.educ.lvl_higher.10_20 + spat.grpavg.marital.age.10_20 + spat.grpavg.mother.circum.fac_yes.10_20 + spat.grpavg.religion_christian.10_20 + spat.grpavg.hh.head.sex_female.10_20 + spat.grpavg.med.help.distance.fac_big_problem.10_20, effect = "twoways", model = "within", index = c("hh.id", "order.fac"))

#within.exogen.only.results <- regress.data$plm(circum ~ governorate + birth.year.fac + wealth.index.2 + educ.lvl + marital.age + mother.circum.fac + religion + hh.head.sex + urban.rural + order + I(order^2) + grpavg.urban.rural_urban + grpavg.wealth.index.2_rich + grpavg.educ.lvl_primary + grpavg.educ.lvl_secondary + grpavg.educ.lvl_higher + grpavg.marital.age + grpavg.mother.circum.fac_yes + grpavg.religion_christian + grpavg.hh.head.sex_female + grpavg.med.help.distance.fac_big_problem, effect = "twoways", model = "within", index = c("hh.id", "order.fac"))

# Exogenous/Endogenous effects using IV ################################################################################   

pooled.se.results.10 <- regress.data$plm(formula(sprintf("circum ~ %s + (%s) * spat.grp.size.10 + spat.grpavg.circum.10 * spat.grp.size.10 | . - spat.grpavg.circum.10 * spat.grp.size.10 + (%s) * spat.grp.size.10", 
                                                         paste(c(hh.regs, daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), collapse = " + "))),
                                                  effect = "individual", model = "pooling", index = c("hh.id", "order.fac"), vcov.fun = NULL)

pooled.se.results.10.2 <- regress.data$plm(formula(sprintf("circum ~ %s + spat.grpavg.circum.10 * spat.grp.size.10 | . - spat.grpavg.circum.10 * spat.grp.size.10 + (%s) * spat.grp.size.10", 
                                                         paste(c(hh.regs, daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), collapse = " + "))),
                                                  effect = "individual", model = "pooling", index = c("hh.id", "order.fac"), vcov.fun = NULL)

within.se.results.10 <- regress.data$plm(formula(sprintf("circum ~ %s + (%s) * spat.grp.size.10 + spat.grpavg.circum.10 * spat.grp.size.10 | . - spat.grpavg.circum.10 * spat.grp.size.10 + (%s) * spat.grp.size.10", 
                                                         paste(c(daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), collapse = " + "))),
                                                  effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = NULL)

within.se.results.10.2 <- regress.data$plm(formula(sprintf("circum ~ %s + spat.grpavg.circum.10 * spat.grp.size.10 | . - spat.grpavg.circum.10 * spat.grp.size.10 + (%s) * spat.grp.size.10", 
                                                         paste(c(daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), collapse = " + "))),
                                                  effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = NULL)

pooled.se.med.results.10 <- regress.data$plm(formula(sprintf("circum ~ %s + (%s) * spat.grp.size.10 + spat.grpavg.med.circum.10 * spat.grp.size.10 + spat.grpavg.circum.10 * spat.grp.size.10 | . - spat.grpavg.circum.10 * spat.grp.size.10 + (%s) * spat.grp.size.10", 
                                                         paste(c(hh.regs, daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), collapse = " + "))),
                                                  effect = "individual", model = "pooling", index = c("hh.id", "order.fac"), vcov.fun = NULL)

pooled.se.med.results.10.2 <- regress.data$plm(formula(sprintf("circum ~ %s + spat.grpavg.med.circum.10 * spat.grp.size.10 + spat.grpavg.circum.10 * spat.grp.size.10 | . - spat.grpavg.circum.10 * spat.grp.size.10 + (%s) * spat.grp.size.10", 
                                                         paste(c(hh.regs, daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), collapse = " + "))),
                                                  effect = "individual", model = "pooling", index = c("hh.id", "order.fac"), vcov.fun = NULL)

within.se.med.results.10 <- regress.data$plm(formula(sprintf("circum ~ %s + (%s) * spat.grp.size.10 + spat.grpavg.med.circum.10 * spat.grp.size.10 + spat.grpavg.circum.10 * spat.grp.size.10 | . - spat.grpavg.circum.10 * spat.grp.size.10 + (%s) * spat.grp.size.10", 
                                                         paste(c(daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), collapse = " + "))),
                                                  effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = NULL)

within.se.med.results.10.2 <- regress.data$plm(formula(sprintf("circum ~ %s + spat.grpavg.med.circum.10 * spat.grp.size.10 + spat.grpavg.circum.10 * spat.grp.size.10 | . - spat.grpavg.circum.10 * spat.grp.size.10 + (%s) * spat.grp.size.10", 
                                                         paste(c(daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), collapse = " + "))),
                                                  effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = NULL)

within.se.med.results.10.3 <- regress.data$plm(formula(sprintf("circum ~ %s + spat.grpavg.med.circum.10 * spat.grpavg.grp.size.10 + spat.grpavg.circum.10 * spat.grpavg.grp.size.10 | . - spat.grpavg.med.circum.10 * spat.grpavg.grp.size.10 - spat.grpavg.circum.10 * spat.grpavg.grp.size.10 + (%s) * spat.grpavg.grp.size.10", 
                                                         paste(c(daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), collapse = " + "))),
                                                  effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = NULL)

# Using spat.intran.grpavg.circum as another instrument
within.se.med.results.10.4 <- regress.data$plm(formula(sprintf("circum ~ %s + spat.grpavg.med.circum.10 * spat.grpavg.grp.size.10 + spat.grpavg.circum.10 * spat.grpavg.grp.size.10 | . - spat.grpavg.med.circum.10 * spat.grpavg.grp.size.10 - spat.grpavg.circum.10 * spat.grpavg.grp.size.10 + (%s) * spat.grpavg.grp.size.10", 
                                                         paste(c(daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(c(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), "spat.intran.grpavg.circum.10"), collapse = " + "))),
                                                  effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = NULL)

within.se.med.results.10.5 <- regress.data$plm(formula(sprintf("circum ~ %s + spat.grpavg.med.circum.10 * spat.grp.size.10 + spat.grpavg.circum.10 * spat.grp.size.10 | . - spat.grpavg.circum.10 * spat.grp.size.10 + (%s) * spat.grp.size.10", 
                                                         paste(c(daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), collapse = " + "))),
                                                  effect = "individual", model = "within", index = c("cluster", "daughter.id"), vcov.fun = NULL)

within.se.med.results.10.6 <- regress.data$plm(formula(sprintf("circum ~ %s + spat.grpavg.med.circum.10 * spat.grp.size.10 + spat.grpavg.circum.10 * spat.grp.size.10 | . - spat.grpavg.med.circum.10 * spat.grp.size.10 - spat.grpavg.circum.10 * spat.grp.size.10 + (%s) * spat.grp.size.10", 
                                                         paste(c(daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), collapse = " + "))),
                                                  effect = "individual", model = "within", index = c("cluster", "daughter.id"), vcov.fun = NULL)

within.se.med.results.10.7 <- regress.data$plm(formula(sprintf("circum ~ %s + spat.grpavg.med.circum.10 * spat.grp.size.10 + spat.grpavg.circum.10 * spat.grp.size.10 | . - spat.grpavg.med.circum.10 * spat.grp.size.10 - spat.grpavg.circum.10 * spat.grp.size.10 + (%s) * spat.grp.size.10", 
                                                         paste(c(daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(c(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), "spat.intran.grpavg.circum.10"), collapse = " + "))),
                                                  effect = "individual", model = "within", index = c("cluster", "daughter.id"), vcov.fun = NULL)

within.se.med.results.10.8 <- regress.data$plm(formula(sprintf("circum ~ %s + spat.grpavg.med.circum.10 * spat.grp.size.10 + spat.grpavg.circum.10 * spat.grp.size.10 | . - spat.grpavg.circum.10 * spat.grp.size.10 + (%s) * spat.grp.size.10", 
                                                         paste(c(daughter.regs, "I(order^2)"), collapse = " + "),
                                                         paste(get.grpavg.regs(regress.data, radius = 10, intran = TRUE), collapse = " + "))),
                                                  effect = "individual", model = "within", index = c("governorate", "daughter.id"), vcov.fun = NULL)
