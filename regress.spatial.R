source("Data.R")
source("DataCollection.R")
source("SpatialData.R")
source("BaseFgmDataRef.R")
source("DaughterFgmDataRef.R")
source("RegressionResults.R")

# library(igraph)

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
# coh.adj.mat <- regress.data$get.cohort.adj.matrix()

for (radius in radii) {
    print(system.time(regress.data$generate.reg.means.spatial(radius = radius, regs = del.regs, prefix = "del.spat.grpavg", postfix = as.character(radius), use.all.cohorts.data = TRUE, year.offset = del.year.offset)))
    print(system.time(regress.data$generate.reg.means.spatial(radius = radius, postfix = as.character(radius))))
    print(system.time(regress.data$generate.reg.means.spatial(radius = radius, prefix = "spat.intran.grpavg", postfix = as.character(radius), degree = 2)))

    #     spat.adj.mat <- regress.data$get.spatial.adj.matrix(radius = radius)
    #     diag(spat.adj.mat) <- 0
    # 
    #     spat.only.graph <- graph.adjacency(spat.adj.mat)
    #     spat.coh.graph <- graph.adjacency(spat.adj.mat * coh.adj.mat)
    # 
    #     spat.only.cl.col <- sprintf("spat.only.graph.cluster.%d", radius)
    #     spat.coh.cl.col <- sprintf("spat.coh.graph.cluster.%d", radius)
    # 
    #     regress.data$spatial.data@data[, spat.only.cl.col] <- clusters(spat.only.graph)$membership + 1
    #     regress.data$spatial.data@data[, spat.coh.cl.col] <- clusters(spat.coh.graph)$membership + 1
    # 
    #     regress.data$sort(spat.only.cl.col)
    #     spat.adj.mat <- regress.data$get.spatial.adj.matrix(radius = radius, recalc.dist.matrix = TRUE)
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

get.grpavg.regs <- function(data, radius, weighted = FALSE, spat = TRUE, intran = FALSE, delivery = FALSE) {
    reg.pattern <- sprintf("%s%s%sgrpavg\\.(?!med\\.circum|circum|grp\\.size).+%d%s$", 
                           if (delivery) "del." else "",
                           if (spat) "spat." else "",
                           if (intran) "intran." else "",
                           radius,
                           if (weighted) ".wt" else "")

    return(grep(reg.pattern, data$names, value = TRUE, perl = TRUE))
}

hh.regs <- c("governorate", "wealth.index.2", "educ.lvl", "marital.age", "mother.circum.fac", "religion", "hh.head.sex", "urban.rural", "med.help.distance.fac", "n.ord")
daughter.regs <- c("birth.year.fac", "order.fac")

# Test the strength of the intran averages on circum averages (1st stage of 2SLS) ################################################################################ 

instruments <- get.grpavg.regs(regress.data, radius = 10, intran = TRUE)

relv.formula.10.1 <- formula(sprintf("spat.grpavg.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         instruments), collapse = " + ")))
relv.results.10.1 <- regress.data$lm(relv.formula.10.1, vcov.fun = vcovHAC)

#relv.instr.1 <- grep("urban\\.rural|higher|mother\\.circum|christian|hh\\.head", instruments, value = TRUE)
#relv.instr.1 <- grep("higher|mother\\.circum|christian|hh\\.head", instruments, value = TRUE)
relv.instr.1 <- grep("higher|mother\\.circum|christian", instruments, value = TRUE)

# I'm getting an F stat that is > 10 only with the relv.instr.1 (Staiger and Stock (1997) in Cameron and Trivedi p.105)
relv.results.10.1$lht(instruments, test = "F") 
relv.results.10.1$lht(relv.instr.1, test = "F")

relv.formula.10.2 <- formula(sprintf("spat.grpavg.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         relv.instr.1), collapse = " + ")))
relv.results.10.2 <- regress.data$lm(relv.formula.10.2, vcov.fun = vcovHAC)
relv.results.10.2$lht(relv.instr.1, test = "F")

# relv.formula.10.3 <- formula(sprintf("spat.grpavg.circum.10 ~ %s", paste(c(setdiff(daughter.regs, "order.fac"),
#                                                                          get.grpavg.regs(regress.data, radius = 10),
#                                                                          get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
#                                                                          instruments), collapse = " + ")))
# relv.results.10.3 <- regress.data$plm(relv.formula.10.2, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)
# 
# relv.results.10.2$lht(instruments, test = "F")

# relv.formula.wt <- list()
# relv.results.wt <- list()
# for (radius in radii) {
#     relv.formula[[radius]] <- formula(sprintf("spat.grpavg.circum.%d.wt ~ %s", radius, get.grpavg.regs(regress.data, radius = radius, intran = TRUE, weighted = TRUE)))
#     relv.results.wt[[radius]] <- regress.data$lm(relv.formula.wt[[radius]])
# }

# Shea's test regressions #################################################################################################### 

# (5.40) in Wooldridge (2010)

# I need to remove the rows that were removed by "model.frame"
shea.data <- regress.data$copy()
shea.data$spatial.data <- shea.data$spatial.data[-(relv.results.10.2$na.action),]

shea.formula.stage.1 <- formula(sprintf("relv.results.10.2$fitted.values ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE)), collapse = " + ")))
shea.results.stage.1 <- shea.data$lm(shea.formula.stage.1, vcov.fun = vcovHAC)

shea.formula.stage.1.2 <- formula(sprintf("spat.grpavg.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE)), collapse = " + ")))
shea.results.stage.1.2 <- shea.data$lm(shea.formula.stage.1.2, vcov.fun = vcovHAC)

shea.measure.1 <- sum(shea.results.stage.1$results$residuals^2) / sum(shea.results.stage.1.2$results$residuals^2)

# Test the strength of the intran averages on med averages ################################################################################ 

relv.med.formula.10.1 <- formula(sprintf("spat.grpavg.med.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         instruments), collapse = " + ")))
relv.med.results.10.1 <- regress.data$lm(relv.med.formula.10.1, vcov.fun = vcovHAC)

#relv.med.instr.1 <- grep("higher|marital|christian", instruments, value = TRUE)
relv.med.instr.1 <- grep("marital|christian", instruments, value = TRUE)

# Again, as above, only the relv.instr has an F > 10
relv.med.results.10.1$lht(instruments, test = "F")
relv.med.results.10.1$lht(relv.med.instr.1, test = "F")

relv.med.formula.10.2 <- formula(sprintf("spat.grpavg.med.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         relv.med.instr.1), collapse = " + ")))
relv.med.results.10.2 <- regress.data$lm(relv.med.formula.10.2, vcov.fun = vcovHAC)
relv.med.results.10.2$lht(relv.med.instr.1, test = "F")

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

exog.regs <- c(hh.regs, daughter.regs,
               get.grpavg.regs(regress.data, radius = 10),
               get.grpavg.regs(regress.data, radius = 10, delivery = TRUE))
exog.regs.eqn <- paste(exog.regs, collapse = " + ")
instr.eqn <- paste(instruments, collapse = " + ")
relv.instr.eqn.1 <- paste(relv.instr.1, collapse = " + ")
relv.instr.eqn.2 <- paste(c(relv.instr.1, relv.med.instr.1), collapse = " + ")

# OLS regressions
main.reg.formula.1 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10", exog.regs.eqn))
main.reg.results.1 <- regress.data$lm(main.reg.formula.1, vcov.fun = vcovHAC)

# Same as above, but I add spat.grpavg.med.circum.  No difference is found
main.reg.formula.2 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10", exog.regs.eqn))
main.reg.results.2 <- regress.data$lm(main.reg.formula.2, vcov.fun = vcovHAC)

# 2SLS
main.reg.formula.eqn.3 <- sprintf("circum ~ %s + spat.grpavg.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, instr.eqn)
main.reg.results.3 <- regress.data$ivreg(main.reg.formula.eqn.3, vcov.fun = vcovHAC)


# Add spat.grpavg.med.circum (assumed exogenous)
main.reg.formula.eqn.4 <- sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | %s + spat.grpavg.med.circum.10 + %s", exog.regs.eqn, exog.regs.eqn, instr.eqn)
main.reg.results.4 <- regress.data$ivreg(main.reg.formula.eqn.4, vcov.fun = vcovHAC)

# Add spat.grpavg.med.circum (assumed endogenous)
main.reg.formula.eqn.5 <- sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, instr.eqn)
main.reg.results.5 <- regress.data$ivreg(main.reg.formula.eqn.5, vcov.fun = vcovHAC)

# 2SLS (with relevant instruments only)
main.reg.formula.eqn.6 <- sprintf("circum ~ %s + spat.grpavg.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.1)
main.reg.results.6 <- regress.data$ivreg(main.reg.formula.eqn.6, vcov.fun = vcovHAC)

# Hausman-Sargan test ((6.31) in Wooldridge (2010))
main.reg.data.6 <- regress.data$copy()
main.reg.data.6$spatial.data <- main.reg.data.6$spatial.data[-(main.reg.results.6$na.action), ]

# Homoskedastic test
main.reg.hs.test.formula.6.1 <- formula(sprintf("main.reg.results.6$residuals ~ %s + %s", exog.regs.eqn, relv.instr.eqn.1))
main.reg.hs.test.results.6.1 <- main.reg.data.6$lm(main.reg.hs.test.formula.6.1)
hs.test.stat.6.1 <- main.reg.hs.test.results.6.1$r.squared * main.reg.data.6$nrow
pchisq(hs.test.stat.6.1, df = 2, lower.tail = FALSE)

# Heteroskedastic test
main.reg.hs.test.formula.6.2.1 <- formula(sprintf("spat.intran.grpavg.educ.lvl_higher.10 ~ %s + relv.results.10.2$fitted.values", exog.regs.eqn))
main.reg.hs.test.formula.6.2.2 <- formula(sprintf("spat.intran.grpavg.mother.circum.fac_yes.10 ~ %s + relv.results.10.2$fitted.values", exog.regs.eqn))
main.reg.hs.test.results.6.2.1 <- main.reg.data.6$lm(main.reg.hs.test.formula.6.2.1)
main.reg.hs.test.results.6.2.2 <- main.reg.data.6$lm(main.reg.hs.test.formula.6.2.2)

test.res.reg.1 <- main.reg.results.6$residuals * main.reg.hs.test.results.6.2.1$residuals
test.res.reg.2 <- main.reg.results.6$residuals * main.reg.hs.test.results.6.2.2$residuals

main.reg.hs.test.results.6.2 <- lm(rep(1, length(test.res.reg.1)) ~ -1 + test.res.reg.1 + test.res.reg.2)

pchisq(length(test.res.reg.1) - sum(main.reg.hs.test.results.6.2$residuals^2), df = 2, lower.tail = FALSE)

# Add spat.grpavg.med.circum (assumed exogenous)
main.reg.formula.eqn.7 <- sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | %s + spat.grpavg.med.circum.10 + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.1)
main.reg.results.7 <- regress.data$ivreg(main.reg.formula.eqn.7, vcov.fun = vcovHAC)

# Add spat.grpavg.med.circum (assumed endogenous)
main.reg.formula.eqn.8 <- sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.2)
main.reg.results.8 <- regress.data$ivreg(main.reg.formula.eqn.8, vcov.fun = vcovHAC)

# Old Exogenous/Endogenous effects using IV ################################################################################   

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
