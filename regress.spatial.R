source("Data.R")
source("DataCollection.R")
source("SpatialData.R")
source("BaseFgmDataRef.R")
source("DaughterFgmDataRef.R")
source("RegressionResults.R")

# library(igraph)

original.data <- DaughterFgmData$new(ir.file = '~/Data/EDHS/2008/EGIR5AFL.DTA', br.file = '~/Data/EDHS/2008/EGBR5AFL.DTA', gps.file = '~/Data/EDHS/2008/EGGE5AFF.dbf', other.grpavg.controls = c("med.circum", "circum")) #, skip.cleanup = T))
original.data$relevel("urban.rural", ref = "rural")
original.data$relevel("med.help.distance.fac", ref = "not big problem")
original.data$relevel("med.help.money.fac", ref = "not big problem")
original.data$relevel("med.help.transportation.fac", ref = "not big problem")

original.data$calc.dist.matrix()
original.data$calc.all.cohorts.dist.matrix()
original.data$calc.hh.matrix()

del.regs <- c("delivery.location", "delivered.by.daya")
del.year.offset <- 12

# radii <- c(10, 20)
radii <- c(10, 15, 20)
# radii <- 10
# coh.adj.mat <- regress.data$get.cohort.adj.matrix()

original.data$generate.reg.means(exclude.self = TRUE)

for (radius in radii) {
    original.data$generate.reg.means.spatial(radius = radius, regs = del.regs, prefix = "del.spat.grpavg", postfix = as.character(radius), use.all.cohorts.data = TRUE, year.offset = del.year.offset)
    original.data$generate.reg.means.spatial(radius = radius, postfix = as.character(radius))
    original.data$generate.reg.means.spatial(radius = radius, prefix = "spat.intran.grpavg", postfix = as.character(radius), degree = 2)
}
    #     original.data$generate.reg.means.spatial(radius = radius, prefix = "spat.intran3.grpavg", postfix = as.character(radius), degree = 3)

    #     spat.adj.mat <- regress.data$get.spatial.adj.matrix(radius = radius)
    #     adj.mat <- coh.adj.mat * spat.adj.mat
    #     adj.mat.2 <- adj.mat %*% adj.mat
    #     adj.mat.3 <- adj.mat.2 %*% adj.mat
    # 
    #     adj.vec.mat <- matrix(c(rep.int(1, nrow(adj.mat) * ncol(adj.mat)), as.vector(adj.mat), as.vector(adj.mat.2), as.vector(adj.mat.3)), ncol = 4)
    # 
    #     adj.vec.mat.rank <- qr(adj.vec.mat)$rank

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
# }

regress.data <- original.data$copy(shallow = TRUE)
regress.data$rm.by.res.years(10)

save.image()

# Only 5% of girls of age >12 are at risk of FGM
# all.spat.data <- regress.data$all.cohorts.spdf$spatial.data@data
# circum.age <- all.spat.data$age.circum[(all.spat.data$circum == 1)]
# circum.age <- circum.age[!is.na(circum.age) & (circum.age < 98)]
# quantile(circum.age, 0.95)

#regs.to.average <- c(regress.data$individual.controls, regress.data$other.grpavg.controls)

# regress.data.10 <- regress.data$copy()
# regress.data.10$subset(grp.size.10 >= 10)
# 
# regress.data.20 <- regress.data$copy()
# regress.data.20$subset(grp.size.20 >= 20)
# 
# regress.data.10_20 <- regress.data$copy()
# regress.data.10_20$subset(grp.size.10_20 >= 10)

get.grpavg.regs <- function(data, radius, weighted = FALSE, spat = TRUE, intran = FALSE, delivery = FALSE) {
    reg.pattern <- sprintf("^%s%s%sgrpavg\\.(?!med\\.circum|circum|grp\\.size).+%d%s$", 
                           if (delivery) "del." else "",
                           if (spat) "spat." else "",
                           if (intran) "intran." else "",
                           radius,
                           if (weighted) ".wt" else "")

    return(grep(reg.pattern, data$names, value = TRUE, perl = TRUE))
}

hh.regs <- c("governorate", "wealth.index", "educ.lvl", "marital.age", "mother.circum.fac", "religion", "hh.head.sex", "urban.rural", "med.help.distance.fac", "med.help.money.fac", "n.ord", "discuss.circum.fac", "received.info.circum.fac")
daughter.regs <- c("birth.year.fac", "order.fac")

instruments <- get.grpavg.regs(regress.data, radius = 10, intran = TRUE)
instruments <- setdiff(instruments, grep("wealth\\.index.2", instruments, value = TRUE))
instruments.2 <- grep("intran3\\.grpavg\\.(?!med\\.circum|circum|grp\\.size)", regress.data$names, value = TRUE, perl = TRUE)

# Test the strength of the intran averages on circum averages (1st stage of 2SLS) ################################################################################ 

relv.instr.re.1.1 <- "higher|mother\\.circum|christian|hh\\.head"
relv.instr.re.1.2 <- "higher|mother\\.circum|christian"
relv.instr.re.1.3 <- "higher|mother\\.circum|christian|richest"
relv.instr.re.1.4 <- "higher|mother\\.circum|richest"
relv.instr.re.1.5 <- "mother\\.circum|richest"

#relv.instr.1 <- grep("urban\\.rural|higher|mother\\.circum|christian|hh\\.head", instruments, value = TRUE)
relv.instr.1 <- grep(relv.instr.re.1.1, instruments, value = TRUE)
relv.instr.1.2 <- grep(relv.instr.re.1.2, instruments.2, value = TRUE)
relv.instr.1.3 <- grep(relv.instr.re.1.3, instruments, value = TRUE)
relv.instr.1.4 <- grep(relv.instr.re.1.4, instruments, value = TRUE)
relv.instr.1.5 <- grep(relv.instr.re.1.5, instruments, value = TRUE)

relv.formula.10.1 <- formula(sprintf("spat.grpavg.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         instruments), collapse = " + ")))
relv.results.10.1 <- regress.data$lm(relv.formula.10.1, vcov.fun = vcovHAC)

relv.results.10.1$lht(instruments, test = "F") 

relv.formula.10.2 <- formula(sprintf("spat.grpavg.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         relv.instr.1), collapse = " + ")))
relv.results.10.2 <- regress.data$lm(relv.formula.10.2, vcov.fun = vcovHAC)

# I'm getting an F stat that is > 10 only with the relv.instr.1 (Staiger and Stock (1997) in Cameron and Trivedi p.105)
relv.results.10.2$lht(relv.instr.1, test = "F")

# relv.formula.10.3 <- formula(sprintf("spat.grpavg.circum.10 ~ %s", paste(c(setdiff(daughter.regs, "order.fac"),
#                                                                          get.grpavg.regs(regress.data, radius = 10),
#                                                                          get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
#                                                                          instruments), collapse = " + ")))
# relv.results.10.3 <- regress.data$plm(relv.formula.10.2, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)
# 
# relv.results.10.2$lht(instruments, test = "F")

relv.formula.10.4 <- formula(sprintf("spat.grpavg.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         relv.instr.1.3), collapse = " + ")))
relv.results.10.4 <- regress.data$lm(relv.formula.10.4, vcov.fun = vcovHAC)

# I'm getting an F stat that is > 10 only with the relv.instr.1 (Staiger and Stock (1997) in Cameron and Trivedi p.105)
relv.results.10.4$lht(relv.instr.1.3, test = "F")

relv.formula.10.5 <- formula(sprintf("spat.grpavg.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         relv.instr.1.4), collapse = " + ")))
relv.results.10.5 <- regress.data$lm(relv.formula.10.5, vcov.fun = vcovHAC)

relv.results.10.5$lht(relv.instr.1.4, test = "F")

relv.formula.10.6 <- formula(sprintf("spat.grpavg.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         relv.instr.1.5), collapse = " + ")))
relv.results.10.6 <- regress.data$lm(relv.formula.10.6, vcov.fun = vcovHAC)

relv.results.10.6$lht(relv.instr.1.5, test = "F")

# relv.formula.wt <- list()
# relv.results.wt <- list()
# for (radius in radii) {
#     relv.formula[[radius]] <- formula(sprintf("spat.grpavg.circum.%d.wt ~ %s", radius, get.grpavg.regs(regress.data, radius = radius, intran = TRUE, weighted = TRUE)))
#     relv.results.wt[[radius]] <- regress.data$lm(relv.formula.wt[[radius]])
# }

# Shea's test regressions (circum grpavg) #################################################################################################### 

# (5.40) in Wooldridge (2010)

# I need to remove the rows that were removed by "model.frame"
shea.data <- regress.data$copy(shallow = TRUE)
shea.data$spatial.data <- shea.data$spatial.data[-(relv.results.10.5$na.action),] # BUGBUG subset() is broken

shea.formula.stage.1 <- formula(sprintf("relv.results.10.5$fitted.values ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE)), collapse = " + ")))
shea.results.stage.1 <- shea.data$lm(shea.formula.stage.1, vcov.fun = vcovHAC)

shea.formula.stage.1.2 <- formula(sprintf("spat.grpavg.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE)), collapse = " + ")))
shea.results.stage.1.2 <- shea.data$lm(shea.formula.stage.1.2, vcov.fun = vcovHAC)

shea.measure.1 <- shea.results.stage.1$ssr / shea.results.stage.1.2$ssr
rm(shea.data)

##

relv.circum.grpavg.instr <- relv.instr.1.4

# Test the strength of the intran averages on med averages ################################################################################ 

relv.med.instr.re.1 <- "christian"
relv.med.instr.re.2 <- "wealth"
relv.med.instr.re.3 <- "wealth|christian"
relv.med.instr.re.4 <- "received|christian"
relv.med.instr.re.5 <- "christian|marital.age"

#relv.med.instr.1 <- grep("higher|marital|christian", instruments, value = TRUE)
relv.med.instr.1 <- grep("marital|christian", instruments, value = TRUE)
relv.med.instr.2 <- grep(relv.med.instr.re.1, instruments, value = TRUE)
relv.med.instr.3 <- grep(relv.med.instr.re.1, instruments.2, value = TRUE)
relv.med.instr.4 <- grep(relv.med.instr.re.2, instruments, value = TRUE)
relv.med.instr.5 <- grep(relv.med.instr.re.3, instruments, value = TRUE)
relv.med.instr.6 <- grep(relv.med.instr.re.4, instruments, value = TRUE)
relv.med.instr.7 <- grep(relv.med.instr.re.5, instruments, value = TRUE)

relv.med.formula.10.1 <- formula(sprintf("spat.grpavg.med.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         instruments), collapse = " + ")))
relv.med.results.10.1 <- regress.data$lm(relv.med.formula.10.1, vcov.fun = vcovHAC)

# Again, as above, only the relv.instr has an F > 10
relv.med.results.10.1$lht(instruments, test = "F")
# relv.med.results.10.1$lht(relv.med.instr.1, test = "F")

relv.med.formula.10.2 <- formula(sprintf("spat.grpavg.med.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         relv.med.instr.1), collapse = " + ")))
relv.med.results.10.2 <- regress.data$lm(relv.med.formula.10.2, vcov.fun = vcovHAC)
relv.med.results.10.2$lht(relv.med.instr.1, test = "F")

relv.med.formula.10.3 <- formula(sprintf("spat.grpavg.med.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         relv.med.instr.2), collapse = " + ")))
relv.med.results.10.3 <- regress.data$lm(relv.med.formula.10.3, vcov.fun = vcovHAC)
relv.med.results.10.3$lht(relv.med.instr.2, test = "F")

relv.med.formula.10.4 <- formula(sprintf("spat.grpavg.med.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         relv.med.instr.5), collapse = " + ")))
relv.med.results.10.4 <- regress.data$lm(relv.med.formula.10.4, vcov.fun = vcovHAC)
relv.med.results.10.4$lht(relv.med.instr.5, test = "F")

relv.med.formula.10.5 <- formula(sprintf("spat.grpavg.med.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         relv.med.instr.6), collapse = " + ")))
relv.med.results.10.5 <- regress.data$lm(relv.med.formula.10.5, vcov.fun = vcovHAC)
relv.med.results.10.5$lht(relv.med.instr.6, test = "F")

relv.med.formula.10.6 <- formula(sprintf("spat.grpavg.med.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
                                                                         relv.med.instr.7), collapse = " + ")))
relv.med.results.10.6 <- regress.data$lm(relv.med.formula.10.6, vcov.fun = vcovHAC)
relv.med.results.10.6$lht(relv.med.instr.7, test = "F")

# Shea's test regressions (med.circum grpavg) #################################################################################################### 

# (5.40) in Wooldridge (2010)

# I need to remove the rows that were removed by "model.frame"
shea.med.data <- regress.data$copy(shallow = TRUE)
shea.med.data$spatial.data <- shea.med.data$spatial.data[-(relv.med.results.10.3$na.action),]

shea.formula.stage.2.1 <- formula(sprintf("relv.med.results.10.3$fitted.values ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE)), collapse = " + ")))
shea.results.stage.2.1 <- shea.med.data$lm(shea.formula.stage.2.1, vcov.fun = vcovHAC)

shea.formula.stage.2.2 <- formula(sprintf("spat.grpavg.med.circum.10 ~ %s", paste(c(hh.regs, 
                                                                         daughter.regs,
                                                                         get.grpavg.regs(regress.data, radius = 10),
                                                                         get.grpavg.regs(regress.data, radius = 10, delivery = TRUE)), collapse = " + ")))
shea.results.stage.2.2 <- shea.med.data$lm(shea.formula.stage.2.2, vcov.fun = vcovHAC)

shea.measure.2 <- shea.results.stage.2.1$ssr / shea.results.stage.2.2$ssr
rm(shea.med.data)

##

relv.med.circum.grpavg.instr <- relv.med.instr.2

# Test the strength of the intran averages on exogenous averages (1st stage of 2SLS) ################################################################################ 

# relv.formula.10.1 <- formula(sprintf("spat.grpavg.circum.10 ~ %s", paste(c(hh.regs, 
#                                                                          daughter.regs,
#                                                                          get.grpavg.regs(regress.data, radius = 10),
#                                                                          get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
#                                                                          instruments), collapse = " + ")))
# relv.results.10.1 <- regress.data$lm(relv.formula.10.1, vcov.fun = vcovHAC)
# 
# relv.results.10.1$lht(instruments, test = "F") 
# 
# relv.instr.1 <- grep("higher|mother\\.circum|christian|hh\\.head", instruments, value = TRUE)
# relv.instr.1.2 <- grep("higher|mother\\.circum|christian", instruments.2, value = TRUE)
# 
# relv.formula.10.2 <- formula(sprintf("spat.grpavg.circum.10 ~ %s", paste(c(hh.regs, 
#                                                                          daughter.regs,
#                                                                          get.grpavg.regs(regress.data, radius = 10),
#                                                                          get.grpavg.regs(regress.data, radius = 10, delivery = TRUE),
#                                                                          relv.instr.1), collapse = " + ")))
# relv.results.10.2 <- regress.data$lm(relv.formula.10.2, vcov.fun = vcovHAC)
# 
# relv.results.10.2$lht(relv.instr.1, test = "F")

# Saving the IV tests stuff ################################################################################ 

save(file = "test.instr.RData", list = ls(pattern = "(relv|shea).+(results|measure)"))
rm(list = ls(pattern = "(relv|shea).+(results|measure)"))

# TODO a within version of the above regression

# Regression equations etc. ################################################################################   

exog.del.grpavg.regs <- get.grpavg.regs(regress.data, radius = 10, delivery = TRUE)
exog.grpavg.regs <- get.grpavg.regs(regress.data, radius = 10)
exog.grpavg.regs.2 <- grep(paste(c(relv.circum.grpavg.instr, relv.med.circum.grpavg.instr), collapse = "|"), get.grpavg.regs(regress.data, radius = 10), value = TRUE)
daughter.regs.eqn <- paste(daughter.regs, collapse = " + ")
dir.regs <- c(hh.regs, daughter.regs)
dir.regs.eqn <- paste(dir.regs, collapse = " + ")
exog.regs <- c(dir.regs,
               exog.grpavg.regs,
               exog.del.grpavg.regs)
exog.del.grpavg.eqn <- paste(exog.del.grpavg.regs, collapse = " + ") 
exog.regs.eqn <- paste(exog.regs, collapse = " + ")
exog.regs.2 <- setdiff(exog.regs, hh.regs)
exog.regs.eqn.2 <- paste(exog.regs.2, collapse = " + ")
exog.regs.3 <- setdiff(exog.regs, exog.del.grpavg.regs)
exog.regs.eqn.3 <- paste(exog.regs.3, collapse = " + ")
exog.regs.4 <- setdiff(exog.regs, exog.grpavg.regs)
exog.regs.eqn.4 <- paste(exog.regs.4, collapse = " + ")
exog.regs.5 <- c(hh.regs, daughter.regs,
                 exog.grpavg.regs.2,
                 exog.del.grpavg.regs)
exog.regs.eqn.5 <- paste(exog.regs.5, collapse = " + ")

power.reg <- function(reg, p) paste(sprintf("I(%s^%d)", reg, p), collapse = " + ")

instr.eqn <- paste(instruments, collapse = " + ")
relv.instr.eqn.1 <- paste(relv.circum.grpavg.instr, collapse = " + ")
relv.instr.eqn.1.squared <- power.reg(relv.circum.grpavg.instr, 2)
relv.instr.eqn.1.cubed <- power.reg(relv.circum.grpavg.instr, 3)
relv.instr.eqn.2 <- paste(union(relv.circum.grpavg.instr, relv.med.circum.grpavg.instr), collapse = " + ")
relv.instr.eqn.3 <- paste(relv.med.circum.grpavg.instr, collapse = " + ")
relv.instr.eqn.3.squared <- power.reg(relv.med.circum.grpavg.instr, 2)
relv.instr.eqn.4 <- paste(relv.instr.1.2, collapse = " + ")
relv.instr.eqn.5 <- paste(union(relv.instr.1.2, relv.med.instr.3), collapse = " + ")
relv.instr.eqn.6 <- sprintf("(%s) * (%s)", paste(relv.circum.grpavg.instr, collapse = " + "), paste(relv.med.circum.grpavg.instr, collapse = " + "))

# Direct effects only ################################################################################   

dir.reg.formula.1 <- formula(sprintf("circum ~ %s", dir.regs.eqn))
dir.reg.results.1 <- regress.data$lm(dir.reg.formula.1, vcov.fun = vcovHAC)

dir.reg.formula.2 <- formula(sprintf("circum ~ %s", daughter.regs.eqn))
dir.reg.results.2 <- regress.data$plm(dir.reg.formula.2, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

# Exogenous social effects only ################################################################################   

# OLS regressions
exog.reg.formula.1 <- formula(sprintf("circum ~ %s", exog.regs.eqn.3))
exog.reg.results.1 <- regress.data$lm(exog.reg.formula.1, vcov.fun = vcovHAC)

# Exogenous/Endogenous effects using IV ################################################################################   

# OLS regressions
main.reg.formula.1 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10", exog.regs.eqn))
main.reg.results.1 <- regress.data$lm(main.reg.formula.1, vcov.fun = vcovHAC)

# Same as above, but I add spat.grpavg.med.circum.  No difference is found
main.reg.formula.2 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10", exog.regs.eqn))
main.reg.results.2 <- regress.data$lm(main.reg.formula.2, vcov.fun = vcovHAC)

main.reg.formula.eqn.2.2 <- sprintf("circum ~ %s + spat.grpavg.med.circum.10", exog.regs.eqn)
main.reg.results.2.2 <- regress.data$lm(main.reg.formula.eqn.2.2, vcov.fun = vcovHAC)

main.reg.formula.2.3 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 * spat.grpavg.med.circum.10", exog.regs.eqn))
main.reg.results.2.3 <- regress.data$lm(main.reg.formula.2.3, vcov.fun = vcovHAC)

regress.data$spatial.data@data$spat.grpavg.rel.med.circum.10 <- regress.data$spatial.data$spat.grpavg.med.circum.10 / regress.data$spatial.data$spat.grpavg.circum.10
regress.data$spatial.data@data$spat.grpavg.rel.med.circum.10[is.nan(regress.data$spatial.data$spat.grpavg.rel.med.circum.10)] <- NA

main.reg.formula.2.4 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.rel.med.circum.10", exog.regs.eqn))
main.reg.results.2.4 <- regress.data$lm(main.reg.formula.2.4, vcov.fun = vcovHAC)

# 2SLS
# main.reg.formula.eqn.3 <- sprintf("circum ~ %s + spat.grpavg.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, instr.eqn)
# main.reg.results.3 <- regress.data$ivreg(main.reg.formula.eqn.3, vcov.fun = vcovHAC)

# Add spat.grpavg.med.circum (assumed exogenous)
# main.reg.formula.eqn.4 <- sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | %s + spat.grpavg.med.circum.10 + %s", exog.regs.eqn, exog.regs.eqn, instr.eqn)
# main.reg.results.4 <- regress.data$ivreg(main.reg.formula.eqn.4, vcov.fun = vcovHAC)

# Add spat.grpavg.med.circum (assumed endogenous)
# main.reg.formula.eqn.5 <- sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, instr.eqn)
# main.reg.results.5 <- regress.data$ivreg(main.reg.formula.eqn.5, vcov.fun = vcovHAC)

# 2SLS (with relevant instruments only)
main.reg.formula.eqn.6 <- sprintf("circum ~ %s + spat.grpavg.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.1)
main.reg.results.6 <- regress.data$ivreg(main.reg.formula.eqn.6, vcov.fun = vcovHAC)

main.reg.formula.eqn.6.2 <- sprintf("circum ~ %s + spat.grpavg.circum.10 + I(spat.grpavg.circum.10^2)| %s + %s + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.1, relv.instr.eqn.1.squared)
main.reg.results.6.2 <- regress.data$ivreg(main.reg.formula.eqn.6.2, vcov.fun = vcovHAC)

main.reg.formula.eqn.6.3 <- sprintf("circum ~ %s + spat.grpavg.circum.10 | %s + %s", exog.regs.eqn.5, exog.regs.eqn.5, relv.instr.eqn.1)
main.reg.results.6.3 <- regress.data$ivreg(main.reg.formula.eqn.6.3, vcov.fun = vcovHAC)

# Interacting religion with grpavg.religion
main.reg.formula.eqn.6.4 <- sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.religion_christian.10 : religion | %s + spat.grpavg.religion_christian.10 : religion + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.1)
main.reg.results.6.4 <- regress.data$ivreg(main.reg.formula.eqn.6.4, vcov.fun = vcovHAC)

# Hausman-Sargan overidentification test ((6.31) in Wooldridge (2010))
main.reg.data.6 <- regress.data$copy(shallow = TRUE)
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

main.reg.formula.eqn.7.2 <- sprintf("circum ~ %s + spat.grpavg.circum.10 + I(spat.grpavg.circum.10^2) + spat.grpavg.med.circum.10 + I(spat.grpavg.med.circum.10^2) | %s + spat.grpavg.med.circum.10 + I(spat.grpavg.med.circum.10^2) + %s + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.1, relv.instr.eqn.1.squared)
main.reg.results.7.2 <- regress.data$ivreg(main.reg.formula.eqn.7.2, vcov.fun = vcovHAC)

# Add spat.grpavg.med.circum (assumed endogenous)
main.reg.formula.eqn.8 <- sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.2)
main.reg.results.8 <- regress.data$ivreg(main.reg.formula.eqn.8, vcov.fun = vcovHAC)

# Heterosk. overidentification test
main.reg.data.8 <- regress.data$copy(shallow = TRUE)
main.reg.data.8$spatial.data <- main.reg.data.8$spatial.data[-(main.reg.results.8$na.action), ]
main.reg.hs.test.formula.8.1 <- formula(sprintf("spat.intran.grpavg.religion_christian.10 ~ %s + relv.results.10.5$fitted.values + relv.med.results.10.3$fitted.values", exog.regs.eqn))
main.reg.hs.test.results.8.1 <- main.reg.data.8$lm(main.reg.hs.test.formula.8.1)
test.reg.8 <- main.reg.data.8$lm(main.reg.results.8$residuals ~ -1 + main.reg.hs.test.results.8.1$residuals) 

main.reg.formula.eqn.8.2 <- sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 + I(spat.grpavg.circum.10^2) + I(spat.grpavg.med.circum.10^2) | %s + %s + %s + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.2, relv.instr.eqn.1.squared, relv.instr.eqn.3.squared)
main.reg.results.8.2 <- regress.data$ivreg(main.reg.formula.eqn.8.2, vcov.fun = vcovHAC)

main.reg.formula.eqn.8.3 <- sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.5)
main.reg.results.8.3 <- regress.data$ivreg(main.reg.formula.eqn.8.3, vcov.fun = vcovHAC)

# This doesn't work; the instruments would not be exogenous: they would influence the grpavg.circum variable in the error term
main.reg.formula.eqn.8.4 <- sprintf("circum ~ %s + spat.grpavg.med.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.4)
main.reg.results.8.4 <- regress.data$ivreg(main.reg.formula.eqn.8.4, vcov.fun = vcovHAC)

main.reg.formula.eqn.8.5 <- sprintf("circum ~ %s + spat.grpavg.circum.10 * spat.grpavg.med.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.6)
main.reg.results.8.5 <- regress.data$ivreg(main.reg.formula.eqn.8.5, vcov.fun = vcovHAC)

main.reg.formula.eqn.8.6 <- sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.rel.med.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.6)
main.reg.results.8.6 <- regress.data$ivreg(main.reg.formula.eqn.8.6, vcov.fun = vcovHAC)

# Using hh panel data
main.reg.formula.9 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | . - spat.grpavg.circum.10 + %s", exog.regs.eqn.2, relv.instr.eqn.1))
main.reg.results.9 <- regress.data$plm(main.reg.formula.9, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.9.2 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 | . - spat.grpavg.circum.10 + %s", exog.regs.eqn.2, relv.instr.eqn.1))
main.reg.results.9.2 <- regress.data$plm(main.reg.formula.9.2, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.9.3 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + I(spat.grpavg.circum.10^2) | . - spat.grpavg.circum.10 - I(spat.grpavg.circum.10^2) + %s + %s", exog.regs.eqn.2, relv.instr.eqn.1, relv.instr.eqn.1.squared))
main.reg.results.9.3 <- regress.data$plm(main.reg.formula.9.3, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.9.4 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + I(spat.grpavg.circum.10^2) + I(spat.grpavg.circum.10^3) | . - spat.grpavg.circum.10 - I(spat.grpavg.circum.10^2) - I(spat.grpavg.circum.10^3) + %s + %s + %s", exog.regs.eqn.2, relv.instr.eqn.1, relv.instr.eqn.1.squared, relv.instr.eqn.1.cubed))
main.reg.results.9.4 <- regress.data$plm(main.reg.formula.9.4, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.14 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 + %s", exog.regs.eqn.2, relv.instr.eqn.2))
main.reg.results.14 <- regress.data$plm(main.reg.formula.14, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.14.2 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + I(spat.grpavg.circum.10^2) + spat.grpavg.med.circum.10 + I(spat.grpavg.med.circum.10^2) | . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 - I(spat.grpavg.med.circum.10^2) - I(spat.grpavg.circum.10^2) + %s + %s + %s", exog.regs.eqn.2, relv.instr.eqn.2, relv.instr.eqn.3.squared, relv.instr.eqn.1.squared))
main.reg.results.14.2 <- regress.data$plm(main.reg.formula.14.2, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.14.3 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 + %s", exog.regs.eqn.2, relv.instr.eqn.5))
main.reg.results.14.3 <- regress.data$plm(main.reg.formula.14.3, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.14.4 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + I(spat.grpavg.circum.10^2) + I(spat.grpavg.circum.10^3) + spat.grpavg.med.circum.10 + I(spat.grpavg.med.circum.10^2) + I(spat.grpavg.med.circum.10^3)| . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 - I(spat.grpavg.med.circum.10^2) - I(spat.grpavg.circum.10^2) - I(spat.grpavg.med.circum.10^3) - I(spat.grpavg.med.circum.10^3) + %s + %s + %s", exog.regs.eqn.2, relv.instr.eqn.2, power.reg(union(relv.circum.grpavg.instr, relv.med.circum.grpavg.instr), 2), power.reg(union(relv.circum.grpavg.instr, relv.med.circum.grpavg.instr), 3)))
main.reg.results.14.4 <- regress.data$plm(main.reg.formula.14.4, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.14.5 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 * spat.grpavg.med.circum.10 | . - (spat.grpavg.circum.10 * spat.grpavg.med.circum.10) + %s", exog.regs.eqn.2, relv.instr.eqn.6))
main.reg.results.14.5 <- regress.data$plm(main.reg.formula.14.5, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.14.6 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.rel.med.circum.10 | . - (spat.grpavg.circum.10 * spat.grpavg.med.circum.10) + %s", exog.regs.eqn.2, relv.instr.eqn.6))
main.reg.results.14.6 <- regress.data$plm(main.reg.formula.14.6, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.15 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 + spat.grpavg.med.circum.10 : urban.rural | . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 - spat.grpavg.med.circum.10 : urban.rural + %s + (%s) : urban.rural", exog.regs.eqn.2, relv.instr.eqn.2, relv.instr.eqn.3))
main.reg.results.15 <- regress.data$plm(main.reg.formula.15, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.15.2 <- formula(sprintf("circum ~ %s + (spat.grpavg.circum.10 + spat.grpavg.med.circum.10 ) * urban.rural - urban.rural | . - (spat.grpavg.circum.10 + spat.grpavg.med.circum.10) * urban.rural + (%s) * urban.rural - urban.rural", exog.regs.eqn.2, relv.instr.eqn.2))
main.reg.results.15.2 <- regress.data$plm(main.reg.formula.15.2, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.15.3 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 * spat.grpavg.med.circum.10 * urban.rural - urban.rural | . - spat.grpavg.circum.10 * spat.grpavg.med.circum.10 * urban.rural + (%s) * (%s) * urban.rural - urban.rural", exog.regs.eqn.2, relv.instr.eqn.1, relv.instr.eqn.3))
main.reg.results.15.3 <- regress.data$plm(main.reg.formula.15.3, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.16 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 + spat.grpavg.med.circum.10 : med.help.distance.fac | . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 - spat.grpavg.med.circum.10 : med.help.distance.fac + %s + (%s) : med.help.distance.fac", exog.regs.eqn.2, relv.instr.eqn.2, relv.instr.eqn.3))
main.reg.results.16 <- regress.data$plm(main.reg.formula.16, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.16.2 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 + spat.grpavg.med.circum.10 : med.help.money.fac | . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 - spat.grpavg.med.circum.10 : med.help.money.fac + %s + (%s) : med.help.distance.fac", exog.regs.eqn.2, relv.instr.eqn.2, relv.instr.eqn.3))
main.reg.results.16.2 <- regress.data$plm(main.reg.formula.16.2, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.17 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 + (spat.grpavg.med.circum.10) : (urban.rural * med.help.distance.fac) | . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 - (spat.grpavg.med.circum.10) : (urban.rural * med.help.distance.fac) + %s + (%s) : (urban.rural * med.help.distance.fac)", exog.regs.eqn.2, relv.instr.eqn.2, relv.instr.eqn.3))
main.reg.results.17 <- regress.data$plm(main.reg.formula.17, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.17.2 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 + spat.grpavg.med.circum.10 : urban.rural + spat.grpavg.med.circum.10 : med.help.distance.fac + spat.grpavg.med.circum.10 : urban.rural : med.help.distance.fac + spat.grpavg.circum.10 : urban.rural + spat.grpavg.circum.10 : med.help.distance.fac + spat.grpavg.circum.10 : urban.rural : med.help.distance.fac | . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 - spat.grpavg.med.circum.10 : urban.rural - spat.grpavg.med.circum.10 : med.help.distance.fac - spat.grpavg.med.circum.10 : urban.rural : med.help.distance.fac - spat.grpavg.circum.10 : urban.rural - spat.grpavg.circum.10 : med.help.distance.fac - spat.grpavg.circum.10 : urban.rural : med.help.distance.fac + %s + (%s) : (med.help.distance.fac + urban.rural + urban.rural : med.help.distance.fac) + (%s) : (med.help.distance.fac + urban.rural + urban.rural : med.help.distance.fac)", exog.regs.eqn.2, relv.instr.eqn.2, relv.instr.eqn.3, relv.instr.eqn.1))
main.reg.results.17.2 <- regress.data$plm(main.reg.formula.17.2, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.17.3 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + I(spat.grpavg.circum.10^2) + spat.grpavg.med.circum.10 + I(spat.grpavg.med.circum.10^2) + (spat.grpavg.med.circum.10 + I(spat.grpavg.med.circum.10^2)) : (med.help.distance.fac * urban.rural) | . - spat.grpavg.circum.10 - I(spat.grpavg.circum.10^2) - spat.grpavg.med.circum.10 - I(spat.grpavg.med.circum.10^2) - (spat.grpavg.med.circum.10 + I(spat.grpavg.med.circum.10^2)) : (med.help.distance.fac * urban.rural) + %s + %s + (%s + %s) : (med.help.distance.fac * urban.rural)", exog.regs.eqn.2, relv.instr.eqn.2, power.reg(relv.circum.grpavg.instr, 2), relv.instr.eqn.3, power.reg(relv.med.circum.grpavg.instr, 2)))
main.reg.results.17.3 <- regress.data$plm(main.reg.formula.17.3, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.17.4 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 + (spat.grpavg.med.circum.10) : (urban.rural * med.help.money.fac) | . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 - (spat.grpavg.med.circum.10) : (urban.rural * med.help.money.fac) + %s + (%s) : (urban.rural * med.help.money.fac)", exog.regs.eqn.2, relv.instr.eqn.2, relv.instr.eqn.3))
main.reg.results.17.4 <- regress.data$plm(main.reg.formula.17.4, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.17.5 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 * spat.grpavg.med.circum.10 + (spat.grpavg.med.circum.10) : (urban.rural * med.help.money.fac) | . - (spat.grpavg.circum.10 * spat.grpavg.med.circum.10) - (spat.grpavg.med.circum.10) : (urban.rural * med.help.money.fac) + %s + (%s) : (urban.rural * med.help.money.fac)", exog.regs.eqn.2, relv.instr.eqn.6, relv.instr.eqn.3))
main.reg.results.17.5 <- regress.data$plm(main.reg.formula.17.5, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.17.6 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 * spat.grpavg.med.circum.10 + (spat.grpavg.med.circum.10) : (urban.rural * med.help.distance.fac) | . - (spat.grpavg.circum.10 * spat.grpavg.med.circum.10) - (spat.grpavg.med.circum.10) : (urban.rural * med.help.distance.fac) + %s + (%s) : (urban.rural * med.help.distance.fac)", exog.regs.eqn.2, relv.instr.eqn.6, relv.instr.eqn.3))
main.reg.results.17.6 <- regress.data$plm(main.reg.formula.17.6, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.19 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 * del.spat.grpavg.delivered.by.daya_yes.10 | . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 + %s + (%s) * del.spat.grpavg.delivered.by.daya_yes.10 ", exog.regs.eqn.2, relv.instr.eqn.1, relv.instr.eqn.3))
main.reg.results.19 <- regress.data$plm(main.reg.formula.19, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

# Endogenous regressor only
main.reg.formula.eqn.10 <- sprintf("circum ~ spat.grpavg.circum.10 | %s", relv.instr.eqn.1)
main.reg.results.10 <- regress.data$ivreg(main.reg.formula.eqn.10, vcov.fun = vcovHAC)

main.reg.formula.eqn.11 <- sprintf("circum ~ spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | %s", relv.instr.eqn.2)
main.reg.results.11 <- regress.data$ivreg(main.reg.formula.eqn.11, vcov.fun = vcovHAC)

main.reg.formula.12 <- formula(sprintf("circum ~ spat.grpavg.circum.10 | . - spat.grpavg.circum.10 + %s", relv.instr.eqn.1))
main.reg.results.12 <- regress.data$plm(main.reg.formula.12, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.12.2 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 | . - spat.grpavg.circum.10 + %s", exog.del.grpavg.eqn, relv.instr.eqn.1))
main.reg.results.12.2 <- regress.data$plm(main.reg.formula.12.2, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.12.3 <- formula(sprintf("circum ~ %s + spat.grpavg.circum.10 | . - spat.grpavg.circum.10 + %s", exog.del.grpavg.eqn, relv.instr.eqn.4))
main.reg.results.12.3 <- regress.data$plm(main.reg.formula.12.3, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.13 <- formula(sprintf("circum ~ spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 + %s", relv.instr.eqn.2))
main.reg.results.13 <- regress.data$plm(main.reg.formula.13, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.reg.formula.13.1 <- formula(sprintf("circum ~ spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 + %s", relv.instr.eqn.5))
main.reg.results.13.1 <- regress.data$plm(main.reg.formula.13.1, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

# Use IV for exogenous regressors too!

# 2SLS
main.reg.formula.eqn.18 <- sprintf("circum ~ %s | %s + %s", exog.regs.eqn, exog.regs.eqn.4, instr.eqn)
main.reg.results.18 <- regress.data$ivreg(main.reg.formula.eqn.18, vcov.fun = vcovHAC)

# Save results ############################################################ 

save(regress.data, file = "regress.data.RData")
save(med.data, file = "med.data.RData")
save(relv.results.10.1, relv.results.10.5, file = "circum.1s.RData")
save(relv.med.results.10.1, relv.med.results.10.3, file = "med.1s.RData")
save(dir.reg.results.1, dir.reg.results.2, file = "direct.effects.RData") 
save(test.reg.8, file = "overident.test.RData")
save(main.reg.results.1, main.reg.results.2, main.reg.results.2.3, main.reg.results.6, main.reg.results.6.2, main.reg.results.7, main.reg.results.7.2, main.reg.results.8, main.reg.results.8.2, main.reg.results.8.5, file = "exog.endog.effects.RData")
save(main.reg.results.9.2, main.reg.results.9.3, main.reg.results.9, main.reg.results.14, main.reg.results.14.2, main.reg.results.14.5, main.reg.results.17, main.reg.results.17.4, file = "exog.endog.effects.fe.RData")
