cutpoints <- c(0, 0.001, 0.01, 0.05, 0.1, 1)
symbols <- c("***", "**", "*", "$\\cdot$", " ")

educ.regs <- list(educ.lvlprimary = "Primary", educ.lvlsecondary = "Secondary", educ.lvlhigher = "Higher")
birth.order.regs <- list(order.fac2 = 2, order.fac3 = 3, order.fac4 = 4, order.fac5 = 5, order.fac6 = 6)
birth.order.fac.info <- list(label = "Birth Order", dict = birth.order.regs)
cohort.range <- 1989:1996
youngest.cohort <- max(cohort.range) 

# exog.se.regs <- list(
#     spat.grpavg.wealth.index.2_rich.10 = "
#     spat.grpavg.urban.rural_urban.10
#     spat.grpavg.educ.lvl_primary.10
#     spat.grpavg.educ.lvl_secondary.10
#     spat.grpavg.educ.lvl_higher.10 
#     spat.grpavg.marital.age.10
#     spat.grpavg.mother.circum.fac_yes.10
#     spat.grpavg.religion_christian.10
#     spat.grpavg.hh.head.sex_female.10
#     spat.grpavg.med.help.distance.fac_big_problem.10
#     spat.grpavg.discuss.circum.fac_yes.10
#     spat.grpavg.received.info.circum.10

