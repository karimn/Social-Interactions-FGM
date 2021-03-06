cutpoints <- c(0, 0.01, 0.05, 0.1, 1)
symbols <- c("***", "**", "*", " ")

educ.regs <- list(educ.lvlprimary = "Primary", educ.lvlsecondary = "Secondary", educ.lvlhigher = "Higher")
wealth.regs <- list(wealth.indexpoorer = "Poorer", wealth.indexmiddle = "Middle", wealth.indexricher = "Richer", wealth.indexrichest = "Richest")
birth.order.regs <- list(order.fac2 = 2, order.fac3 = 3, order.fac4 = 4, order.fac5 = 5, order.fac6 = 6)
birth.order.fac.info <- list(label = "Birth Order", dict = birth.order.regs)
wealth.index.fac.info <- list(label = "Wealth", dict = wealth.regs)
cohort.range <- 1989:1996
youngest.cohort <- max(cohort.range) 

exog.se.regs <- list(
 spat.grpavg.wealth.index_poorer.10 = "Wealth (Poorer)",
 spat.grpavg.wealth.index_middle.10 = "Wealth (Middle)",
 spat.grpavg.wealth.index_richer.10 = "Wealth (Richer)",
 spat.grpavg.wealth.index_richest.10 = "Wealth (Richest)",
 spat.grpavg.urban.rural_urban.10 = "Residence (Urban)",
 spat.grpavg.educ.lvl_primary.10 = "Education (Primary)",
 spat.grpavg.educ.lvl_secondary.10 = "Education (Secondary)",
 spat.grpavg.educ.lvl_higher.10 = "Education (Higher)",
 spat.grpavg.marital.age.10 = "Marital Age",
 spat.grpavg.mother.circum.fac_yes.10 = "Mother FGM",
 spat.grpavg.religion_christian.10 = "Religion (Christian)",
 spat.grpavg.hh.head.sex_female.10 = "HH Head Sex (Female)",
 spat.grpavg.med.help.distance.fac_big_problem.10 = "Medical Help Dist. (Big Problem)",
 spat.grpavg.discuss.circum.fac_yes.10 = "Discussed FGM",
 spat.grpavg.received.info.circum.10 = "Received Info on FGM"
)

exog.se.intran.regs <- list(
 spat.intran.grpavg.wealth.index_poorer.10 = "Wealth (Poorer)",
 spat.intran.grpavg.wealth.index_middle.10 = "Wealth (Middle)",
 spat.intran.grpavg.wealth.index_richer.10 = "Wealth (Richer)",
 spat.intran.grpavg.wealth.index_richest.10 = "Wealth (Richest)",
 spat.intran.grpavg.urban.rural_urban.10 = "Residence (Urban)",
 spat.intran.grpavg.educ.lvl_primary.10 = "Education (Primary)",
 spat.intran.grpavg.educ.lvl_secondary.10 = "Education (Secondary)",
 spat.intran.grpavg.educ.lvl_higher.10 = "Education (Higher)",
 spat.intran.grpavg.marital.age.10 = "Marital Age",
 spat.intran.grpavg.mother.circum.fac_yes.10 = "Mother FGM",
 spat.intran.grpavg.religion_christian.10 = "Religion (Christian)",
 spat.intran.grpavg.hh.head.sex_female.10 = "HH Head Sex (Female)",
 spat.intran.grpavg.med.help.distance.fac_big_problem.10 = "Medical Help Dist. (Big Problem)",
 spat.intran.grpavg.discuss.circum.fac_yes.10 = "Discussed FGM",
 spat.intran.grpavg.received.info.circum.10 = "Received Info on FGM"
)
