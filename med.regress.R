# Direct med regression ############################################################ 

med.data <- original.data$copy(shallow = TRUE)
med.data$spatial.data <- med.data$spatial.data[med.data$spatial.data$circum == 1, ]

dir.med.reg.formula.1 <- formula(sprintf("med.circum ~ %s", dir.regs.eqn))
dir.med.reg.results.1 <- regress.data$lm(dir.med.reg.formula.1, vcov.fun = vcovHAC)

dir.med.reg.formula.2 <- formula(sprintf("med.circum ~ %s", daughter.regs.eqn))
dir.med.reg.results.2 <- regress.data$plm(dir.med.reg.formula.2, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

# Exogenous social effects only ################################################################################   

# OLS regressions
exog.med.reg.formula.1 <- formula(sprintf("med.circum ~ %s", exog.regs.eqn.3))
exog.med.reg.results.1 <- regress.data$lm(exog.med.reg.formula.1, vcov.fun = vcovHAC)

# Exogenous/Endogenous effects using IV ################################################################################   

# OLS regressions
main.med.reg.formula.1 <- formula(sprintf("med.circum ~ %s + spat.grpavg.med.circum.10", exog.regs.eqn))
main.med.reg.results.1 <- regress.data$lm(main.med.reg.formula.1, vcov.fun = vcovHAC)

# Same as above, but I add spat.grpavg.med.circum.  No difference is found
main.med.reg.formula.2 <- formula(sprintf("med.circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10", exog.regs.eqn))
main.med.reg.results.2 <- regress.data$lm(main.med.reg.formula.2, vcov.fun = vcovHAC)

main.med.reg.formula.eqn.2.2 <- sprintf("med.circum ~ %s + spat.grpavg.circum.10", exog.regs.eqn)
main.med.reg.results.2.2 <- regress.data$lm(main.med.reg.formula.eqn.2.2, vcov.fun = vcovHAC)

# 2SLS
main.med.reg.formula.eqn.3 <- sprintf("med.circum ~ %s + spat.grpavg.med.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.3)
main.med.reg.results.3 <- regress.data$ivreg(main.med.reg.formula.eqn.3, vcov.fun = vcovHAC)

# Add spat.grpavg.circum (assumed exogenous)
main.med.reg.formula.eqn.4 <- sprintf("med.circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | %s + spat.grpavg.circum.10 + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.3)
main.med.reg.results.4 <- regress.data$ivreg(main.med.reg.formula.eqn.4, vcov.fun = vcovHAC)

# Add spat.grpavg.circum (assumed endogenous)
main.med.reg.formula.eqn.5 <- sprintf("med.circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | %s + %s", exog.regs.eqn, exog.regs.eqn, relv.instr.eqn.2)
main.med.reg.results.5 <- regress.data$ivreg(main.med.reg.formula.eqn.5, vcov.fun = vcovHAC)

# Hausman-Sargan overidentification test ((6.31) in Wooldridge (2010))
main.med.reg.data.6 <- med.data$copy(shallow = TRUE)
main.med.reg.data.6$spatial.data <- main.med.reg.data.6$spatial.data[-(main.med.reg.results.6$na.action), ]

# Heteroskedastic test
main.med.reg.hs.test.formula.6.2.1 <- formula(sprintf("spat.intran.grpavg.marital.age.10 ~ %s + relv.med.results.10.2$fitted.values", exog.regs.eqn))
main.med.reg.hs.test.formula.6.2.2 <- formula(sprintf("spat.intran.grpavg.religion_christian.10 ~ %s + relv.med.results.10.2$fitted.values", exog.regs.eqn))
main.med.reg.hs.test.results.6.2.1 <- main.med.reg.data.6$lm(main.med.reg.hs.test.formula.6.2.1)
main.med.reg.hs.test.results.6.2.2 <- main.med.reg.data.6$lm(main.med.reg.hs.test.formula.6.2.2)

med.test.res.reg.1 <- main.med.reg.results.3$residuals * main.med.reg.hs.test.results.6.2.1$residuals
med.test.res.reg.2 <- main.med.reg.results.3$residuals * main.med.reg.hs.test.results.6.2.2$residuals

main.med.reg.hs.test.results.6.2 <- lm(rep(1, length(test.res.reg.1)) ~ -1 + med.test.res.reg.1 + med.test.res.reg.2)

pchisq(length(med.test.res.reg.1) - main.med.reg.hs.test.results.6.2$ssr, df = 1, lower.tail = FALSE)

# Panel 

main.med.reg.formula.9 <- formula(sprintf("med.circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | . - spat.grpavg.med.circum.10 + %s", exog.regs.eqn.2, relv.instr.eqn.3))
main.med.reg.results.9 <- regress.data$plm(main.med.reg.formula.9, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

main.med.reg.formula.14 <- formula(sprintf("med.circum ~ %s + spat.grpavg.circum.10 + spat.grpavg.med.circum.10 | . - spat.grpavg.circum.10 - spat.grpavg.med.circum.10 + %s", exog.regs.eqn.2, relv.instr.eqn.2))
main.med.reg.results.14 <- regress.data$plm(main.med.reg.formula.14, effect = "individual", model = "within", index = c("hh.id", "order.fac"), vcov.fun = vcovHC)

# Save results ############################################################ 

save(med.data, file = "med.data.RData")
