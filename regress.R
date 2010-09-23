library(plm)
library(car)
library(lmtest)
library(sandwich)
library(AER)

pl1 <- plm(has.or.intends.circum ~ ADM1SALBNA + birth.year.fac + urban.rural + religion + wealth.index + educ.lvl + med.help.permission.fac +
                                   med.help.distance.fac + med.help.transportation.fac +
                                   marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac,
           data = fgm.data.daughters.08@data,
           index = c("hh.id", "order.fac"),
           model = "pooling")

# This is problematic: I'm selecting circumcized only and hence was for latter years I only capture those who have been circumcized younger
pl2 <- plm(age.circum ~ ADM1SALBNA + birth.year.fac + urban.rural + religion + wealth.index,
           data = fgm.data.daughters.08@data,
           subset = ((circum == 1) & (age.circum < 98)),
           index = c("hh.id", "order.fac"),
           model = "pooling")

pl3 <- plm(med.circum ~ ADM1SALBNA + birth.year.fac + urban.rural + religion + wealth.index,
           data = fgm.data.daughters.08@data,
           subset = circum == 1,
           index = c("hh.id", "order.fac"),
           model = "pooling")


###########################################################################################
# Ref group: governorate

calc.grpavg.daughters <- function(df, total.fgm, cohort.range, knetwork, instr.cohort.range = cohort.range)
{
  current.birth.year <- df$birth.year[1]
  current.governorate <- df$governorate[1]
  current.urban.rural <- df$urban.rural[1]

  grp.country <- total.fgm[(total.fgm[["birth.year"]] <= current.birth.year + cohort.range) & 
                          (total.fgm[["birth.year"]] >= current.birth.year - cohort.range), ]

  inst.grp <- total.fgm[(total.fgm[["birth.year"]] <= current.birth.year + instr.cohort.range) & 
                          (total.fgm[["birth.year"]] >= current.birth.year - instr.cohort.range), ]

  grp.geo <- grp.country[(grp.country[["governorate"]] == current.governorate), ]

  if (!is.null(knetwork))
  {
    current.knetwork <- df[1, knetwork]

    grp <- grp.geo[(grp.geo[[knetwork]] == current.knetwork), ]
    grp.larger <- grp.country[(grp.country[[knetwork]] == current.knetwork), ]
    inst.grp <- inst.grp[(inst.grp[[knetwork]] == current.knetwork), ]
  }
  else
  {
    grp <- grp.geo 
    grp.larger <- grp.country
  }

  if ('has.or.intends.circum' %in% names(df)) 
    df$grpavg.has.or.intends.circum <- vapply(1:nrow(df), function(rowid) mean(grp[-rowid, "has.or.intends.circum"], na.rm = TRUE), 0) 

  grp.mean <- function(rowid, grp.data, column) mean(grp.data[-rowid, column], na.rm = TRUE)

  df$grpavg.circum <- vapply(1:nrow(df), grp.mean, 0, grp, "circum") 
  df$grpavg.circum2 <- vapply(1:nrow(df), grp.mean, 0, grp.larger, "circum") 
  df$grpavg.circum3 <- vapply(1:nrow(df), grp.mean, 0, grp.geo, "circum") 
  df$grpavg.circum4 <- vapply(1:nrow(df), grp.mean, 0, grp.country, "circum") 
  df$grpavg.med <- vapply(1:nrow(df), grp.mean, 0, grp, "med.circum") 
  df$grpavg.med2 <- vapply(1:nrow(df), grp.mean, 0, grp.larger, "med.circum") 
  df$grpavg.med3 <- vapply(1:nrow(df), grp.mean, 0, grp.geo, "med.circum") 
  df$grpavg.med4 <- vapply(1:nrow(df), grp.mean, 0, grp.country, "med.circum") 
  df$grpavg.circum_med <- df$grpavg.circum * df$grpavg.med
  df$grpavg.circum_med2 <- df$grpavg.circum * df$grpavg.med2
  df$grpavg.circum_med3 <- df$grpavg.circum * df$grpavg.med3
  df$grpavg.circum_med4 <- df$grpavg.circum * df$grpavg.med4
  df$grpavg.circum3_med <- df$grpavg.circum3 * df$grpavg.med

  if ('has.or.intends.circum' %in% names(df)) 
    df$inst.grpavg.has.or.intends.circum <- mean(inst.grp$has.or.intends.circum, na.rm = TRUE) 

  df$inst.grpavg.circum <- mean(inst.grp$circum, na.rm = TRUE)
  df$inst.grpavg.med <- mean(inst.grp$med.circum, na.rm = TRUE)
  df$inst.grpavg.circum_med <- df$inst.grpavg.circum * df$inst.grpavg.med

  return(df)
}

subset.to.regress.daughters <- function(fgm.data, youngest.cohort = 1996, oldest.cohort = NULL, cohort.range = 1, knetwork = "urban.rural", na.rows = NULL)
{
  if (!is.null(youngest.cohort)) fgm.data <- subset(fgm.data, birth.year <= youngest.cohort)
  if (!is.null(oldest.cohort)) fgm.data <- subset(fgm.data, birth.year >= oldest.cohort)
  if (!is.null(na.rows)) fgm.data <- fgm.data[-na.rows,]

  fgm.data <- subset(fgm.data, circum <= 1)
  
  do.call(rbind, by(fgm.data, fgm.data[c(c("birth.year.fac", "governorate"), knetwork)], calc.grpavg.daughters, fgm.data, cohort.range, knetwork))
}

p12.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.circum + grpavg.circum:birth.year.fac + grpavg.med + grpavg.med:birth.year.fac + grpavg.circum_med + grpavg.circum_med:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p12 <- p12.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(p12)))
  p12 <- p12.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(p12)))

vac.12 <- vcovHAC(p12)

coeftest(p12, vcov = vac.12)

coef.names <- names(coef(p12))
F12.1 <- linearHypothesis(p12, coef.names[grep("_med$", coef.names)], vcov = vac.12)
F12.2 <- linearHypothesis(p12, coef.names[grep("[^_]med$", coef.names)], vcov = vac.12)
F12.3 <- linearHypothesis(p12, coef.names[grep("grpavg\\..*circum$", coef.names)], vcov = vac.12)

p13.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.circum + grpavg.circum:birth.year.fac + grpavg.med + grpavg.med:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p13 <- p13.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(p13)))
  p13 <- p13.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(p13)))

vac.13 <- vcovHAC(p13)

coeftest(p13, vcov = vac.13)

coef.names <- names(coef(p13))
F13.1 <- linearHypothesis(p13, coef.names[grep("^birth.year.*circum$", coef.names)], vcov = vac.13)
F13.2 <- linearHypothesis(p13, coef.names[grep("^birth.year.*med$", coef.names)], vcov = vac.13)

p14.lm <- function(fgm)
  lm(has.or.intends.circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.has.or.intends.circum + grpavg.has.or.intends.circum:birth.year.fac + grpavg.med + grpavg.med:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p14 <- p14.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(p14)))
  p14 <- p14.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(p14)))

vac.14 <- vcovHAC(p14)

coeftest(p14, vcov = vac.14)

coef.names <- names(coef(p14))
F14.1 <- linearHypothesis(p14, coef.names[grep("med$", coef.names)], vcov = vac.14)
F14.2 <- linearHypothesis(p14, coef.names[grep("^birth.year.*circum$", coef.names)], vcov = vac.14)

p15.lm <- function(fgm)
  lm(has.or.intends.circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.has.or.intends.circum + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p15 <- p15.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(p15)))
  p15 <- p15.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(p15)))

vac.15 <- vcovHAC(p15)

coeftest(p15, vcov = vac.15)

p16.lm <- function(fgm)
  lm(has.or.intends.circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.has.or.intends.circum + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

yngst <- 2000

p16 <- p16.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, youngest.cohort = yngst))

if (!is.null(na.action(p16)))
  p16 <- p16.lm(fgm <- subset.to.regress.daughters(fgm, youngest.cohort = yngst, na.rows = na.action(p16)))

vac.16 <- vcovHAC(p16)

coeftest(p16, vcov = vac.16)

# Same as 12 but with no cohort interaction with grpavg.circum
p17.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.circum + grpavg.med + grpavg.med:birth.year.fac + grpavg.circum_med + grpavg.circum_med:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p17 <- p17.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(p17)))
  p17 <- p17.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(p17)))

vac.17 <- vcovHAC(p17)

coeftest(p17, vcov = vac.17)

coef.names <- names(coef(p17))
F17.1 <- linearHypothesis(p17, coef.names[grep("_med$", coef.names)], vcov = vac.17)
F17.2 <- linearHypothesis(p17, coef.names[grep("[^_]med$", coef.names)], vcov = vac.17)

# Same as 12 but without circum_med
p18.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.circum + grpavg.circum:birth.year.fac + grpavg.med + grpavg.med:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p18 <- p18.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(p18)))
  p18 <- p18.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(p18)))

vac.18 <- vcovHAC(p18)

coeftest(p18, vcov = vac.18)

coef.names <- names(coef(p18))
F18.1 <- linearHypothesis(p18, coef.names[grep("_med$", coef.names)], vcov = vac.18)
F18.2 <- linearHypothesis(p18, coef.names[grep("[^_]med$", coef.names)], vcov = vac.18)

# Using med2 
p19.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.circum + grpavg.circum:birth.year.fac + grpavg.circum_med2 + grpavg.circum_med2:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p19 <- p19.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(p19)))
  p19 <- p19.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(p19)))

vac.19 <- vcovHAC(p19)

coeftest(p19, vcov = vac.19)

coef.names <- names(coef(p19))
F19.1 <- linearHypothesis(p19, coef.names[grep("_med$", coef.names)], vcov = vac.19)
F19.2 <- linearHypothesis(p19, coef.names[grep("[^_]med$", coef.names)], vcov = vac.19)

# No social effects
p20.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p20 <- p20.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(p20)))
  p20 <- p20.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(p20)))

vac.20 <- vcovHAC(p20)

coeftest(p20, vcov = vac.20)

# Same as 20 but with fgm.or.intends
p21.lm <- function(fgm)
  lm(has.or.intends.circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p21 <- p21.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, youngest.cohort = 2008))

if (!is.null(na.action(p21)))
  p21 <- p21.lm(fgm <- subset.to.regress.daughters(fgm, youngest.cohort == 2008, na.rows = na.action(p21)))

vac.21 <- vcovHAC(p21)

coeftest(p21, vcov = vac.21)

p22.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + wealth.index.2 + wealth.index.2:birth.year.fac + grpavg.circum + grpavg.circum:birth.year.fac + grpavg.circum_med2 + grpavg.circum_med2:birth.year.fac + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + partner.educlvl.fac, data = fgm) 

p22 <- p22.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, cohort.range = 2, knetwork = "wealth.index.2"))

if (!is.null(na.action(p22)))
  p22 <- p22.lm(fgm <- subset.to.regress.daughters(fgm, cohort.range = 2, knetwork = "wealth.index.2", na.rows = na.action(p22)))

vac.22 <- vcovHAC(p22)

coeftest(p22, vcov = vac.22)

coef.names <- names(coef(p22))
F22.1 <- linearHypothesis(p22, coef.names[grep("_med$", coef.names)], vcov = vac.22)
F22.2 <- linearHypothesis(p22, coef.names[grep("[^_]med$", coef.names)], vcov = vac.22)
F22.3 <- linearHypothesis(p22, coef.names[grep("grpavg\\..*circum$", coef.names)], vcov = vac.22)

p23.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + wealth.index.2 + wealth.index.2:birth.year.fac + grpavg.circum + grpavg.circum:birth.year.fac + grpavg.med2 + grpavg.med2:birth.year.fac + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + partner.educlvl.fac, data = fgm) 

p23 <- p23.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, cohort.range = 2, knetwork = "wealth.index.2"))

if (!is.null(na.action(p23)))
  p23 <- p23.lm(fgm <- subset.to.regress.daughters(fgm, cohort.range = 2, knetwork = "wealth.index.2", na.rows = na.action(p23)))

vac.23 <- vcovHAC(p23)

coeftest(p23, vcov = vac.23)
coef.names <- names(coef(p23))

p24.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + wealth.index.2 + wealth.index.2:birth.year.fac + grpavg.circum + grpavg.circum_med3 + grpavg.circum_med3:birth.year.fac + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + partner.educlvl.fac, data = fgm) 

p24 <- p24.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, knetwork = "wealth.index.2"))

if (!is.null(na.action(p24)))
  p24 <- p24.lm(fgm <- subset.to.regress.daughters(fgm, knetwork = "wealth.index.2", na.rows = na.action(p24)))

vac.24 <- vcovHAC(p24)

coeftest(p24, vcov = vac.24)
coef.names <- names(coef(p24))

p25.lm <- function(fgm)
  lm(circum ~ governorate*birth.year.fac + wealth.index.2*birth.year.fac + grpavg.med*birth.year.fac + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + partner.educlvl.fac, data = fgm) 

p25 <- p25.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, knetwork = "wealth.index.2"))

if (!is.null(na.action(p25)))
  p25 <- p25.lm(fgm <- subset.to.regress.daughters(fgm, knetwork = "wealth.index.2", na.rows = na.action(p25)))

vac.25 <- vcovHAC(p25)

coeftest(p25, vcov = vac.25)
coef.names <- names(coef(p25))

F25.1 <- linearHypothesis(p25, coef.names[grep("med$", coef.names)], vcov = vac.25)
F25.2 <- linearHypothesis(p25, coef.names[grep("birth.year.*med$", coef.names)], vcov = vac.25)
F25.3 <- linearHypothesis(p25, coef.names[grep("birth.year.*index\\.2rich$", coef.names)], vcov = vac.25)

p26.lm <- function(fgm)
  lm(circum ~ governorate*birth.year.fac + wealth.index.2*birth.year.fac + grpavg.circum*birth.year.fac + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + partner.educlvl.fac, data = fgm) 

p26 <- p26.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, knetwork = "wealth.index.2"))

if (!is.null(na.action(p26)))
  p26 <- p26.lm(fgm <- subset.to.regress.daughters(fgm, knetwork = "wealth.index.2", na.rows = na.action(p26)))

vac.26 <- vcovHAC(p26)

coeftest(p26, vcov = vac.26)
coef.names <- names(coef(p26))

F26.1 <- linearHypothesis(p26, coef.names[grep("circum$", coef.names)], vcov = vac.26)
F26.2 <- linearHypothesis(p26, coef.names[grep("birth.year.*circum$", coef.names)], vcov = vac.26)
F26.3 <- linearHypothesis(p26, coef.names[grep("birth.year.*index\\.2rich$", coef.names)], vcov = vac.26)

p27.lm <- function(fgm)
  lm(circum ~ governorate*birth.year.fac + wealth.index.2*birth.year.fac + grpavg.circum_med*birth.year.fac + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + partner.educlvl.fac, data = fgm) 

p27 <- p27.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, knetwork = "wealth.index.2"))

if (!is.null(na.action(p27)))
  p27 <- p27.lm(fgm <- subset.to.regress.daughters(fgm, knetwork = "wealth.index.2", na.rows = na.action(p27)))

vac.27 <- vcovHAC(p27)

coeftest(p27, vcov = vac.27)
coef.names <- names(coef(p27))

F27.1 <- linearHypothesis(p27, coef.names[grep("circum$", coef.names)], vcov = vac.27)
F27.2 <- linearHypothesis(p27, coef.names[grep("birth.year.*circum$", coef.names)], vcov = vac.27)
F27.3 <- linearHypothesis(p27, coef.names[grep("birth.year.*index\\.2rich$", coef.names)], vcov = vac.27)

p28.lm <- function(fgm)
  lm(circum ~ governorate*birth.year.fac + wealth.index.2*birth.year.fac + grpavg.circum*birth.year.fac + grpavg.circum_med4*birth.year.fac + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + partner.educlvl.fac, data = fgm) 

p28 <- p28.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, knetwork = "wealth.index.2"))

if (!is.null(na.action(p28)))
  p28 <- p28.lm(fgm <- subset.to.regress.daughters(fgm, knetwork = "wealth.index.2", na.rows = na.action(p28)))

vac.28 <- vcovHAC(p28)

coeftest(p28, vcov = vac.28)
coef.names <- names(coef(p28))

F28.1 <- linearHypothesis(p28, coef.names[grep("circum$", coef.names)], vcov = vac.28)
F28.2 <- linearHypothesis(p28, coef.names[grep("birth.year.*circum$", coef.names)], vcov = vac.28)
F28.3 <- linearHypothesis(p28, coef.names[grep("birth.year.*index\\.2rich$", coef.names)], vcov = vac.28)

p29.lm <- function(fgm)
  lm(circum ~ (governorate + wealth.index.2 + birth.year.fac)^3 + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + partner.educlvl.fac, data = fgm) 

p29 <- p29.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, knetwork = "wealth.index.2"))

if (!is.null(na.action(p29)))
  p29 <- p29.lm(fgm <- subset.to.regress.daughters(fgm, knetwork = "wealth.index.2", na.rows = na.action(p29)))

vac.29 <- vcovHAC(p29)

coeftest(p29, vcov = vac.29)
coef.names <- names(coef(p29))

F29.1 <- linearHypothesis(p29, coef.names[grep("governorate.*wealth.index.2.*birth.year.*", coef.names)], vcov = vac.29)

m1.lm <- function(fgm)
  lm(med.circum ~ birth.year.fac + governorate + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm, subset = circum == 1) 
yngst <- 2000

m1 <- m1.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(m1)))
  m1 <- m1.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(m1)))

vac.m1 <- vcovHAC(m1)

coeftest(m1, vcov = vac.m1)

coef.names <- names(coef(m1))
linearHypothesis(m1, coef.names[grep("med$", coef.names)], vcov = vac.m1)

m2.lm <- function(fgm)
  lm(med.circum ~ birth.year.fac + governorate + wealth.index.2 + wealth.index.2:birth.year.fac + grpavg.med + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + partner.educlvl.fac, data = fgm, subset = circum == 1) 
m2 <- m2.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, knetwork = "wealth.index.2"))

if (!is.null(na.action(m2)))
  m2 <- m2.lm(fgm <- subset.to.regress.daughters(fgm, knetwork = "wealth.index.2", na.rows = na.action(m2)))

vac.m2 <- vcovHAC(m2)

coeftest(m2, vcov = vac.m2)

coef.names <- names(coef(m2))
linearHypothesis(m2, coef.names[grep("med$", coef.names)], vcov = vac.m2)

m3.lm <- function(fgm)
  lm(med.circum ~ birth.year.fac + governorate + wealth.index.2 + wealth.index.2:birth.year.fac + grpavg.circum + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + partner.educlvl.fac, data = fgm, subset = circum == 1) 
m3 <- m3.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, knetwork = "wealth.index.2"))

if (!is.null(na.action(m3)))
  m3 <- m3.lm(fgm <- subset.to.regress.daughters(fgm, knetwork = "wealth.index.2", na.rows = na.action(m3)))

vac.m3 <- vcovHAC(m3)

coeftest(m3, vcov = vac.m3)

coef.names <- names(coef(m3))
linearHypothesis(m3, coef.names[grep("med$", coef.names)], vcov = vac.m3)

i1.lm <- function(fgm)
  ivreg(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + grpavg.circum + grpavg.med + grpavg.circum_med + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac | birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac + inst.grpavg.circum + inst.grpavg.med + inst.grpavg.circum_med, data = fgm) 

i1 <- i1.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(i1)))
  i1 <- i1.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(i1)))

vcov.type <- "HC0"
vc <- vcovHC(i1, vcov.type)

coeftest(i1, vcov = vc)

coef.names <- names(coef(i1))
linearHypothesis(i1, coef.names[grep("grpavg", coef.names)], vcov = vc)

i2.lm <- function(fgm)
  ivreg(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + grpavg.circum + grpavg.med + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac | birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac + inst.grpavg.circum + inst.grpavg.med, data = fgm) 

i2 <- i2.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(i2)))
  i2 <- i2.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(i2)))

vcov.type <- "HC0"
vc <- vcovHC(i2, vcov.type)

coeftest(i2, vcov = vc)

coef.names <- names(coef(i2))
linearHypothesis(i2, coef.names[grep("grpavg", coef.names)], vcov = vc)

i3.lm <- function(fgm)
  ivreg(circum ~ governorate*birth.year.fac + wealth.index.2*birth.year.fac + grpavg.med*birth.year.fac + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + partner.educlvl.fac | governorate*birth.year.fac + wealth.index.2*birth.year.fac + grpavg.med3*birth.year.fac + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + partner.educlvl.fac, data = fgm) 

i3 <- i3.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, cohort.range = 1, knetwork = "wealth.index.2"))

if (!is.null(na.action(i3)))
  i3 <- i3.lm(fgm <- subset.to.regress.daughters(fgm, cohort.range = 1, knetwork = "wealth.index.2", na.rows = na.action(i3)))

vac.i3 <- vcovHAC(i3)

coeftest(i3, vcov = vac.i3)
coef.names <- names(coef(i3))

Fi3.1 <- linearHypothesis(i3, coef.names[grep("med$", coef.names)], vcov = vac.i3)
Fi3.2 <- linearHypothesis(i3, coef.names[grep("birth.year.*med$", coef.names)], vcov = vac.i3)
Fi3.3 <- linearHypothesis(i3, coef.names[grep("birth.year.*index\\.2rich$", coef.names)], vcov = vac.i3)
###########################################################################################

calc.grpavg.all <- function(df)
{
  current.birth.year <- df$birth.year[1]
  current.governorate <- df$governorate[1]
  current.urban.rural <- df$urban.rural[1]

  grp <- subset(fgm.data.all.08, 
                (birth.year <= current.birth.year + cohort.range) & (birth.year >= current.birth.year - cohort.range) & 
                (governorate == current.governorate) & (urban.rural == current.urban.rural))

  if ('has.or.intends.circum' %in% names(df)) 
    df$grpavg.has.or.intends.circum <- vapply(1:nrow(df), function(rowid) mean(grp[-rowid, "has.or.intends.circum"], na.rm = TRUE), 0) 

  df$grpavg.circum <- vapply(1:nrow(df), function(rowid) mean(grp[-rowid, "circum"], na.rm = TRUE), 0) 
  df$grpavg.med <- vapply(1:nrow(df), function(rowid) mean(grp[-rowid, "med.circum"], na.rm = TRUE), 0) 
  df$grpavg.circum_med <- df$grpavg.circum * df$grpavg.med

  return(df)
}

subset.to.regress.all <- function(fgm.data, youngest.cohort = 1988, oldest.cohort = 1959, na.rows = NULL)
{
  if (!is.null(youngest.cohort)) fgm.data <- subset(fgm.data, (birth.year <= youngest.cohort) & (circum <= 1))
  if (!is.null(oldest.cohort)) fgm.data <- subset(fgm.data, (birth.year >= oldest.cohort) & (circum <= 1))
  if (!is.null(na.rows)) fgm.data <- fgm.data[-na.rows,]
  
  do.call(rbind, by(fgm.data, fgm.data[c("birth.year.fac", "governorate", "urban.rural")], calc.grpavg.all))
}

p15.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.circum + grpavg.circum:birth.year.fac + grpavg.med + grpavg.med:birth.year.fac + grpavg.circum_med + grpavg.circum_med:birth.year.fac + educ.lvl + marital.age + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p15 <- p15.lm(fgm <- subset.to.regress.all(fgm.data.all.08))

if (!is.null(na.action(p15)))
  p15 <- p15.lm(subset.to.regress.all(fgm, na.rows = na.action(p15)))

vcov.type <- "HC0"
vc <- vcovHC(p15, vcov.type)

coeftest(p15, vcov = vc)

coef.names <- names(coef(p15))
linearHypothesis(p15, coef.names[grep("med$", coef.names)], vcov = vc)
