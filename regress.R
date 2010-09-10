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

p1 <- lm(circum.num ~ ADM1SALBNA + birth.year.fac + urban.rural + religion + wealth.index,
           data = fgm.data.all@data)

p2 <- lm(circum.age ~ ADM1SALBNA + birth.year.fac + urban.rural + religion + wealth.index,
           data = fgm.data.all@data,
           subset = (circum == 'yes'))

p3 <- lm(has.or.intends.circum ~ ADM1SALBNA + birth.year.fac + urban.rural + religion + wealth.index + educ.lvl + med.help.permission.fac +
                                   med.help.distance.fac + med.help.transportation.fac +
                                   marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac,
           data = fgm.data.daughters.08@data)
           
###########################################################################################

factor.mean <- function(df, col.name, prefix)
{
  stopifnot(is.factor(df[, col.name]))

  col.lvls <- levels(df[, col.name])
  df.nrow <- nrow(df)

  for (i in 2:length(col.lvls))
  {
    cleaned.lvl.name <- gsub(",", '', gsub("[\\s-&\\.]+", "_", col.lvls[i], perl = TRUE))
    new.col.name <- paste(paste(prefix, col.name, sep = '.'), cleaned.lvl.name, sep = '_')
    df[, new.col.name] <- sum(df[, col.name] == col.lvls[i], na.rm = TRUE) / df.nrow
  }

  return(df)
}

###########################################################################################
# Ref groups: birth.year.fac, governorate

grpavg.col.names <- c('urban.rural', 'religion', 'educ.lvl', 'med.help.permission.fac', 'med.help.distance.fac', 'med.help.transportation.fac',
                      'mother.circum.fac', 'partner.educlvl.fac', 'occupation.2.fac', 'partner.occupation.2.fac')

grpavg.formula <- "has.or.intends.circum ~ governorate * birth.year.fac + urban.rural + religion + wealth.index + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac"

fgm <- do.call(rbind, by(fgm.data.daughters.08@data, fgm.data.daughters.08@data[c('governorate', 'birth.year.fac')], function(df)
{
  for (col.name in grpavg.col.names)
  {
    df <- factor.mean(df, col.name, 'grpavg')
  }

  df$grpavg.marital.age <- mean(df$marital.age, na.rm = TRUE)

  return(df)
}))

grpavg.formula <- paste(grpavg.formula, paste(grep("^grpavg", names(fgm), value = TRUE), collapse = " + "), sep = " + ")

p4 <- lm(formula(grpavg.formula), data = fgm)

###########################################################################################
# Ref groups: birth.year.fac, governorate, urban.rural 

grpavg.col.names <- c('religion', 'educ.lvl', 'med.help.permission.fac', 'med.help.distance.fac', 'med.help.transportation.fac',
                      'mother.circum.fac', 'partner.educlvl.fac', 'occupation.2.fac', 'partner.occupation.2.fac')

grpavg.formula <- "has.or.intends.circum ~ governorate + governorate:birth.year.fac + birth.year.fac + urban.rural + urban.rural:birth.year.fac + religion + wealth.index + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac"

fgm <- do.call(rbind, by(fgm.data.daughters.08@data, fgm.data.daughters.08@data[c('governorate', 'birth.year.fac', 'urban.rural')], function(df)
{
  for (col.name in grpavg.col.names)
  {
    df <- factor.mean(df, col.name, 'grpavg')
  }

  df$grpavg.marital.age <- mean(df$marital.age, na.rm = TRUE)

  return(df)
}))

grpavg.formula <- paste(grpavg.formula, paste(grep("^grpavg", names(fgm), value = TRUE), collapse = " + "), sep = " + ")

p5 <- lm(formula(grpavg.formula), data = fgm)

###########################################################################################
# Ref groups: birth.year.fac, governorate, wealth.index 

grpavg.col.names <- c('urban.rural', 'educ.lvl', 'religion', 'med.help.permission.fac', 'med.help.distance.fac', 'med.help.transportation.fac',
                      'mother.circum.fac', 'partner.educlvl.fac', 'occupation.2.fac', 'partner.occupation.2.fac')

grpavg.formula <- "has.or.intends.circum ~ (governorate + birth.year.fac + wealth.index)^2 + religion + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac"

fgm <- do.call(rbind, by(fgm.data.daughters.08@data, fgm.data.daughters.08@data[c('governorate', 'birth.year.fac', 'wealth.index')], function(df)
{
  for (col.name in grpavg.col.names)
  {
    df <- factor.mean(df, col.name, 'grpavg')
  }

  df$grpavg.marital.age <- mean(df$marital.age, na.rm = TRUE)

  return(df)
}))

grpavg.formula <- paste(grpavg.formula, paste(grep("^grpavg", names(fgm), value = TRUE), collapse = " + "), sep = " + ")

p6 <- lm(formula(grpavg.formula), data = fgm)
###########################################################################################

radius <- 1 
grpavg.col.names <- c('urban.rural', 'educ.lvl', 'religion', 'med.help.permission.fac', 'med.help.distance.fac', 'med.help.transportation.fac',
                      'mother.circum.fac', 'partner.educlvl.fac', 'occupation.2.fac', 'partner.occupation.2.fac', 'wealth.index')

grpavg.formula <- "has.or.intends.circum ~ birth.year.fac + governorate + wealth.index + religion + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac"

byrad.grpavg.fun <- function(uc, df)
{
  df <- df@data
  for (col.name in grpavg.col.names)
  {
    df <- factor.mean(df, col.name, 'grpavg')
  }

  df$grpavg.marital.age <- mean(df$marital.age, na.rm = TRUE)

  # This is important to only return the rows for which the above means are relevant
  retss <- df[df$unique.cluster == uc, ]

  return(retss)
}

fgm <- do.call(rbind, by.radius(fgm.data.daughters.08, radius, indices = fgm.data.daughters.08@data[c("birth.year.fac")], byrad.grpavg.fun))

grpavg.formula <- paste(grpavg.formula, paste(grep("^grpavg", names(fgm), value = TRUE), collapse = " + "), sep = " + ")
p7 <- lm(formula(grpavg.formula), data = fgm)

###########################################################################################

# Ref group: governorate and religion

fgm <- do.call(rbind, by(fgm.data.daughters.08@data, fgm.data.daughters.08@data[c("birth.year.fac", "governorate", "religion")], function(df)
{
  df$grpavg.has.or.intends.circum <- mean(df$has.or.intends.circum, na.rm = TRUE) 
  df$grpavg.circum <- mean(df$circum, na.rm = TRUE)
  df$grpavg.med <- mean(df$med.circum, na.rm = TRUE)
  df$grpavg.circum_med <- df$grpavg.circum * df$grpavg.med

  return(df)
}))

p8 <- lm(has.or.intends.circum ~ birth.year.fac + governorate + governorate:birth.year.fac + religion + religion:birth.year.fac + grpavg.has.or.intends.circum + wealth.index + grpavg.circum_med + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac,
        data = fgm) 
        
###########################################################################################

# Ref group: governorate

fgm <- do.call(rbind, by(fgm.data.daughters.08@data, fgm.data.daughters.08@data[c("birth.year.fac", "governorate", "urban.rural")], function(df)
{
  df$grpavg.has.or.intends.circum <- mean(df$has.or.intends.circum, na.rm = TRUE) 
  df$grpavg.circum <- mean(df$circum, na.rm = TRUE)
  df$grpavg.med <- mean(df$med.circum, na.rm = TRUE)
  df$grpavg.circum_med <- df$grpavg.circum * df$grpavg.med

  return(df)
}))

p9 <- lm(has.or.intends.circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.has.or.intends.circum + grpavg.circum_med + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index,
        data = fgm) 

###########################################################################################
# Ref group: governorate

calc.grpavg <- function(df)
{
  current.birth.year <- df$birth.year[1]
  current.governorate <- df$governorate[1]
  current.urban.rural <- df$urban.rural[1]

  grp <- subset(fgm.data.daughters.08@data, 
                (birth.year <= current.birth.year) & (birth.year >= current.birth.year - 2) & 
                (governorate == current.governorate) & (urban.rural == current.urban.rural))

  df$grpavg.has.or.intends.circum <- mean(grp$has.or.intends.circum, na.rm = TRUE) 
  df$grpavg.circum <- mean(grp$circum, na.rm = TRUE)
  df$grpavg.med <- mean(grp$med.circum, na.rm = TRUE)
  df$grpavg.circum_med <- df$grpavg.circum * df$grpavg.med

  return(df)
}

fgm <- do.call(rbind, by(fgm.data.daughters.08@data, fgm.data.daughters.08@data[c("birth.year.fac", "governorate", "urban.rural")], calc.grpavg))

p10 <- lm(has.or.intends.circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.has.or.intends.circum + grpavg.has.or.intends.circum:birth.year.fac + grpavg.circum_med + grpavg.circum_med:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index,
        data = fgm) 
###########################################################################################
# Ref group: governorate

cohort.range <- 1

calc.grpavg.daughters <- function(df)
{
  current.birth.year <- df$birth.year[1]
  current.governorate <- df$governorate[1]
  current.urban.rural <- df$urban.rural[1]

  grp <- subset(fgm.data.daughters.08@data, 
                (birth.year <= current.birth.year + cohort.range) & (birth.year >= current.birth.year - cohort.range) & 
                (governorate == current.governorate) & (urban.rural == current.urban.rural))

  if ('has.or.intends.circum' %in% names(df)) 
    df$grpavg.has.or.intends.circum <- vapply(1:nrow(df), function(rowid) mean(grp[-rowid, "has.or.intends.circum"], na.rm = TRUE), 0) 

  df$grpavg.circum <- vapply(1:nrow(df), function(rowid) mean(grp[-rowid, "circum"], na.rm = TRUE), 0) 
  df$grpavg.med <- vapply(1:nrow(df), function(rowid) mean(grp[-rowid, "med.circum"], na.rm = TRUE), 0) 
  df$grpavg.circum_med <- df$grpavg.circum * df$grpavg.med

  inst.grp <- subset(fgm.data.daughters.08@data, 
                (birth.year <= current.birth.year + cohort.range) & (birth.year >= current.birth.year - cohort.range) & 
                (urban.rural == current.urban.rural))

  if ('has.or.intends.circum' %in% names(df)) 
    df$inst.grpavg.has.or.intends.circum <- mean(inst.grp$has.or.intends.circum, na.rm = TRUE) 

  df$inst.grpavg.circum <- mean(inst.grp$circum, na.rm = TRUE)
  df$inst.grpavg.med <- mean(inst.grp$med.circum, na.rm = TRUE)
  df$inst.grpavg.circum_med <- df$inst.grpavg.circum * df$inst.grpavg.med

  return(df)
}

subset.to.regress.daughters <- function(fgm.data, youngest.cohort = 1996, na.rows = NULL)
{
  if (!is.null(youngest.cohort)) fgm.data <- subset(fgm.data, (birth.year <= youngest.cohort) & (circum <= 1))
  if (!is.null(na.rows)) fgm.data <- fgm.data[-na.rows,]
  
  do.call(rbind, by(fgm.data, fgm.data[c("birth.year.fac", "governorate", "urban.rural")], calc.grpavg.daughters))
}

vcov.type <- "HC0"

#p11 <- lm(has.or.intends.circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.circum + grpavg.circum:birth.year.fac + grpavg.circum_med + grpavg.circum_med:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index,
#        data = fgm) 

p12.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.circum + grpavg.circum:birth.year.fac + grpavg.med + grpavg.med:birth.year.fac + grpavg.circum_med + grpavg.circum_med:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p12 <- p12.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(p12)))
  p12 <- p12.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(p12)))

vc.12 <- vcovHC(p12, vcov.type)
vac.12 <- vcovHAC(p12)

coeftest(p12, vcov = vac.12)

coef.names <- names(coef(p12))
F12.1 <- linearHypothesis(p12, coef.names[grep("_med$", coef.names)], vcov = vac.12)
F12.2 <- linearHypothesis(p12, coef.names[grep("[^_]med$", coef.names)], vcov = vac.12)

p13.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.circum + grpavg.circum:birth.year.fac + grpavg.med + grpavg.med:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p13 <- p13.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(p13)))
  p13 <- p13.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(p13)))

vc.13 <- vcovHC(p13, vcov.type)
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

vc14 <- vcovHC(p14, vcov.type)
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

vc15 <- vcovHC(p15, vcov.type)
vac.15 <- vcovHAC(p15)

coeftest(p15, vcov = vac.15)

coef.names <- names(coef(p15))

p16.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.circum + grpavg.med + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p16 <- p16.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(p16)))
  p16 <- p16.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(p16)))

vc.16 <- vcovHC(p16, vcov.type)
vac.16 <- vcovHAC(p16)

coeftest(p16, vcov = vac.16)

m1.lm <- function(fgm)
  lm(med.circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + grpavg.circum + grpavg.circum:birth.year.fac + grpavg.med + grpavg.med:birth.year.fac + grpavg.circum_med + grpavg.circum_med:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm, subset = circum == 1) 

m1 <- m1.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(m1)))
  m1 <- m1.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(m1)))

vc.m1 <- vcovHC(m1, vcov.type)
vac.m1 <- vcovHAC(m1)

coeftest(m1, vcov = vac.m1)

coef.names <- names(coef(m1))
linearHypothesis(m1, coef.names[grep("med$", coef.names)], vcov = vac.m1)

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
###########################################################################################

#reg.pooled.lagged.med <- function(fgm.data) {
  #dynformula() doesn't seem to be working for me
  #lagged.formula <- dynformula(circum.yesno ~ year.circum + med.circum, list(2,1,c(2,2)))
  #lagged.formula <- dynformula(circum.yesno ~ med.circum, list(1, c(1, 1)))

res.yesno <- plm(circum.yesno ~ lag(circum.yesno, 1) + region + urban.rural + religion + hh.head.sex + wealth.index + lag(med.circum, 1), 
                 data = fgm.data.08, index = c("hh.id", "order.fac"), model = "pooling")
res.intends <- plm(has.or.intends.circum ~ lag(has.or.intends.circum, 1) + lag(med.circum, 1) + region + urban.rural + religion + hh.head.sex + wealth.index + order.fac, 
                   data = fgm.data.08, index = c("hh.id", "order.fac"), model = "pooling")

#return (c(res.yesno, res.intends))
#}

# Can't regress on year.circum because we only have data for years where circum.yesno is 1
#res.fe <- plm(circum.yesno ~ year.circum, data = fgm.data.08, index = c("hh.id", "order"), model = "within")

res.fe.yesno <- plm(circum.yesno ~ order.fac, data = fgm.data.08, index = c("hh.id", "order.fac"), model = "within", effect = "individual")


