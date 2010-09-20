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

factor.mean <- function(df, grp, col.name, prefix)
{
  stopifnot(is.factor(df[, col.name]))

  col.lvls <- levels(df[, col.name])
  grp.nrow <- nrow(grp)

  for (i in 2:length(col.lvls))
  {
    cleaned.lvl.name <- gsub(",", '', gsub("[\\s-&\\.]+", "_", col.lvls[i], perl = TRUE))
    new.col.name <- paste(paste(prefix, col.name, sep = '.'), cleaned.lvl.name, sep = '_')
    df[, new.col.name] <- sum(grp[, col.name] == col.lvls[i], na.rm = TRUE) / grp.nrow
  }

  return(df)
}

calc.grpavg <- function(df, total.df, ref.reg, cohort.range, dir.reg)
{
  current.birth.year <- df$birth.year[1]
  current.governorate <- df$governorate[1]
  current.refreg <- df[1, ref.reg]

  grp <- total.df[(total.df[["birth.year"]] <= current.birth.year + cohort.range) & (total.df[["birth.year"]] >= current.birth.year - cohort.range) & 
                  (total.df[[ref.reg]] == current.refreg), ] 

  for (col.name in dir.reg$factors)
  {
    stopifnot(col.name %in% names(grp))
    df <- factor.mean(df, grp, col.name, 'grpavg')
  }

  for (col.name in dir.reg$cont)
  {
    stopifnot(col.name %in% names(grp))
    df[, paste(col.name, 'grpavg', sep = '.')] <- mean(grp[, col.name], na.rm = TRUE)
  }

  return(df)
}

subset.to.reg <- function(fgm.data, dir.reg, ref.reg = NULL, calc.grpavg.fun = calc.grpavg, cohort.range = 1, youngest.cohort = 1996, na.rows = NULL)
{
  if (!is.null(youngest.cohort)) fgm.data <- subset(fgm.data, birth.year <= youngest.cohort)
  if (!is.null(na.rows)) fgm.data <- fgm.data[-na.rows,]

  fgm.data <- subset(fgm.data, circum <= 1)

  do.call(rbind, by(fgm.data, fgm.data[c(c("birth.year.fac", "governorate"), ref.reg)], calc.grpavg.fun, fgm.data, ref.reg, cohort.range, dir.reg))
}

gen.reg.formula <- function(fgm, dep.reg, ref.reg, dir.reg, ref.cohort.interact = TRUE)
{
  ref.reg.fe.formula <- NULL

  if (is.null(ref.reg))
    ref.reg.fe.formula <- ""
  else if (ref.cohort.interact)
    ref.reg.fe.formula <- paste('+', paste(paste(ref.reg, collapse = ' + '), paste(ref.reg, ':birth.year.fac', sep = '', collapse = ' + '), sep = " + "))
  else
    ref.reg.fe.formula <- paste('+', paste(ref.reg, collapse = ' + '))

  grpavg.reg <- paste(grep("^grpavg", names(fgm), value = TRUE), collapse = " + ")
  sprintf("%s ~ birth.year.fac + governorate + governorate:birth.year.fac %s + %s + %s", 
          dep.reg, 
          ref.reg.fe.formula, 
          paste(unlist(dir.reg, use.names = FALSE), collapse = ' + '),
          grpavg.reg)
}

###########################################################################################
# Ref groups: birth.year.fac, governorate, urban.rural

dep.reg <- c('circum')
ref.reg <- c("urban.rural")
all.fac.reg <- c("urban.rural", 'religion', 'educ.lvl', 'med.help.distance.fac', 'med.help.transportation.fac', 'mother.circum.fac', 'partner.educlvl.fac', 'wealth.index')
#dir.reg <- list(factors = c('urban.rural', 'religion', 'educ.lvl', 'med.help.permission.fac', 'med.help.distance.fac', 'med.help.transportation.fac',
#                            'mother.circum.fac', 'partner.educlvl.fac', 'occupation.2.fac', 'partner.occupation.2.fac'),
#                cont = c('marital.age'))
dir.reg <- list(factors = all.fac.reg[!all.fac.reg %in% ref.reg],
                cont = c('marital.age'))
fgm <- subset.to.reg(fgm.data.daughters.08@data, dir.reg, ref.reg)

grpavg.formula <- gen.reg.formula(fgm, dep.reg, ref.reg, dir.reg, ref.cohort.interact = FALSE)

p4 <- lm(formula(grpavg.formula), data = fgm) 

if (!is.null(na.action(p4)))
{
  fgm <- subset.to.reg(fgm, dir.reg, ref.reg, na.rows = na.action(p4))
  p4 <- lm(formula(grpavg.formula), data = fgm)  
}

vac.4 <- vcovHAC(p4)

coeftest(p4, vcov = vac.4)

###########################################################################################

dep.reg <- c('circum')
ref.reg <- c("wealth.index.2")
all.fac.reg <- c("urban.rural", 'religion', 'educ.lvl', 'med.help.distance.fac', 'med.help.transportation.fac', 'mother.circum.fac', 'wealth.index.2')
dir.reg <- list(factors = all.fac.reg[!all.fac.reg %in% ref.reg],
                cont = c('marital.age'))

fgm <- subset.to.reg(fgm.data.daughters.08@data, dir.reg, ref.reg)

grpavg.formula <- gen.reg.formula(fgm, dep.reg, ref.reg, dir.reg, ref.cohort.interact = FALSE)

p5 <- lm(formula(grpavg.formula), data = fgm) 

if (!is.null(na.action(p5)))
{
  fgm <- subset.to.reg(fgm, dir.reg, ref.reg, na.rows = na.action(p5))
  p5 <- lm(formula(grpavg.formula), data = fgm)  
}

vac.5 <- vcovHAC(p5)

coeftest(p5, vcov = vac.5)

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

calc.grpavg.daughters <- function(df, total.fgm, cohort.range, knetwork)
{
  current.birth.year <- df$birth.year[1]
  current.governorate <- df$governorate[1]
  current.urban.rural <- df$urban.rural[1]
  current.knetwork <- df[1, knetwork]

  grp <- total.fgm[(total.fgm[["birth.year"]] <= current.birth.year + cohort.range) & 
                   (total.fgm[["birth.year"]] >= current.birth.year - cohort.range) & 
                   (total.fgm[["governorate"]] == current.governorate) & 
                   (total.fgm[[knetwork]] == current.knetwork), ]

  grp.larger <- total.fgm[(total.fgm[["birth.year"]] <= current.birth.year + cohort.range) & 
                          (total.fgm[["birth.year"]] >= current.birth.year - cohort.range) & 
                          (total.fgm[[knetwork]] == current.knetwork), ]

  if ('has.or.intends.circum' %in% names(df)) 
    df$grpavg.has.or.intends.circum <- vapply(1:nrow(df), function(rowid) mean(grp[-rowid, "has.or.intends.circum"], na.rm = TRUE), 0) 

  df$grpavg.circum <- vapply(1:nrow(df), function(rowid) mean(grp[-rowid, "circum"], na.rm = TRUE), 0) 
  df$grpavg.med <- vapply(1:nrow(df), function(rowid) mean(grp[-rowid, "med.circum"], na.rm = TRUE), 0) 
  df$grpavg.med2 <- vapply(1:nrow(df), function(rowid) mean(grp.larger[-rowid, "med.circum"], na.rm = TRUE), 0) 
  df$grpavg.circum_med <- df$grpavg.circum * df$grpavg.med
  df$grpavg.circum_med2 <- df$grpavg.circum * df$grpavg.med2

  inst.grp <- grp.larger

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
F12.3 <- linearHypothesis(p12, coef.names[grep("grpavg\\..*circum$", coef.names)], vcov = vac.12)

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

p16.lm <- function(fgm)
  lm(has.or.intends.circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.has.or.intends.circum + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

yngst <- 2000

p16 <- p16.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, youngest.cohort = yngst))

if (!is.null(na.action(p16)))
  p16 <- p16.lm(fgm <- subset.to.regress.daughters(fgm, youngest.cohort = yngst, na.rows = na.action(p16)))

vc.16 <- vcovHC(p16, vcov.type)
vac.16 <- vcovHAC(p16)

coeftest(p16, vcov = vac.16)

# Same as 12 but with no cohort interaction with grpavg.circum
p17.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.circum + grpavg.med + grpavg.med:birth.year.fac + grpavg.circum_med + grpavg.circum_med:birth.year.fac + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + occupation.2.fac + partner.occupation.2.fac + religion + wealth.index + partner.educlvl.fac, data = fgm) 

p17 <- p17.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data))

if (!is.null(na.action(p17)))
  p17 <- p17.lm(fgm <- subset.to.regress.daughters(fgm, na.rows = na.action(p17)))

vc.17 <- vcovHC(p17, vcov.type)
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

p22 <- p22.lm(fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, knetwork = "wealth.index.2"))

if (!is.null(na.action(p22)))
  p22 <- p22.lm(fgm <- subset.to.regress.daughters(fgm, knetwork = "wealth.index.2", na.rows = na.action(p22)))

vac.22 <- vcovHAC(p22)

coeftest(p22, vcov = vac.22)

coef.names <- names(coef(p22))
F22.1 <- linearHypothesis(p22, coef.names[grep("_med$", coef.names)], vcov = vac.22)
F22.2 <- linearHypothesis(p22, coef.names[grep("[^_]med$", coef.names)], vcov = vac.22)
F22.3 <- linearHypothesis(p22, coef.names[grep("grpavg\\..*circum$", coef.names)], vcov = vac.22)

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


