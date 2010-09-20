library(car)
library(lmtest)
library(sandwich)

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

