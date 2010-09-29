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
    df[, paste('grpavg', col.name, sep = '.')] <- mean(grp[, col.name], na.rm = TRUE)
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
    #ref.reg.fe.formula <- paste('+', paste(paste(ref.reg, collapse = ' + '), paste(ref.reg, ':birth.year.fac', sep = '', collapse = ' + '), sep = " + "))
    ref.reg.fe.formula <- sprintf("birth.year.fac*%s", ref.reg)
  else
    #ref.reg.fe.formula <- paste('+', paste(ref.reg, collapse = ' + '))
    ref.reg.fe.formula <- ref.reg

  grpavg.reg <- paste(grep("^grpavg", names(fgm), value = TRUE), collapse = " + ")

  sprintf("%s ~ birth.year.fac*governorate + %s + %s + %s", 
          dep.reg, 
          ref.reg.fe.formula, 
          paste(unlist(dir.reg, use.names = FALSE), collapse = ' + '),
          grpavg.reg)
}

###########################################################################################
# Ref groups: birth.year.fac, governorate, urban.rural

dep.reg <- c('has.or.intends.circum', 'circum')
ref.reg <- c("wealth.index.2", "urban.rural", "religion")
all.fac.reg <- c("urban.rural", 'religion', 'educ.lvl', 'med.help.distance.fac', 'med.help.transportation.fac', 'mother.circum.fac', 'partner.educlvl.fac', 'wealth.index.2', 'hh.head.sex')
#dir.reg <- list(factors = c('urban.rural', 'religion', 'educ.lvl', 'med.help.permission.fac', 'med.help.distance.fac', 'med.help.transportation.fac',
#                            'mother.circum.fac', 'partner.educlvl.fac', 'occupation.2.fac', 'partner.occupation.2.fac'),
#                cont = c('marital.age'))
cont.reg <- c("marital.age", "order")

fgm.regress.manski <- function(fgm.data, dep.reg = dep.reg, all.fac.reg = all.fac.reg, cont.reg = cont.reg, ref.reg = ref.reg)
{
  results = vector("list", length(ref.reg))

  names(results) <- ref.reg

  for (k in names(results))
  {
    results[[k]] <- list("vector", length(dep.reg))
    names(results[[k]]) <- dep.reg

    for (dep in dep.reg)
    {
      dir.reg <- list(factors = all.fac.reg[!all.fac.reg %in% k],
                      cont = cont.reg)

      fgm <- subset.to.reg(fgm.data, dir.reg, k)

      grpavg.formula <- gen.reg.formula(fgm, dep, k, dir.reg, ref.cohort.interact = TRUE)

      print(grpavg.formula)

      p <- lm(formula(grpavg.formula), data = fgm) 

      if (!is.null(na.action(p)))
      {
        fgm <- subset.to.reg(fgm, dir.reg, k, na.rows = na.action(p))
        p <- lm(formula(grpavg.formula), data = fgm)  
      }

      results[[k]][[dep]] <- list(fgm <-fgm, lm = p, formula = grpavg.formula) #, vcov = vcovHAC(p))
    }
  }

  return(results)
}

###########################################################################################

