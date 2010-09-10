library(plm)
library(car)
library(lmtest)
library(sandwich)

oldest.cohort <- 1988
youngest.cohort <- 1996 

fgm.daughters <- subset(fgm.data.daughters.08@data, 
                        (birth.year >= oldest.cohort) & (birth.year <= youngest.cohort) & (circum <= 1),
                        select = c(circum, birth.year, birth.year.fac, governorate, urban.rural, educ.lvl, marital.age, med.circum,
                                   religion, wealth.index, partner.educlvl.fac))


fgm.all <- subset(fgm.data.all.08, 
                  (birth.year >= oldest.cohort) & (birth.year <= youngest.cohort) & (circum <= 1),
                  select = c(circum, birth.year, birth.year.fac, governorate, urban.rural, educ.lvl, marital.age, med.circum,
                             religion, wealth.index, partner.educlvl.fac))

fgm <- rbind(fgm.daughters, fgm.all)

cohort.range <- 1

calc.grpavg <- function(df)
{
  current.birth.year <- df$birth.year[1]
  current.governorate <- df$governorate[1]
  current.urban.rural <- df$urban.rural[1]

  grp <- subset(fgm, 
                (birth.year <= current.birth.year + cohort.range) & (birth.year >= current.birth.year - cohort.range) & 
                (governorate == current.governorate) & (urban.rural == current.urban.rural))

  df$grpavg.circum <- vapply(1:nrow(df), function(rowid) mean(grp[-rowid, "circum"], na.rm = TRUE), 0) 
  df$grpavg.med <- vapply(1:nrow(df), function(rowid) mean(grp[-rowid, "med.circum"], na.rm = TRUE), 0) 
  df$grpavg.circum_med <- df$grpavg.circum * df$grpavg.med

  return(df)
}

subset.to.regress <- function(fgm.data, na.rows = NULL, k.netw = "urban.rural")
{
  if (!is.null(na.rows)) fgm.data <- fgm.data[-na.rows,]
  
  do.call(rbind, by(fgm.data, fgm.data[c("birth.year.fac", "governorate", k.netw)], calc.grpavg))
}

run.lm <- function(fgm)
  lm(circum ~ birth.year.fac + governorate + governorate:birth.year.fac + urban.rural + urban.rural:birth.year.fac + grpavg.circum + grpavg.circum:birth.year.fac + grpavg.med + grpavg.med:birth.year.fac + grpavg.circum_med + grpavg.circum_med:birth.year.fac + marital.age + religion + wealth.index, data = fgm) 

lm.res <- run.lm(fgm <- subset.to.regress(fgm))

if (!is.null(na.action(lm.res)))
  lm.res <- run.lm(fgm <- subset.to.regress(fgm, na.rows = na.action(lm.res)))

vc <- vcovHC(lm.res)

coeftest(lm.res, vcov = vc)

coef.names <- names(coef(lm.res))
linearHypothesis(lm.res, coef.names[grep("circum$", coef.names)], vcov = vc)
