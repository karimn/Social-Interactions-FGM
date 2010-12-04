library(sp)
library(spatstat)
library(maptools)

calc.grpavg <- function(df, total.df, cohort.range, regs)
{
  current.birth.year <- df$birth.year[1]
  current.governorate <- df$governorate[1]

  grp <- total.df[(total.df[["birth.year"]] <= current.birth.year) & (total.df[["birth.year"]] >= current.birth.year - cohort.range), ] 

  for (col.name in regs)
  {
    if (is.factor(df[, col.name]))
    {
      df <- factor.mean(df, grp, col.name, 'grpavg')
    }
    else
    {
      df[, paste('grpavg', col.name, sep = '.')] <- mean(grp[, col.name], na.rm = TRUE)
    }
  }

  return(df)
}

cleanup.by.hh <- function(df) {
  # Removing birth.year duplicates
  dup <- duplicated(df$birth.year)
  df <- df[!dup,]

  n <- nrow(df)

  df$n.ord <- n
  df$order <- 1:n

  return(df)
}

DaughterFgmData <- setRefClass("DaughterFgmData", 
  contains = "BaseFgmData",

  methods = list(
    initialize = function(ir.file, br.file, gps.file, youngest.cohort = 1996, dhs.year = 2008, ...)
    {
      individual.controls <<- c("wealth.index.2", "urban.rural", "educ.lvl", "med.help.permission.fac", "med.help.distance.fac", 
                                "med.help.transportation.fac", "marital.age", "mother.circum.fac", "occupation.2.fac", "religion", 
                                "partner.educlvl.fac", "order.fac", "hh.head.sex")

      cols <- quote(c(eval(FgmData.cols), sdcol.1:sdcol.7, s906.1:s906.7, s908.1:s908.7, s909.1:s909.7, s910.1:s910.7, s911.1:s911.7))

      callSuper(ir.file = ir.file, gps.file = gps.file, cols = cols, col.names = FgmData.col.names, dhs.year = dhs.year, ...)

      num.col <- length(FgmData.col.names)

      spdf@data <<- reshape(spdf@data, 
                           varying = list(names()[(num.col + 1):(num.col + 7)], 
                                          names()[(num.col + 8):(num.col + 14)], 
                                          names()[(num.col + 15):(num.col + 21)], 
                                          names()[(num.col + 22):(num.col + 28)], 
                                          names()[(num.col + 29):(num.col + 35)], 
                                          names()[(num.col + 36):(num.col + 42)]), 
                           direction = 'long', 
                           sep = '.', 
                           v.names = c('sdcol', 'line.num', 'mar.status', 'circum', 'who.circum', 'age.circum'), 
                           timevar = 'order')

      spdf@data <<- subset(spdf@data, !is.na(sdcol), select = c(-sdcol))
      spdf@data <<- transform(spdf@data, 
                             circum = as.numeric(circum), 
                             mar.status = as.numeric(mar.status), 
                             who.circum = as.numeric(who.circum))
      spdf@data <<- transform(spdf@data,
                             circum.yesno = ifelse(circum == 1, 1, 0))

      br <- read.dta(br.file, convert.underscore = TRUE)
      br <- subset(br, select = c(v001:v003, bidx, v437, v438, b2, sdno))
      spdf@data <<- merge(spdf@data, br, by.x = c('cluster', 'hh', 'respond', 'line.num'), by.y = c('v001', 'v002', 'v003', 'bidx'))
      rm(br)
      
      names(spdf@data)[names(spdf@data) == 'v437'] <- 'weight'
      names(spdf@data)[names(spdf@data) == 'v438'] <- 'height'
      names(spdf@data)[names(spdf@data) == 'b2'] <- 'birth.year'

      spdf@data <<- within(spdf@data, 
      {
        age <- dhs.year - birth.year
        has.or.intends.circum <- ifelse(circum == 1 | ((age <= 12) & (intends.circum == 1)), 1, 0)
        med.circum <- ifelse(circum == 1 & (who.circum %in% c(1, 2)), 1, 0)
        year.circum <- ifelse(circum == 1 & age.circum <= 19, birth.year + age.circum, NA)
        year.circum.fac <- factor(year.circum)
        order.fac <- factor(order)
        married <- ifelse(mar.status == 1, 1, 0)
        birth.year.fac <- factor(birth.year)
      })

      if (!is.null(youngest.cohort))
        spdf@data <<- subset(spdf@data, birth.year <= youngest.cohort)

      spdf@data <<- do.call(rbind, by(spdf@data, spdf@data$hh.id, cleanup.by.hh))
      spdf@data <<- spdf@data[ order(spdf@data$hh.id, spdf@data$birth.year), ]

      spdf@data <<- subset(spdf@data, circum <= 1)

      return(.self)
    }
))

DaughterFgmData$methods(
  generate.reg.means = function(cohort.range = 1)
  {
    spdf@data <<- do.call(rbind, by(spdf@data, spdf@data[c("birth.year.fac", "governorate")], calc.grpavg.fun, spdf@data, cohort.range))
  }
)

DaughterFgmData$methods(
  get.regress.formula = function(dep.var, include.grpavg = FALSE) 
  {
    individ.controls.formula <- paste(spdf@individual.controls, collapse = " + ")
    grpavg.formula <- paste(grep("^grpavg", names(), value = TRUE), collapse = " + ")

    sprintf("%s ~ birth.year.fac*governorate + %s %s",
            dep.var, 
            individ.controls.formula, 
            if (include.grpavg) grpavg.formula else "")
  }
)

DaughterFgmData$methods(
  regress = function(dep.var, include.grpavg = FALSE) 
  {
    regress.formula <- get.regress.formula(dep.var, include.grpavg) 

    lm(regress.formula, data = spdf@data)
  }
)

#if (!isGeneric("attach")) setGeneric("attach")

#setMethod("attach",
#          signature = c(what = "FgmData"),
#          function(what)
#          {
#            attach(what@data)
#          }
#)
#
#if (!isGeneric("detach")) setGeneric("detach")
#
#setMethod("detach",
#          signature = c(name = "FgmData"),
#          function(name, pos = 2, unload = FALSE, character.only = FALSE, force = FALSE)
#          {
#            detach(name@data, pos, unload, character.only, force)
#          }
#)

#if (!isGeneric("plm")) setGeneric("plm")
#
#setMethod("plm",
#          signature = c(formula = "ANY", data = "FgmData"),
#          function(formula, data, ...)
#          {
#            plm(formula, data = data@data, ...)
#            
#          }
#)


