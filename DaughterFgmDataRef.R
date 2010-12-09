library(sp)
library(spatstat)
library(maptools)
library(sandwich)
library(lmtest)
library(plm)
library(AER)

calc.grpavg <- function(df, total.df, cohort.range, regs, prefix = FgmData.grpavg.prefix, lag = 0, exclude.self = FALSE)
{
  #print(sprintf("calc.grpavg: size = %i", nrow(df)))
  current.birth.year <- df$birth.year[1] - lag
  current.governorate <- df$governorate[1]

  grp <- total.df[(total.df[["birth.year"]] <= current.birth.year) & 
                  (total.df[["birth.year"]] >= current.birth.year - cohort.range) &
                  (total.df[["governorate"]] == current.governorate), ] 

  if (length(regs) > 0)
    for (col.name in regs)
    {
      if (is.factor(df[, col.name]))
      {
        df <- factor.mean(df, grp, col.name, prefix, exclude.self)
      }
      else
      {
        new.col.name <- paste(prefix, col.name, sep = '.')
        #df[, new.col.name] <- if (nrow(grp) != 0) mean(grp[, col.name], na.rm = TRUE) else NA
        df[, new.col.name] <- vapply(1:nrow(df), grp.mean, 0, grp, col.name, NULL, exclude.self)
      }
    }

  df$grp.size <- nrow(grp) 

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


DaughterFgmData.individual.controls <- c("wealth.index.2", "urban.rural", "educ.lvl", "marital.age", "mother.circum.fac", "religion", "hh.head.sex")
                                         
#DaughterFgmData.individual.controls <- c("wealth.index.2", "urban.rural", "educ.lvl", "med.help.permission.fac", "med.help.distance.fac", 
#                                         "med.help.transportation.fac", "marital.age", "mother.circum.fac", "occupation.2.fac", "religion", 
#                                         "partner.educlvl.fac", "hh.head.sex")

DaughterFgmData <- setRefClass("DaughterFgmData", 
  contains = "BaseFgmData",

  methods = list(
    initialize = function(ir.file = character(0), br.file, gps.file = character(0), 
                          youngest.cohort = 1996, 
                          dhs.year = 2008, 
                          individual.controls = DaughterFgmData.individual.controls, 
                          other.grpavg.controls = character(0),
                          ...)
    {
      cols <- quote(c(eval(FgmData.cols), sdcol.1:sdcol.7, s906.1:s906.7, s908.1:s908.7, s909.1:s909.7, s910.1:s910.7, s911.1:s911.7))

      callSuper(ir.file = ir.file, gps.file = gps.file, cols = cols, col.names = FgmData.col.names, dhs.year = dhs.year, individual.controls = individual.controls, other.grpavg.controls = other.grpavg.controls, ...)

      if (!is.empty(ir.file) & !is.empty(gps.file))
      {
        num.col <- length(FgmData.col.names)

        spdf@data <<- reshape(spdf@data, 
                             varying = list(get.names()[(num.col + 1):(num.col + 7)], 
                                            get.names()[(num.col + 8):(num.col + 14)], 
                                            get.names()[(num.col + 15):(num.col + 21)], 
                                            get.names()[(num.col + 22):(num.col + 28)], 
                                            get.names()[(num.col + 29):(num.col + 35)], 
                                            get.names()[(num.col + 36):(num.col + 42)]), 
                             direction = 'long', 
                             sep = '.', 
                             v.names = c('sdcol', 'line.num', 'mar.status', 'circum', 'who.circum', 'age.circum'), 
                             timevar = 'order')

        spdf@data <<- base::subset(spdf@data, !is.na(sdcol), select = c(-sdcol))
        spdf@data <<- transform(spdf@data, 
                               circum = as.numeric(circum), 
                               mar.status = as.numeric(mar.status), 
                               who.circum = as.numeric(who.circum))
        spdf@data <<- transform(spdf@data,
                               circum.yesno = ifelse(circum == 1, 1, 0))

        br <- read.dta(br.file, convert.underscore = TRUE)
        br <- base::subset(br, select = c(v001:v003, bidx, v437, v438, b2, sdno))
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
          spdf@data <<- base::subset(spdf@data, birth.year <= youngest.cohort)

        spdf@data <<- do.call(rbind, base::by(spdf@data, spdf@data$hh.id, cleanup.by.hh))
        spdf@data <<- spdf@data[ order(spdf@data$hh.id, spdf@data$birth.year), ]

        spdf@data <<- base::subset(spdf@data, circum <= 1)
      }
      else
      {
        if (!is.null(youngest.cohort) & !is.empty(spdf@data))
          spdf@data <<- base::subset(spdf@data, birth.year <= youngest.cohort)
      }

      return(.self)
    }
))

RegressionResults <- setRefClass("RegressionResults",
  fields = list(
    .lm = "ANY", 
    vcov = "matrix",
    regress.formula = "formula",
    data = "DaughterFgmData"),

  methods = list(
    summary = function()
    {
      coeftest(.lm, vcov = vcov)
    },
    
    lht = function(hypothesis)
    {
      linearHypothesis(.lm, hypothesis, vcov = if (!is.empty(vcov)) vcov)
    })
)

DaughterFgmData$methods(
  generate.reg.means = function(cohort.range = 1, regs = c(individual.controls, other.grpavg.controls), exclude.self = FALSE)
  {
    spdf@data <<- do.call(rbind, 
                          base::by(spdf@data, spdf@data[c("birth.year.fac", "governorate")], calc.grpavg, spdf@data, cohort.range, regs, 
                          lag = 0, exclude.self = exclude.self))
    spdf@data <<- do.call(rbind, 
                          base::by(spdf@data, spdf@data[c("birth.year.fac", "governorate")], calc.grpavg, spdf@data, cohort.range, regs, 
                          lag = 1, prefix = FgmData.lagged.grpavg.prefix, exclude.self = exclude.self))
  }
)

DaughterFgmData$methods(
  rm.by.grp.size = function(min.grp.size)
  {
    spdf@data <<- spdf@data[spdf@data$grp.size >= min.grp.size, ]
  }
)

DaughterFgmData$methods(
  rm.by.res.years = function(min.res.years = 95) # 95 is the code for having always lived there
  {
    if (min.res.years > 95) min.res.years <- 95
    spdf@data <<- spdf@data[(spdf@data$years.lived.res >= min.res.years) & (spdf@data$years.lived.res <= 95), ]
  }
)

DaughterFgmData$methods(
  get.regress.formula = function(dep.var, 
                                 include.grpavg = FALSE, 
                                 interact.govern.cohort = FALSE, 
                                 instr.grpavg = FALSE, 
                                 instr.regs = c(individual.controls, other.grpavg.controls)) 
  {
    individ.controls.formula <- if (length(individual.controls) > 0) paste(individual.controls, collapse = " + ")
    grpavg.controls.regex <- gsub("\\.", "\\\\.", sprintf("((%s))", paste(c(individual.controls, other.grpavg.controls), collapse = ")|(")))
    grpavg.regs <- grep(sprintf("^%s\\.%s", FgmData.grpavg.prefix, grpavg.controls.regex), get.names(), value = TRUE)
    grpavg.formula <- paste(grpavg.regs, collapse = " + ")
    lagged.grpavg.controls.regex <- gsub("\\.", "\\\\.", sprintf("((%s))", paste(instr.regs, collapse = ")|(")))
    lagged.grpavg.formula <- paste(grep(sprintf("^%s\\.%s", FgmData.lagged.grpavg.prefix, lagged.grpavg.controls.regex), get.names(), value = TRUE), 
                                   collapse = " + ")
    not.instr.grpavg.formula <- paste(grep(sprintf("^%s\\.%s", 
                                                   FgmData.grpavg.prefix, 
                                                   lagged.grpavg.controls.regex),
                                           grpavg.regs, 
                                           value = TRUE, 
                                           invert = TRUE), 
                                      collapse = " + ")

    timevar.fe <- sprintf("birth.year.fac%sgovernorate", if (interact.govern.cohort) "*" else " + ")
    instr.formula <- sprintf("| %s %s %s %s", 
                             timevar.fe,
                             if (length(individ.controls.formula) > 0) paste("+", individ.controls.formula) else "",
                             paste("+", not.instr.grpavg.formula), 
                             paste("+", lagged.grpavg.formula))

    sprintf("%s ~ %s %s %s %s",
            dep.var, 
            timevar.fe,
            if (length(individ.controls.formula) > 0) paste("+", individ.controls.formula) else "", 
            if (include.grpavg) paste("+", grpavg.formula) else "",
            if (instr.grpavg) instr.formula else "")
  }
)

DaughterFgmData$methods(
  regress = function(dep.var, 
                     include.grpavg = FALSE, 
                     interact.govern.cohort = FALSE, 
                     instr.grpavg = FALSE, 
                     instr.regs = c(individual.controls, other.grpavg.controls), 
                     gen.vcov = TRUE) 
  {
    regress.formula <- get.regress.formula(dep.var, include.grpavg, interact.govern.cohort, instr.grpavg, instr.regs) 

    r <- if (instr.grpavg) ivreg(formula(regress.formula),data = spdf@data) else lm(formula(regress.formula), data = spdf@data)
    v <- if (gen.vcov) tryCatch(vcovHAC(r), error = function(e) { matrix(NA, 0, 0) }) else NULL

    RegressionResults$new(.lm = r, vcov = v, data = .self, regress.formula = formula(regress.formula)) }
)

DaughterFgmData$methods(
  get.regress.formula.panel = function(dep.var, interact.govern.cohort = FALSE, instr.grpavg = FALSE) 
  {
    individ.controls.formula <- paste(individual.controls, collapse = " + ")
    grpavg.controls.regex <- gsub("\\.", "\\\\.", sprintf("((%s))", paste(c(individual.controls, other.grpavg.controls), collapse = ")|(")))
    grpavg.formula <- paste(grep(sprintf("^%s\\.%s", FgmData.grpavg.prefix, grpavg.controls.regex), get.names(), value = TRUE), collapse = " + ")
    lagged.grpavg.formula <- paste(grep(sprintf("^%s\\.(%s)", FgmData.lagged.grpavg.prefix, grpavg.controls.regex), get.names(), value = TRUE), collapse = " + ")

    timevar.fe <- sprintf("birth.year.fac%s", if (interact.govern.cohort) ":governorate" else "")
    instr.formula <- sprintf("| %s + %s", timevar.fe, lagged.grpavg.formula)

    sprintf("%s ~ %s + %s %s",
            dep.var, 
            timevar.fe,
            grpavg.formula,
            if (instr.grpavg) instr.formula else "")
  }
)

DaughterFgmData$methods(
  regress.panel = function(dep.var, 
                           interact.govern.cohort = FALSE, 
                           instr.grpavg = FALSE, 
                           instr.regs = c(individual.controls, other.grpavg.controls),
                           gen.vcov = TRUE) 
  {
    #regress.formula <- get.regress.formula.panel(dep.var, interact.govern.cohort, instr.grpavg) 
    regress.formula <- get.regress.formula(dep.var, TRUE, interact.govern.cohort, instr.grpavg, instr.regs) 

    r <- plm(formula(regress.formula), index = c("hh.id", "order.fac"), effects = "individual", model = "within", data = spdf@data)

    v <- if (gen.vcov) tryCatch(vcovHC(r), error = function(e) { matrix(NA, 0, 0) }) else NULL

    RegressionResults$new(.lm = r, vcov = v, data = .self, regress.formula = formula(regress.formula))
  }
)

DaughterFgmData$methods(
  regress.logit = function(dep.var)
  {
    regress.formula <- get.regress.formula(dep.var)
    
    r <- glm(formula(regress.formula), family = binomial(link = "logit"), data = spdf@data)
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


