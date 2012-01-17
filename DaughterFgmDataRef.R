library(sp)
library(spatstat)
library(maptools)
library(sandwich)
library(lmtest)
library(plm)
library(AER)

#calc.grpavg <- function(df, total.df, cohort.range, regs, prefix = FgmData.grpavg.prefix, lag = 0, exclude.self = FALSE,
calc.grpavg <- function(all.data, ind.mask, grp.index.values, cohort.range, regs, prefix = FgmData.grpavg.prefix, lag = 0, exclude.self = FALSE, range.type = c("both", "older"), other.network.reg = NULL)
{
  #print(sprintf("calc.grpavg: size = %i", nrow(df)))
  current.birth.year <- grp.index.values$birth.year - lag #df$birth.year[1] - lag
  current.governorate <- grp.index.values$governorate #df$governorate[1]

  if (!is.null(other.network.reg))
  {
    current.other.network <- grp.index.values[[other.network.reg]] # df[1, other.network.reg]
    grp <- switch(match.arg(range.type),
                  both = all.data$get.subset((all.data$spatial.data[["birth.year"]] <= current.birth.year + cohort.range) & 
                                  (all.data$spatial.data[["birth.year"]] >= current.birth.year - cohort.range) &
                                  (all.data$spatial.data[["governorate"]] == current.governorate) &
                                  (all.data$spatial.data[[other.network.reg]] == current.other.network)),
                  older = all.data$get.subset((all.data$spatial.data[["birth.year"]] <= current.birth.year) & 
                                   (all.data$spatial.data[["birth.year"]] >= current.birth.year - cohort.range) &
                                   (all.data$spatial.data[["governorate"]] == current.governorate) &
                                   (all.data$spatial.data[[other.network.reg]] == current.other.network)))
  }
  else
  {
    grp <- switch(match.arg(range.type),
                  both = all.data$get.subset((all.data$spatial.data[["birth.year"]] <= current.birth.year + cohort.range) & 
                                  (all.data$spatial.data[["birth.year"]] >= current.birth.year - cohort.range) &
                                  (all.data$spatial.data[["governorate"]] == current.governorate)),
                  older = all.data$get.subset((all.data$spatial.data[["birth.year"]] <= current.birth.year) & 
                                   (all.data$spatial.data[["birth.year"]] >= current.birth.year - cohort.range) &
                                   (all.data$spatial.data[["governorate"]] == current.governorate))) 
  }

  if (length(regs) > 0)
    for (col.name in regs)
    {
      if (is.factor(all.data[, col.name]))
      {
        factor.mean(all.data, ind.mask, grp, col.name, prefix, exclude.self)
      }
      else
      {
        new.col.name <- paste(prefix, col.name, sep = '.')
        #df[, new.col.name] <- if (nrow(grp) != 0) mean(grp[, col.name], na.rm = TRUE) else NA
        all.data$spatial.data[ind.mask, new.col.name] <- vapply(1L:length(which(ind.mask)), grp.mean, 0L, grp, col.name, NULL, exclude.self)
      }
    }

  all.data$spatial.data[ind.mask, "grp.size"] <- nrow(grp) 

  return()
}

#calc.delivery.avgs <- function(df, total.df, year.range, year.offset, regs, prefix = FgmData.grpavg.prefix, range.type = c("both", "older")) {
calc.delivery.avgs <- function(all.data, grp.mask, grp.index.values, year.range, year.offset, regs, prefix = FgmData.grpavg.prefix, range.type = c("both", "older")) {
  current.circum.year <- grp.index.values$birth.year + year.offset #df$birth.year[1] + year.offset
  current.governorate <- grp.index.values$governoate #df$governorate[1]


  grp <- switch(match.arg(range.type),
	  both = all.data$get.subset((all.data$spatial.data[["birth.year"]] <= current.circum.year + year.range) & 
			  (all.data$spatial.data[["birth.year"]] >= current.circum.year - year.range) &
			  (all.data$spatial.data[["governorate"]] == current.governorate)) ,
	  older = all.data$get.subset[(all.data$spatial.data[["birth.year"]] <= current.circum.year) & 
			   (all.data$spatial.data[["birth.year"]] >= current.circum.year - year.range) &
			   (all.data$spatial.data[["governorate"]] == current.governorate), ]) 

  #browser(condition = current.circum.year, expr = nrow(grp) > 0)

  if (length(regs) > 0)
    for (col.name in regs)
    {
      if (is.factor(all.data$spatial.data[, col.name]))
      {
        factor.mean(all.data, grp.mask, grp, col.name, prefix)
      }
      else
      {
        new.col.name <- paste(prefix, col.name, sep = '.')
        #df[, new.col.name] <- if (nrow(grp) != 0) mean(grp[, col.name], na.rm = TRUE) else NA
        all.data$spatial.data[grp.mask, new.col.name] <- vapply(1L:length(which(grp.mask)), grp.mean, 0L, grp, col.name, NULL)
      }
    }

   return()
}

cleanup.by.hh <- function(df.obj) {
  # Removing birth.year duplicates
  dup <- duplicated(df.obj$spatial.data$birth.year)
  df.obj$spatial.data <- df.obj$spatial.data[!dup,]

  n <- df.obj$nrow
  df.obj$sort("age", decreasing = TRUE)

  df.obj$spatial.data$n.ord <- n
  df.obj$spatial.data$$order <- 1:n

  return(df)
}


DaughterFgmData.individual.controls <- c("wealth.index.2", "urban.rural", "educ.lvl", "marital.age", "mother.circum.fac", "religion", "hh.head.sex", "med.help.distance.fac", "discuss.circum.fac", "received.info.circum")
                                         
#DaughterFgmData.individual.controls <- c("wealth.index.2", "urban.rural", "educ.lvl", "med.help.permission.fac", "med.help.distance.fac", 
#                                         "med.help.transportation.fac", "marital.age", "mother.circum.fac", "occupation.2.fac", "religion", 
#                                         "partner.educlvl.fac", "hh.head.sex")

DaughterFgmData <- setRefClass("DaughterFgmData", 
  contains = "BaseFgmData",

  fields = list(
    all.cohorts.spdf = "SpatialPointsDataFrame",
    individual.controls = "character",
    other.grpavg.controls = "character",
    birth.data = "data.frame"),

  methods = list(
    initialize = function(ir.file = character(0), br.file, gps.file = character(0), 
                          new.column.names = FgmData.col.names
                          youngest.cohort = 1996, 
                          dhs.year = 2008, 
                          individual.controls = DaughterFgmData.individual.controls, 
                          other.grpavg.controls = character(0),
                          birth.data = "data.frame",
                          ...)
    {
      columns <- quote(c(eval(FgmData.cols), sdcol.1:sdcol.7, s906.1:s906.7, s908.1:s908.7, s909.1:s909.7, s910.1:s910.7, s911.1:s911.7))

      callSuper(ir.file = ir.file, gps.file = gps.file, columns = columns, new.column.names = new.column.names, dhs.year = dhs.year, ...)
      initFields(individual.controls = individual.controls, other.grpavg.controls = other.grpavg.controls)

      if (!is.empty(ir.file) & !is.empty(gps.file))
      {
        num.col <- length(new.column.names) + 1

        reshape(varying = list(names[(num.col + 1):(num.col + 7)], 
                               names[(num.col + 8):(num.col + 14)], 
                               names[(num.col + 15):(num.col + 21)], 
                               names[(num.col + 22):(num.col + 28)], 
                               names[(num.col + 29):(num.col + 35)], 
                               names[(num.col + 36):(num.col + 42)]), 
                     direction = 'long', 
                     sep = '.', 
                     v.names = c('sdcol', 'line.num', 'mar.status', 'circum', 'who.circum', 'age.circum'), 
                     timevar = 'order')

        subset(!is.na(sdcol), select = c(-sdcol))

        spdf@data <<- transform(spdf@data, 
                               circum = as.numeric(circum), 
                               mar.status = as.numeric(mar.status), 
                               who.circum = as.numeric(who.circum))
        spdf@data <<- transform(spdf@data,
                               circum.yesno = ifelse(circum == 1, 1, 0))

        br <- read.dta(br.file, convert.underscore = TRUE)
        br <- base::subset(br, select = c(v001:v003, bidx, v437, v438, b2, sdno, m3g, m15))
        br$delivery.location <- factor(br$m15 %/% 10, labels = c("home", "public sector", "private sector", "other", "missing"))
        br$delivered.by.daya <- factor(br$m3g, labels = c("no", "yes"), levels = 0:1)

        spdf@data <<- merge(spdf@data, br, by.x = c('cluster', 'hh', 'respond', 'line.num'), by.y = c('v001', 'v002', 'v003', 'bidx'), all.x = TRUE)

        br <- merge(br, spdf@data[, c("cluster", "hh", "respond", "line.num", "governorate")], by.x = c('v001', 'v002', 'v003', 'bidx'), by.y = c('cluster', 'hh', 'respond', 'line.num'), all.x = TRUE)

        birth.data <<- br
		    
        rm(br)

        change.column.names(c('v437', 'v438', 'b2') , c('weight', 'height', 'birth.year'))

        spdf@data <<- within(spdf@data, 
        {
          age <- dhs.year - birth.year
          has.or.intends.circum <- ifelse(circum == 1 | ((age <= 12) & (intends.circum == 1)), 1, 0)
          med.circum <- ifelse(circum == 1 & (who.circum %in% c(1, 2)), 1, 0)
          year.circum <- ifelse(circum == 1 & age.circum <= 19, birth.year + age.circum, NA)
          year.circum.fac <- factor(year.circum)
          #order.fac <- factor(order)
          married <- ifelse(mar.status == 1, 1, 0)
          birth.year.fac <- factor(birth.year)
        })

        all.cohorts.spdf <<- spdf

        if (!is.null(youngest.cohort))
          subset(birth.year <= youngest.cohort)

        #spdf@data <<- do.call(rbind, base::by(spdf@data, spdf@data$hh.id, cleanup.by.hh))
        tapply.change(c("hh.id"), cleanup.by.hh)

        spdf@data$order.fac <<- factor(spdf@data$order)
        sort(c("hh.id", "birth.year"))

        subset(circum <= 1)
      }
      else
      {
        all.cohorts.spdf <<- spdf

        if (!is.null(youngest.cohort) & !is.empty(spdf@data))
          subset(birth.year <= youngest.cohort)
      }

      #spdf@data$hh.id <<- spdf@data$hh.id[,drop = TRUE]
      return(.self)
    }
))


DaughterFgmData$methods(
  generate.reg.means = function(cohort.range = 1, 
                                regs = c(individual.controls, other.grpavg.controls), 
                                other.network.reg = NULL, 
                                exclude.self = FALSE,
                                range.type = c("both", "older"))
  {
    #spdf@data <<- do.call(rbind, 
    #                      base::by(spdf@data, spdf@data[c("birth.year.fac", "governorate", other.network.reg)], calc.grpavg, spdf@data, cohort.range, regs, 
                          #lag = 0, exclude.self = exclude.self))
    quick.upate(c("birth.year.fac", "governorate", other.network.reg), calc.grpavg. cohort.range, regs, lag = 0, exclude.self = exclude.self)

    #spdf@data <<- do.call(rbind, 
    #                      base::by(spdf@data, spdf@data[c("birth.year.fac", "governorate", other.network.reg)], calc.grpavg, spdf@data, cohort.range, regs, 
    #                      lag = 1, prefix = FgmData.lagged.grpavg.prefix, exclude.self = exclude.self))
    quick.update(c("birth.year.fac", "governorate", other.network.reg), calc.grpavg, cohort.range, regs, lag = 1, prefix = FgmData.lagged.grpavg.prefix, exclude.self = exclude.self)
  }
)

DaughterFgmData$methods(
  generate.delivery.means = function(year.range = 1, year.offset = 12, regs = c("delivery.location", "delivered.by.daya"), range.type = c("both", "older"))
  {
    quick.update(c("birth.year.fac", "governorate"), calc.delivery.avgs, year.range, year.offset, regs)
    #spdf@data <<- do.call(rbind, 
    #                      base::by(spdf@data, spdf@data[c("birth.year.fac", "governorate")], calc.delivery.avgs, all.cohorts.spdf@data, year.range, year.offset, regs))
  }
)

DaughterFgmData$methods(
  rm.by.grp.size = function(min.grp.size)
  {
    #spdf@data <<- spdf@data[spdf@data$grp.size >= min.grp.size, ]
    subset(grp.size >= min.grp.size)
     
    # TODO figure out what this is for (!)
    #spdf@data$hh.id <<- spdf@data$hh.id[,drop = TRUE]
  }
)

DaughterFgmData$methods(
  rm.by.res.years = function(min.res.years = 95) # 95 is the code for having always lived there
  {
    if (min.res.years > 95) min.res.years <- 95

    subset((years.lived.res >= min.res.years) & (years.lived.res <= 95))

    # TODO Again, what the heck is this? drop is TRUE by default anyway
    #spdf@data$hh.id <<- spdf@data$hh.id[,drop = TRUE]
  }
)

DaughterFgmData$methods(
  rm.duplicate = function(rm.by = NULL)
  {
    dup <- if (is.null(rm.by)) duplicated(spdf@data) else duplicated(spdf@data[rm.by])
    spdf@data <<- spdf@data[!dup,]
    spdf@data$hh.id <<- spdf@data$hh.id[,drop = TRUE]
  }
)

#DaughterFgmData$methods(
#  copy = function()
#  {
#    DaughterFgmData$new(spdf = spdf, cluster.info = cluster.info, individual.controls = individual.controls, other.grpavg.controls = other.grpavg.controls)
#  }
#)

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

    return (.self$plm(formula(regress.formula), index = c("hh.id", "order.fac"), effects = "individual", model = "within", gen.vcov = gen.vcov))
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


