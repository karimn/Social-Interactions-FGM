library(sp)
library(spatstat)
library(maptools)
library(sandwich)
library(lmtest)
library(plm)
library(AER)

calc.grpavg <- function(all.data, grp.ids, grp.index.col.values, cohort.range, regs, prefix = FgmData.grpavg.prefix, lag = 0, exclude.self = FALSE, range.type = c("both", "older"), other.network.reg = NULL)
{
  stopifnot(length(unique(all.data$spatial.data@data[grp.ids, "birth.year"])) == 1)
  stopifnot(length(unique(all.data$spatial.data@data[grp.ids, "governorate"])) == 1)

  current.birth.year <- all.data$spatial.data@data[grp.ids[1], "birth.year"] - lag 
  current.governorate <- all.data$spatial.data@data[grp.ids[1], "governorate"] 

  if (!is.null(other.network.reg))
  {
    current.other.network <- grp.index.values[[other.network.reg]] 
    peer.ids <- switch(match.arg(range.type),
                  both = all.data$get.subset.rows((all.data$spatial.data[["birth.year"]] <= current.birth.year + cohort.range) & 
                                  (all.data$spatial.data[["birth.year"]] >= current.birth.year - cohort.range) &
                                  (all.data$spatial.data[["governorate"]] == current.governorate) &
                                  (all.data$spatial.data[[other.network.reg]] == current.other.network)),
                  older = all.data$get.subset.rows((all.data$spatial.data[["birth.year"]] <= current.birth.year) & 
                                   (all.data$spatial.data[["birth.year"]] >= current.birth.year - cohort.range) &
                                   (all.data$spatial.data[["governorate"]] == current.governorate) &
                                   (all.data$spatial.data[[other.network.reg]] == current.other.network)))
  }
  else
  {
    peer.ids <- switch(match.arg(range.type),
                  both = all.data$get.subset.rows((all.data$spatial.data[["birth.year"]] <= current.birth.year + cohort.range) & 
                                  (all.data$spatial.data[["birth.year"]] >= current.birth.year - cohort.range) &
                                  (all.data$spatial.data[["governorate"]] == current.governorate)),
                  older = all.data$get.subset.rows((all.data$spatial.data[["birth.year"]] <= current.birth.year) & 
                                   (all.data$spatial.data[["birth.year"]] >= current.birth.year - cohort.range) &
                                   (all.data$spatial.data[["governorate"]] == current.governorate))) 
  }

  if ((length(regs) > 0) && (length(peer.ids) > 0)) {
    for (col.name in regs) {
        grp.mean(all.data, grp.ids, peer.ids, col.name, prefix, col.lvl = NULL, new.col.name = NULL, exclude.self = exclude.self)
    }
  }

  all.data$spatial.data@data[grp.ids, "grp.size"] <- length(peer.ids) 
  return()
}

calc.grpavg.spatial <- function(all.data, grp.ids, grp.index.col.values, radius, cohort.range, regs, prefix = FgmData.grpavg.prefix, lag = 0, exclude.self = FALSE, range.type = c("both", "older"), other.network.reg = NULL) {
  stopifnot(length(unique(all.data$spatial.data@data[grp.ids, "birth.year"])) == 1)
  stopifnot(length(unique(all.data$coords[grp.ids, 1])) == 1)
  stopifnot(length(unique(all.data$coords[grp.ids, 2])) == 1)

  current.birth.year <- all.data$spatial.data@data[grp.ids[1], "birth.year"] - lag 
  current.coords <- all.data$coords[grp.ids[1],]

  if (!is.null(other.network.reg))
  {
    current.other.network <- grp.index.values[[other.network.reg]] 
    peer.ids <- switch(match.arg(range.type),
                  both = all.data$get.subset.rows((all.data$spatial.data[["birth.year"]] <= current.birth.year + cohort.range) & 
                                  (all.data$spatial.data[["birth.year"]] >= current.birth.year - cohort.range) &
                                  (all.data$spatial.data[[other.network.reg]] == current.other.network), center = current.coords, radius = radius),
                  older = all.data$get.subset.rows((all.data$spatial.data[["birth.year"]] <= current.birth.year) & 
                                   (all.data$spatial.data[["birth.year"]] >= current.birth.year - cohort.range) &
                                   (all.data$spatial.data[[other.network.reg]] == current.other.network), center = current.coords, radius = radius))
  }
  else
  {
    peer.ids <- switch(match.arg(range.type),
                  both = all.data$get.subset.rows((all.data$spatial.data[["birth.year"]] <= current.birth.year + cohort.range) & 
                                  (all.data$spatial.data[["birth.year"]] >= current.birth.year - cohort.range), center = current.coords, radius = radius),
                  older = all.data$get.subset.rows((all.data$spatial.data[["birth.year"]] <= current.birth.year) & 
                                   (all.data$spatial.data[["birth.year"]] >= current.birth.year - cohort.range), center = current.coords, radius = radius)) 
  }

  if ((length(regs) > 0) && (length(peer.ids) > 0)) {
    for (col.name in regs) {
        grp.mean(all.data, grp.ids, peer.ids, col.name, prefix, col.lvl = NULL, new.col.name = NULL, exclude.self = exclude.self)
    }
  }

  all.data$spatial.data@data[grp.ids, "grp.size"] <- length(peer.ids) 
  return()
}

calc.delivery.avgs <- function(all.data, grp.ids, grp.index.col.values, all.cohorts.data, year.range, year.offset, regs, prefix = FgmData.grpavg.prefix, range.type = c("both", "older")) {
  stopifnot(length(unique(all.data$spatial.data@data[grp.ids, "birth.year"])) == 1)
  stopifnot(length(unique(all.data$spatial.data@data[grp.ids, "governorate"])) == 1)

  current.circum.year <- all.data$spatial.data@data[grp.ids[1], "birth.year"] + year.offset
  current.governorate <- all.data$spatial.data@data[grp.ids[1], "governorate"] 


  peer.row.ids <- switch(match.arg(range.type),
	  both = all.cohorts.data$get.subset.rows((all.cohorts.data$spatial.data[["birth.year"]] <= current.circum.year + year.range) & 
			  (all.cohorts.data$spatial.data[["birth.year"]] >= current.circum.year - year.range) &
			  (all.cohorts.data$spatial.data[["governorate"]] == current.governorate)) ,
	  older = all.cohorts.data$get.subset.rows[(all.cohorts.data$spatial.data[["birth.year"]] <= current.circum.year) & 
			   (all.cohorts.data$spatial.data[["birth.year"]] >= current.circum.year - year.range) &
			   (all.cohorts.data$spatial.data[["governorate"]] == current.governorate), ]) 

  grp.row.ids <- grp.ids

  peer.ids <- all.cohorts.data$spatial.data@data$daughter.id[peer.row.ids]
  grp.ids <- all.data$spatial.data@data$daughter.id[grp.ids]

  if ((length(regs) > 0) && (length(peer.ids) > 0)) {
    for (col.name in regs) {
        grp.mean(all.data, grp.ids, peer.ids, col.name, prefix, col.lvl = NULL, new.col.name = NULL, exclude.self = FALSE, peers.data = all.cohorts.data, ids.col = "daughter.id")
    }
  }

  all.data$spatial.data@data[grp.row.ids, "delivery.grp.size"] <- length(peer.ids) 
  return()
}

calc.delivery.avgs.spatial <- function(all.data, grp.ids, grp.index.col.values, radius, all.cohorts.data, year.range, year.offset, regs, prefix = FgmData.grpavg.prefix, range.type = c("both", "older")) {
  stopifnot(length(unique(all.data$spatial.data@data[grp.ids, "birth.year"])) == 1)
  stopifnot(length(unique(all.data$coords[grp.ids, 1])) == 1)
  stopifnot(length(unique(all.data$coords[grp.ids, 2])) == 1)
  stopifnot(length(grp.ids) > 0)

  current.circum.year <- all.data$spatial.data@data[grp.ids[1], "birth.year"] + year.offset 
  current.coords <- all.data$coords[grp.ids[1],]

  peer.row.ids <- switch(match.arg(range.type),
	  both = all.cohorts.data$get.subset.rows((all.cohorts.data$spatial.data[["birth.year"]] <= current.circum.year + year.range) & 
			  (all.cohorts.data$spatial.data[["birth.year"]] >= current.circum.year - year.range), center = current.coords, radius = radius) ,
	  older = all.cohorts.data$get.subset.rows((all.cohorts.data$spatial.data[["birth.year"]] <= current.circum.year) & 
			   (all.cohorts.data$spatial.data[["birth.year"]] >= current.circum.year - year.range), center = current.coords, radius = radius)) 

  stopifnot(spDistsN(all.cohorts.data$spatial.data[peer.row.ds,], current.coords) <= radius)

  grp.row.ids <- grp.ids

  peer.ids <- all.cohorts.data$spatial.data@data$daughter.id[peer.row.ids]
  grp.ids <- all.data$spatial.data@data$daughter.id[grp.ids]

  if ((length(regs) > 0) && (length(peer.ids) > 0)) {
    for (col.name in regs) {
        grp.mean(all.data, grp.ids, peer.ids, col.name, prefix, col.lvl = NULL, new.col.name = NULL, exclude.self = FALSE, peers.data = all.cohorts.data, ids.col = "daughter.id")
    }
  }

  all.data$spatial.data@data[grp.row.ids, "delivery.grp.size"] <- length(peer.ids) 
  return()
}

cleanup.by.hh <- function(df.obj) {
  # Removing birth.year duplicates
  dup <- df.obj$duplicated("birth.year")
  df.obj$spatial.data <- df.obj$spatial.data[!dup,]

  n <- df.obj$nrow
  df.obj$sort("age", decreasing = TRUE)

  df.obj$spatial.data$n.ord <- n
  df.obj$spatial.data$order <- 1:n

  return(df.obj)
}


DaughterFgmData.individual.controls <- c("wealth.index.2", "urban.rural", "educ.lvl", "marital.age", "mother.circum.fac", "religion", "hh.head.sex", "med.help.distance.fac", "discuss.circum.fac", "received.info.circum")
                                         
#DaughterFgmData.individual.controls <- c("wealth.index.2", "urban.rural", "educ.lvl", "med.help.permission.fac", "med.help.distance.fac", 
#                                         "med.help.transportation.fac", "marital.age", "mother.circum.fac", "occupation.2.fac", "religion", 
#                                         "partner.educlvl.fac", "hh.head.sex")

DaughterFgmData <- setRefClass("DaughterFgmData", 
  contains = "BaseFgmData",
  fields = list(
    all.cohorts.spdf = "SpatialData", # "SpatialPointsDataFrame",
    individual.controls = "character",
    other.grpavg.controls = "character",
    birth.data = "data.frame"))

DaughterFgmData$methods(initialize = function(ir.file = NULL, br.file = NULL, gps.file = NULL, 
                          new.column.names = FgmData.col.names,
                          youngest.cohort = 1996, 
                          dhs.year = 2008, 
                          individual.controls = DaughterFgmData.individual.controls, 
                          other.grpavg.controls = character(0),
                          birth.data = "data.frame",
                          skip.cleanup = FALSE, # This is just to speed up initialization in the interest of debugging
                          ...) {
      if ("data" %in% names(list(...))) {
          callSuper(...)
      } else {
          columns <- quote(c(eval(FgmData.cols), sdcol.1:sdcol.7, s906.1:s906.7, s908.1:s908.7, s909.1:s909.7, s910.1:s910.7, s911.1:s911.7))

          initFields(individual.controls = individual.controls, other.grpavg.controls = other.grpavg.controls)
          callSuper(ir.file = ir.file, gps.file = gps.file, columns = columns, new.column.names = new.column.names, dhs.year = dhs.year, ...)

          if (!is.null(ir.file) & !is.null(gps.file))
          {
            reshape.col <- c("sdcol", "s906", "s908", "s909", "s910", "s911")

            reshape(varying = lapply(reshape.col, function(col) paste(col, 1:7, sep = ".")), 
                    direction = 'long', 
                    v.names = c('sdcol', 'line.num', 'mar.status', 'circum', 'who.circum', 'age.circum'),
                    timevar = 'order',
                    idvar = "mother.id") # A new column is created that is unique over mothers

            # This needs to be done first; the below call to subset() seems to convert any "character" type
            # columns into factors.  The "circum" column is text because of some "yes" and "no" values.
            spatial.data@data <<- within(spatial.data@data, {
                circum <- as.numeric(circum) 
                mar.status <- as.numeric(mar.status) 
                who.circum <- as.numeric(who.circum)
                circum.yesno <- ifelse(circum == 1, 1, 0)
            })

            subset(!is.na(sdcol), select = c(-sdcol))

            br <- read.dta(br.file, convert.underscore = TRUE)
            br <- base::subset(br, select = c(v001:v003, bidx, v437, v438, b2, sdno, m3g, m15))
            br$delivery.location <- factor(br$m15 %/% 10, labels = c("home", "public sector", "private sector", "other", "missing"))
            br$delivered.by.daya <- factor(br$m3g, labels = c("no", "yes"), levels = 0:1)

            merge(br, by.x = c('cluster', 'hh', 'respond', 'line.num'), by.y = c('v001', 'v002', 'v003', 'bidx'), all.x = TRUE)

            br <- base::merge(br, spatial.data@data[, c("cluster", "hh", "respond", "line.num", "governorate")], by.x = c('v001', 'v002', 'v003', 'bidx'), by.y = c('cluster', 'hh', 'respond', 'line.num'), all.x = TRUE)

            birth.data <<- br
                
            rm(br)

            change.column.names(c('v437', 'v438', 'b2') , c('weight', 'height', 'birth.year'))

            spatial.data@data <<- within(spatial.data@data, 
            {
              age <- dhs.year - birth.year
              has.or.intends.circum <- ifelse(circum == 1 | ((age <= 12) & (intends.circum == 1)), 1, 0)
              med.circum <- ifelse(circum == 1 & (who.circum %in% c(1, 2)), 1, 0)
              year.circum <- ifelse(circum == 1 & age.circum <= 19, birth.year + age.circum, NA)
              year.circum.fac <- factor(year.circum)
              #order.fac <- factor(order)
              married <- ifelse(mar.status == 1, 1, 0)
              birth.year.fac <- factor(birth.year)
              daughter.id <- seq_along(age) # Create a unique id per daughter
            })

            all.cohorts.spdf <<- SpatialData$new(copy = .self) #spatial.data 

            if (!is.null(youngest.cohort))
              subset(birth.year <= youngest.cohort)

            if (!skip.cleanup) {
                by.list <- by(c("hh.id"), cleanup.by.hh)
                spatial.data <<- do.call(base::rbind, lapply(by.list[!is.na(by.list)], function(obj) obj$spatial.data))
                spatial.data@data$order.fac <<- factor(spatial.data@data$order)
            }

            sort(c("hh.id", "birth.year"))

            subset(circum <= 1)
          }
          else
          {
            all.cohorts.spdf <<- SpatialData$new(copy = .self) #spatial.data  

            if (!is.null(youngest.cohort) & (nrow != 0))
              subset(birth.year <= youngest.cohort)
          }
      }
    })

DaughterFgmData$methods(create.new.from.data = function(df, ...) {
    callSuper(df = df, individual.controls = individual.controls, other.grpavg.controls = other.grpavg.controls, birth.data = birth.data, ...)
})

DaughterFgmData$methods(
  generate.reg.means = function(cohort.range = 1, 
                                regs = c(individual.controls, other.grpavg.controls), 
                                other.network.reg = NULL, 
                                exclude.self = FALSE,
                                range.type = c("both", "older"))
  {
    quick.update(c("birth.year.fac", "governorate", other.network.reg), calc.grpavg, cohort.range, regs, lag = 0, exclude.self = exclude.self)
    quick.update(c("birth.year.fac", "governorate", other.network.reg), calc.grpavg, cohort.range, regs, lag = 1, prefix = FgmData.lagged.grpavg.prefix, exclude.self = exclude.self)
  }
)

DaughterFgmData$methods(generate.reg.means.spatial = function(radius, 
                                                              cohort.range = 1, 
                                                              regs = c(individual.controls, other.grpavg.controls), 
                                                              other.network.reg = NULL, 
                                                              exclude.self = FALSE,
                                                              range.type = c("both", "older")) {
    if (missing(radius)) {
        stop("Missing radius value")
    }

    quick.update(c("birth.year.fac", "cluster.fac", other.network.reg), calc.grpavg.spatial, radius, cohort.range, regs, lag = 0, exclude.self = exclude.self)
})

DaughterFgmData$methods(
  generate.delivery.means = function(year.range = 1, year.offset = 12, regs = c("delivery.location", "delivered.by.daya"), range.type = c("both", "older"))
  {

      quick.update(c("birth.year.fac", "governorate"), calc.delivery.avgs, all.cohorts.spdf, year.range, year.offset, regs)
  }
)

DaughterFgmData$methods(
  generate.delivery.means.spatial = function(radius, year.range = 1, year.offset = 12, regs = c("delivery.location", "delivered.by.daya"), range.type = c("both", "older"))
  {
      if (missing(radius)) {
          stop("Missing radius value")
      }

      quick.update(c("birth.year.fac", "cluster.fac"), calc.delivery.avgs.spatial, radius, all.cohorts.spdf, year.range, year.offset, regs)
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
    dup <- if (is.null(rm.by)) duplicated() else duplicated(rm.by)
    remove.rows(which(dup))
    #spdf@data <<- spdf@data[!dup,]
    #spdf@data$hh.id <<- spdf@data$hh.id[,drop = TRUE]
  }
)

#DaughterFgmData$methods(
#  copy = function()
#  {
#    DaughterFgmData$new(spdf = spdf, cluster.info = cluster.info, individual.controls = individual.controls, other.grpavg.controls = other.grpavg.controls)
#  }
#)

#DaughterFgmData$methods(
#  get.regress.formula = function(dep.var, 
#                                 include.grpavg = FALSE, 
#                                 interact.govern.cohort = FALSE, 
#                                 instr.grpavg = FALSE, 
#                                 instr.regs = c(individual.controls, other.grpavg.controls)) 
#  {
#    individ.controls.formula <- if (length(individual.controls) > 0) paste(individual.controls, collapse = " + ")
#    grpavg.controls.regex <- gsub("\\.", "\\\\.", sprintf("((%s))", paste(c(individual.controls, other.grpavg.controls), collapse = ")|(")))
#    grpavg.regs <- grep(sprintf("^%s\\.%s", FgmData.grpavg.prefix, grpavg.controls.regex), names, value = TRUE)
#    grpavg.formula <- paste(grpavg.regs, collapse = " + ")
#    lagged.grpavg.controls.regex <- gsub("\\.", "\\\\.", sprintf("((%s))", paste(instr.regs, collapse = ")|(")))
#    lagged.grpavg.formula <- paste(grep(sprintf("^%s\\.%s", FgmData.lagged.grpavg.prefix, lagged.grpavg.controls.regex), names, value = TRUE), 
#                                   collapse = " + ")
#    not.instr.grpavg.formula <- paste(grep(sprintf("^%s\\.%s", 
#                                                   FgmData.grpavg.prefix, 
#                                                   lagged.grpavg.controls.regex),
#                                           grpavg.regs, 
#                                           value = TRUE, 
#                                           invert = TRUE), 
#                                      collapse = " + ")
#
#    timevar.fe <- sprintf("birth.year.fac%sgovernorate", if (interact.govern.cohort) "*" else " + ")
#    instr.formula <- sprintf("| %s %s %s %s", 
#                             timevar.fe,
#                             if (length(individ.controls.formula) > 0) paste("+", individ.controls.formula) else "",
#                             paste("+", not.instr.grpavg.formula), 
#                             paste("+", lagged.grpavg.formula))
#
#    sprintf("%s ~ %s %s %s %s",
#            dep.var, 
#            timevar.fe,
#            if (length(individ.controls.formula) > 0) paste("+", individ.controls.formula) else "", 
#            if (include.grpavg) paste("+", grpavg.formula) else "",
#            if (instr.grpavg) instr.formula else "")
#  }
#)
#
#DaughterFgmData$methods(
#  regress = function(dep.var, 
#                     include.grpavg = FALSE, 
#                     interact.govern.cohort = FALSE, 
#                     instr.grpavg = FALSE, 
#                     instr.regs = c(individual.controls, other.grpavg.controls), 
#                     gen.vcov = TRUE) 
#  {
#    regress.formula <- get.regress.formula(dep.var, include.grpavg, interact.govern.cohort, instr.grpavg, instr.regs) 
#
#    r <- if (instr.grpavg) ivreg(formula(regress.formula),data = spdf@data) else lm(formula(regress.formula), data = spdf@data)
#    v <- if (gen.vcov) tryCatch(vcovHAC(r), error = function(e) { matrix(NA, 0, 0) }) else NULL
#
#    RegressionResults$new(.lm = r, vcov = v, data = .self, regress.formula = formula(regress.formula)) }
#)
#
#DaughterFgmData$methods(
#  get.regress.formula.panel = function(dep.var, interact.govern.cohort = FALSE, instr.grpavg = FALSE) 
#  {
#    individ.controls.formula <- paste(individual.controls, collapse = " + ")
#    grpavg.controls.regex <- gsub("\\.", "\\\\.", sprintf("((%s))", paste(c(individual.controls, other.grpavg.controls), collapse = ")|(")))
#    grpavg.formula <- paste(grep(sprintf("^%s\\.%s", FgmData.grpavg.prefix, grpavg.controls.regex), get.names(), value = TRUE), collapse = " + ")
#    lagged.grpavg.formula <- paste(grep(sprintf("^%s\\.(%s)", FgmData.lagged.grpavg.prefix, grpavg.controls.regex), get.names(), value = TRUE), collapse = " + ")
#
#    timevar.fe <- sprintf("birth.year.fac%s", if (interact.govern.cohort) ":governorate" else "")
#    instr.formula <- sprintf("| %s + %s", timevar.fe, lagged.grpavg.formula)
#
#    sprintf("%s ~ %s + %s %s",
#            dep.var, 
#            timevar.fe,
#            grpavg.formula,
#            if (instr.grpavg) instr.formula else "")
#  }
#)
#
#DaughterFgmData$methods(
#  regress.panel = function(dep.var, 
#                           interact.govern.cohort = FALSE, 
#                           instr.grpavg = FALSE, 
#                           instr.regs = c(individual.controls, other.grpavg.controls),
#                           gen.vcov = TRUE) 
#  {
#    #regress.formula <- get.regress.formula.panel(dep.var, interact.govern.cohort, instr.grpavg) 
#    regress.formula <- get.regress.formula(dep.var, TRUE, interact.govern.cohort, instr.grpavg, instr.regs) 
#
#    return (.self$plm(formula(regress.formula), index = c("hh.id", "order.fac"), effects = "individual", model = "within", gen.vcov = gen.vcov))
#  }
#)
#
#DaughterFgmData$methods(
#  regress.logit = function(dep.var)
#  {
#    regress.formula <- get.regress.formula(dep.var)
#    
#    r <- glm(formula(regress.formula), family = binomial(link = "logit"), data = spdf@data)
#  }
#)
#
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


