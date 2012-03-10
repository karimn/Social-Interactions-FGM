library(sp)
library(spatstat)
library(maptools)
library(sandwich)
library(lmtest)
library(plm)
library(AER)

clean.grpavg.matrix.colnames <- function(curr.names, regs, prefix = "spat.grpavg", postfix = NULL) {
    lvl.removed <- sub(sprintf("^(%s)(.+)", paste(regs, collapse = "|")), "\\1_", curr.names)
    lvls <- clean.lvl.name(sub(sprintf("^(%s)(.*)", paste(regs, collapse = "|")), "\\2", curr.names))
    new.col.names <- paste(prefix, paste(lvl.removed, lvls, sep = ""), sep = ".")

    if (!is.null(postfix)) {
        new.col.names <- paste(new.col.names, postfix, sep = ".")
    }

    return(new.col.names)
}

calc.grpavg <- function(all.data, grp.ids, grp.index.col.values, cohort.range, regs, prefix = FgmData.grpavg.prefix, lag = 0, exclude.self = FALSE, range.type = c("both", "older"), other.network.reg = NULL, radius)
{
  stopifnot(length(unique(all.data$spatial.data@data[grp.ids, "birth.year"])) == 1)
  stopifnot(length(unique(all.data$spatial.data@data[grp.ids, "governorate"])) == 1)

  current.birth.year <- all.data$spatial.data@data[grp.ids[1], "birth.year"] - lag 
  current.governorate <- all.data$spatial.data@data[grp.ids[1], "governorate"] 

  cohort.upper.bound <- current.birth.year + switch(match.arg(range.type), both = cohort.range, older = 0)
  cohort.lower.bound <- current.birth.year - cohort.range

  if (!is.null(other.network.reg)) {
    current.other.network <- grp.index.values[[other.network.reg]] 
    peer.ids <- all.data$get.subset.rows((birth.year <= cohort.upper.bound) & (birth.year >= cohort.lower.bound) &
                                         (governorate == current.governorate) &
                                         (all.data$spatial.data[[other.network.reg]] == current.other.network))  
  } else {
    peer.ids <- all.data$get.subset.rows((birth.year <= cohort.upper.bound) & (birth.year >= cohort.lower.bound) &
                                         (governorate == current.governorate))   
  }

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
        all.data$grp.mean(grp.ids, peer.ids, col.name, prefix, col.lvl = NULL, new.col.name = NULL, exclude.self = exclude.self)
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

  cohort.upper.bound <- current.circum.year + switch(match.arg(range.type), both = year.range, older = 0)
  cohort.lower.bound <- current.circum.year - year.range

  peer.row.ids <- all.cohorts.data$get.subset.rows((birth.year <= cohort.upper.bound) & 
			                                       (birth.year >= cohort.lower.bound) &
			                                       (governorate == current.governorate))

  grp.row.ids <- grp.ids

  peer.ids <- all.cohorts.data$spatial.data@data$daughter.id[peer.row.ids]
  grp.ids <- all.data$spatial.data@data$daughter.id[grp.ids]

  if ((length(regs) > 0) && (length(peer.ids) > 0)) {
    for (col.name in regs) {
        all.data$grp.mean(grp.ids, peer.ids, col.name, prefix, col.lvl = NULL, new.col.name = NULL, exclude.self = FALSE, peers.data = all.cohorts.data, ids.col = "daughter.id")
    }
  }

  all.data$spatial.data@data[grp.row.ids, "delivery.grp.size"] <- length(peer.ids) 
  return()
}

cleanup.by.hh <- function(df.obj) {
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
    all.cohorts.spdf = "BaseFgmData", # "SpatialPointsDataFrame",
    all.cohorts.dist.mat = "AdjMatrix", #"matrix",
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
              hh.id <- factor(hh.id)
            })

            all.cohorts.spdf <<- BaseFgmData$new(copy = .self)

            if (!is.null(youngest.cohort))
              subset(birth.year <= youngest.cohort)

            if (!skip.cleanup) {
                by.list <- by(c("hh.id"), cleanup.by.hh)
                #by.list <- apply("hh.id", cleanup.by.hh)
                spatial.data <<- do.call(base::rbind, lapply(by.list[!is.na(by.list)], function(obj) obj$spatial.data))
                #apply.ret.list <- par.apply("hh.id", cleanup.by.hh)
                #spatial.data <<- do.call(base::rbind, lapply(apply.ret.list, function(obj) obj$spatial.data))
                spatial.data@data$order.fac <<- factor(spatial.data@data$order)
            }

            sort(c("hh.id", "birth.year"))

            subset(circum <= 1)
          }
          else
          {
            all.cohorts.spdf <<- BaseFgmData$new(copy = .self) #spatial.data  

            if (!is.null(youngest.cohort) & (nrow != 0))
              subset(birth.year <= youngest.cohort)
          }
      }
    })

#DaughterFgmData$lock("individual.controls", "other.grpavg.controls", "birth.data") # "all.cohorts.spdf", 

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
    apply(c("birth.year.fac", "governorate", other.network.reg), calc.grpavg, cohort.range, regs, lag = 0, exclude.self = exclude.self)
    apply(c("birth.year.fac", "governorate", other.network.reg), calc.grpavg, cohort.range, regs, lag = 1, prefix = FgmData.lagged.grpavg.prefix, exclude.self = exclude.self)
  }
)

DaughterFgmData$methods(calc.all.cohorts.dist.matrix = function() {
    all.cohorts.dist.mat <<- all.cohorts.spdf$get.dist.matrix(subset = daughter.id %in% spatial.data$daughter.id)
})


DaughterFgmData$methods(generate.reg.means.spatial = function(radius, 
                                                              cohort.range = 1, 
                                                              regs = c(individual.controls, other.grpavg.controls), 
                                                              #other.network.reg = NULL
                                                              #exclude.self = FALSE,
                                                              range.type = c("both", "older"),
                                                              prefix = "spat.grpavg",
                                                              postfix = NULL,
                                                              year.offset = 0,
                                                              use.all.cohorts.data = FALSE,
                                                              degree = 1) {
                                                              #inner.radius = 0,
                                                              #dist.wt = FALSE) {
    if (missing(radius)) {
        stop("Missing radius value")
    }

    if (!use.all.cohorts.data) {
        adj.matrix <- get.spatial.adj.matrix(radius)
        cohort.adj.matrix <- get.cohort.adj.matrix(cohort.range, range.type, year.offset = year.offset)
        adj.matrix$mat <- adj.matrix$mat * cohort.adj.matrix$mat * (!hh.mat$mat) # Also removing members of own household

        reg.matrix <- model.matrix(formula(paste("~", paste(regs, collapse = " + "))), data = spatial.data)[, -1]
    } else {
        adj.matrix <- AdjMatrix$new(nrow = all.cohorts.dist.mat$nrow, ncol = all.cohorts.dist.mat$ncol)
        #         adj.matrix$mat <- ifelse(all.cohorts.dist.mat$mat <= radius, 1, 0) # This seems to be faster
        adj.matrix$mat <- all.cohorts.dist.mat$mat <= radius

        cohort.adj.matrix <- all.cohorts.spdf$get.cohort.adj.matrix(cohort.range, range.type, year.offset = year.offset, subset = daughter.id %in% spatial.data$daughter.id)
        adj.matrix$mat <- adj.matrix$mat * cohort.adj.matrix$mat

        reg.matrix <- model.matrix(formula(paste("~", paste(regs, collapse = " + "))), data = all.cohorts.spdf$spatial.data)[, -1]
    }

    diag(adj.matrix$mat) <- 0

    colnames(reg.matrix) <- clean.grpavg.matrix.colnames(colnames(reg.matrix), regs, prefix, postfix)
    which.rows <- as.integer(rownames(reg.matrix))

    if (!use.all.cohorts.data) {
        adj.matrix$mat <- adj.matrix$mat[which.rows, which.rows]
    } else {
        adj.matrix$mat <- adj.matrix$mat[, which.rows]
    }

    #     w.matrix <- make.row.stochastic(adj.matrix)
    w.matrix <- adj.matrix$copy()
    w.matrix$make.row.stochastic()
   
    if (degree > 1) {
        original.w.matrix <- w.matrix$copy()
        for (curr.deg in 2:degree) {
            w.matrix$mat <- w.matrix$mat %*% original.w.matrix$mat
        }
    }

   reg.matrix <- w.matrix$mat %*% reg.matrix

   grp.size <- adj.matrix$mat %*% rep.int(1, adj.matrix$ncol)
   grp.size.col <- "grp.size"
   if (!is.null(prefix)) grp.size.col <- paste(prefix, grp.size.col, sep = ".")
   if (!is.null(postfix)) grp.size.col <- paste(grp.size.col, postfix, sep = ".")

   if (!use.all.cohorts.data) {
       reg.matrix <- cbind(reg.matrix, spatial.data$daughter.id[which.rows], grp.size)
       colnames(reg.matrix)[ncol(reg.matrix) - c(1, 0)] <- c("daughter.id", grp.size.col)

       merge(reg.matrix, by = "daughter.id", all.x = TRUE)
   } else {
       spatial.data@data[, grp.size.col] <<- grp.size
       spatial.data@data <<- cbind(spatial.data@data, reg.matrix)
   }
})

DaughterFgmData$methods(
  generate.delivery.means = function(year.range = 1, year.offset = 12, regs = c("delivery.location", "delivered.by.daya"), range.type = c("both", "older"))
  {
      apply(c("birth.year.fac", "governorate"), calc.delivery.avgs, all.cohorts.spdf, year.range, year.offset, regs)
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
  }
)

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


