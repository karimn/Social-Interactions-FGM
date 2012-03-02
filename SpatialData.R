library(sp)
library(googleVis)
library(parallel)

SpatialData <- setRefClass("SpatialData",
    contains = "Data",
    fields = list(
        spatial.data = "SpatialPointsDataFrame",
        coordinate.names = "character",
        proj4string = "character",
        names = function(value) { 
            if (missing(value)) {
                base::names(spatial.data@data) 
            } else {
                if ((nrow > 0) || (ncol > 0)) { 
                    base::names(spatial.data@data) <<- value
                } else {
                    # Do nothing!
                    # This is a bit of hack because the copy() function attempts to assign this field 
                    # before spatial.data has been copied!
                }
            }
        },
        #nrow = function(value) if (missing(value)) base::nrow(spatial.data@data) else stop("nrow is not mutable"),
        nrow = function(value) base::nrow(spatial.data),
        #ncol = function(value) if (missing(value)) base::ncol(spatial.data@data) else stop("ncol is not mutable"),
        ncol = function(value) base::ncol(spatial.data),
        coords = function(value) if (missing(value)) spatial.data@coords else spatial.data@coords <<- value))

SpatialData$lock(c("coordinate.names", "proj4string", "data"))

SpatialData$methods(init.from.list = function(obj.list) {
    stopifnot(all(base::sapply(obj.list, function(obj) is(obj, getClass()))) | is.empty(obj.list))

    spatial.data <<- do.call(base::rbind, base::lapply(obj.list, function(obj) obj$spatial.data))
    coordinate.names <<- obj.list[[1L]]$coordinate.names
    proj4string <<- objlist[[1L]]$proj4string
})

SpatialData$methods(initialize = function(data, columns, new.column.names, coordinate.names = NULL, proj4string = "+proj=longlat +ellps=WGS84", copy, ...) {
    arg.list <- list(...)
    if (!missing(copy) && is(copy, "SpatialData")) {
        spatial.data <<- copy$spatial.data
        coordinate.names <<- copy$coordinate.names
        proj4string <<- copy$proj4string
        .self$data <- data.frame(0)
    } else if (all(names(arg.list) != "collection") & !missing(data)) {
        temp.data <- data
        .self$data <- data.frame(0)

        callSuper(coordinate.names = coordinate.names, proj4string = proj4string, ...)

        if (!is.empty(temp.data) & !is.null(coordinate.names)) {
            coordinates(temp.data) <- coordinate.names
            proj4string(temp.data) <- proj4string 

            spatial.data <<- temp.data

            # The reason I do these here and not the base superclass's initialize() is because the subset() call there would attempt to use data 
            # that is not ready in spatial.data (the subset there will call this class's subset method)
            if (!missing(columns)) { # Select columns
                subset(select = eval(columns))
            }

            if (!missing(new.column.names)) { # Rename column names
                stopifnot(length(new.column.names) <= ncol(data))
                names[1:length(new.column.names)] <<- new.column.names
            }
        }
    } else {
        callSuper(...)
    }
})

SpatialData$methods(sort = function(by, ...) {
    spatial.data <<- spatial.data[do.call(order, c(as.list(as.data.frame(spatial.data)[, by, drop = FALSE]), list(...))), , drop = FALSE]
})

# BUG implement same function in class Data
SpatialData$methods(get.subset.rows = function(subset, center, radius, inner.radius = 0, ...) {
    stopifnot(!xor(missing(center), missing(radius)))

    if (".eval.frame.n" %in% names(list(...))) {
        n <- list(...)[[".eval.frame.n"]]
    } else {
        n <- 1
    }

    r <- rep(TRUE, nrow)

    if (!missing(subset)) {
        if (n > 1) {
            e <- substitute(subset, env = parent.frame(n - 1))
        } else {
            e <- substitute(subset)
        }

        r <- r & eval(e, spatial.data@data, parent.frame(n))
        r <- r & !is.na(r)
    }

    if(!missing(center)) {
        dists <- spDistsN1(spatial.data, center, longlat = TRUE) # in kilometers
        r <- r & (dists <= radius) & (dists >= inner.radius)
    }

    return(which(r))
})

# BUG Add rows parameter to Data class version 
SpatialData$methods(subset = function(subset, select, drop = FALSE, rows, center, radius, ...) { 
    stopifnot(!xor(missing(center), missing(radius)))

    origin.data <- as.data.frame(spatial.data)

    if (".eval.frame.n" %in% names(list(...))) {
        n <- list(...)[[".eval.frame.n"]]
    } else {
        n <- 1
    }

    r <- get.subset.rows(subset = subset, center = center, radius = radius, .eval.frame.n = n + 1)

    if (!missing(rows)) {
        r <- intersect(r, rows)
    }

    if (missing(select)) {
        vars <- TRUE
    } else {
        nl <- as.list(seq_along(origin.data))
        names(nl) <- names(origin.data)
        vars <- eval(substitute(select), nl, parent.frame(n))  
        if (min(vars) > 0) {
            vars <- c(vars, unlist(nl[coordinate.names])) # Making sure the coordinates are not removed 
                                                          #(this is why I'm basically reimplementing subset()
        }
    }

    origin.data <- origin.data[r, vars, drop = drop]

    if (nrow(origin.data) == 0) {
        signalCondition(simpleError("Empty data", call = 1)) # Using "call" to indicate this type of error
        # to distinguish it from a regular stop() call
    }

    coordinates(origin.data) <- coordinate.names
    proj4string(origin.data) <- proj4string 

    spatial.data <<- origin.data
}) 

SpatialData$methods(get.subset = function(...) { 
    new.data.obj <- copy()

    # I need to do this because the call to subset() might result in an empty data.frame which cannot be converted into
    # a SpatialPointsDataFrame (the required type of the field spatial.data)
    tryCatch({
        new.data.obj$subset(.eval.frame.n = 2, ...) 
        new.data.obj
    },
    error = function(e) {
        if (!is.null(conditionCall(e)) && (conditionCall(e) == 1)) {
            return(NULL)
        } else {
            stop(e)
        }
    })
})

SpatialData$methods(remove.rows = function(rows) {
    spatial.data <<- spatial.data[-rows, ]
})

SpatialData$methods(create.new.from.data = function(df, ...) {
    callSuper(df = df, coordinate.names = coordinate.names, proj4string = proj4string, ...)
})

SpatialData$methods(aggregate = function(by, FUN, ...) {
    origin.data <- as.data.frame(spatial.data)
    stats::aggregate(origin.data, origin.data[by], stub.callback, FUN, .self, ...)
})

SpatialData$methods(split = function(by, ...) {
    origin.data <- as.data.frame(spatial.data)
    DataCollection$new(coll = base::lapply(base::split(origin.data, origin.data[by], ...), 
                                           function(df) create.new.from.data(df)))
})

SpatialData$methods(by = function(by.indices, FUN, rm.na = TRUE, ...) {
    origin.data <- as.data.frame(spatial.data)
    base::by(origin.data, INDICES = origin.data[by.indices], FUN = stub.callback, FUN, .self, rm.na, ...)
})

SpatialData$methods(apply = function(by, FUN, ..., subset, .eval.frame.n) {
    "The callback function will receive a reference to self, group IDs, the value of the group indices, and \"...\""
    origin.data <- as.data.frame(spatial.data)
    dot.list <- list(...)

    if (missing(subset)) {
        subset.rows <- NULL 
    } else {
        #if (".eval.frame.n" %in% names(dot.list)) {
        if (!missing(.eval.frame.n)) {  #" %in% names(dot.list)) {
            #             n <- dot.list[[".eval.frame.n"]]
            n <- .eval.frame.n
            #dot.list <- dot.list[names(dot.list) != ".eval.frame.n"]
        } else {
            n <- 1
        }
        subset.rows <- get.subset.rows(subset = subset, .eval.frame.n = n + 1)
    }

    grp.ind <- base::tapply(origin.data[[1L]], origin.data[, by])

    invisible(lapply(unique(grp.ind), function(grp.index) {
        grp.ids <- which(grp.ind == grp.index)

        if (!is.null(subset.rows)) {
            grp.ids <- intersect(grp.ids, subset.rows)

            if (length(grp.ids) == 0) return(NULL)
        }
        
        stopifnot(all(grp.ids <= nrow))
        grp.index.col.values <- as.list(origin.data[grp.ids[1], by]) 
        return(FUN(.self, grp.ids, grp.index.col.values, ...))
    }))
})

SpatialData$methods(par.apply = function(by, FUN, ...) {
    "The callback function will receive a reference to self, group IDs, the value of the group indices, and \"...\""
    origin.data <- as.data.frame(spatial.data)
    grp.ind <- base::tapply(origin.data[[1L]], origin.data[[by]])
    max.grp.index <- max(grp.ind)

    ret.list <- mclapply(1L:max.grp.index, function(grp.index) { 
        grp.ids <- which(grp.ind == grp.index)
        first.in.grp <- min(grp.ids)
        stopifnot(all(grp.ids <= nrow))

        if (length(grp.ids) > 0) {
            grp.index.col.values <- list()

            for (by.index in by) {
                grp.index.col.values[by.index] <- origin.data[first.in.grp, by.index]
            }

            return(FUN(.self, grp.ids, grp.index.col.values, ...))
        } else {
            return(NULL)
        }
    })

    invisible(ret.list)
})

SpatialData$methods(reshape = function(...) {
    origin.data <- as.data.frame(spatial.data)
    origin.data <- stats::reshape(origin.data, ...)
    coordinates(origin.data) <- coordinate.names
    proj4string(origin.data) <- proj4string 

    spatial.data <<- origin.data
})

SpatialData$methods(duplicated = function(by, ...) {
    origin.data <- as.data.frame(spatial.data)
    if (missing(by)) {
        base::duplicated(origin.data, ...)
    } else {
        base::duplicated(origin.data[by], ...)
    }
})

SpatialData$methods(relevel = function(column, ref, ...) {
    spatial.data@data[,column] <<- stats::relevel(spatial.data@data[,column], ref = ref, ...)
})

SpatialData$methods(summary = function(columns = TRUE, ...) {
    base::summary(spatial.data@data[, columns], ...)
})

SpatialData$methods(merge = function(other.data, ...) {
    origin.data <- as.data.frame(spatial.data)
    origin.data <- base::merge(origin.data, other.data, ...)
    coordinates(origin.data) <- coordinate.names
    proj4string(origin.data) <- proj4string 

    spatial.data <<- origin.data
})

# Mapping

SpatialData$methods(plot.gvis.map = function(tip.col, ...) {
    map.df <- data.frame(latlong = base::apply(coords, 1, function(row) paste(rev(row), collapse = ":")), 
                         tip = if (missing(tip.col)) seq_along(rownames(spatial.data@data)) else spatial.data@data[,tip.col])
    plot(googleVis::gvisMap(map.df, "latlong", "tip", ...))
})

# Regression methods

SpatialData$methods(lm = function(formula, vcov.fun = NULL, ...) {
    r <- stats::lm(formula, data = spatial.data)

    if (!is.null(vcov.fun)) {
        vcov <- vcov.fun(r, ...)

        RegressionResults$new(results = r, regress.formula = formula, vcov = vcov)
    } else {
        RegressionResults$new(results = r, regress.formula = formula) 
    }
  })

SpatialData$methods(tsls = function(formula, vcov.fun = NULL, ...) {
    r <- sem::tsls(formula, formula, data = spatial.data)

    if (!is.null(vcov.fun)) {
        vcov <- vcov.fun(r, ...) 
        RegressionResults$new(results = r, regress.formula = formula, vcov = vcov)
    } else {
        RegressionResults$new(results = r, regress.formula = formula) 
    }
  })

SpatialData$methods(ivreg = function(formula, vcov.fun = NULL, ...) {
    r <- AER::ivreg(formula, formula, data = spatial.data)

    if (!is.null(vcov.fun)) {
        vcov <- vcov.fun(r, ...) 
        RegressionResults$new(results = r, regress.formula = formula, vcov = vcov)
    } else {
        RegressionResults$new(results = r, regress.formula = formula) 
    }
  })

SpatialData$methods(plm = function(formula, model, ..., vcov.fun = NULL) {
    r <- plm::plm(formula, model = model, ..., data = spatial.data@data)

    if (!is.null(vcov.fun)) {
        vcov <- vcov.fun(r) 
        PanelRegressionResults$new(results = r, regress.formula = formula, vcov = vcov)
    } else {
        PanelRegressionResults$new(results = r, regress.formula = formula) 
    }
  }
)
