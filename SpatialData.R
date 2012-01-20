library(sp)

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

SpatialData$methods(subset = function(subset, select, drop = FALSE, center, radius, ...) { 
    stopifnot(!xor(missing(center), missing(radius)))
    origin.data <- as.data.frame(spatial.data)

    if (".eval.frame.n" %in% names(list(...))) {
        n <- list(...)[[".eval.frame.n"]]
    } else {
        n <- 1
    }

    if (missing(subset)) {
        r <- TRUE
    } else {
        e <- substitute(subset)
        r <- eval(e, origin.data, parent.frame(n))
        if (!is.logical(r)) { 
            stop("'subset' must evaluate to logical")
        }
        r <- r & !is.na(r)
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

    if(!missing(center)) {
        bb <- bbox(spatial.data)
        colnames(bb) <- NULL
        w <- owin(bb[1, ], bb[2, ])
        cc <- coordinates(spatial.data)
        spatial.ppp <- ppp(cc[, 1], cc[, 2], window = w, marks = 1L:nrow, check = FALSE)
        neighborhood <- spatial.ppp[, disc(radius, center)]
        r <- if (is.logical(r)) neighborhood$marks else intersect(r, neighborhood$marks)
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

#SpatialData$methods(tapply = function(by, FUN, ...) {
#    origin.data <- as.data.frame(spatial.data)
#    base::tapply(origin.data, origin.data[by], stub.callback, FUN, .self, ...)
#})

#Data$methods(tapply.change = function(by, FUN, ...) {
#    "The callback function must return an object of the same type of this class"  
#    do.call(base::rbind, lapply(base::tapply(data, data[by], stub.callback, FUN, .self, ...)), function (obj) obj$spatial.data)
#})

SpatialData$methods(split = function(by, ...) {
    origin.data <- as.data.frame(spatial.data)
    DataCollection$new(coll = base::lapply(base::split(origin.data, origin.data[by], ...), 
                                           function(df) create.new.from.data(df)))
})

SpatialData$methods(by = function(by.indices, FUN, rm.na = TRUE, ...) {
    origin.data <- as.data.frame(spatial.data)
    base::by(origin.data, INDICES = origin.data[by.indices], FUN = stub.callback, FUN, .self, rm.na, ...)
})

SpatialData$methods(quick.update = function(by, FUN, ...) {
    "The callback function will receive a reference to self, group IDs, the value of the group indices, and \"...\""
    origin.data <- as.data.frame(spatial.data)
    grp.ind <- base::tapply(origin.data[[1L]], origin.data[, by])
    max.grp.index <- max(grp.ind)
    for (grp.index in 1L:max.grp.index) {
        # mask <- grp.ind == grp.index
        grp.ids <- which(grp.ind == grp.index)
        first.in.grp <- min(grp.ids)

        grp.index.col.values <- list()

        for (by.index in by) {
            # BUG This is not very useful if the by.index column is a factor
            grp.index.col.values[by.index] <- origin.data[first.in.grp, by.index]
        }

        FUN(.self, grp.ids, grp.index.col.values, ...)
    }
})

SpatialData$methods(reshape = function(...) {
    #spatial.data <<- do.call("reshape", c(spatial.data, ...))

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

