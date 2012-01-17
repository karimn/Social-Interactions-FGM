library(sp)

SpatialData <- setRefClass("SpatialData",
    contains = "Data",
    fields = list(
        spatial.data = "SpatialPointsDataFrame",
        coordinate.names = "character",
        proj4string = "character",
        names = function(value) if (missing(value)) base::names(spatial.data@data) else base::names(spatial.data@data) <<- value,
        #nrow = function(value) if (missing(value)) base::nrow(spatial.data@data) else stop("nrow is not mutable"),
        nrow = function(value) base::nrow(spatial.data@data),
        #ncol = function(value) if (missing(value)) base::ncol(spatial.data@data) else stop("ncol is not mutable"),
        ncol = function(value) base::ncol(spatial.data@data),
        coords = function(value) if (missing(value)) spatial.data@coords else spatial.data@coords <<- value))

SpatialData$lock(c("coordinate.names", "proj4string"))

SpatialData$methods(init.from.list = function(obj.list) {
    stopifnot(all(base::sapply(obj.list, function(obj) is(obj, getClass()))) | is.empty(obj.list))

    spatial.data <<- do.call(base::rbind, base::lapply(obj.list, function(obj) obj$spatial.data))
    coordinate.names <<- obj.list[[1L]]$coordinate.names
    proj4string <<- objlist[[1L]]$proj4string
})

SpatialData$methods(initialize = function(coordinate.names, proj4string = "+proj=longlat +ellps=WGS84", ...) {
    if (all(names(list(...)) != "collection")) {
        callSuper(coordinate.names = coordinate.names, proj4string = proj4string, ...)

        temp.data <- data
        data <<- data.frame()

        coordinates(temp.data) <- coordinate.names
        proj4string(temp.data) <- proj4string 

        spatial.data <<- temp.data
    } else {
        callSuper(...)
    }
})

SpatialData$methods(sort = function(by, ...) {
    spatial.data <<- spatial.data[order(spatial.data[[by]], ...), , drop = FALSE]
})

SpatialData$methods(subset = function(subset, select, ...) { 
    #spatial.data <<- do.call("subset", c(list(x = as.data.frame(spatial.data), subset = substitute(subset), select = substitute(select)), list(...)), envir = parent.frame()) 
    temp.data <- do.call(base::subset, c(list(x = as.data.frame(spatial.data), subset = substitute(subset), select = substitute(select)), list(...)), envir = parent.frame()) 

    coordinates(temp.data) <- coordinate.names
    proj4string(temp.data) <- proj4string 

    spatial.data <<- temp.data
}) 

SpatialData$methods(remove.rows = function(rows) {
    spatial.data <<- data[-rows, ]
})

SpatialData$methods(create.new.from.data = function(df, ...) {
    callSuper(data = df, coordinate.names = coordinate.names, proj4string = proj4string, ...)
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

SpatialData$methods(by = function(by.indices, FUN, ...) {
    origin.data <- as.data.frame(spatial.data)
    base::by(origin.data, INDICES = origin.data[by.indices], FUN = stub.callback, FUN, .self, ...)
})

SpatialData$methods(quick.update = function(by, FUN, ...) {
    "The callback function will receive a reference to self, a group mask, the value of the group indices, and \"...\""
    origin.data <- as.data.frame(spatial.data)
    grp.ind <- base::tapply(origin.data[[1L]], origin.data[[by]])
    max.grp.index <- max(grp.ind)
    for (grp.index in 1L:max.grp.index) {
        mask <- grp.ind == grp.index
        first.in.grp <- which.min(mask)

        grp.index.values <- list()

        for (by.index in by) {
            grp.index.values[by.index] <- origin.data[first.in.grp, by.index]
        }

        FUN(.self, mask, grp.index.values, ...)
    }
})

SpatialData$methods(reshape = function(...) {
    spatial.data <<- do.call("reshape", c(spatial.data, ...))
})

SpatialData$methods(duplicated = function(by, ...) {
    origin.data <- as.data.frame(spatial.data)
    if (missing(by)) {
        base::duplicated(origin.data, ...)
    } else {
        base::duplicated(origin.data[by], ...)
    }
})
