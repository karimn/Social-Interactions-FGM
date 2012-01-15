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

SpatialData$methods(initialize = function(coordinate.names, proj4string = "+proj=longlat +ellps=WGS84", ...) {
    callSuper(...)
    initFields(coordinate.names = coordinate.names, proj4string = proj4string)

    temp.data <- data
    data <<- data.frame()

    coordinates(temp.data) <- coordinate.names
    proj4string(temp.data) <- proj4string 

    spatial.data <<- temp.data
})

SpatialData$methods(subset = function(subset, select, ...) { 
    temp.data <- do.call(base::subset, c(list(x = as.data.frame(spatial.data), subset = substitute(subset), select = substitute(select)), list(...)), envir = parent.frame()) 

    coordinates(temp.data) <- coordinate.names
    proj4string(temp.data) <- proj4string 

    spatial.data <<- temp.data
}) 

SpatialData$methods(convert.callback = function(df, original.callback, ...) {
    original.callback(getRefClass()$new(data = df, coordinate.names = coordinate.names, proj4string = proj4string), ...)
})

SpatialData$methods(aggregate = function(by, FUN, ...) {
    stats::aggregate(spatial.data@data, spatial.data@data[by], stub.callback, FUN, .self, ...)
})

SpatialData$methods(tapply = function(by, FUN, ...) {
    base::tapply(spatial.data@data, spatial.data@data[by], stub.callback, FUN, .self, ...)
})

SpatialData$methods(split = function(by, ...) {
    DataCollection$new(coll = base::lapply(base::split(spatial.data@data, by, spatial.data@data[by], ...), 
                                           function(df) getRefClass()$new(data = df, coordinate.names = coordinate.names, proj4string = proj4string)))
})

SpatialData$methods(by = function(INDICES, FUN, ...) {
    base::by(spatial.data@data, INDICES = eval(substitute(INDICES), envir = spatial.data@data), FUN = stub.callback, FUN, .self, ...) 
})
