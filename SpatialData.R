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

SpatialData$methods(initialize = function(coords, proj4string = "+proj=longlat +ellps=WGS84", ...) {
    callSuper(...)
    initFields(coordinate.names = coords, proj4string = proj4string)

    temp.data <- data
    data <<- data.frame()

    coordinates(temp.data) <- coords 
    proj4string(temp.data) <- proj4string 

    spatial.data <<- temp.data
})

SpatialData$methods(subset = function(subset, select, ...) { 
    temp.data <- do.call(base::subset, c(list(x = as.data.frame(spatial.data), subset = substitute(subset), select = substitute(select)), list(...)), envir = parent.frame()) 

    coordinates(temp.data) <- coordinate.names
    proj4string(temp.data) <- proj4string 

    spatial.data <<- temp.data
}) 


SpatialData$methods(by = function(INDICES, FUN, ...) {
    convert.FgmData <- function(df, ...)
    {
      df <- merge(df, data@cluster.info[, c('DHSYEAR', 'DHSCLUST', 'LONGNUM', 'LATNUM')], by.x = c('dhs.year', 'cluster'), by.y = c('DHSYEAR', 'DHSCLUST'))
      coordinates(df) <- c('LONGNUM', 'LATNUM')
      proj4string(df) <- CRS("+proj=longlat +ellps=WGS84")

      FUN(getRefClass()$new(

      FUN(new("FgmData", df, cluster.info = data@cluster.info), ...)
    } 

    by(spdf@data, INDICES, convert.FgmData, ...)
  }
)
