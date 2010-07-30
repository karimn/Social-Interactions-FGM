setClass("FgmDataAll",
         representation(cluster.info = "data.frame"),
         contains = "SpatialPointsDataFrame")

if (!isGeneric("cluster.coordinates<-")) setGeneric("cluster.coordinates<-", function(object, value) standardGeneric("cluster.coordinates<-"))

setMethod("cluster.coordinates<-", 
          signature = c(object = "data.frame"),
          function(object, value) 
          {
            fgm.spdf <- merge(object, value, by.x = c('cluster'), by.y = c('DHSCLUST'))
            coordinates(fgm.spdf) <- c("LONGNUM", "LATNUM")
            proj4string(fgm.spdf) <- CRS("+proj=longlat +ellps=WGS84")

            new("FgmDataAll", fgm.spdf, cluster.info = value)
          }
)


