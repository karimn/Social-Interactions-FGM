library(sp)
library(spatstat)
library(maptools)

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

if (!isGeneric("by")) setGeneric("by")

setMethod("by",
          signature = c(data = "FgmDataAll", INDICES = "list", FUN = "function"),
          function(data, INDICES, FUN, ...)
          {
            by(data@data, INDICES, FUN, ...)
          }
)

if (!isGeneric("by.radius")) setGeneric("by.radius", function(self, radius, fun) standardGeneric("by.radius"))

setMethod("by.radius",
          signature = c(self = "FgmDataAll", radius = "numeric", fun = "function"),
          function(self, radius, fun)
          {
            clinfo.sp <- self@cluster.info
            coordinates(clinfo.sp) <- c("LONGNUM", "LATNUM")
            proj4string(clinfo.sp) <- CRS("+proj=longlat +ellps=WGS84")

            # I got this from the definition of as.ppp.SpatialPoints() from maptools
            bb <- bbox(clinfo.sp)
            colnames(bb) <- NULL
            w <- owin(bb[1, ], bb[2, ])
            cc <- coordinates(clinfo.sp)
            # I'm going to use the "marks" field to identify the clusters
            clinfo.ppp <- ppp(cc[, 1], cc[, 2], window = w, marks = clinfo.sp$DHSCLUST, check = FALSE)

            by.cluster <- function(df)
            {
              cluster <- df$cluster[1] 
              cluster.ppp <- clinfo.ppp[clinfo.ppp$marks == cluster]
              cluster.coords <- coords(cluster.ppp)
              neighbor <- clinfo.ppp[, disc(radius, c(cluster.coords$x, cluster.coords$y))]  


            }
            
            # TODO Make the "by" overridden implementation take indices from the "self" directly 
            by(self, self@data[c("cluster")], by.cluster)
          }
)

#fgm.data.all.sp <- as(fgm.data.all, "SpatialPoints")
#fgm.data.all.ppp <- as(fgm.data.all.sp, "ppp")
#fgm.data.all.ppp
#summary(fgm.data.all.ppp)
#fgm.data.all.ppp[228]
#fgm.data.all.ppp[228,]
#? ppp
#? disc
#n <- fgm.data.all.ppp[, disc(1, c(f))))
#fgm.data.all$LONGNUM
#coordinates(fgm.data.all)
#coordinates(fgm.data.all[228])
#coordinates(fgm.data.all[228,])
#coordinates(fgm.data.all[228,])$LONGNUM
#coordinates(fgm.data.all[228,])[1,1]
#coordinates(fgm.data.all[228,])[1,2]
#n <- fgm.data.all.ppp[, disc(1, coordinates(fgm.data.all[228,]))]
#n
#fgm.data.all.ppp
#plot(as(n, "SpatialPoints"))
#n <- fgm.data.all.ppp[, disc(0.5, coordinates(fgm.data.all[228,]))]
#plot(as(n, "SpatialPoints"))
#plot(as(fgm.data.all.ppp[, disc(0.5, coordinates(fgm.data.all[228,]))], "SpatialPoints"))
#plot(as(fgm.data.all.ppp[, disc(2, coordinates(fgm.data.all[228,]))], "SpatialPoints"))

