library(sp)
library(spatstat)
library(maptools)

setClass("FgmDataAll",
         representation(cluster.info = "data.frame"),
         contains = "SpatialPointsDataFrame")

if (isGeneric("cluster.coordinates<-")) removeGeneric("cluster.coordinates<-")
setGeneric("cluster.coordinates<-", function(object, value) standardGeneric("cluster.coordinates<-"))

setMethod("cluster.coordinates<-", 
          signature = c(object = "data.frame"),
          function(object, value) 
          {
            fgm.spdf <- merge(object, value, by.x = c('dhs.year', 'cluster'), by.y = c('DHSYEAR', 'DHSCLUST'))
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
            convert.FgmDataAll <- function(df)
            {
              cluster.coordinates(df) <- data@cluster.info
              FUN(df)
            }

            by(data@data, INDICES, convert.FgmDataAll, ...)
          }
)

if (!isGeneric("subset")) setGeneric("subset")

setMethod("subset",
          signature = c(x = "FgmDataAll"),
          function(x, subset, ...)
          {
            # same implementation as subset.base().  I had to do this because of a parent.frame() used in evaluating the subset param
            if (missing(subset)) 
                r <- TRUE
            else 
            {
                e <- substitute(subset)
                r <- eval(e, x@data, parent.frame(n = 2))
                if (!is.logical(r)) 
                    stop("'subset' must evaluate to logical")
                r <- r & !is.na(r)
            }

            x[r, ]
          }
)

if (!isGeneric("nrow")) setGeneric("nrow")

setMethod("nrow",
          signature = c(x = "FgmDataAll"),
          function(x)
          {
            nrow(x@data)
          }
)

if (!isGeneric("by.radius")) setGeneric("by.radius", function(self, radius, fun, ...) standardGeneric("by.radius"))

setMethod("by.radius",
          signature = c(self = "FgmDataAll", radius = "numeric", fun = "function"),
          function(self, radius, fun, by.cluster = TRUE)
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
            clinfo.ppp <- ppp(cc[, 1], cc[, 2], window = w, marks = clinfo.sp$unique.cluster, check = FALSE)

            by.cluster.fun <- function(df)
            {
              dhs.year <- df$dhs.year[1]
              cluster <- df$cluster[1]
              unique.cluster <- df$unique.cluster[1] 
              cluster.ppp <- clinfo.ppp[clinfo.ppp$marks == unique.cluster]
              cluster.coords <- coords(cluster.ppp)
              neighbor.ppp <- clinfo.ppp[, disc(radius, c(cluster.coords$x, cluster.coords$y))]  
              neighbor.clusters <- neighbor.ppp$marks
              neighbor.clusters.fgm <- subset(self, unique.cluster %in% neighbor.clusters)

              if (by.cluster)
                fun(dhs.year, cluster, neighbor.clusters.fgm)
              else
              {
                by.row.fun <- function(dfhh, dfrl)
                {
                  fun(dhs.year, cluster, dfhh, dfrl, subset(neighbor.clusters.fgm, (cluster != cluster) | (hh != dfhh) | (respond.linenum != dfrl)))
                }

                vec.by.row.fun <- Vectorize(by.row.fun)

                vec.by.row.fun(df$hh, df$respond.linenum)
              }
            }
            
            # TODO Make the "by" overridden implementation take indices from the "self" directly 
            by(self, self@data[c("unique.cluster")], by.cluster.fun)
          }
)

