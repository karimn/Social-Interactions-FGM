library(sp)
library(spatstat)
library(maptools)

setClass("FgmData",
         representation(cluster.info = "data.frame"),
         contains = "SpatialPointsDataFrame")

if (isGeneric("cluster.coordinates<-")) removeGeneric("cluster.coordinates<-")
setGeneric("cluster.coordinates<-", function(object, value) standardGeneric("cluster.coordinates<-"))

setMethod("cluster.coordinates<-", 
          signature = c(object = "data.frame"),
          function(object, value) 
          {
            if (is.null(value$unique.cluster)) 
              value$unique.cluster <- as.numeric(row.names(value))

            fgm.spdf <- merge(object, value, by.x = c('dhs.year', 'cluster'), by.y = c('DHSYEAR', 'DHSCLUST'))
            coordinates(fgm.spdf) <- c("LONGNUM", "LATNUM")
            proj4string(fgm.spdf) <- CRS("+proj=longlat +ellps=WGS84")

            new("FgmData", fgm.spdf, cluster.info = value)
          }
)

if (!isGeneric("names")) setGeneric("names")

setMethod("names",
          signature = c(x = "FgmData"),
          function(x)
          {
            names(x@data)
          }
)

if (!isGeneric("by")) setGeneric("by")

setMethod("by",
          signature = c(data = "FgmData", INDICES = "list", FUN = "function"),
          function(data, INDICES, FUN, ...)
          {
            convert.FgmData <- function(df)
            {
              df <- merge(df, data@cluster.info[, c('DHSYEAR', 'DHSCLUST', 'LONGNUM', 'LATNUM')], by.x = c('dhs.year', 'cluster'), by.y = c('DHSYEAR', 'DHSCLUST'))
              coordinates(df) <- c('LONGNUM', 'LATNUM')
              proj4string(df) <- CRS("+proj=longlat +ellps=WGS84")

              FUN(new("FgmData", df, cluster.info = data@cluster.info))
            } 
            by(data@data, INDICES, convert.FgmData, ...)
          }
)

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

if (!isGeneric("subset")) setGeneric("subset")

setMethod("subset",
          signature = c(x = "FgmData"),
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
          signature = c(x = "FgmData"),
          function(x)
          {
            nrow(x@data)
          }
)

if (!isGeneric("by.radius")) setGeneric("by.radius", function(self, radius, fun, ...) standardGeneric("by.radius"))

setMethod("by.radius",
          signature = c(self = "FgmData", radius = "numeric", fun = "function"),
          function(self, radius, fun, indices = NULL, by.cluster = TRUE)
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
              stopifnot(nrow(df) > 0)

              u.cluster <- df$unique.cluster[1] 
              cluster.ppp <- clinfo.ppp[clinfo.ppp$marks == u.cluster]
              cluster.coords <- coords(cluster.ppp)
              neighbor.ppp <- clinfo.ppp[, disc(radius, c(cluster.coords$x, cluster.coords$y))]  
              neighbor.clusters <- neighbor.ppp$marks
              neighbor.clusters.fgm <- subset(self, unique.cluster %in% neighbor.clusters)

              if (by.cluster)
              {
                fun(u.cluster, neighbor.clusters.fgm)
              }
              else
              {
                by.row.fun <- function(dfhh, dfrl)
                {
                  fun(u.cluster, dfhh, dfrl, subset(neighbor.clusters.fgm, (unique.cluster != u.cluster) | (hh != dfhh) | (respond.linenum != dfrl)))
                }

                vec.by.row.fun <- Vectorize(by.row.fun)

                vec.by.row.fun(df$hh, df$respond.linenum)
              }
            }

            # TODO Make the "by" overridden implementation take indices from the "self" directly 
            if (is.null(indices))
              by(self, self@data[c('unique.cluster')], by.cluster.fun)
            else
              by(self, indices, function(df) do.call(rbind, by.radius(df, radius, fun, by.cluster = by.cluster)))
          }
)

