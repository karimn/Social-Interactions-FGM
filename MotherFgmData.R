library(sp)
library(spatstat)
library(maptools)

setClass("MotherFgmData",
         contains = "BaseFgmData")

setMethod("initialize", "MotherFgmData",
          function(.Object, ..., ir.file, gps.file, dhs.year = 2008)
          {
            callNextMethod(.Object, ..., ir.file = ir.file, gps.file = gps.file, dhs.year = dhs.year)
          }
)

#if (isGeneric("cluster.coordinates<-")) removeGeneric("cluster.coordinates<-")
#setGeneric("cluster.coordinates<-", function(object, value) standardGeneric("cluster.coordinates<-"))

##setMethod("cluster.coordinates<-", 
#          signature = c(object = "data.frame"),
#          function(object, value) 
#          {
#            if (is.null(value$unique.cluster)) 
#              value$unique.cluster <- as.numeric(row.names(value))
#
#            fgm.spdf <- merge(object, value, by.x = c('dhs.year', 'cluster'), by.y = c('DHSYEAR', 'DHSCLUST'))
#            coordinates(fgm.spdf) <- c("LONGNUM", "LATNUM")
#            proj4string(fgm.spdf) <- CRS("+proj=longlat +ellps=WGS84")
#
#            new("FgmData", fgm.spdf, cluster.info = value)
#          }
#)


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


