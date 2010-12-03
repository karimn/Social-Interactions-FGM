library(sp)
library(spatstat)
library(maptools)

cleanup.by.hh <- function(df) {
  # Removing birth.year duplicates
  dup <- duplicated(df$birth.year)
  df <- df[!dup,]

  n <- nrow(df)

  df$n.ord <- n
  df$order <- 1:n

  return(df)
}

setClass("DaughterFgmData",
         contains = "BaseFgmData")

setMethod("initialize", "DaughterFgmData",
          function(.Object, ..., ir.file, br.file, gps.file, dhs.year = 2008)
          {
            cols <- quote(c(eval(FgmData.cols), sdcol.1:sdcol.7, s906.1:s906.7, s908.1:s908.7, s909.1:s909.7, s910.1:s910.7, s911.1:s911.7))
            this <- callNextMethod(.Object, ..., ir.file = ir.file, gps.file = gps.file, cols = cols, dhs.year = dhs.year)

            num.col <- length(FgmData.col.names)

            this@data <- reshape(this@data, 
                                 varying = list(names(this@data)[(num.col + 1):(num.col + 7)], 
                                                names(this@data)[(num.col + 8):(num.col + 14)], 
                                                names(this@data)[(num.col + 15):(num.col + 21)], 
                                                names(this@data)[(num.col + 22):(num.col + 28)], 
                                                names(this@data)[(num.col + 29):(num.col + 35)], 
                                                names(this@data)[(num.col + 36):(num.col + 42)]), 
                                 direction = 'long', 
                                 sep = '.', 
                                 v.names = c('sdcol', 'line.num', 'mar.status', 'circum', 'who.circum', 'age.circum'), 
                                 timevar = 'order')

            this@data <- subset(this@data, !is.na(sdcol), select = c(-sdcol))
            this@data <- transform(this@data, 
                                   circum = as.numeric(circum), 
                                   mar.status = as.numeric(mar.status), 
                                   who.circum = as.numeric(who.circum))
            this@data <- transform(this@data,
                                   circum.yesno = ifelse(circum == 1, 1, 0))

            br <- read.dta(br.file, convert.underscore = TRUE)
            br <- subset(br, select = c(v001:v003, bidx, v437, v438, b2, sdno))
            this@data <- merge(this@data, br, by.x = c('cluster', 'hh', 'respond', 'line.num'), by.y = c('v001', 'v002', 'v003', 'bidx'))
            rm(br)
            
            names(this@data)[names(this@data) == 'v437'] <- 'weight'
            names(this@data)[names(this@data) == 'v438'] <- 'height'
            names(this@data)[names(this@data) == 'b2'] <- 'birth.year'

            this@data <- within(this@data, 
            {
              age <- dhs.year - birth.year
              has.or.intends.circum <- ifelse(circum == 1 | ((age <= 12) & (intends.circum == 1)), 1, 0)
              med.circum <- ifelse(circum == 1 & (who.circum %in% c(1, 2)), 1, 0)
              year.circum <- ifelse(circum == 1 & age.circum <= 19, birth.year + age.circum, NA)
              year.circum.fac <- factor(year.circum)
              order.fac <- factor(order)
              married <- ifelse(mar.status == 1, 1, 0)
              birth.year.fac <- factor(birth.year)
            })

            this@data <- do.call(rbind, by(this@data, this@data$hh.id, cleanup.by.hh))
            this@data <- this@data[ order(this@data$hh.id, this@data$birth.year), ]

            return(this)
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


