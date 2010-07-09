library(foreign)

cleanup.by.hh <- function(df) {
  # Removing birth.year duplicates
  dup <- duplicated(df$birth.year)
  df <- df[!dup,]

  n <- nrow(df)

  df$n.ord <- n
  df$order <- 1:n

  return(df)
}

get.fgm.data <- function(ir.file, br.file) {
  ir <- read.dta(ir.file, convert.underscore = TRUE)

  #if (!('g116' %in% names(ir))) {
  #  ir$g116 = NA
  #}
  
  fgm.data <- subset(ir, select = c(v000, v001, v002, v003, v004, v005, v023, v024, v025, v104, v130, v151, v190, g116, sdcol.1:sdcol.7, s906.1:s906.7, s908.1:s908.7, s909.1:s909.7, s910.1:s910.7, s911.1:s911.7))

  rm(ir)

  names(fgm.data)[1:14] <- c('phase', 'cluster', 'hh', 'respond', 'area.unit', 'weight', 'domain', 'region', 'urban.rural', 'years.lived.res', 'religion', 'hh.head.sex', 'wealth.index', 'intends.circum')

  fgm.data <- reshape(fgm.data, varying = list(names(fgm.data)[15:21], names(fgm.data)[22:28], names(fgm.data)[29:35], names(fgm.data)[36:42], names(fgm.data)[43:49], names(fgm.data)[50:56]), direction = 'long', sep = '.', v.names = c('sdcol', 'line.num', 'mar.status', 'circum', 'who.circum', 'age.circum'), timevar = 'order')

  fgm.data <- subset(fgm.data, !is.na(sdcol), select = c(-sdcol))

  fgm.data <- transform(fgm.data, circum = as.numeric(circum), mar.status = as.numeric(mar.status), who.circum = as.numeric(who.circum))

  fgm.data$circum.yesno <- ifelse(fgm.data$circum == 1, 1, 0)

  fgm.data$hh.id <- paste(fgm.data$cluster, fgm.data$hh, sep = '-')
  fgm.data$hh.id <- factor(fgm.data$hh.id)

  br <- read.dta(br.file, convert.underscore = TRUE)
  fgm.data <- merge(fgm.data, br, by.x = c('cluster', 'hh', 'respond', 'line.num'), by.y = c('v001', 'v002', 'v003', 'bidx'))
  rm(br)

  fgm.data$birth.year <- factor(fgm.data$b2)
  names(fgm.data)[names(fgm.data) == 'v437'] <- 'weight'
  names(fgm.data)[names(fgm.data) == 'v438'] <- 'height'
  fgm.data <- subset(fgm.data, select = c(cluster:hh.id, birth.year, weight, height, sdno)) 
  fgm.data <- subset(fgm.data, select = c(-phase)) #excluding phase for now
  fgm.data <- fgm.data[ order(fgm.data$hh.id, fgm.data$birth.year), ]

  fgm.data <- do.call(rbind, by(fgm.data, fgm.data$hh.id, cleanup.by.hh))

  # To check if there are any duplicated birth.year's in the same hh
  # by(fgm.data, fgm.data[c('hh.id')], function(df) anyDuplicated(df$birth.year))

  fgm.data$has.or.intends.circum <- ifelse(fgm.data$circum == 1 | fgm.data$intends.circum == 1, 1, 0)
  fgm.data$med.circum <- ifelse(fgm.data$circum == 1 & (fgm.data$who.circum %in% c(1, 2)), 1, 0)
  fgm.data$year.circum <- factor(with(fgm.data, ifelse(circum == 1 & age.circum <= 19, as.numeric(levels(birth.year)[birth.year]) + age.circum, NA)), ordered = TRUE)

  fgm.data$timeperiod.circum <- factor(with(fgm.data, ifelse(year.circum > "2007", 0,
                                                             ifelse(year.circum >= "1997", 1, 
                                                                    ifelse(year.circum < "1997", 2, NA)))),
                                       labels = c("After 2007", "1997-2007", "Before 1997"))

  fgm.data$order.fac <- factor(with(fgm.data, as.numeric(levels(order)[order])), levels = 1:7)
  fgm.data$married <- ifelse(fgm.data$mar.status == 1, 1, 0)

  return(fgm.data)
}

#ir.08 <- read.dta('/home/karim/Data/EDHS/2008/EGIR5AFL.DTA', convert.underscore = TRUE)
#br.08 <- read.dta('/home/karim/Data/EDHS/2008/EGBR5AFL.DTA', convert.underscore = TRUE)

#fgm.data.05 <- get.fgm.data('~/Data/EDHS/2005/egir51fl.dta', '~/Data/EDHS/2005/EGBR51FL.dta')
fgm.data.08 <- get.fgm.data('~/Data/EDHS/2008/EGIR5AFL.DTA', '~/Data/EDHS/2008/EGBR5AFL.DTA')

