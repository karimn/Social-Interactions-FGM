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

ir <- read.dta('/home/karim/Data/EDHS/2008/EGIR5AFL.DTA', convert.underscore = TRUE)

#setClass("fgmDaughterDataFrame", contains = "data.frame")
#setMethod("initialize", "fgmDaughterDataFrame",
#  function(.Object, ir.file, br.file, ...) {
#get.fgm.data <- function(ir.file, br.file) {
#ir <- read.dta(ir.file, convert.underscore = TRUE)

#if (!('g116' %in% names(ir))) {
#  ir$g116 = NA
#}

fgm.data.daughters <- subset(ir, select = c(v000, v001, v002, v003, v004, v005, v023, v024, v025, v104, v130, v151, v190, g116, sdcol.1:sdcol.7, s906.1:s906.7, s908.1:s908.7, s909.1:s909.7, s910.1:s910.7, s911.1:s911.7))

rm(ir)

names(fgm.data.daughters)[1:14] <- c('phase', 'cluster', 'hh', 'respond', 'area.unit', 'wt', 'domain', 'region', 'urban.rural', 'years.lived.res', 'religion', 'hh.head.sex', 'wealth.index', 'intends.circum')

fgm.data.daughters <- reshape(fgm.data.daughters, varying = list(names(fgm.data.daughters)[15:21], names(fgm.data.daughters)[22:28], names(fgm.data.daughters)[29:35], names(fgm.data.daughters)[36:42], names(fgm.data.daughters)[43:49], names(fgm.data.daughters)[50:56]), direction = 'long', sep = '.', v.names = c('sdcol', 'line.num', 'mar.status', 'circum', 'who.circum', 'age.circum'), timevar = 'order')

fgm.data.daughters <- subset(fgm.data.daughters, !is.na(sdcol), select = c(-sdcol))

fgm.data.daughters <- transform(fgm.data.daughters, circum = as.numeric(circum), mar.status = as.numeric(mar.status), who.circum = as.numeric(who.circum))

fgm.data.daughters$circum.yesno <- ifelse(fgm.data.daughters$circum == 1, 1, 0)

fgm.data.daughters$hh.id <- paste(fgm.data.daughters$cluster, fgm.data.daughters$hh, sep = '-')
fgm.data.daughters$hh.id <- factor(fgm.data.daughters$hh.id)

#br <- read.dta(br.file, convert.underscore = TRUE)
br <- read.dta('/home/karim/Data/EDHS/2008/EGBR5AFL.DTA', convert.underscore = TRUE)

fgm.data.daughters <- merge(fgm.data.daughters, br, by.x = c('cluster', 'hh', 'respond', 'line.num'), by.y = c('v001', 'v002', 'v003', 'bidx'))
rm(br)

fgm.data.daughters$birth.year <- factor(fgm.data.daughters$b2)
names(fgm.data.daughters)[names(fgm.data.daughters) == 'v437'] <- 'weight'
names(fgm.data.daughters)[names(fgm.data.daughters) == 'v438'] <- 'height'

pr <- read.dta('/home/karim/Data/EDHS/2008/EGPR5AFL.DTA', convert.underscore = TRUE)

fgm.data.daughters <- merge(fgm.data.daughters, pr, by.x = c('cluster', 'hh', 'respond'), by.y = c('hv001', 'hv002', 'hv003'))
rm(pr)

names(fgm.data.daughters)[names(fgm.data.daughters) == 'hc61'] <- 'mother.educlvl'
names(fgm.data.daughters)[names(fgm.data.daughters) == 'hc62'] <- 'mother.educyr'

fgm.data.daughters <- transform(fgm.data.daughters, mother.edulvl = factor(mother.educlvl, levels = c(0:3, 9), labels = c("no education", "primary", "secondary", "higher"), exclude = 9))

fgm.data.daughters <- subset(fgm.data.daughters, select = c(cluster:hh.id, birth.year, weight, height, sdno, mother.educlvl, mother.educyr)) 
fgm.data.daughters <- subset(fgm.data.daughters, select = c(-phase)) #excluding phase for now

fgm.data.daughters <- fgm.data.daughters[ order(fgm.data.daughters$hh.id, fgm.data.daughters$birth.year), ]

fgm.data.daughters <- do.call(rbind, by(fgm.data.daughters, fgm.data.daughters$hh.id, cleanup.by.hh))

# To check if there are any duplicated birth.year's in the same hh
# by(fgm.data.daughters, fgm.data.daughters[c('hh.id')], function(df) anyDuplicated(df$birth.year))

fgm.data.daughters$has.or.intends.circum <- ifelse(fgm.data.daughters$circum == 1 | fgm.data.daughters$intends.circum == 1, 1, 0)
fgm.data.daughters$med.circum <- ifelse(fgm.data.daughters$circum == 1 & (fgm.data.daughters$who.circum %in% c(1, 2)), 1, 0)
fgm.data.daughters$year.circum <- factor(with(fgm.data.daughters, ifelse(circum == 1 & age.circum <= 19, as.numeric(levels(birth.year)[birth.year]) + age.circum, NA)), ordered = TRUE)

fgm.data.daughters$timeperiod.circum <- factor(with(fgm.data.daughters, ifelse(year.circum > "2007", 0,
                                                           ifelse(year.circum >= "1997", 1, 
                                                                  ifelse(year.circum < "1997", 2, NA)))),
                                     labels = c("After 2007", "1997-2007", "Before 1997"))

fgm.data.daughters$order.fac <- factor(with(fgm.data.daughters, as.numeric(levels(order)[order])), levels = 1:7)
fgm.data.daughters$married <- ifelse(fgm.data.daughters$mar.status == 1, 1, 0)

fgm.data.daughters$religion <- factor(fgm.data.daughters.08$religion, labels = c("Muslim", "Christian", "Missing"))

#return(fgm.data.daughters)
#callNextMethod(.Object, ir.file = ir.file, br.file = br.file, ...)
#}

#ir.08 <- read.dta('/home/karim/Data/EDHS/2008/EGIR5AFL.DTA', convert.underscore = TRUE)
#br.08 <- read.dta('/home/karim/Data/EDHS/2008/EGBR5AFL.DTA', convert.underscore = TRUE)

#fgm.data.daughters.05 <- get.fgm.data.daughters('~/Data/EDHS/2005/egir51fl.dta', '~/Data/EDHS/2005/EGBR51FL.dta')
#fgm.data.daughters.08 <- get.fgm.data.daughters('~/Data/EDHS/2008/EGIR5AFL.DTA', '~/Data/EDHS/2008/EGBR5AFL.DTA')

