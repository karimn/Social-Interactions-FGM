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

ir <- read.dta('~/Data/EDHS/2008/EGIR5AFL.DTA', convert.underscore = TRUE)

#setClass("fgmDaughterDataFrame", contains = "data.frame")
#setMethod("initialize", "fgmDaughterDataFrame",
#  function(.Object, ir.file, br.file, ...) {
#get.fgm.data <- function(ir.file, br.file) {
#ir <- read.dta(ir.file, convert.underscore = TRUE)

#if (!('g116' %in% names(ir))) {
#  ir$g116 = NA
#}

fgm.data.daughters.08 <- subset(ir, select = c(v000, v001, v002, v003, v004, v005, v023, v024, v025, v104,
                                               v130, v151, v190, g116, 
                                               sdcol.1:sdcol.7, s906.1:s906.7, s908.1:s908.7, s909.1:s909.7, s910.1:s910.7, s911.1:s911.7))

rm(ir)

names(fgm.data.daughters.08)[1:14] <- c('phase', 'cluster', 'hh', 'respond', 'area.unit', 'wt', 'domain', 'region', 'urban.rural', 'years.lived.res', 
                                        'religion', 'hh.head.sex', 'wealth.index', 'intends.circum')

fgm.data.daughters.08 <- reshape(fgm.data.daughters.08, 
                                 varying = list(names(fgm.data.daughters.08)[15:21], 
                                                names(fgm.data.daughters.08)[22:28], 
                                                names(fgm.data.daughters.08)[29:35], 
                                                names(fgm.data.daughters.08)[36:42], 
                                                names(fgm.data.daughters.08)[43:49], 
                                                names(fgm.data.daughters.08)[50:56]), 
                                 direction = 'long', 
                                 sep = '.', 
                                 v.names = c('sdcol', 'line.num', 'mar.status', 'circum', 'who.circum', 'age.circum'), 
                                 timevar = 'order')

fgm.data.daughters.08 <- subset(fgm.data.daughters.08, !is.na(sdcol), select = c(-sdcol))

fgm.data.daughters.08 <- transform(fgm.data.daughters.08, 
                                   circum = as.numeric(circum), 
                                   mar.status = as.numeric(mar.status), 
                                   who.circum = as.numeric(who.circum),
                                   dhs.year = 2008)

fgm.data.daughters.08 <- within(fgm.data.daughters.08, 
{
  circum.yesno <- ifelse(circum == 1, 1, 0)
  hh.id <- factor(paste(cluster, hh, sep = '-'))
})

br <- read.dta('~/Data/EDHS/2008/EGBR5AFL.DTA', convert.underscore = TRUE)

fgm.data.daughters.08 <- merge(fgm.data.daughters.08, br, by.x = c('cluster', 'hh', 'respond', 'line.num'), by.y = c('v001', 'v002', 'v003', 'bidx'))
rm(br)

fgm.data.daughters.08$birth.year <- factor(fgm.data.daughters.08$b2)
names(fgm.data.daughters.08)[names(fgm.data.daughters.08) == 'v437'] <- 'weight'
names(fgm.data.daughters.08)[names(fgm.data.daughters.08) == 'v438'] <- 'height'

#pr <- read.dta('~/Data/EDHS/2008/EGPR5AFL.DTA', convert.underscore = TRUE)

#fgm.data.daughters.08 <- merge(fgm.data.daughters.08, pr, by.x = c('cluster', 'hh', 'respond'), by.y = c('hv001', 'hv002', 'hv003'))
#rm(pr)

#names(fgm.data.daughters.08)[names(fgm.data.daughters.08) == 'hc61'] <- 'mother.educlvl'
#names(fgm.data.daughters.08)[names(fgm.data.daughters.08) == 'hc62'] <- 'mother.educyr'

#fgm.data.daughters.08 <- transform(fgm.data.daughters.08, mother.edulvl = factor(mother.educlvl, levels = c(0:3, 9), labels = c("no education", "primary", "secondary", "higher"), exclude = 9))

fgm.data.daughters.08 <- subset(fgm.data.daughters.08, select = c(cluster:hh.id, birth.year, weight, height, sdno)) 
#fgm.data.daughters.08 <- subset(fgm.data.daughters.08, select = c(-phase)) #excluding phase for now

fgm.data.daughters.08 <- fgm.data.daughters.08[ order(fgm.data.daughters.08$hh.id, fgm.data.daughters.08$birth.year), ]

fgm.data.daughters.08 <- do.call(rbind, by(fgm.data.daughters.08, fgm.data.daughters.08$hh.id, cleanup.by.hh))

# To check if there are any duplicated birth.year's in the same hh
# by(fgm.data.daughters.08, fgm.data.daughters.08[c('hh.id')], function(df) anyDuplicated(df$birth.year))

# To see how many hhs per cluster
# by(fgm.data.daughters.08, fgm.data.daughters.08[c('cluster')], function(df) length(unique(df$hh)))

fgm.data.daughters.08 <- within(fgm.data.daughters.08, 
{
  has.or.intends.circum <- ifelse(circum == 1 | intends.circum == 1, 1, 0)
  med.circum <- ifelse(circum == 1 & (who.circum %in% c(1, 2)), 1, 0)
  year.circum <- factor(ifelse(circum == 1 & age.circum <= 19, as.numeric(levels(birth.year)[birth.year]) + age.circum, NA), ordered = TRUE)

  timeperiod.circum <- factor(ifelse(year.circum > "2007", 0,
                                                           ifelse(year.circum >= "1997", 1, 
                                                                                         ifelse(year.circum < "1997", 2, NA))),
                                     labels = c("After 2007", "1997-2007", "Before 1997"))

  order.fac <- factor(as.numeric(levels(order)[order]), levels = 1:7)
  married <- ifelse(mar.status == 1, 1, 0)

  religion <- factor(religion, labels = c("Muslim", "Christian", "Missing"))
})

#return(fgm.data.daughters.08)
#callNextMethod(.Object, ir.file = ir.file, br.file = br.file, ...)
#}

#ir.08 <- read.dta('/home/karim/Data/EDHS/2008/EGIR5AFL.DTA', convert.underscore = TRUE)
#br.08 <- read.dta('/home/karim/Data/EDHS/2008/EGBR5AFL.DTA', convert.underscore = TRUE)

#fgm.data.daughters.08.05 <- get.fgm.data.daughters.08('~/Data/EDHS/2005/egir51fl.dta', '~/Data/EDHS/2005/EGBR51FL.dta')
#fgm.data.daughters.08 <- get.fgm.data.daughters.08('~/Data/EDHS/2008/EGIR5AFL.DTA', '~/Data/EDHS/2008/EGBR5AFL.DTA')

# GPS Data
############

gps.08 <- read.dbf("~/Data/EDHS/2008/EGGE5BFL.DBF")

cluster.coordinates(fgm.data.daughters.08) <- gps.08

# This will result in the loss of about 268 observations with not GPS info
#fgm.data.daughters.08 <- merge(fgm.data.daughters.08, gps, by.x = c('cluster'), by.y = c('DHSCLUST'))

rm(gps.08)

