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

fgm.data.daughters.08 <- subset(ir, select = c(v000, v001, v002, v003, v004, v005, v023, v024, v025, v104,
                                               v130, v151, v190, g116, v106, v155, v467b, v467c, v467d, v467e,
                                               v511, v704, v705, v714, v716, v717, v719, v721, v729, v730,
                                               g102, g106, g107, g118, g119, sgovern, s103g,
                                               v3a08j, v3a08q,
#                                               m2a, m2b, m2g, m2k, m2n,
#                                               m3a, m3b, m3g, m3k, m3n,
#                                               m14, m15, m43, m44, m57a, m57b, m57e, m57f, m57g,
#                                               m57i, m57j, m57k, m57l, m57m, m57n, m57o, m57p, m57q, m57r,
#                                               m57x,
#                                               m70, m72, m73,
                                               sdcol.1:sdcol.7, s906.1:s906.7, s908.1:s908.7, s909.1:s909.7, s910.1:s910.7, s911.1:s911.7))

                                               # To consider: "reason did not deliver at health facility"

rm(ir)

col.names <- c(
               'phase', 'cluster', 'hh', 'respond', 'area.unit', 'wt', 'domain', 'region', 'urban.rural', 'years.lived.res', 
               'religion', 'hh.head.sex', 'wealth.index', 'intends.circum', 'educ.lvl', 'literacy', 'med.help.permission', 'med.help.money', 'med.help.distance', 'med.help.transportation',
               'marital.age', 'partner.occupation.1', 'partner.occupation.2', 'working', 'occupation.1', 'occupation.2', 'work.for.whom', 'work.home', 'partner.educlvl', 'partner.age',
               'mother.circum', 'mother.circum.age', 'mother.circum.bywhom', 'circum.byreligion', 'circum.continue', 'governate', 'prev.governate',
               'nofpuse.husbandopposed', 'nofpuse.noaccess.toofar' 
#               'prenatal.doctor', 'prenatal.nurse', 'prenatal.daya', 'prenatal.other', 'prenatal.noone',
#               'assist.doctor', 'assist.nurse', 'assist.daya', 'assist.other', 'assist.noone', 
#               'antenatal.visits', 'place.delivery', 'preg.told.complicate', 'preg.told.where.complicate', 'antenatal.care.home', 'antenatal.care.otherhome', 'antenatal.care.govthosp.urban', 'antenatal.care.govthu.urban', 'antenatal.care.govthealthoff', 
#               'antenatal.care.govthosp.rural', 'antenatal.care.govthu.rural', 'antenatal.care.mch', 'antenatal.care.govt.other', 'antenatal.care.privhosp', 'antenatal.care.privdoc', 'antenatal.care.fpassoc', 'antenatal.care.csi', 'antenatal.care.ngo', 'antenatal.care.priv.other', 
#               'antenatal.care.other',
#               'postnatal.check.2mon', 'postnatal.check.who', 'postnatal.check.where'
               )

num.col <- length(col.names)

names(fgm.data.daughters.08)[1:num.col] <- col.names

fgm.data.daughters.08 <- reshape(fgm.data.daughters.08, 
                                 varying = list(names(fgm.data.daughters.08)[(num.col + 1):(num.col + 7)], 
                                                names(fgm.data.daughters.08)[(num.col + 8):(num.col + 14)], 
                                                names(fgm.data.daughters.08)[(num.col + 15):(num.col + 21)], 
                                                names(fgm.data.daughters.08)[(num.col + 22):(num.col + 28)], 
                                                names(fgm.data.daughters.08)[(num.col + 29):(num.col + 35)], 
                                                names(fgm.data.daughters.08)[(num.col + 36):(num.col + 42)]), 
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

names(fgm.data.daughters.08)[names(fgm.data.daughters.08) == 'v437'] <- 'weight'
names(fgm.data.daughters.08)[names(fgm.data.daughters.08) == 'v438'] <- 'height'

#pr <- read.dta('~/Data/EDHS/2008/EGPR5AFL.DTA', convert.underscore = TRUE)

#fgm.data.daughters.08 <- merge(fgm.data.daughters.08, pr, by.x = c('cluster', 'hh', 'respond'), by.y = c('hv001', 'hv002', 'hv003'))
#rm(pr)

#names(fgm.data.daughters.08)[names(fgm.data.daughters.08) == 'hc61'] <- 'mother.educlvl'
#names(fgm.data.daughters.08)[names(fgm.data.daughters.08) == 'hc62'] <- 'mother.educyr'

#fgm.data.daughters.08 <- transform(fgm.data.daughters.08, mother.edulvl = factor(mother.educlvl, levels = c(0:3, 9), labels = c("no education", "primary", "secondary", "higher"), exclude = 9))

fgm.data.daughters.08$birth.year <- fgm.data.daughters.08$b2

fgm.data.daughters.08 <- subset(fgm.data.daughters.08, select = c(cluster:hh.id, birth.year,  weight, height, sdno)) 
#fgm.data.daughters.08 <- subset(fgm.data.daughters.08, select = c(-phase)) #excluding phase for now

fgm.data.daughters.08 <- fgm.data.daughters.08[ order(fgm.data.daughters.08$hh.id, fgm.data.daughters.08$birth.year), ]

fgm.data.daughters.08 <- do.call(rbind, by(fgm.data.daughters.08, fgm.data.daughters.08$hh.id, cleanup.by.hh))

# To check if there are any duplicated birth.year's in the same hh
# by(fgm.data.daughters.08, fgm.data.daughters.08[c('hh.id')], function(df) anyDuplicated(df$birth.year))

# To see how many hhs per cluster
# by(fgm.data.daughters.08, fgm.data.daughters.08[c('cluster')], function(df) length(unique(df$hh)))

occupation.2.levels <- c(0:9) #, 98, 99)
occupation.2.labels <- c('no work', 'prof., tech., manag.', 'clerical', 'sales', 'agri-self employed', 'agri-employee', 'hh & domestic', 
                         'services', 'skilled manual', 'unskilled manual') #, "don't know", 'missing')

med.help.levels <- 0:2 #,9
med.help.labels <- c('no problem', 'big problem', 'not big problem') #, 'missing')

fgm.data.daughters.08 <- within(fgm.data.daughters.08, 
{
  age <- dhs.year - birth.year
  has.or.intends.circum <- ifelse(circum == 1 | ((age <= 12) & (intends.circum == 1)), 1, 0)
  med.circum <- ifelse(circum == 1 & (who.circum %in% c(1, 2)), 1, 0)
  year.circum <- factor(ifelse(circum == 1 & age.circum <= 19, birth.year + age.circum, NA), ordered = TRUE)

#  timeperiod.circum <- factor(ifelse(year.circum > "2007", 0,
#                                                           ifelse(year.circum >= "1997", 1, 
#                                                                                         ifelse(year.circum < "1997", 2, NA))),
#                                     labels = c("After 2007", "1997-2007", "Before 1997"))

  order.fac <- factor(order)
  married <- ifelse(mar.status == 1, 1, 0)
  religion <- factor(religion, levels = 1:2, labels = c("muslim", "christian")) #, "missing"))
  birth.year.fac <- factor(birth.year)
  literacy.fac <- factor(literacy, 
                         levels = c(0:2), #, 9), 
                         labels = c('cannot read', 'reads with difficulty', 'reads easily')) #, 'missing'))
  med.help.permission.fac <- factor(med.help.permission, 
                                    levels = med.help.levels,
                                    labels = med.help.labels)
  med.help.distance.fac <- factor(med.help.distance, 
                                  levels = med.help.levels, 
                                  labels = med.help.labels)
  med.help.transportation.fac <- factor(med.help.transportation, 
                                        levels = med.help.levels,
                                        labels = med.help.labels)
  partner.occupation.1.fac <- factor(partner.occupation.1)
  partner.occupation.2.fac <- factor(partner.occupation.2, levels = occupation.2.levels, labels = occupation.2.labels)
  occupation.1.fac <- factor(occupation.1)
  occupation.2.fac <- factor(occupation.2, levels = occupation.2.levels, labels = occupation.2.labels)
  partner.educlvl.fac <- factor(partner.educlvl, 
                                levels = c(0:3), #, 8, 9), 
                                labels = c('no educ', 'primary', 'secondary', 'higher')) #, "don't know", 'missing'))
  mother.circum.fac <- factor(mother.circum, 
                              levels = c(0:1), #, 9), 
                              labels = c('no', 'yes')) #, 'missing'))
})

# GPS Data
############

gps.08 <- read.dbf("~/Data/EDHS/2008/EGGE5BFL.DBF")

cluster.coordinates(fgm.data.daughters.08) <- gps.08

# This will result in the loss of about 268 observations with not GPS info
#fgm.data.daughters.08 <- merge(fgm.data.daughters.08, gps, by.x = c('cluster'), by.y = c('DHSCLUST'))

rm(gps.08)

