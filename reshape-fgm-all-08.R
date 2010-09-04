library(foreign)
library(sp)

ir <- read.dta('/home/karim/Data/EDHS/2008/EGIR5AFL.DTA', convert.underscore = TRUE)

fgm.data.all.08 <- subset(ir, select = c(v000, v001, v002, v003, v004, v005, v010, v012, v013, v023, 
                                         v024, v025, v034, v104, v106, v133, v130, v150, v151, v155, 
                                         v190, v218, v467b, v467c, v467d, v467e, v501, v502, v503, v508, 
                                         v511, v704, v705, v714, v716, v717, v719, v721, v729, v730, 
                                         g102, g106, g107, g108, g115, g116, g118, g119, sgovern, s103g, 
                                         s805, s806, s912))

rm(ir)

names(fgm.data.all.08) <- 
  c('phase', 'cluster', 'hh', 'respond.linenum', 'area.unit', 'wt', 'birth.year', 'age', 'age.grp', 'domain', 
    'region', 'urban.rural', 'husband.linenum', 'years.lived.res', 'educ.lvl', 'edu.year', 'religion', 'hh.head.rel', 'hh.head.sex', 'literacy',
    'wealth.index', 'num.children', 'med.help.permission', 'med.help.money', 'med.help.distance', 'med.help.transportation', 'marital.status', 'marital.currformer', 'marital.num', 'marital.year', 
    'marital.age', 'partner.occupation.1', 'partner.occupation.2', 'working', 'occupation.1', 'occupation.2', 'work.for.whom', 'work.home', 'partner.educlvl', 'partner.age',
    'circum', 'circum.age', 'circum.bywhom', 'circum.daughters.num', 'circum.anydaughter.not', 'circum.intends', 'circum.byreligion', 'circum.continue', 'governorate', 'prev.governorate', 
    'husband.related', 'husband.relation', 'circum.daughternot.num')

fgm.data.all.08 <- transform(fgm.data.all.08, circum.fac = factor(circum, levels = c(0, 1), labels = c("no", "yes")),
                                              dhs.year = 2008)

fgm.data.all.08 <- transform(fgm.data.all.08, hh.id = factor(paste(dhs.year, cluster, hh, sep = '-')))
                                              #unique.cluster = factor(paste(dhs.year, cluster, sep = '-')))

#pr <- read.dta('/home/karim/Data/EDHS/2008/EGPR5AFL.DTA', convert.underscore = TRUE)

#fgm.data.all.08 <- merge(fgm.data.all.08, pr, by.x = c('cluster', 'hh', 'respond.linenum'), by.y = c('hv001', 'hv002', 'hvidx'), all.x = TRUE)
#rm(pr)

#names(fgm.data.all.08)[names(fgm.data.all.08) == 'hc61'] <- 'mother.educlvl'
#names(fgm.data.all.08)[names(fgm.data.all.08) == 'hc62'] <- 'mother.educyr'

#fgm.data.all.08 <- transform(fgm.data.all.08, mother.educlvl = factor(mother.educlvl, levels = c(0:3, 9), labels = c("no education", "primary", "secondary", "higher"), exclude = 9))

#fgm.data.all.08 <- subset(fgm.data.all.08, select = c(cluster:hh.id, mother.educlvl)) 
fgm.data.all.08 <- fgm.data.all.08[ order(fgm.data.all.08$hh.id, fgm.data.all.08$birth.year), ]

occupation.2.levels <- c(0:9) #, 98, 99)
occupation.2.labels <- c('no work', 'prof., tech., manag.', 'clerical', 'sales', 'agri-self employed', 'agri-employee', 'hh & domestic', 
                         'services', 'skilled manual', 'unskilled manual') #, "don't know", 'missing')

fgm.data.all.08 <- within(fgm.data.all.08, {
  birth.year.fac <- factor(birth.year)
  circum.bywhom <- factor(circum.bywhom, labels = c("doctor", "trained nurse/midwife", "daya", "barber", "ghagaria", "other (unspecified)", "don't know"), exclude = c(NA, 99))
  religion <- factor(religion, levels = 1:2, labels = c("muslim", "christian")) #, "missing"))
  marital.num <- ifelse(marital.num == 9, NA, marital.num)
  literacy <- factor(literacy, levels = c(0:2), labels = c("cannot read", "reads with difficulty", "reads easily"), exclude = NA)
  med.help.permission <- factor(med.help.permission, levels = 0:2, labels = c("no problem", "big problem", "small problem"), exclude = NA)
  med.help.money <- factor(med.help.money, levels = 0:2, labels = c("no problem", "big problem", "small problem"), exclude = NA)
  med.help.distance <- factor(med.help.distance, levels = 0:2, labels = c("no problem", "big problem", "small problem"), exclude = NA)
  med.help.transportation <- factor(med.help.transportation, levels = 0:2, labels = c("no problem", "big problem", "small problem"), exclude = NA)

  #partner.occupation.2 <- factor(partner.occupation.2, levels = c(0:9, 98, 99), 
  #                               labels = c("did not work", "prof., tech., manag.", "clerical", "sales", "agric-self employed",
  #                                          "agric-employee", "household & domestic", "services", "skilled manual", 
  #                                          "unskilled manual", "don't know", "missing"))

  partner.occupation.2.fac <- factor(partner.occupation.2, levels = occupation.2.levels, labels = occupation.2.labels)

  working <- factor(working, levels = 0:1, labels = c("no", "yes"), exclude = NA)


  #occupation.2 <- factor(occupation.2, levels = c(0:9, 98, 99), 
  #                       labels = c("did not work", "prof., tech., manag.", "clerical", "sales", "agric-self employed",
  #                                  "agric-employee", "household & domestic", "services", "skilled manual", 
  #                                  "unskilled manual", "don't know", "missing"))
                                    
  occupation.2.fac <- factor(occupation.2, levels = occupation.2.levels, labels = occupation.2.labels)

  work.for.whom <- factor(work.for.whom, levels = c(1:3, 9), labels = c("for family member", "for someone else", "self-employed", "missing"))
  work.home <- factor(work.home, levels = 1:2, labels = c("at home", "away"), exclude = NA)
  partner.educlvl.fac <- factor(partner.educlvl, 
                                levels = c(0:5), #, 8, 9), 
                                labels = c('no educ', 'incomplete primary', 'complete primary', 'incomplete secondary', 'complete secondary', 'higher')) #, "don't know", 'missing'))
  circum.intends <- factor(circum.intends, levels = c(0:1, 8, 9), labels = c("no", "yes", "don't know", "missing"), exclude = NA)
  circum.byreligion <- factor(circum.byreligion, levels = c(0:1, 8, 9), labels = c("no", "yes", "unsure", "missing"), exclude = NA)
  circum.continue <- factor(circum.continue, levels = c(1:3, 8, 9), labels = c("continued", "discontinued", "depends", "dk", "missing"), exclude = NA)
  circum.age <- ifelse(circum.age >= 98, NA, circum.age)
  husband.related <- factor(husband.related, levels = 0:1, labels = c("no", "yes"), exclude = NA)
})

fgm.data.all.08 <- transform(fgm.data.all.08, 
                             med.circum = ifelse((circum == 1) & (circum.bywhom %in% c("doctor", "trained nurse/midwife")), 1, 0),
                             circum.year = ifelse(circum == "yes", birth.year + circum.age, NA))
                                        #circum.year = factor(ifelse(circum == 1, birth.year + circum.age, NA))) 

gps.08 <- read.dbf("~/Data/EDHS/2008/EGGE5BFL.DBF")
