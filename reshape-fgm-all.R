library(foreign)

ir <- read.dta('/home/karim/Data/EDHS/2008/EGIR5AFL.DTA', convert.underscore = TRUE)

fgm.data.all <- subset(ir, select = c(v000, v001, v002, v003, v004, v005, v010, v012, v013, v023, v024, v025, v034, v104, v106, v133, v130, v150, v151, v155, v190, v218, v467b, v467c, v467d, v467e, v501, v502, v503, v508, v511, v704, v705, v715, v716, v717, v719, v721, v729, v730, g102, g106, g107, g108, g115, g116, g118, g119, sgovern, s103g, s805, s806, s912))

rm(ir)

names(fgm.data.all) <- c('phase', 'cluster', 'hh', 'respond.linenum', 'area.unit', 'wt', 'birth.year', 'age', 'age.grp', 'domain', 'region', 'urban.rural', 'husband.linenum', 'years.lived.res', 'educ.lvl', 'edu.year', 'religion', 'hh.head.rel', 'hh.head.sex', 'literacy', 'wealth.index', 'num.children', 'med.help.permission', 'med.help.money', 'med.help.distance', 'med.help.transportation', 'marital.status', 'marital.currformer', 'marital.num', 'marital.year', 'marital.age', 'partner.occupation.1', 'partner.occupation.2', 'working', 'occupation.1', 'occupation.2', 'work.for.whom', 'work.home', 'partner.educlvl', 'partner.age', 'circum', 'circum.age', 'circum.bywhom', 'circum.daughters.num', 'circum.anydaughter.not', 'circum.intends', 'circum.byreligion', 'circum.continue', 'governate', 'prev.governate', 'husband.related', 'husband.relation', 'circum.daughternot.num')

fgm.data.all <- transform(fgm.data.all, circum.yesno = ifelse(circum == 1, 1, 0),
                                        hh.id = factor(paste(cluster, hh, sep = '-')))

#pr <- read.dta('/home/karim/Data/EDHS/2008/EGPR5AFL.DTA', convert.underscore = TRUE)

#fgm.data.all <- merge(fgm.data.all, pr, by.x = c('cluster', 'hh', 'respond.linenum'), by.y = c('hv001', 'hv002', 'hvidx'), all.x = TRUE)
#rm(pr)

#names(fgm.data.all)[names(fgm.data.all) == 'hc61'] <- 'mother.educlvl'
#names(fgm.data.all)[names(fgm.data.all) == 'hc62'] <- 'mother.educyr'

#fgm.data.all <- transform(fgm.data.all, mother.educlvl = factor(mother.educlvl, levels = c(0:3, 9), labels = c("no education", "primary", "secondary", "higher"), exclude = 9))

#fgm.data.all <- subset(fgm.data.all, select = c(cluster:hh.id, mother.educlvl)) 
fgm.data.all <- fgm.data.all[ order(fgm.data.all$hh.id, fgm.data.all$birth.year), ]

fgm.data.all <- within(fgm.data.all, {
  circum.bywhom <- factor(circum.bywhom, labels = c("doctor", "trained nurse/midwife", "daya", "barber", "ghagaria", "other (unspecified)", "don't know"), exclude = c(NA, 99))
  religion <- factor(religion, labels = c("muslim", "christian", "missing"))
  marital.num <- ifelse(marital.num == 9, NA, marital.num)
  birth.year <- factor(birth.year) 
  literacy <- factor(literacy, levels = c(0:2), labels = c("cannot read", "reads with difficulty", "reads easily"), exclude = NA)
  med.help.permission <- factor(med.help.permission, levels = 0:2, labels = c("no problem", "big problem", "not big problem"), exclude = NA)
  med.help.money <- factor(med.help.money, levels = 0:2, labels = c("no problem", "big problem", "not big problem"), exclude = NA)
  med.help.distance <- factor(med.help.distance, levels = 0:2, labels = c("no problem", "big problem", "not big problem"), exclude = NA)
  med.help.transportation <- factor(med.help.transportation, levels = 0:2, labels = c("no problem", "big problem", "not big problem"), exclude = NA)
  marital.year <- factor(marital.year)
  working <- factor(working, levels = 0:1, labels = c("no", "yes"), exclude = NA)
  work.home <- factor(work.home, levels = 1:2, labels = c("at home", "away"), exclude = NA)
  partner.educlvl <- factor(partner.educlvl, levels = c(0:5), labels = c("no educ", "incomplete primary", "complete primary", "incomplete secondary", "complete secondary", "higher"), exclude = NA)
  circum.intends <- factor(circum.intends, levels = 0:1, labels = c("no", "yes"), exclude = NA)
  circum.byreligion <- factor(circum.byreligion, levels = 0:1, labels = c("no", "yes"), exclude = NA)
  circum.continue <- factor(circum.continue, levels = 1:3, labels = c("continue", "discontinue", "depends"), exclude = NA)
  circum.age <- ifelse(circum.age >= 98, NA, circum.age)
  husband.related <- factor(husband.related, levels = 0:1, labels = c("no", "yes"), exclude = NA)
  #husband.relation <- factor(husband.relation, levels = 1:7, labels = c("
})

fgm.data.all <- transform(fgm.data.all, med.circum = ifelse((circum == 1) & (circum.bywhom %in% c("doctor", "trained nurse/midwife")), 1, 0),
                                        circum.year = factor(ifelse(circum == 1, as.numeric(levels(birth.year)[birth.year]) + circum.age, NA)))
                                        #circum.year = factor(ifelse(circum == 1, birth.year + circum.age, NA))) 


#fgm.data.all <- transform(fgm.data.all, circum.timeperiod = factor(ifelse(circum.year > "2007", 0,
#                                                                     ifelse(circum.year >= "1997", 1, 
#                                                                       ifelse(circum.year < "1997", 2, NA))), labels = c("After 2007", "1997-2007", "Before 1997")),

 #fgm.data.all <- transform(fgm.data.all, religion = factor(religion, labels = c("Muslim", "Christian", "Missing")))

