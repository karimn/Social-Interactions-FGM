library(foreign)
library(sp)

ir <- read.dta('/home/karim/Data/EDHS/2005/egir51fl.dta', convert.underscore = TRUE)

fgm.data.all.05 <- subset(ir, select = c(v000, v001, v002, v003, v004, v005, v010, v012, v013, v023, 
                                         v024, v025, v034, v104, v106, v133, v130, v150, v151, v155, 
                                         v190, v218, v467b, v467c, v467d, v467e, v501, v502, v503, v508, 
                                         v511, v704, v705, v714, v716, v717, v719, v721, v729, v730, 
                                         s801, s802, s811, s815, s816, sgovern, s103g #s805, #s806, s912
))

rm(ir)

names(fgm.data.all.05) <- c('phase', 'cluster', 'hh', 'respond.linenum', 'area.unit', 'wt', 'birth.year', 'age', 'age.grp', 'domain', 
                            'region', 'urban.rural', 'husband.linenum', 'years.lived.res', 'educ.lvl', 'edu.year', 'religion', 'hh.head.rel', 'hh.head.sex', 'literacy', 
                            'wealth.index', 'num.children', 'med.help.permission', 'med.help.money', 'med.help.distance', 'med.help.transportation', 'marital.status', 'marital.currformer', 'marital.num', 'marital.year', 
                            'marital.age', 'partner.occupation.1', 'partner.occupation.2', 'working', 'occupation.1', 'occupation.2', 'work.for.whom', 'work.home', 'partner.educlvl', 'partner.age', 
                            'circum', 'circum.age', 'circum.intends', 'circum.byreligion', 'circum.continue', 'governate', 'prev.governate' 
                            #'husband.related', 'husband.relation', 'circum.daughternot.num'
)

fgm.data.all.05 <- transform(fgm.data.all.05, circum.bywhom = NA, 
                                        husband.related = NA, 
                                        husband.relation = NA, 
                                        circum.daughternot.num = NA,
                                        circum.daughters.num = NA,
                                        circum.anydaughter.not = NA,
                                        dhs.year = 2005)

fgm.data.all.05 <- transform(fgm.data.all.05, hh.id = factor(paste(dhs.year, cluster, hh, sep = '-')))

#pr <- read.dta('/home/karim/Data/EDHS/2008/EGPR5AFL.DTA', convert.underscore = TRUE)

#fgm.data.all.05 <- merge(fgm.data.all.05, pr, by.x = c('cluster', 'hh', 'respond.linenum'), by.y = c('hv001', 'hv002', 'hvidx'), all.x = TRUE)
#rm(pr)

#names(fgm.data.all.05)[names(fgm.data.all.05) == 'hc61'] <- 'mother.educlvl'
#names(fgm.data.all.05)[names(fgm.data.all.05) == 'hc62'] <- 'mother.educyr'

#fgm.data.all.05 <- transform(fgm.data.all.05, mother.educlvl = factor(mother.educlvl, levels = c(0:3, 9), labels = c("no education", "primary", "secondary", "higher"), exclude = 9))

#fgm.data.all.05 <- subset(fgm.data.all.05, select = c(cluster:hh.id, mother.educlvl)) 

fgm.data.all.05 <- fgm.data.all.05[ order(fgm.data.all.05$hh.id, fgm.data.all.05$birth.year), ]

fgm.data.all.05 <- within(fgm.data.all.05, {

  hh.head.rel <- ifelse(hh.head.rel == "dk", NA, hh.head.rel)
  levels(hh.head.rel) <- c("head", "wife or husband", "son/daughter", "son/daughter-in-law", "grandchild", "parent", "parent-in-law", "brother/sister",
                           "co-spouse", "other relative", "adopted/foster child", "not related", "niece/nehew by blood", "niece/nephew by marriage")
  hh.head.rel <- addNA(hh.head.rel)

  #circum.bywhom <- factor(circum.bywhom, labels = c("doctor", "trained nurse/midwife", "daya", "barber", "ghagaria", "other (unspecified)", "don't know"), exclude = c(NA, 99))
  religion <- factor(religion, labels = c("muslim", "christian"))
  marital.num <- ifelse(marital.num == 9, NA, marital.num)
  literacy <- factor(literacy, levels = c(0:2), labels = c("cannot read", "reads with difficulty", "reads easily"), exclude = NA)
  circum.age <- ifelse(circum.age >= 98, NA, circum.age)
})

fgm.data.all.05 <- transform(fgm.data.all.05, 
                                        med.circum = NA,
                                        circum.year = ifelse(circum == 'yes', birth.year + circum.age, NA))

gps.05 <- read.dbf("~/Data/EDHS/2005/EGGE51FL.dbf")
