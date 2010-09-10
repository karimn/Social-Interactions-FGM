library("lattice")

histogram(~ age.circum | birth.year.fac, data = fgm, subset = age.circum <= 18)

densityplot(~ age.circum | birth.year.fac, data = fgm, subset = age.circum <= 18, ref = T, plot.points = F)

densityplot(~ age.circum, data = fgm, subset = age.circum <= 18, ref = T, plot.points = F, groups = birth.year.fac, auto.key = list(columsn = 3))
