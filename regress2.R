source("BaseFgmDataRef.R")
source("DaughterFgmDataRef.R")

x <- DaughterFgmData$new(spdf = spdf, cluster.info = clust.info, youngest.cohort = 2000, individual.controls = "religion")
x$generate.reg.means(exclude.self = TRUE)
x$generate.reg.means(regs = "has.or.intends.circum", exclude.self = TRUE)
x$rm.by.res.years(20)
x$rm.by.grp.size(24)

r <- x$regress("has.or.intends.circum", TRUE)

r.panel <- x$regress.panel("has.or.intends.circum")
