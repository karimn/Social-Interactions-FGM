fgm.data.all <- rbind(fgm.data.all.05, fgm.data.all.08)
gps <- rbind(gps.05, gps.08)
gps$unique.cluster <- as.numeric(row.names(gps))

fgm.data.all <- within(fgm.data.all,
{
  birth.year <- factor(birth.year)
  marital.year <- factor(marital.year)
})

fgm.data.all <- subset(fgm.data.all, select = c(-domain, -literacy, -partner.occupation.1, -prev.governate))

cluster.coordinates(fgm.data.all) <- gps
