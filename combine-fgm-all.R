fgm.data.all <- rbind(fgm.data.all.05, fgm.data.all.08)
gps <- rbind(gps.05, gps.08)
gps$unique.cluster <- as.numeric(row.names(gps))

cluster.coordinates(fgm.data.all) <- gps
