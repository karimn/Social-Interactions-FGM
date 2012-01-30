PeerClusterLinks <- setRefClass("PeerClusterLinks",
                                contains = "SpatialData",
                                fields = list(linked.data = "BaseFgmData"))

PeerClusterLinks$methods(initialize = function(linked.data, outer.radius, inner.radius = 0, cohort.range = 0, range.type = c("both", "older")) {
    browser()

    peer.links <- do.call(rbind, linked.data$apply(c("cluster", "birth.year"), function(all.data, grp.ids, grp.index.col.values) {
        current.coords <- all.data$coords[grp.ids[1],]
        return(data.frame(cluster = grp.index.col.values["cluster"], birth.year = grp.index.col.values["birth.year"], long = current.coords[1], lat = current.coords[2], count = length(grp.ids)))
    }))
    
    peer.links$id <- seq_along(peer.links$cluster)

    callSuper(data = peer.links, coordinate.names = c("long", "lat"), linked.data = linked.data)

    upper.offset <- switch(match.arg(range.type), both = cohort.range, older = 0)

    updated.list <- apply("cluster", function(all.data, grp.ids, grp.index.col.values) {
        stopifnot(length(unique(all.data$coords[grp.ids, 1])) == 1)
        stopifnot(length(unique(all.data$coords[grp.ids, 2])) == 1)
        dists <- spDistsN1(spatial.data, all.data$spatial.data[grp.ids[1],])

        return(do.call(rbind, lapply(grp.ids, function(grp.id) {
            current.birth.year <- all.data$spatial.data$birth.year[grp.id]
            neighbor.row.ids <- (dists <= outer.radius) & (dists >= inner.radius) & 
                                (all.data$spatial.data$birth.year <= current.birth.year + upper.offset) & (all.data$spatial.data$birth.year >= current.birth.year - cohort.range)
            return(cbind(all.data$spatial.data[grp.id,], neighbor = all.data$spatial.data$id[neighbor.row.ids], dist = dists[neighbor.row.ids]))
        })))
    })

    spatial.data <<- do.call(rbind, updated.list)
})

PeerClusterLinks$lock(c("linked.data"))
