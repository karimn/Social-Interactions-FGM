library(sp)
library(spatstat)
library(maptools)
library(sem)
library(AER)
library(plm)

make.row.stochastic <- function(adj.matrix) {
    t(apply(adj.matrix, 1, function(row) {
          row.sum <- sum(row)
          if (row.sum > 0) row[row == 1] <- 1/row.sum
          return(row)
    }))
}

test.adj.matrix <- function(adj.matrix) {
    isSymmetric(adj.matrix)
}

clean.lvl.name <- function(lvl.name) {
    gsub(",", '', gsub("[\\s-&\\.]+", "_", lvl.name, perl = TRUE))
}

FgmData.cols = quote(c(v000, v001, v002, v003, v004, v005, v023, v024, v025, v104,
                                               v130, v151, v190, g116, v106, v155, v467b, v467c, v467d, v467e,
                                               v511, v704, v705, v714, v716, v717, v719, v721, v729, v730,
                                               g102, g106, g107, g118, g119, sgovern, s103g, s912, s915, s916,
                                               v394,
                                               s917a:s917x,
                                               v3a08j, v3a08q))
#                                               m2a, m2b, m2g, m2k, m2n,
#                                               m3a, m3b, m3g, m3k, m3n,
#                                               m14, m15, m43, m44, m57a, m57b, m57e, m57f, m57g,
#                                               m57i, m57j, m57k, m57l, m57m, m57n, m57o, m57p, m57q, m57r,
#                                               m57x,
#                                               m70, m72, m73,
#sdcol.1:sdcol.7, s906.1:s906.7, s908.1:s908.7, s909.1:s909.7, s910.1:s910.7, s911.1:s911.7))

                                                           # To consider: "reason did not deliver at health facility"
FgmData.circum.info <- c('circum.info.tv', 'circum.info.radio', 'circum.info.papers', 'circum.info.pamph', 'circum.info.poster', 'circum.info.comm', 'circum.info.hw.home', 'circum.info.hw.facil', 'circum.info.husb', 'circum.info.relfr', 'circum.info.other')

FgmData.col.names <- c(
               'phase', 'cluster', 'hh', 'respond', 'area.unit', 'wt', 'domain', 'region', 'urban.rural', 'years.lived.res', 
               'religion', 'hh.head.sex', 'wealth.index', 'intends.circum', 'educ.lvl', 'literacy', 'med.help.permission', 'med.help.money', 'med.help.distance', 'med.help.transportation',
               'marital.age', 'partner.occupation.1', 'partner.occupation.2', 'working', 'occupation.1', 'occupation.2', 'work.for.whom', 'work.home', 'partner.educlvl', 'partner.age',
               'mother.circum', 'mother.circum.age', 'mother.circum.bywhom', 'circum.byreligion', 'circum.continue', 'governorate', 'prev.governate', 'num.not.circum', 'discuss.circum', 'received.info.circum',
               'visit.health.facil.12mon',
               FgmData.circum.info,
               'nofpuse.husbandopposed', 'nofpuse.noaccess.toofar' 
#               'prenatal.doctor', 'prenatal.nurse', 'prenatal.daya', 'prenatal.other', 'prenatal.noone',
#               'assist.doctor', 'assist.nurse', 'assist.daya', 'assist.other', 'assist.noone', 
#               'antenatal.visits', 'place.delivery', 'preg.told.complicate', 'preg.told.where.complicate', 'antenatal.care.home', 'antenatal.care.otherhome', 'antenatal.care.govthosp.urban', 'antenatal.care.govthu.urban', 'antenatal.care.govthealthoff', 
#               'antenatal.care.govthosp.rural', 'antenatal.care.govthu.rural', 'antenatal.care.mch', 'antenatal.care.govt.other', 'antenatal.care.privhosp', 'antenatal.care.privdoc', 'antenatal.care.fpassoc', 'antenatal.care.csi', 'antenatal.care.ngo', 'antenatal.care.priv.other', 
#               'antenatal.care.other',
#               'postnatal.check.2mon', 'postnatal.check.who', 'postnatal.check.where'
               )

FgmData.occupation.2.levels <- c(0:9) #, 98, 99)
FgmData.occupation.2.labels <- c('no work', 'prof., tech., manag.', 'clerical', 'sales', 'agri-self employed', 'agri-employee', 'hh & domestic', 
                         'services', 'skilled manual', 'unskilled manual') #, "don't know", 'missing')

FgmData.med.help.levels <- 1:2 #,9
FgmData.med.help.labels <- c('big problem', 'not big problem') #, 'missing')
FgmData.religions <- c("muslim", "christian")
FgmData.literacy.labels <- c('cannot read', 'reads with difficulty', 'reads easily')
FgmData.partner.educlvl.labels <- c('no educ', 'incomplete primary', 'complete primary', 'incomplete secondary', 'complete secondary', 'higher')

FgmData.grpavg.prefix = "grpavg"
FgmData.lagged.grpavg.prefix = "lagged.grpavg"

BaseFgmData <- setRefClass("BaseFgmData",
  contains = "SpatialData",
  fields = list(
    dist.mat = "matrix",
    cluster.info = "SpatialData"), ##"SpatialPointsDataFrame"), #"data.frame"), 

  methods = list(
    initialize = function(ir.file = NULL, gps.file = NULL, columns = FgmData.cols, new.column.names = FgmData.col.names, dhs.year = 2008, ...)
    {
      if ("data" %in% names(list(...))) {
          callSuper(...) 
      } else if (!is.null(ir.file) & !is.null(gps.file) & all(names(list(...)) != "collection")) {
          fgm.data <- read.dta(ir.file, convert.underscore = TRUE)
          gps <- read.dbf(gps.file)

          fgm.data$dhs.year <- dhs.year

          fgm.data <- base::merge(fgm.data, gps, by.x = c('dhs.year', 'v001'), by.y = c('DHSYEAR', 'DHSCLUST'))

          callSuper(data = fgm.data, columns = columns, new.column.names = new.column.names, coordinate.names = c("LONGNUM", "LATNUM"), proj4string = "+proj=longlat +ellps=WGS84")

          cluster.info <<- SpatialData$new(data = gps, coordinate.names = coordinate.names, proj4string = proj4string)

          rm(fgm.data)
          rm(gps)

          spatial.data@data <<- within(spatial.data@data, 
          {
            cluster.fac <- factor(cluster)
            hh.id <- factor(paste(cluster, hh, sep = '-')) # This is unique on households, but there might be 
                                                           # multiple mothers in the same hh
            religion <- factor(religion, 
                               levels = 1:2, 
                               labels = FgmData.religions) #, "missing"))
            literacy.fac <- factor(literacy, 
                                   levels = c(0:2), #, 9), 
                                   labels = FgmData.literacy.labels) #, 'missing'))
            med.help.permission.fac <- factor(med.help.permission, 
                                              levels = FgmData.med.help.levels,
                                              labels = FgmData.med.help.labels)
            med.help.distance.fac <- factor(med.help.distance, 
                                            levels = FgmData.med.help.levels, 
                                            labels = FgmData.med.help.labels)
            med.help.transportation.fac <- factor(med.help.transportation, 
                                                  levels = FgmData.med.help.levels,
                                                  labels = FgmData.med.help.labels)
            med.help.money.fac <- factor(med.help.money, 
                                                  levels = FgmData.med.help.levels,
                                                  labels = FgmData.med.help.labels)
            partner.occupation.1.fac <- factor(partner.occupation.1)
            partner.occupation.2.fac <- factor(partner.occupation.2, levels = FgmData.occupation.2.levels, labels = FgmData.occupation.2.labels)
            occupation.1.fac <- factor(occupation.1)
            occupation.2.fac <- factor(occupation.2, levels = FgmData.occupation.2.levels, labels = FgmData.occupation.2.labels)
            partner.educlvl.fac <- factor(partner.educlvl, 
                                          levels = c(0:5), #, 8, 9), 
                                          labels = FgmData.partner.educlvl.labels) #, "don't know", 'missing'))
            mother.circum.fac <- factor(mother.circum, 
                                        levels = c(0:1), #, 9), 
                                        labels = c('no', 'yes')) #, 'missing'))

            wealth.index.2 <- factor(ifelse(wealth.index %in% c("rich", "richest"), 1, 0), levels = c(0, 1), labels = c("poor", "rich"))

            discuss.circum.fac <- factor(discuss.circum, level = 0:1, labels = c("no", "yes"))
            received.info.circum.fac <- factor(received.info.circum, level = 0:1, labels = c("no", "yes"))
            visit.health.facil.12mon.fac <- factor(visit.health.facil.12mon, level = 0:1, labels = c("no", "yes"))
          })

          for (ci in FgmData.circum.info)
            spatial.data@data[,paste(ci, "fac", sep = ".")] <<- factor(spatial.data@data[,ci], levels = 0:1, labels = c("no", "yes"))

          sort("hh.id")

          # What's this?
          #if (is.null(fgm.data$unique.cluster)) 
          #  fgm.data$unique.cluster <- as.numeric(row.names(fgm.data))
      } else {
          callSuper(coordinate.names = c("LONGNUM", "LATNUM"),...)
      }
    }
))

BaseFgmData$lock("cluster.info")

BaseFgmData$methods(create.new.from.data = function(df, ...) {
    callSuper(df = df, cluster.info = cluster.info$copy(), dist.mat = dist.mat, ...)
})

BaseFgmData$methods(get.cluster.cohort.summary = function() {
    peer.links <- do.call(rbind, apply(c("cluster", "birth.year"), function(all.data, grp.ids, grp.index.col.values) {
        current.coords <- all.data$coords[grp.ids[1],]
        return(data.frame(cluster = grp.index.col.values["cluster"], birth.year = grp.index.col.values["birth.year"], long = current.coords[1], lat = current.coords[2], count = length(grp.ids)))
    }))
    
    peer.links$id <- seq_along(peer.links$cluster)

    return(SpatialData$new(data = peer.links, coordinate.names = c("long", "lat")))
})

BaseFgmData$methods(grp.mean = function(grp.ids, peer.ids, col.name, prefix, postfix = NULL, col.lvl = NULL, new.col.name = NULL, exclude.self = FALSE, peers.data = NULL, ids.col = NULL, weights = NULL) {
  if (is.null(peers.data)) {
      peers.data <- .self
  }

  col.is.factor <- is.factor(spatial.data@data[, col.name])

  stopifnot(is.null(col.lvl) || col.is.factor)
  stopifnot(is.null(col.lvl) == is.null(new.col.name)) 
  stopifnot(col.is.factor == is.factor(peers.data$spatial.data@data[, col.name]))
  stopifnot(is.null(weights) || (length(weights) == length(peer.ids)))

  if (is.null(col.lvl) && col.is.factor) {
      col.lvls <- levels(spatial.data@data[, col.name])

      for (current.col.lvl in col.lvls[-1]) {
          cleaned.lvl.name <- clean.lvl.name(current.col.lvl)
          new.col.name <- paste(paste(prefix, col.name, sep = '.'), cleaned.lvl.name, sep = '_')
          grp.mean(grp.ids, peer.ids, col.name, prefix, postfix = postfix, col.lvl = current.col.lvl, new.col.name = new.col.name, exclude.self = exclude.self, peers.data = peers.data, ids.col = ids.col, weights = weights)
      }
  } else { 
      if (is.null(new.col.name)) {
          new.col.name <- paste(prefix, col.name, sep = '.')
      }

      if (!is.null(postfix)) {
          new.col.name <- paste(new.col.name, postfix, sep = '.')
      }

      peers.nrow <- length(peer.ids) 

      exclude.mask <- (grp.ids %in% peer.ids) & exclude.self

      if (!is.null(ids.col)) {
          peer.ids <- which(peers.data$spatial.data@data[, ids.col] %in% peer.ids)
          grp.ids <- which(spatial.data@data[, ids.col] %in% grp.ids)
      }

      # Doing this because there might be repeats in the peer.ids; simple [] indexing removes
      # repeats
      peer.col.vals <- sapply(peer.ids, function(p.id) { 
        col.val <- peers.data$spatial.data@data[p.id, col.name]
        if (col.is.factor) as.integer(col.val == col.lvl) else col.val
      })

      if (col.is.factor) {
          grp.col.vals <- as.integer(spatial.data@data[grp.ids, col.name] == col.lvl )
      } else {
          grp.col.vals <- spatial.data@data[grp.ids, col.name]
      }

      non.na.count <- sum(!is.na(peer.col.vals))
      weight.sum <- if (is.null(weights)) 1 else sum(weights[!is.na(peer.col.vals)]) 

      if (is.null(weights)) {
          all.peers.mean <- mean(peer.col.vals, na.rm = TRUE) 
      } else {
          all.peers.mean <- weighted.mean(peer.col.vals, weights, na.rm = TRUE)
      }

      na.mask <- is.na(spatial.data@data[grp.ids, col.name])
      exclude.mask <- as.numeric(exclude.mask & na.mask)
      grp.col.vals[na.mask] <- 0 # I need to put a zero in the NA rows so when the are multiplied with the zero in the na.mask they don't result in
      # an NA


      if ((peers.nrow == 0L) || (non.na.count == 0L)) {
          spatial.data@data[grp.ids, new.col.name] <<- NA
      } else { 
          if (is.null(weights)) {
              grp.weights <- rep(1/non.na.count, length(grp.ids))
          } else {
              grp.weights <- ifelse(grp.ids %in% peer.ids, weights, 0)
          }
          
          replace.vals <- (rep(weight.sum * all.peers.mean, length(grp.ids)) - (grp.col.vals * exclude.mask * grp.weights)) / (weight.sum - (grp.weights * exclude.mask))
          stopifnot(length(replace.vals) == length(grp.ids))
          spatial.data@data[grp.ids, new.col.name] <<- replace.vals
      }
  }

  return() 
})


BaseFgmData$methods(get.dist.matrix = function(subset, ...) {
    if (".eval.frame.n" %in% names(list(...))) {
        n <- list(...)[[".eval.frame.n"]]
    } else {
        n <- 1
    }

    l.dist.mat <- do.call(rbind, apply("cluster.fac", function(all.data, grp.ids, grp.index.col.value) {
          ret.mat <- matrix(0, ncol = nrow + 1, nrow = length(grp.ids))

          current.coords <- all.data$coords[grp.ids[1],]
          ret.mat[, 1:nrow] <- matrix(spDistsN1(all.data$spatial.data, current.coords, longlat = TRUE), ncol = nrow, nrow = length(grp.ids), byrow = TRUE)

          ret.mat[, nrow + 1] <- grp.ids

          return(ret.mat)
    }, subset = subset, .eval.frame.n = n + 1))

    l.dist.mat <- l.dist.mat[order(l.dist.mat[, nrow + 1]), 1:nrow]

    return(l.dist.mat)
})

BaseFgmData$methods(calc.dist.matrix = function() {
    dist.mat <<- get.dist.matrix()
})

BaseFgmData$methods(get.spatial.adj.matrix = function(radius, recalc.dist.matrix = FALSE, subset) {
    if (!recalc.dist.matrix && missing(subset)) {
        return(ifelse(dist.mat <= radius, 1, 0))
    }

    if (missing(subset)) {
        dist.matrix <- get.dist.matrix()
    } else {
        dist.matrix <- get.dist.matrix(subset, .eval.frame.n = 2)
    }

    return(ifelse(dist.matrix <= radius, 1, 0))
})

BaseFgmData$methods(get.cohort.adj.matrix = function(cohort.range = 1, cohort.range.type = c("both", "older"), year.offset = 0, subset, ...) {
    if (".eval.frame.n" %in% names(list(...))) {
        n <- list(...)[[".eval.frame.n"]]
    } else {
        n <- 1
    }

    cohort.adj.mat <- do.call(rbind, apply("birth.year.fac", function(all.data, grp.ids, grp.index.col.value, cohort.range, cohort.range.type = c("both", "older"), year.offset) {
          ret.mat <- matrix(0, ncol = nrow + 1, nrow = length(grp.ids))

          current.birth.year <- all.data$spatial.data@data[grp.ids[1], "birth.year"] + year.offset

          cohort.upper.bound <- current.birth.year + switch(match.arg(cohort.range.type), both = cohort.range, older = 0)
          cohort.lower.bound <- current.birth.year - cohort.range
          # 
          peer.ids <- all.data$get.subset.rows((birth.year <= cohort.upper.bound) & (birth.year >= cohort.lower.bound))   

          ret.mat[, peer.ids] <- 1 

          ret.mat[, nrow + 1] <- grp.ids

          return(ret.mat)
    }, cohort.range, cohort.range.type, year.offset, subset = subset, .eval.frame.n = n + 1))

    cohort.adj.mat <- cohort.adj.mat[order(cohort.adj.mat[, nrow + 1]), 1:nrow]

    return(cohort.adj.mat)
})


