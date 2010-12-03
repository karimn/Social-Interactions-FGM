library(sp)
library(spatstat)
library(maptools)

factor.mean <- function(df, grp, col.name, prefix)
{
  stopifnot(is.factor(df[, col.name]))

  col.lvls <- levels(df[, col.name])
  grp.nrow <- nrow(grp)

  for (i in 2:length(col.lvls))
  {
    cleaned.lvl.name <- gsub(",", '', gsub("[\\s-&\\.]+", "_", col.lvls[i], perl = TRUE))
    new.col.name <- paste(paste(prefix, col.name, sep = '.'), cleaned.lvl.name, sep = '_')
    df[, new.col.name] <- sum(grp[, col.name] == col.lvls[i], na.rm = TRUE) / grp.nrow
  }

  return(df)
}

calc.grpavg <- function(df, total.df, cohort.range, regs)
{
  current.birth.year <- df$birth.year[1]
  current.governorate <- df$governorate[1]

  grp <- total.df[(total.df[["birth.year"]] <= current.birth.year) & (total.df[["birth.year"]] >= current.birth.year - cohort.range), ] 

  for (col.name in regs)
  {
    if (is.factor(df[, col.name]))
    {
      df <- factor.mean(df, grp, col.name, 'grpavg')
    }
    else
    {
      df[, paste('grpavg', col.name, sep = '.')] <- mean(grp[, col.name], na.rm = TRUE)
    }
  }

  return(df)
}

setClass("BaseFgmData",
         representation(cluster.info = "data.frame",
                        individual.controls = "character"),
         contains = c("SpatialPointsDataFrame"))

FgmData.cols = quote(c(v000, v001, v002, v003, v004, v005, v023, v024, v025, v104,
                                               v130, v151, v190, g116, v106, v155, v467b, v467c, v467d, v467e,
                                               v511, v704, v705, v714, v716, v717, v719, v721, v729, v730,
                                               g102, g106, g107, g118, g119, sgovern, s103g, s912, s915,
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
               'mother.circum', 'mother.circum.age', 'mother.circum.bywhom', 'circum.byreligion', 'circum.continue', 'governorate', 'prev.governate', 'num.not.circum', 'discuss.circum',
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

setMethod("initialize", 
          signature(.Object = "BaseFgmData"), #, ir.file = "character", gps.file = "character"),
          function(.Object, ..., ir.file = character(0), gps.file = character(0), cols = FgmData.cols, col.names = FgmData.col.names, dhs.year = 2008)
          {
            if (is.empty(ir.file) | is.empty(gps.file))
              return(callNextMethod(.Object, ...))

            ir <- read.dta(ir.file, convert.underscore = TRUE)
            fgm.data <- subset(ir, select = eval(cols))
            
            rm(ir)

            num.col <- length(col.names)
            names(fgm.data)[1:num.col] <- col.names

            fgm.data$dhs.year <- dhs.year

            fgm.data <- within(fgm.data, 
            {
              hh.id <- factor(paste(cluster, hh, sep = '-'))
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
              visit.health.facil.12mon.fac <- factor(visit.health.facil.12mon, level = 0:1, labels = c("no", "yes"))
            })

            for (ci in FgmData.circum.info)
              fgm.data[,paste(ci, "fac", sep = ".")] <- factor(fgm.data[,ci], levels = 0:1, labels = c("no", "yes"))

            fgm.data <- fgm.data[ order(fgm.data$hh.id), ]

            if (is.null(fgm.data$unique.cluster)) 
              fgm.data$unique.cluster <- as.numeric(row.names(fgm.data))

            gps <- read.dbf(gps.file)

            fgm.spdf <- merge(fgm.data, gps, by.x = c('dhs.year', 'cluster'), by.y = c('DHSYEAR', 'DHSCLUST'))
            coordinates(fgm.spdf) <- c("LONGNUM", "LATNUM")
            proj4string(fgm.spdf) <- CRS("+proj=longlat +ellps=WGS84")

            as(.Object, "SpatialPointsDataFrame") <- fgm.spdf
            .Object@cluster.info <- gps

            rm(gps)

            .Object

            #.Object <- callNextMethod(.Object, fgm.spdf, cluster.info = gps, ...)
          }
)

if (!isGeneric("names")) setGeneric("names")

setMethod("names",
          signature = c(x = "BaseFgmData"),
          function(x)
          {
            names(x@data)
          }
)

if (!isGeneric("by")) setGeneric("by")

setMethod("by",
          signature = c(data = "BaseFgmData", INDICES = "list", FUN = "function"),
          function(data, INDICES, FUN, ...)
          {
            convert.FgmData <- function(df, ...)
            {
              df <- merge(df, data@cluster.info[, c('DHSYEAR', 'DHSCLUST', 'LONGNUM', 'LATNUM')], by.x = c('dhs.year', 'cluster'), by.y = c('DHSYEAR', 'DHSCLUST'))
              coordinates(df) <- c('LONGNUM', 'LATNUM')
              proj4string(df) <- CRS("+proj=longlat +ellps=WGS84")

              FUN(new("FgmData", df, cluster.info = data@cluster.info), ...)
            } 
            by(data@data, INDICES, convert.FgmData, ...)
          }
)

if (!isGeneric("subset")) setGeneric("subset")

setMethod("subset",
          signature = c(x = "BaseFgmData"),
          function(x, subset, ...)
          {
            # same implementation as subset.base().  I had to do this because of a parent.frame() used in evaluating the subset param
            if (missing(subset)) 
                r <- TRUE
            else 
            {
                e <- substitute(subset)
                r <- eval(e, x@data, parent.frame(n = 2))
                if (!is.logical(r)) 
                    stop("'subset' must evaluate to logical")
                r <- r & !is.na(r)
            }

            x[r, ]
          }
)

if (!isGeneric("nrow")) setGeneric("nrow")

setMethod("nrow",
          signature = c(x = "BaseFgmData"),
          function(x)
          {
            nrow(x@data)
          }
)

if (!isGeneric("by.radius")) setGeneric("by.radius", function(self, radius, fun, ...) standardGeneric("by.radius"))

setMethod("by.radius",
          signature = c(self = "BaseFgmData", radius = "numeric", fun = "function"),
          function(self, radius, fun, ..., indices = NULL, by.cluster = TRUE)
          {
            clinfo.sp <- self@cluster.info
            coordinates(clinfo.sp) <- c("LONGNUM", "LATNUM")
            proj4string(clinfo.sp) <- CRS("+proj=longlat +ellps=WGS84")

            # I got this from the definition of as.ppp.SpatialPoints() from maptools
            bb <- bbox(clinfo.sp)
            colnames(bb) <- NULL
            w <- owin(bb[1, ], bb[2, ])
            cc <- coordinates(clinfo.sp)
            # I'm going to use the "marks" field to identify the clusters
            clinfo.ppp <- ppp(cc[, 1], cc[, 2], window = w, marks = clinfo.sp$unique.cluster, check = FALSE)

            by.cluster.fun <- function(df, fun, by.cluster, ...)
            {
              stopifnot(nrow(df) > 0)

              u.cluster <- df$unique.cluster[1] 
              cluster.ppp <- clinfo.ppp[clinfo.ppp$marks == u.cluster]
              cluster.coords <- coords(cluster.ppp)
              neighbor.ppp <- clinfo.ppp[, disc(radius, c(cluster.coords$x, cluster.coords$y))]  
              neighbor.clusters <- neighbor.ppp$marks
              neighbor.clusters.fgm <- subset(self, unique.cluster %in% neighbor.clusters)

              if (by.cluster)
              {
                fun(u.cluster, neighbor.clusters.fgm, ...)
              }
              else
              {
                by.row.fun <- function(dfhh, dfrl)
                {
                  fun(u.cluster, dfhh, dfrl, subset(neighbor.clusters.fgm, (unique.cluster != u.cluster) | (hh != dfhh) | (respond.linenum != dfrl)), ...)
                }

                vec.by.row.fun <- Vectorize(by.row.fun)

                vec.by.row.fun(df$hh, df$respond.linenum)
              }
            }

            with.indices.fun <- function(df, fun, by.cluster, radius, ...)
              do.call(rbind, by.radius(df, radius, fun, ..., by.cluster = by.cluster))

            # TODO Make the "by" overridden implementation take indices from the "self" directly 
            if (is.null(indices))
              by(self, self@data[c('unique.cluster')], by.cluster.fun, fun, by.cluster, ...)
            else
              by(self, indices, with.indices.fun, fun, by.cluster, radius, ...)
          }
)

if (!isGeneric("generate.reg.means")) setGeneric("generate.reg.means", function(self, ...) standardGeneric("generate.reg.means"))
setMethod("generate.reg.means",
          "BaseFgmData",
          function(self, ..., cohort.range = 1)
          {
            self@data <- do.call(rbind, by(self@data, self@data[c("birth.year.fac", "governorate")], calc.grpavg.fun, self@data, cohort.range))
          }
)
