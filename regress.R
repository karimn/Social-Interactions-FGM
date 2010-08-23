library(plm)

pl1 <- plm(has.or.intends.circum ~ ADM1SALBNA + birth.year.fac + urban.rural + religion + wealth.index + educ.lvl + med.help.permission.fac +
                                   med.help.distance.fac + med.help.transportation.fac +
                                   marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac,
           data = fgm.data.daughters.08@data,
           index = c("hh.id", "order.fac"),
           model = "pooling")

# This is problematic: I'm selecting circumcized only and hence was for latter years I only capture those who have been circumcized younger
pl2 <- plm(age.circum ~ ADM1SALBNA + birth.year.fac + urban.rural + religion + wealth.index,
           data = fgm.data.daughters.08@data,
           subset = ((circum == 1) & (age.circum < 98)),
           index = c("hh.id", "order.fac"),
           model = "pooling")

pl3 <- plm(med.circum ~ ADM1SALBNA + birth.year.fac + urban.rural + religion + wealth.index,
           data = fgm.data.daughters.08@data,
           subset = circum == 1,
           index = c("hh.id", "order.fac"),
           model = "pooling")

###########################################################################################

p1 <- lm(circum.num ~ ADM1SALBNA + birth.year.fac + urban.rural + religion + wealth.index,
           data = fgm.data.all@data)

p2 <- lm(circum.age ~ ADM1SALBNA + birth.year.fac + urban.rural + religion + wealth.index,
           data = fgm.data.all@data,
           subset = (circum == 'yes'))

p3 <- lm(has.or.intends.circum ~ ADM1SALBNA + birth.year.fac + urban.rural + religion + wealth.index + educ.lvl + med.help.permission.fac +
                                   med.help.distance.fac + med.help.transportation.fac +
                                   marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac,
           data = fgm.data.daughters.08@data)
           
###########################################################################################

factor.mean <- function(df, col.name, mean.col.name)
{
  stopifnot(is.factor(df[, col.name]))

  col.lvls <- levels(df[, col.name])
  df.nrow <- nrow(df)

  for (i in 2:length(col.lvls))
  {
    df[, paste(mean.col.name, col.lvls[i], sep = '.')] <- sum(df[, col.name] == col.lvls[i], na.rm = TRUE) / df.nrow
  }

  return(df)
}

###########################################################################################
# Ref groups: governate

fgm <- do.call(rbind, by(fgm.data.daughters.08@data, fgm.data.daughters.08@data[c('governate', 'birth.year.fac')], function(df)
{
  df <- factor.mean(df, 'urban.rural', 'grpavg.urban.rural')
  df <- factor.mean(df, 'religion', 'grpavg.religion')
  df <- factor.mean(df, 'educ.lvl', 'grpavg.educ.lvl')
  df <- factor.mean(df, 'med.help.permission.fac', 'grpavg.med.help.permission.fac')
  df <- factor.mean(df, 'med.help.distance.fac', 'grpavg.med.help.distance.fac')
  df <- factor.mean(df, 'med.help.transportation.fac', 'grpavg.med.help.transportation.fac')
  df$grpavg.marital.age <- mean(df$marital.age, na.rm = TRUE)
  df <- factor.mean(df, 'mother.circum.fac', 'grpavg.mother.cricum.fac')
  df <- factor.mean(df, 'partner.educlvl.fac', 'grpavg.partner.educlvl.fac')
  df <- factor.mean(df, 'occupation.2.fac', 'grpavg.occupation.2.fac')
  df <- factor.mean(df, 'partner.occupation.2.fac', 'grpavg.partner.occupation.2.fac')
}))

###########################################################################################

radius <- 1

by.radius(fgm.data.daughters.08, radius, function(dhs.year, cluster, df)
{
  by(df@data, df@data[c('birth.year.fac')], function(by.df)
  {
    df <- factor.mean(df, 'urban.rural', 'grpavg.urban.rural')
  })
})

###########################################################################################

#reg.pooled.lagged.med <- function(fgm.data) {
  #dynformula() doesn't seem to be working for me
  #lagged.formula <- dynformula(circum.yesno ~ year.circum + med.circum, list(2,1,c(2,2)))
  #lagged.formula <- dynformula(circum.yesno ~ med.circum, list(1, c(1, 1)))

res.yesno <- plm(circum.yesno ~ lag(circum.yesno, 1) + region + urban.rural + religion + hh.head.sex + wealth.index + lag(med.circum, 1), 
                 data = fgm.data.08, index = c("hh.id", "order.fac"), model = "pooling")
res.intends <- plm(has.or.intends.circum ~ lag(has.or.intends.circum, 1) + lag(med.circum, 1) + region + urban.rural + religion + hh.head.sex + wealth.index + order.fac, 
                   data = fgm.data.08, index = c("hh.id", "order.fac"), model = "pooling")

#return (c(res.yesno, res.intends))
#}

# Can't regress on year.circum because we only have data for years where circum.yesno is 1
#res.fe <- plm(circum.yesno ~ year.circum, data = fgm.data.08, index = c("hh.id", "order"), model = "within")

res.fe.yesno <- plm(circum.yesno ~ order.fac, data = fgm.data.08, index = c("hh.id", "order.fac"), model = "within", effect = "individual")


