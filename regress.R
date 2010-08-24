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

factor.mean <- function(df, col.name, prefix)
{
  stopifnot(is.factor(df[, col.name]))

  col.lvls <- levels(df[, col.name])
  df.nrow <- nrow(df)

  for (i in 2:length(col.lvls))
  {
    cleaned.lvl.name <- gsub(",", '', gsub("[\\s-&\\.]+", "_", col.lvls[i], perl = TRUE))
    new.col.name <- paste(paste(prefix, col.name, sep = '.'), cleaned.lvl.name, sep = '_')
    df[, new.col.name] <- sum(df[, col.name] == col.lvls[i], na.rm = TRUE) / df.nrow
  }

  return(df)
}

###########################################################################################
# Ref groups: birth.year.fac, governate

grpavg.col.names <- c('urban.rural', 'religion', 'educ.lvl', 'med.help.permission.fac', 'med.help.distance.fac', 'med.help.transportation.fac',
                      'mother.circum.fac', 'partner.educlvl.fac', 'occupation.2.fac', 'partner.occupation.2.fac')

grpavg.formula <- "has.or.intends.circum ~ governate * birth.year.fac + urban.rural + religion + wealth.index + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac"

fgm <- do.call(rbind, by(fgm.data.daughters.08@data, fgm.data.daughters.08@data[c('governate', 'birth.year.fac')], function(df)
{
  for (col.name in grpavg.col.names)
  {
    df <- factor.mean(df, col.name, 'grpavg')
  }

  df$grpavg.marital.age <- mean(df$marital.age, na.rm = TRUE)

  return(df)
}))

grpavg.formula <- paste(grpavg.formula, paste(grep("^grpavg", names(fgm), value = TRUE), collapse = " + "), sep = " + ")

p4 <- lm(formula(grpavg.formula), data = fgm)

###########################################################################################
# Ref groups: birth.year.fac, governate, urban.rural 

grpavg.col.names <- c('religion', 'educ.lvl', 'med.help.permission.fac', 'med.help.distance.fac', 'med.help.transportation.fac',
                      'mother.circum.fac', 'partner.educlvl.fac', 'occupation.2.fac', 'partner.occupation.2.fac')

grpavg.formula <- "has.or.intends.circum ~ (governate + birth.year.fac + urban.rural)^3 + religion + wealth.index + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac"

fgm <- do.call(rbind, by(fgm.data.daughters.08@data, fgm.data.daughters.08@data[c('governate', 'birth.year.fac', 'urban.rural')], function(df)
{
  for (col.name in grpavg.col.names)
  {
    df <- factor.mean(df, col.name, 'grpavg')
  }

  df$grpavg.marital.age <- mean(df$marital.age, na.rm = TRUE)

  return(df)
}))

grpavg.formula <- paste(grpavg.formula, paste(grep("^grpavg", names(fgm), value = TRUE), collapse = " + "), sep = " + ")

p5 <- lm(formula(grpavg.formula), data = fgm)

###########################################################################################
# Ref groups: birth.year.fac, governate, wealth.index 

grpavg.col.names <- c('urban.rural', 'educ.lvl', 'religion', 'med.help.permission.fac', 'med.help.distance.fac', 'med.help.transportation.fac',
                      'mother.circum.fac', 'partner.educlvl.fac', 'occupation.2.fac', 'partner.occupation.2.fac')

grpavg.formula <- "has.or.intends.circum ~ (governate + birth.year.fac + wealth.index)^2 + religion + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac"

fgm <- do.call(rbind, by(fgm.data.daughters.08@data, fgm.data.daughters.08@data[c('governate', 'birth.year.fac', 'wealth.index')], function(df)
{
  for (col.name in grpavg.col.names)
  {
    df <- factor.mean(df, col.name, 'grpavg')
  }

  df$grpavg.marital.age <- mean(df$marital.age, na.rm = TRUE)

  return(df)
}))

grpavg.formula <- paste(grpavg.formula, paste(grep("^grpavg", names(fgm), value = TRUE), collapse = " + "), sep = " + ")

p6 <- lm(formula(grpavg.formula), data = fgm)
###########################################################################################

radius <- 0.5
grpavg.col.names <- c('urban.rural', 'educ.lvl', 'religion', 'med.help.permission.fac', 'med.help.distance.fac', 'med.help.transportation.fac',
                      'mother.circum.fac', 'partner.educlvl.fac', 'occupation.2.fac', 'partner.occupation.2.fac', 'wealth.index')

grpavg.formula <- "has.or.intends.circum ~ governate * birth.year.fac + wealth.index + religion + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac"

byrad.grpavg.fun <- function(uc, df)
{
  df <- df@data
  for (col.name in grpavg.col.names)
  {
    df <- factor.mean(df, col.name, 'grpavg')
  }

  df$grpavg.marital.age <- mean(df$marital.age, na.rm = TRUE)

  # This is important to only return the rows for which the above means are relevant
  retss <- df[df$unique.cluster == uc, ]

  return(retss)
}

fgm <- do.call(rbind, by.radius(fgm.data.daughters.08, radius, indices = fgm.data.daughters.08@data[c("birth.year.fac")], byrad.grpavg.fun))

grpavg.formula <- paste(grpavg.formula, paste(grep("^grpavg", names(fgm), value = TRUE), collapse = " + "), sep = " + ")
p7 <- lm(formula(grpavg.formula), data = fgm)

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


