library(car)
library(lmtest)
library(sandwich)

###########################################################################################

radius <- 1 
grpavg.col.names <- c('urban.rural', 'educ.lvl', 'religion', 'med.help.permission.fac', 'med.help.distance.fac', 'med.help.transportation.fac',
                      'mother.circum.fac', 'partner.educlvl.fac', 'occupation.2.fac', 'partner.occupation.2.fac', 'wealth.index')

grpavg.formula <- "has.or.intends.circum ~ birth.year.fac + governorate + wealth.index + religion + urban.rural + educ.lvl + med.help.permission.fac + med.help.distance.fac + med.help.transportation.fac + marital.age + mother.circum.fac + partner.educlvl.fac + occupation.2.fac + partner.occupation.2.fac"

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
