library(plm)

reg.pooled.lagged.med <- function(fgm.data) {
  #lagged.formula <- dynformula(circum.yesno ~ year.circum + med.circum, list(2,1,c(2,2)))
  #lagged.formula <- dynformula(circum.yesno ~ med.circum, list(1, c(1, 1)))

  return(plm(circum.yesno ~ lag(circum.yesno, 1) + lag(med.circum, 1), data = fgm.data, index = c("hh.id", "order"), model = "pooling"))
}


