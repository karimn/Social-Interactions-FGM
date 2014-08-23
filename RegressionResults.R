library(car)
library(lmtest)
library(plm)

RegressionResults <- setRefClass("RegressionResults",
  fields = list(
    results = "ANY",  # because in the PanelRegressionResults subclass I need to have a "plm" object stored here 
    vcov = "matrix",
    regress.formula = "formula",
    #     data = "Data",
    fitted.values = function(val) results$fitted.values,
    residuals = function(val) results$residuals,
    na.action = function(val) if (missing(val)) results$na.action else stop("na.action is not mutable"),
    ssr = function(val) sum(results$residuals^2),
    r.squared = function(val) base::summary(results)$r.squared,
    adj.r.squared = function(val) base::summary(results)$adj.r.squared) )

RegressionResults$lock("regress.formula") #, "data") "results", 

RegressionResults$methods(initialize = function(...) {
    initFields(...)
    if (is(vcov, "uninitializedField")) vcov <<- NULL
  })

RegressionResults$methods(summary = function(vcov.fun, ...) {
    if (!missing(vcov.fun)) {
        lmtest::coeftest(results, vcov. = if (!is.null(vcov.fun)) vcov.fun(results, ...) else NULL)
    } else {
        lmtest::coeftest(results, vcov. = if (all(vcov == c(0, 0))) NULL else vcov)
    }
  })

RegressionResults$methods(lht = function(hypothesis, test = c("Chisq", "F"), vcov.fun, ...) {
    if (!missing(vcov.fun)) {
        linearHypothesis(results, hypothesis, test = test, vcov = if (!is.null(vcov.fun)) vcov.fun(results, ...) else NULL)
    } else {
        linearHypothesis(results, hypothesis, test = test, vcov = vcov)
    }
  })

#RegressionResults$methods(adj.r.squared = function() {
#    base::summary(results)$adj.r.squared
#  })
#
#RegressionResults$methods(r.squared = function() {
#    base::summary(results)$r.squared
#  })

PanelRegressionResults <- setRefClass("PanelRegressionResults",
  contains = "RegressionResults",
  fields = list(
    #results = "list",
    r.squared = function(val) base::summary(results)$r.squared["rsq"],
    adj.r.squared = function(val) base::summary(results)$r.squared["adjrsq"] )

#  methods = list(
#    r.squared = function()
#    {
#      return(base::summary(results)$r.squared["rsq"])
#    },
#
#    adj.r.squared = function()
#    {
#      return(base::summary(results)$r.squared["adjrsq"])
#    }
#    )
)
