library(car)

RegressionResults <- setRefClass("RegressionResults",
  fields = list(
    results = "lm", 
    vcov = "matrix",
    regress.formula = "formula",
    data = "Data",
    na.action = function(val) if (missing(val)) results$na.action else stop("na.action is not mutable")) )

RegressionResults$lock("results", "regress.formula", "data")

RegressionResults$methods(initialize = function(...) {
    initFields(...)
    if (is(vcov, "uninitializedField")) vcov <<- NULL
  })

RegressionResults$methods(summary = function() {
    coeftest(results, vcov. = vcov)
  })

RegressionResults$methods(lht = function(hypothesis, test = c("Chisq", "F"), vcov.arg) {
    linearHypothesis(results, hypothesis, test = test, vcov = vcov)
  })

RegressionResults$methods(adj.r.squared = function() {
    base::summary(results)$adj.r.squared
  })

RegressionResults$methods(r.squared = function() {
    base::summary(results)$r.squared
  })

PanelRegressionResults <- setRefClass("PanelRegressionResults",
  contains = "RegressionResults",

  methods = list(
    r.squared = function()
    {
      return(base::summary(results)$r.squared["rsq"])
    },

    adj.r.squared = function()
    {
      return(base::summary(results)$r.squared["adjrsq"])
    }
    )
)
