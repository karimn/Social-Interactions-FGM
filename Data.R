library(foreign)

Data <- setRefClass("Data",
    fields = list(
        data = "data.frame",
        names = function(value) if (missing(value)) base::names(data) else base::names(data) <<- value,
        #nrow = function(value) if (missing(value)) base::nrow(data) else stop("nrow is not mutable"),
        nrow = function(value) base::nrow(data),
        #ncol = function(value) if (missing(value)) base::ncol(data) else stop("ncol is not mutable")))
        ncol = function(value) base::ncol(data)))

Data$methods(initialize = function(columns = NULL, new.column.names = NULL, ...) {
    initFields(...)

    if (!is.null(columns)) { # Select columns
        subset(select = eval(columns))
    }

    if (!is.null(new.column.names)) { # Rename column names
        stopifnot(length(new.column.names) <= ncol(data))
        names[1:length(new.column.names)] <<- new.column.names
    }
  })

Data$methods(subset = function(subset, select, ...) { 
    data <<- do.call(base::subset, c(list(x = data, subset = substitute(subset), select = substitute(select)), list(...)), envir = parent.frame()) 
  }) 

Data$methods(get.subset = function(...) { 
    new.data.obj <- copy()
    new.data.obj$subset(...)
    return(new.data.obj)
})

stub.callback <- function(df, FUN, self.obj, ...) {
    self.obj$convert.callback(df, FUN, ...)
}

Data$methods(convert.callback = function(df, original.callback, ...) {
    original.callback(getRefClass()$new(data = df), ...)
})

Data$methods(aggregate = function(by, FUN, ...) {
    stats::aggregate(data, data[by], stub.callback, FUN, .self, ...)
})

Data$methods(tapply = function(by, FUN, ...) {
    base::tapply(data, data[by], stub.callback, FUN, .self, ...)
})

Data$methods(split = function(by, ...) {
    DataCollection$new(coll = base::lapply(base::split(data, by, data[by], ...), function(df) getRefClass()$new(data = df)))
})

Data$methods(by = function(INDICES, FUN, ...) {
    base::by(data, INDICES = eval(substitute(INDICES), envir = data), FUN = stub.callback, FUN, .self, ...)
})

Data$methods(lm = function(formula, vcov.fun = vcovHAC, ...) {
    r <- stats::lm(formula, data = data)

    if (!is.null(vcov.fun)) {
        vcov <- vcov.fun(r, ...)

        RegressionResults$new(results = r, regress.formula = formula, data = .self, vcov = vcov)
    } else {
        RegressionResults$new(results = r, regress.formula = formula, data = .self) 
    }
  })

Data$methods(tsls = function(formula, vcov.fun = vcovHAC, ...) {
    r <- sem::tsls(formula, formula, data = data)

    if (!is.null(vcov.fun)) {
        vcov <- vcov.fun(r, ...) 
        RegressionResults$new(results = r, regress.formula = formula, data = .self, vcov = vcov)
    } else {
        RegressionResults$new(results = r, regress.formula = formula, data = .self) 
    }
  })

Data$methods(ivreg = function(formula, vcov.fun = vcovHAC, ...) {
    r <- AER::ivreg(formula, formula, data = data)

    if (!is.null(vcov.fun)) {
        vcov <- vcov.fun(r, ...) 
        RegressionResults$new(results = r, regress.formula = formula, data = .self, vcov = vcov)
    } else {
        RegressionResults$new(results = r, regress.formula = formula, data = .self) 
    }
  })

Data$methods(plm = function(formula, effect, model, index, vcov.fun = vcovSCC, ...) {
    r <- plm::plm(formula, effect = effect, model = model, index = index, data = data)
    v <- if (gen.vcov) tryCatch(vcovSCC(r), error = function(e) { matrix(NA, 0, 0) }) else NULL
     if (!is.null(vcov.fun)) {
        vcov <- vcov.fun(r, ...) 
        PanelRegressionResults$new(results = r, regress.formula = formula, data = .self, vcov = vcov)
    } else {
        PanelRegressionResults$new(results = r, regress.formula = formula, data = .self) 
     }
  }
)

StataData <- setRefClass("StataData",
                         contains = c("Data"))

StataData$methods(initialize = function(dta.file, ...) {
    data <<- read.dta(dta.file, convert.underscore = TRUE)

    callSuper(...)
})
