library(foreign)

Data <- setRefClass("Data",
    fields = list(
        data = "data.frame",
        names = function(value) if (missing(value)) base::names(data) else base::names(data) <<- value,
        #nrow = function(value) if (missing(value)) base::nrow(data) else stop("nrow is not mutable"),
        nrow = function(value) base::nrow(data),
        #ncol = function(value) if (missing(value)) base::ncol(data) else stop("ncol is not mutable")))
        ncol = function(value) base::ncol(data)))

Data$methods(init.from.list = function(obj.list) {
    stopifnot(all(base::sapply(obj.list, function(obj) is(obj, getClass()))) | is.empty(obj.list))

    data <<- do.call(base::rbind, base::lapply(obj.list, function(obj) obj$data))
})

Data$methods(initialize = function(columns, new.column.names, collection, copy, ...) {
    initFields(...)

    if(!missing(copy) && is(copy, "Data")) {
        data <<- copy$data
    } else if (!missing(collection)) {
        init.from.list(collection)
    }

    if (!missing(columns)) { # Select columns
        subset(select = eval(columns))
    }

    if (!missing(new.column.names)) { # Rename column names
        stopifnot(length(new.column.names) <= ncol(data))
        names[1:length(new.column.names)] <<- new.column.names
    }
})


Data$methods(change.column.names = function(old.names, new.names) {
    names[names %in% old.names] <<- new.names
})

Data$methods(sort = function(by, ...) {
    data <<- data[do.call(order, c(as.list(data[, by, drop = FALSE]), list(...))), , drop = FALSE]
})

Data$methods(subset = function(subset, select, ...) { 
    if (".eval.frame.n" %in% names(list(...))) {
        n <- list(...)[[".eval.frame.n"]]
    } else {
        n <- 1
    }

    if (missing(subset)) {
        data <<- do.call(base::subset, c(list(x = data, select = substitute(select)), list(...)), envir = parent.frame(n)) 
    } else if (missing(select)) {
        data <<- do.call(base::subset, c(list(x = data, subset = substitute(subset)), list(...)), envir = parent.frame(n)) 
    } else {
        data <<- do.call(base::subset, c(list(x = data, subset = substitute(subset), select = substitute(select)), list(...)), envir = parent.frame(n)) 
    }
  }) 

Data$methods(get.subset = function(...) { 
    new.data.obj <- copy()
    new.data.obj$subset(.eval.frame.n = 2, ...)
    return(new.data.obj)
})

Data$methods(remove.rows = function(rows) {
    data <<- data[-rows, ]
})

stub.callback <- function(df, FUN, self.obj, rm.na = TRUE, ...) {
    if (!rm.na || !is.na(df)) {
        ret.obj <- self.obj$convert.callback(df, FUN, ...)
        return(ret.obj)
    } else {
        return(NULL)
    }
}

Data$methods(create.new.from.data = function(df, ...) {
    getRefClass()$new(data = df, ...)
})

Data$methods(convert.callback = function(df, original.callback, ...) {
    original.callback(create.new.from.data(df), ...)
})

Data$methods(aggregate = function(by, FUN, ...) {
    stats::aggregate(data, data[by], stub.callback, FUN, .self, ...)
})

#Data$methods(tapply = function(by, FUN, ...) {
#    base::tapply(data, data[by], stub.callback, FUN, .self, ...)
#})

#Data$methods(tapply.change = function(by, FUN, ...) {
#    "The callback function must return an object of the same type of this class"  
#    do.call(base::rbind, lapply(base::tapply(data, data[by], stub.callback, FUN, .self, ...)), function (obj) obj$data)
#})

Data$methods(split = function(by, ...) {
    DataCollection$new(coll = base::lapply(base::split(data, by, data[by], ...), function(df) create.new.from.data(df)))
})

Data$methods(by = function(by.indices, FUN, rm.na = TRUE, ...) {
    base::by(data, INDICES = data[by.indices], FUN = stub.callback, FUN, .self, na.rm, ...)
})

Data$methods(quick.update = function(by, FUN, ...) {
    "The callback function will receive a reference to self, group IDs, the value of the group indices, and \"...\""
    grp.ind <- base::tapply(data[[1L]], data[[by]])
    max.grp.index <- max(grp.ind)
    for (grp.index in 1L:max.grp.index) {
        # mask <- grp.ind == grp.index
        grp.ids <- which(grp.ind == grp.index)
        first.in.grp <- min(grp.ids)

        grp.index.col.values <- list()

        for (by.index in by) {
            grp.index.col.values[by.index] <- data[first.in.grp, by.index]
        }

        FUN(.self, grp.ids, grp.index.col.values, ...)
    }
})

Data$methods(reshape = function(...) {
    data <<- stats::reshape(data, ...)
})

Data$methods(duplicated = function(by, ...) {
    if (missing(by)) {
        base::duplicated(data, ...)
    } else {
        base::duplicated(data[by], ...)
    }
})

Data$methods(relevel = function(column, ref, ...) {
    data[,column] <<- stats::relevel(data[,column], ref = ref, ...)
})

Data$methods(summary = function(columns = TRUE, ...) {
    base::summary(data[, columns], ...)
})

Data$methods(merge = function(other.data, ...) {
    data <<- base::merge(data, other.data, ...)
})

# Regression methods

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

