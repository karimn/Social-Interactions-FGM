
DataCollection <- setRefClass("DataCollection",
                              fields = list(coll = "list",
                                            split.by = "character",
                                            class.name = "character",
                                            names = function(value) if (missing(value)) base::names(coll) else base::names(coll) <- value,
                                            length = function (value) if (missing(value)) base::length(coll) else base::length(coll) <- value ))

DataCollection$lock("class.name")

DataCollection$methods(initialize = function(coll, ...) {
    stopifnot(length(coll) > 0)
    first.obj.class <- class(coll[[1]])
    stopifnot(all(base::sapply(coll, function(obj) class(obj) == first.obj.class))) 
    initFields(coll = coll, ...)
})

DataCollection$methods(rbind = function() {
    coll[[1]]$getRefClass()$new(collection = coll)
})
                              
