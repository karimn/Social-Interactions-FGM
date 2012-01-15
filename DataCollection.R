
DataCollection <- setRefClass("DataCollection",
                              fields = list(coll = "list",
                                            split.by = "character",
                                            names = function(value) if (missing(value)) base::names(coll) else base::names(coll) <- value,
                                            length = function (value) if (missing(value)) base::length(coll) else base::length(coll) <- value ))
