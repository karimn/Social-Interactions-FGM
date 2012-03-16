source("RegressionResults.R")

print.row <- function(results, reg.name, label, cutpoints, symbols, indent = 0, tex.newline = TRUE) {
  cat(sprintf("%s%s ", paste(rep("~", indent), collapse = ""), if (is.null(label)) reg.name else label))

  for (r in results) {
    if (reg.name %in% rownames(r$summary())) {
      cat(sprintf(" & %.3f%s", r$summary()[reg.name, 1], 
			       symnum(r$summary()[reg.name, 4], 
				      corr = FALSE, na = FALSE, cutpoints = cutpoints, symbols = symbols)))
    } else {
      cat(" &")
    }
  }

  cat("\\\\\n")

  for (r in results) {
    if (reg.name %in% rownames(r$summary())) {
      cat(sprintf("& (%.3f)", r$summary()[reg.name, 2]))
    } else {
      cat(" &")
    }
  }

  if (tex.newline) cat("\\\\\n") else cat("\n")
}

print.categorical <- function(results, cat.label, factor.dict, cutpoints, symbols, last.tex.newline = TRUE) {
  cat(sprintf("\\emph{%s} & \\\\\n", cat.label))

  for (lvl in names(factor.dict)) {
    print.row(results, lvl, factor.dict[[lvl]], cutpoints, symbols, 
              indent = 1, tex.newline = (last.tex.newline || (lvl != tail(names(factor.dict), 1))))
  }
}

print.cohorts <- function(results, cohort.range, cutpoints, symbols, last.tex.newline = TRUE) {
  cohort.dict <- as.list(cohort.range)
  names(cohort.dict) <- paste("birth.year.fac", cohort.range, sep = "")
  print.categorical(results, "Cohort", cohort.dict, cutpoints, symbols, last.tex.newline)
}

print.educlvl <- function(results, educlvl.dict, cutpoints, symbols, last.tex.newline = TRUE) {
  #educlvl.dict <- list(educ.lvlprimary = "Primary", educ.lvlsecondary = "Secondary", educ.lvlhigher = "Higher")
  print.categorical(results, "Education Level", educlvl.dict, cutpoints, symbols, last.tex.newline)
}

print.reg.list <- function(results, reg.dict, cutpoints, symbols, last.tex.newline = TRUE, inter.results.space = FALSE) {
  is.first <- TRUE

  reg.names <- if (is.list(reg.dict)) names(reg.dict) else reg.dict

  for (reg.name in reg.names) {
    if (!is.first) {
      if (inter.results.space) cat("\\\\\n")
    } else is.first <- FALSE

    print.row(results, reg.name, if (is.list(reg.dict)) reg.dict[[reg.name]], cutpoints, symbols, 
              indent = 0, tex.newline = (last.tex.newline || (reg.name != tail(names(reg.dict), 1))))
  }
}

print.nrow <- function(results) {
  cat(" N ")
  for (r in results) {
        #cat(sprintf("& %i", r$na.action))
      cat(sprintf("& %i", length(r$results$residuals)))
  }
  cat("\\\\\n")
}

print.r.squared <- function(results) {
  cat(" $R^2$ ")
  for (r in results) {
    cat(sprintf("& %.3f", r$r.squared))
  }
  cat("\\\\\n")
}

print.adj.r.squared <- function(results) {
  cat(" Adjusted $R^2$ ")
  for (r in results) {
    cat(sprintf("& %.3f", r$adj.r.squared))
  }
  cat("\\\\\n")
}

handle.row.callback <- function(cb, results, cutpoints, symbols) {
    if ("label" %in% names(cb)) {
        cat(sprintf(" %s ", cb$label))
    } 

    if ("fun" %in% names(cb)) {
        for (r in results) {
            cat(sprintf("& %s ", cb$fun(r, cutpoints, symbols)))
        }
    }

    cat("\\\\\n")
}

print.results.table <- function(results, reg.dict, cohort.range, educ.reg.dict, grpavg.reg.dict, cutpoints, symbols, other.fac = NULL, table.type = "tabular", inter.results.space = FALSE, est.stderr.header = TRUE, col.num.header = TRUE, show.r.squared = TRUE, show.n = TRUE, column.labels = NULL, row.callbacks = NULL) {
  stopifnot(is.null(column.labels) || (length(column.labels) == length(results))) 
  num.col <- length(results)
  cat(sprintf("\\begin{%s}{l*{%d}{c}}\n", table.type, num.col))

  cat("\\toprule\n")

  if (est.stderr.header) {
    cat(sprintf("& \\multicolumn{%d}{c}{Estimate} \\\\\n", num.col))
    cat(sprintf("& \\multicolumn{%d}{c}{(Standard Error)} \\\\\n", num.col))
    cat(sprintf("\\cmidrule{2-%d}\n", num.col + 1))
  }

  if (col.num.header && (num.col > 1)) {
    cat(paste(sprintf("& (%d)", 1:num.col), collapse = " "))
    cat("\\\\\n")

    if (!is.null(column.labels)) {
        cat(paste(sprintf("& %s", column.labels), collapse = " "))
        cat("\\\\\n")
    }

    cat(sprintf("\\cmidrule{2-%d}\n", num.col + 1))
  }


  if (!is.null(other.fac)) {
      for (fac in other.fac) {
          print.categorical(results, fac$label, fac$dict, cutpoints, symbols)
      }
  }

  if (!is.null(cohort.range))
    print.cohorts(results, cohort.range, cutpoints, symbols)

  if (!is.null(reg.dict))
    print.reg.list(results, reg.dict, cutpoints, symbols, inter.results.space = inter.results.space)

  if (!is.null(educ.reg.dict))
    print.educlvl(results, educ.reg.dict, cutpoints, symbols)

  if (!is.null(grpavg.reg.dict))
    print.reg.list(results, grpavg.reg.dict, cutpoints, symbols)

  if (!is.null(row.callbacks)) {
    cat("\\midrule\n")
    for (cb in row.callbacks) {
        handle.row.callback(cb, results, cutpoints, symbols)
    }
  }

  if (show.n) { 
    cat("\\midrule\n")
    print.nrow(c(results))
  }

  if (show.r.squared) {
    cat("\\midrule\n")
    print.r.squared(c(results))
    print.adj.r.squared(c(results))
  }
  
  cat("\\bottomrule\n")
  cat(sprintf("\\end{%s}\n", table.type))
}

print.interact.table <- function(result, base.reg, v.reg, h.reg, v.names, cutpoints, symbols, p.value = TRUE) {
  summary <- result$summary()  
  sig.measure.index <- if (p.value) 4 else 3

  lht.v0.h1 <- result$lht(sprintf("%s + %s:%s", base.reg, base.reg, h.reg))

  if (is.null(v.reg)) {
    cat(sprintf(" & %.3f%s%s & %.3f%s \\\\\n",
	       summary[base.reg, 1],
	       symnum(summary[base.reg, 4], 
		      corr = FALSE, na = FALSE, cutpoints = cutpoints, symbols = symbols),
	       if (!p.value) "\\tnote[1]" else "",
	       summary[base.reg, 1] + summary[sprintf("%s:%s", base.reg, h.reg), 1],
	       symnum(lht.v0.h1[2,4],
		      corr = FALSE, na = FALSE, cutpoints = cutpoints, symbols = symbols)))
    if (p.value)
      cat(sprintf("& (%.3f) & (%.3f) \\\\\n", summary[base.reg, 4], lht.v0.h1[2, sig.measure.index]))
    else 
      cat(sprintf("& & (%.3f) \\\\\n", lht.v0.h1[2, sig.measure.index]))
  } else {
    lht.v1.h0 <- result$lht(sprintf("%s + %s:%s", base.reg, base.reg, v.reg))
    lht.v1.h1 <- result$lht(sprintf("%s + %s:%s + %s:%s + %s:%s:%s", base.reg, base.reg, h.reg, base.reg, v.reg, base.reg, h.reg, v.reg))
    lht.vdiff.h1 <- result$lht(sprintf("%s:%s + %s:%s:%s", base.reg, v.reg, base.reg, h.reg, v.reg))

    cat(sprintf("%s & %.3f%s%s & %.3f%s \\\\\n",
	       v.names[1],
	       summary[base.reg, 1],
	       symnum(summary[base.reg, 4], 
		      corr = FALSE, na = FALSE, cutpoints = cutpoints, symbols = symbols),
	       if (!p.value) "\\tnote[1]" else "",
	       summary[base.reg, 1] + summary[sprintf("%s:%s", base.reg, h.reg), 1],
	       symnum(lht.v0.h1[2,4],
		      corr = FALSE, na = FALSE, cutpoints = cutpoints, symbols = symbols)))
    if (p.value)
      cat(sprintf("& (%.3f) & (%.3f) \\\\\n", summary[base.reg, 4], lht.v0.h1[2, sig.measure.index]))
    else 
      cat(sprintf("& & (%.3f) \\\\\n", lht.v0.h1[2, sig.measure.index]))

    cat(sprintf("%s & %.3f%s & %.3f%s \\\\\n", 
	       v.names[2],
	       summary[base.reg, 1] + summary[sprintf("%s:%s", base.reg, v.reg), 1],
	       symnum(lht.v1.h0[2,4],
		      corr = FALSE, na = FALSE, cutpoints = cutpoints, symbols = symbols),
	       summary[base.reg, 1] + summary[sprintf("%s:%s", base.reg, h.reg), 1] + summary[sprintf("%s:%s", base.reg, v.reg), 1] + summary[sprintf("%s:%s:%s", base.reg, h.reg, v.reg), 1],
	       symnum(lht.v1.h1[2,4],
		      corr = FALSE, na = FALSE, cutpoints = cutpoints, symbols = symbols)))

    cat(sprintf("& (%.3f) & (%.3f) \\\\\n", lht.v1.h0[2, sig.measure.index], lht.v1.h1[2, sig.measure.index]))
    cat("\\cmidrule{2-3}\n")
    cat(sprintf("Difference & %.3f%s & %.3f%s \\\\\n",
		- summary[sprintf("%s:%s", base.reg, v.reg), 1],
		symnum(summary[sprintf("%s:%s", base.reg, v.reg), 4], 
		       corr = FALSE, na = FALSE, cutpoints = cutpoints, symbols = symbols),
		- summary[sprintf("%s:%s", base.reg, v.reg), 1] - summary[sprintf("%s:%s:%s", base.reg, h.reg, v.reg), 1],
	       symnum(lht.vdiff.h1[2,4],
		      corr = FALSE, na = FALSE, cutpoints = cutpoints, symbols = symbols)))

    if (p.value) 
      cat(sprintf("& (%.3f) & (%.3f) \\\\\n", summary[sprintf("%s:%s", base.reg, v.reg), 4], lht.vdiff.h1[2, sig.measure.index]))
    else
      cat(sprintf("& & (%.3f) \\\\\n", lht.vdiff.h1[2, sig.measure.index]))
  }
}
