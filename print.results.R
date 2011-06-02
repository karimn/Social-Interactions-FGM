
print.row <- function(results, reg.name, label, cutpoints, symbols, indent = 0, tex.newline = TRUE) {
  
  cat(sprintf("%s%s ", paste(rep("~", indent), collapse = ""), label))

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

print.educlvl <- function(results, cutpoints, symbols, last.tex.newline = TRUE) {
  educlvl.dict <- list(educ.lvlprimary = "Primary", educ.lvlsecondary = "Secondary", educ.lvlhigher = "Higher")
  print.categorical(results, "Education Level", educlvl.dict, cutpoints, symbols, last.tex.newline)
}

print.reg.list <- function(results, reg.dict, cutpoints, symbols, last.tex.newline = TRUE) {
  for (reg.name in names(reg.dict)) {
    print.row(results, reg.name, reg.dict[[reg.name]], cutpoints, symbols, 
              indent = 0, tex.newline = (last.tex.newline || (reg.name != tail(names(reg.dict), 1))))
  }
}
