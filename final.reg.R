library(car)
library(lmtest)
library(sandwich)
library(AER)

sum.coefs <- function(reg.res, data, time.dummy, vcov = NULL, to.sum = NULL)
{
  ret.mat <- NULL
  row.names <- NULL
  signif <- NULL
  time.xtabs <- xtabs(formula(sprintf("~ %s", time.dummy)), data = data)
  time.lvls <- names(time.xtabs)[time.xtabs > 0] # levels(data[, time.dummy])

  coefs <- coeftest(reg.res, vcov = vcov)

  if (!is.null(to.sum) && (length(grep(to.sum, rownames(coefs), fixed = TRUE)) == 0))
    return(NULL)

  timeinvar.estimate <- ifelse(!is.null(to.sum), coefs[to.sum, 1], coefs["(Intercept)", 1])

  # First cohort

  signif.test <- sprintf("%s", ifelse(is.null(to.sum), "(Intercept)", to.sum))
  lh <- if (!is.null(vcov)) linearHypothesis(reg.res, signif.test, vcov = vcov) else linearHypothesis(reg.res, signif.test)
  fstat.sum <- lh[2,3]
  pval.sum <- lh[2,4]

  ret.mat <- c(timeinvar.estimate, fstat.sum, pval.sum)
  row.names <- sprintf("%s%s_%s", ifelse(is.null(to.sum), "", paste(to.sum, ":", sep = '')), time.dummy, time.lvls[1])

  # The rest

  for (time.lvl in time.lvls[2:length(time.lvls)])
  {
    lvl.name <- sprintf("%s%s%s",  time.dummy, time.lvl, ifelse(is.null(to.sum), "", paste(":", to.sum, sep = '')))
    rev.lvl.name <- sprintf("%s%s%s", ifelse(is.null(to.sum), "", paste(":", to.sum, sep = '')), time.dummy, time.lvl)
    where.index <- grep(sprintf("^%s$", gsub("\\.", "\\\\.", lvl.name)), rownames(coefs))

    stopifnot(length(where.index) < 2)

    if (length(where.index) == 0)
    {
      where.index <- grep(sprintf("^%s$", gsub("\\.", "\\\\.", rev.lvl.name)), rownames(coefs))

      stopifnot(length(where.index) < 2)

      if (length(where.index) == 0)
        next
      else
        lvl.name <- rev.lvl.name
    }

    estimate.sum <- coefs[lvl.name, 1] + timeinvar.estimate

    signif.test <- sprintf("%s + %s", ifelse(is.null(to.sum), "(Intercept)", to.sum), lvl.name)
    lh <- if (!is.null(vcov)) linearHypothesis(reg.res, signif.test, vcov = vcov) else linearHypothesis(reg.res, signif.test)
    fstat.sum <- lh[2,3]
    pval.sum <- lh[2,4]

    signif <- c(signif, signif.test)

    ret.mat <- rbind(ret.mat, c(estimate.sum, fstat.sum, pval.sum))
    row.names <- c(row.names, sprintf("%s%s_%s", ifelse(is.null(to.sum), "", paste(to.sum, ":", sep = '')), time.dummy, time.lvl))
  }

  if (is.matrix(ret.mat))
  {
    rownames(ret.mat) <- row.names
    colnames(ret.mat) <- c("Estimate", "F statistic", "p-value")
    attr(ret.mat, 'signif') <- signif
  }

  return(ret.mat)
}

print.cohorts <- function(results, coef.name = NULL, indent = 1, pvalue = TRUE)
{
  row.names <- NULL
  
  for (d in names(results))
    for (k in names(results[[d]]))
      for (s in names(results[[d]][[k]]))
      {
        results[[d]][[k]][[s]]$sum.coefs <- sum.coefs(results[[d]][[k]][[s]]$lm, 
                                                      results[[d]][[k]][[s]]$fgm, 
                                                      "birth.year.fac", 
                                                      results[[d]][[k]][[s]]$vcov,
                                                      coef.name)

        if (!is.null(results[[d]][[k]][[s]]$sum.coefs))
          if (is.matrix(results[[d]][[k]][[s]]$sum.coefs))
          {
            if (is.null(row.names))
              row.names <- rownames(results[[d]][[k]][[s]]$sum.coefs)
              
            results[[d]][[k]][[s]]$sum.coefs.signif <- symnum(results[[d]][[k]][[s]]$sum.coefs[, 3], 
                                                                   corr = FALSE, 
                                                                   na = FALSE, 
                                                                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                                                   symbols = c("***", "**", "*", ".", " "))
          }
          else
          {
            results[[d]][[k]][[s]]$sum.coefs.signif <- symnum(results[[d]][[k]][[s]]$sum.coefs[3], 
                                                                   corr = FALSE, 
                                                                   na = FALSE, 
                                                                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                                                   symbols = c("***", "**", "*", ".", " "))
          }
      }

  for (r in row.names)
  {
    if (indent > 0) for (i in 1:indent) cat("~~")

    cat(sub(".*birth\\.year\\.fac_(\\d+)", "\\1", r))

    #for (model.refgrp in names(results))
    for (d in names(results))
      for (k in names(results[[d]]))
        for (s in names(results[[d]][[k]]))
        {
          if (is.matrix(results[[d]][[k]][[s]]$sum.coefs))
          {
              est <- results[[d]][[k]][[s]]$sum.coefs[r, 1] 
              pval <- results[[d]][[k]][[s]]$sum.coefs[r, 3]

              if (!is.na(est))
                cat(paste("   &   ", round(est,3), results[[d]][[k]][[s]]$sum.coefs.signif[r]))
              else
                cat ("   & ")
          }
          else
            cat ("   & ")
        }

    cat (" \\\\* \n ")

    if (pvalue)
      #for (model.refgrp in names(results))
      for (d in names(results))
        for (k in names(results[[d]]))
          for (s in names(results[[d]][[k]]))
          {
            if (is.matrix(results[[d]][[k]][[s]]$sum.coefs))
            {
                est <- results[[d]][[k]][[s]]$sum.coefs[r, 1] 
                pval <- results[[d]][[k]][[s]]$sum.coefs[r, 3]

                if (!is.na(est))
                  cat(paste("   &   [", round(pval, 3), "]", sep=""))
                else
                  cat ("   &   ")
            }
            else
              cat ("   &   ")

          }

    cat (" \\\\\n ")
  }
}

print.factors <- function(results, regname, desc, pvalue = TRUE, indent = 1)
{
  if (length(levels(results[[1]][[1]][[1]]$fgm[[regname]])) <= 2)
  {
    lvl <- levels(results[[1]][[1]][[1]]$fgm[[regname]])[2]
    fac <- sub("(.)(.+)", "\\U\\1\\E\\2", lvl, perl = TRUE)
    coef.name <- sprintf("%s%s", regname, lvl)

    cat(sprintf("%s\\\\\n", fac))

    if (!is.null(desc))
      cat(sprintf("(%s)", desc))
    #else
    #  cat("}")

    #for (model.refgrp in names(results))
    for (d in names(results))
      for (k in names(results[[d]]))
        for (s in names(results[[d]][[k]]))
        {
          if (regname != k)
          {
              est <- results[[d]][[k]][[s]]$coefs[coef.name, 1] #coef(model)[regname]
              pval <- results[[d]][[k]][[s]]$coefs[coef.name, 4]

              if (!is.na(est))
                cat(paste("   &   ", round(est,3), results[[d]][[k]][[s]]$signif[coef.name]))
              else
                cat ("   & ")
          }
          else
            cat ("   & ")
        }

    cat (" \\\\* \n ")

    if (pvalue)
#      for (model.refgrp in names(results))
      for (d in names(results))
        for (k in names(results[[d]]))
          for (s in names(results[[d]][[k]]))
          {
            if (regname != k)
            {
              est <- results[[d]][[k]][[s]]$coefs[coef.name, 1] #coef(model)[regname]
              pval <- results[[d]][[k]][[s]]$coefs[coef.name, 4]

                if (!is.na(est))
                  cat(paste("   &   [", round(pval, 3), "]", sep=""))
                else
                  cat ("   &   ")
            }
            else
              cat ("   &   ")
          }

    cat (" \\\\\n ")
  }
  else
  {
    lvls <- levels(results[[1]][[1]][[1]]$fgm[[regname]])

    cat(sprintf("%s \\\\\n", desc))

    for (lvl in lvls[2:length(lvls)])
    {
      if (summary(results[[1]][[1]][[1]]$fgm[[regname]])[[lvl]] < 1)
        next

      lvl.name <- sub("(.)(.+)", "\\U\\1\\E\\2", lvl, perl = TRUE) 
      coef.name <- sprintf("%s%s", regname, lvl)

      if (indent > 0) for (i in 1:indent) cat("~~")

      cat(sprintf("  %s", lvl.name))

#      for (model.refgrp in names(results))
      for (d in names(results))
        for (k in names(results[[d]]))
          for (s in names(results[[d]][[k]]))
          {
            #if (regname != model.refgrp)
            {
                est <- results[[d]][[k]][[s]]$coefs[coef.name, 1] #coef(model)[regname]
                pval <- results[[d]][[k]][[s]]$coefs[coef.name, 4]

                if (!is.na(est))
                  cat(paste("   &   ", round(est,3), results[[d]][[k]][[s]]$signif[coef.name]))
                else
                  cat ("   & ")
            }
            #else
            #  cat ("   &   ")
          }

      cat (" \\\\* \n ")

      if (pvalue)
#        for (model.refgrp in names(results))
        for (d in names(results))
          for (k in names(results[[d]]))
            for (s in names(results[[d]][[k]]))
            {
              #if (regname != model.refgrp)
              {
                est <- results[[d]][[k]][[s]]$coefs[coef.name, 1] #coef(model)[regname]
                pval <- results[[d]][[k]][[s]]$coefs[coef.name, 4]

                  if (!is.na(est))
                    cat(paste("   &   [", round(pval, 3), "]", sep=""))
                  else
                    cat ("   &   ")
              }
              #else
              #  cat ("   &   ")
            }

      cat (" \\\\\n ")
    }
  }
}

var.dict <- list(circum = "Circumcision", has.or.intends.circum = "Circumcision/Intention")

fgm.results.outreg <- function(results, var.dict, title = "Regression", label = "", stderr = FALSE, pvalue = TRUE, exclude.foot = FALSE, exclude.k = NULL)
{
  num.dep <- length(results)
  num.models <- 0

  for (d in names(results))
  {
    results[[d]]$num.models <- 0

    for (k in names(results[[d]]))
    {
      if (k %in% exclude.k)
      {
        results[[d]][[k]] <- NULL
        next 
      }
      
      for (s in names(results[[d]][[k]]))
      {
        results[[d]]$num.models <- results[[d]]$num.models + 1
        num.models <- num.models + 1

        results[[d]][[k]][[s]]$coefs <- coeftest(results[[d]][[k]][[s]]$lm, vcov = results[[d]][[k]][[s]]$vcov)
        results[[d]][[k]][[s]]$signif <- symnum(results[[d]][[k]][[s]]$coefs[, 4], 
                                                     corr = FALSE, 
                                                     na = FALSE, 
                                                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                                     symbols = c("***", "**", "*", ".", " "))
      }
    }
  }

  num.col <- num.models + 1

  cat("\\begin{center}\n ")
  cat(sprintf("\\begin{longtable}{*{%i}{l}}\n ", num.col))
  cat(sprintf("\\caption{%s \\label{%s}}\\\\ \n ", title, label)) # TODO \\label{",label,"}\n ")
  cat("\\hline \n")
  cat("\\hline \n")

  for (d in names(results))
    cat(sprintf("& \\multicolumn{%i}{c}{%s} ", results[[d]]$num.models, var.dict[[d]]))

  cat("\\\\ \n")

  model.count <- 1
  for (d in names(results))
    for (k in names(results[[d]]))
      for (s in names(results[[d]][[k]]))
      {
        cat(sprintf("& \\multicolumn{1}{c}{(%i)} ", model.count))
        model.count <- model.count + 1
      }

  cat("\\\\ \n")
  cat("\\hline \n")
  cat("\\endfirsthead \n")

  cat(sprintf("\\multicolumn{%i}{c}{{\\tablename} \\thetable{} -- Continued} \\\\\n", num.col)) 

  for (d in names(results))
    cat(sprintf("& \\multicolumn{%i}{c}{%s} ", results[[d]]$num.models, var.dict[[d]]))

  cat("\\\\ \n")

  model.count <- 1
  for (d in names(results))
    for (k in names(results[[d]]))
      for (s in names(results[[d]][[k]]))
      {
        cat(sprintf("& \\multicolumn{1}{c}{(%i)} ", model.count))
        model.count <- model.count + 1
      }

  cat("\\\\ \n")
  cat("\\hline \n")
  cat("\\endhead \n")

  cat("\\hline \n")
  cat("\\hline \n")
  cat("\\endlastfoot \n")

  cat("Cohort\\\\\n")
  print.cohorts(results, indent = 1)

  cat("Cohort $\\times$ Rich\\\\\n") # (Household Wealth Index)\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "wealth.index.2rich")

  cat("Cohort $\\times$ Rural\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "urban.ruralrural")

  cat("Cohort $\\times$ Christian\\\\\n") # (Household Religion)\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "religionchristian")

  cat("Cohort $\\times \\overline{fgm}_{j(-i),k,t(n)}$\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "grpavg.circum")

  cat("Cohort $\\times \\overline{int}_{j(-i),k,t(n)}$\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "grpavg.has.or.intends.circum")

  cat("Cohort $\\times \\overline{med}_{j(-i),k,t(n)}$\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "grpavg.med5")

  for (regname in names(covar.dict))
  {
    if (is.factor(results[[1]][[1]][[1]]$fgm[[regname]]))
    {
      print.factors(results, regname, covar.dict[[regname]], indent = 1)
    }
    else
    {
      cat(sprintf("%s", covar.dict[[regname]]))

    #  for (model.refgrp in names(results))
      for (d in names(results))
        for (k in names(results[[d]]))
          for (s in names(results[[d]][[k]]))
          {
            #if (model.refgrp != reg.name)
            {
                est <- results[[d]][[k]][[s]]$coefs[regname, 1] #coef(model)[regname]
                se <- results[[d]][[k]][[s]]$coefs[regname, 2] #sqrt(diag(vcov(model)))[regname]
                pval <- results[[d]][[k]][[s]]$coefs[regname, 4]

                if (!is.na(est))
                  cat(paste("   &   ", round(est,3), results[[d]][[k]][[s]]$signif[regname]))
                else
                  cat ("   & ")
            }
            #else
            #  cat ("   &   ")

          }

      cat (" \\\\* \n ")

      if (stderr)
      {
#        for (model.refgrp in names(results))
        for (d in names(results))
          for (k in names(results[[d]]))
            for (s in names(results[[d]][[k]]))
          #if (model.refgrp != regname)
            {
                est <- results[[d]][[k]][[s]]$coefs[regname, 1] #coef(model)[regname]
                se <- results[[d]][[k]][[s]]$coefs[regname, 2] #sqrt(diag(vcov(model)))[regname]

                if (!is.na(est))
                  cat(paste("   &   (",round(se, 3), ")", sep=""))
                else
                  cat ("   &   ")
            }
          #else
          #  cat ("   &   ")

        cat (" \\\\* \n ")
      }

      if (pvalue)
#        for (model.refgrp in names(results))
        for (d in names(results))
          for (k in names(results[[d]]))
            for (s in names(results[[d]][[k]]))
          #if (model.refgrp != regname)
            {
                est <- results[[d]][[k]][[s]]$coefs[regname, 1] #coef(model)[regname]
                pval <- results[[d]][[k]][[s]]$coefs[regname, 4]

                if (!is.na(est))
                  cat(paste("   &   [", round(pval, 3), "]", sep=""))
                else
                  cat ("   &   ")
            }
          #else
          #  cat ("   &   ")

      cat (" \\\\\n ")
    }
  }

  cat("\\hline \n")

  ### Print a row for the number of cases
  cat(paste("N"), sep="")

  for (d in names(results))
    for (k in names(results[[d]]))
      for (s in names(results[[d]][[k]]))
      {
        myDF <- sum(results[[d]][[k]][[s]]$lm$df[-3]) #omit third value from df vector
        cat (paste("   &   ", myDF))
      }

  cat (" \\\\\n ")

  cat(paste("$R^2$"),sep="")
  
  for (d in names(results))
    for (k in names(results[[d]]))
      for (s in names(results[[d]][[k]]))
      {
        r.square <- summary(results[[d]][[k]][[s]]$lm)$r.square
        cat( paste("       &", if (is.numeric(r.square)) round(r.square, 3)))
      }

  cat (" \\\\\n ")
  cat(paste("Adjusted $R^2$"),sep="")
  
  for (d in names(results))
    for (k in names(results[[d]]))
      for (s in names(results[[d]][[k]]))
      {
        adj.r.squared <- summary(results[[d]][[k]][[s]]$lm)$adj.r.squared
        cat( paste("       &", if (is.numeric(adj.r.squared)) round(adj.r.squared, 3)))
      }

  cat ("  \\\\\n ")

  if (!exclude.foot)
  {
    cat("\\end{longtable}\n")
    cat("\\end{center}\n")
  }
}

fgm.outreg <- function(results, covar.dict, title="My Regression", label="", pvalue = TRUE, stderr = FALSE, time.reg = "birth.year.fac", dummies = c("governorate"), which.alter = "circum") # modelLabel = NULL, varLabels = NULL, showAIC=TRUE, lyx=TRUE, varCallback = NULL, isPresentFuncs = NULL, vcov = NULL, tight=TRUE, longtable = FALSE
{
  #which.alter <- 2 # I tried different alternative regressions.  Sticking to the second one for now.
  num.models <- length(results)
  num.col <- num.models + 1

  cat("\\begin{center}\n ")
  cat(sprintf("\\begin{longtable}{*{%i}{l}}\n ", num.col))
  cat(sprintf("\\caption{%s \\label{%s}}\\\\\n ", title, label)) # TODO \\label{",label,"}\n ")

  # TODO Model labels

  cat("           ")

  for (i in 1:num.models) 
    cat (" & Estimate ") 

  cat(" \\\\\n")
  
  if (stderr)
  {
    cat("            ")
    for (i in 1:num.models) 
      cat (" & (S.E.) ")

    cat(" \\\\\n")
  }

  if (pvalue) 
  {
    cat("            ")

    for (i in 1:num.models) 
      cat (" & [p-value] ")

    cat(" \\\\\n")
  }

  cat("\\hline \n \\hline\n ")

  #signif <- vector("list") #, num.models)
  #model.coefs <- vector("list") #, num.models)
  
  for (i in 1:num.models)
  {
    results[[i]][[which.alter]]$coefs <- coeftest(results[[i]][[which.alter]]$lm, vcov = results[[i]][[which.alter]]$vcov)
    results[[i]][[which.alter]]$signif <- symnum(results[[i]][[which.alter]]$coefs[, 4], 
                                                 corr = FALSE, 
                                                 na = FALSE, 
                                                 cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                                 symbols = c("***", "**", "*", ".", " "))
  }

  cat("Cohort\\\\\n")
  print.cohorts(results, indent = 1, which.alter = which.alter)

  cat("Cohort $\\times$ Rich (Household Wealth Index)\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "wealth.index.2rich", which.alter = which.alter)

  cat("Cohort $\\times$ Rural\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "urban.ruralrural", which.alter = which.alter)

  cat("Cohort $\\times$ Christian (Household Religion)\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "religionchristian", which.alter = which.alter)

  cat("Cohort $\\times \\overline{med}_{j(-i),k,t(n)}$\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "grpavg.med", which.alter = which.alter)

  for (regname in names(covar.dict))
  {
    if (is.factor(results[[1]][[which.alter]]$fgm[[regname]]))
    {
      print.factors(results, regname, covar.dict[[regname]], which.alter = which.alter, indent = 1)
    }
    else
    {
      cat(covar.dict[[regname]])

      for (model.refgrp in names(results))
      {
        #if (model.refgrp != reg.name)
        {
            est <- results[[model.refgrp]][[which.alter]]$coefs[regname, 1] #coef(model)[regname]
            se <- results[[model.refgrp]][[which.alter]]$coefs[regname, 2] #sqrt(diag(vcov(model)))[regname]
            pval <- results[[model.refgrp]][[which.alter]]$coefs[regname, 4]

            if (!is.na(est))
              cat(paste("   &   ", round(est,3), results[[model.refgrp]][[which.alter]]$signif[regname]))
            else
              cat ("   &   ")
        }
        #else
        #  cat ("   &   ")

      }

      cat (" \\\\\n ")

      if (stderr)
        for (model.refgrp in names(results))
          #if (model.refgrp != regname)
          {
              est <- results[[model.refgrp]][[which.alter]]$coefs[regname, 1] #coef(model)[regname]
              se <- results[[model.refgrp]][[which.alter]]$coefs[regname, 2] #sqrt(diag(vcov(model)))[regname]

              if (!is.na(est))
                cat(paste("   &   (",round(se, 3), ")", sep=""))
              else
                cat ("   &   ")
          }
          #else
          #  cat ("   &   ")

      cat (" \\\\\n ")

      if (pvalue)
        for (model.refgrp in names(results))
          #if (model.refgrp != regname)
          {
              est <- results[[model.refgrp]][[which.alter]]$coefs[regname, 1] #coef(model)[regname]
              pval <- results[[model.refgrp]][[which.alter]]$coefs[regname, 4]

              if (!is.na(est))
                cat(paste("   &   [", round(pval, 3), "]", sep=""))
              else
                cat ("   &   ")
          }
          #else
          #  cat ("   &   ")

      cat (" \\\\\n ")
    }
  }
  
  cat("\\hline \n")

  ### Print a row for the number of cases
  cat(paste("N"), sep="")

  for (i in 1:num.models)
  {
    myDF <- sum(results[[i]][[which.alter]]$lm$df[-3]) #omit third value from df vector
    cat (paste("   &   ", myDF))
  }

  cat (" \\\\\n ")

  cat(paste("$R^2$"),sep="")
  
  for (i in 1:num.models)
  {
    r.square <- summary(results[[i]][[which.alter]]$lm)$r.square
    cat( paste("       &", if (is.numeric(r.square)) round(r.square, 3)))
  }

  cat(paste("Adjusted $R^2$"),sep="")
  
  for (i in 1:num.models)
  {
    adj.r.squared <- summary(results[[i]][[which.alter]]$lm)$adj.r.squared
    cat( paste("       &", if (is.numeric(adj.r.squared)) round(adj.r.squared, 3)))
  }

  cat ("  \\\\\n ")

  cat("\\hline\\hline\n")

  cat("\\end{longtable}\n")

  cat("\\end{center}\n")
}


calc.grpavg.daughters <- function(df, total.fgm, cohort.range, knetwork, instr.cohort.range = cohort.range, u.cluster = NULL)
{
  current.birth.year <- df$birth.year[1]
  current.governorate <- df$governorate[1]
  current.urban.rural <- df$urban.rural[1]

  grp.country <- total.fgm[(total.fgm[["birth.year"]] <= current.birth.year + cohort.range) & 
                          (total.fgm[["birth.year"]] >= current.birth.year - cohort.range), ]

  inst.grp <- total.fgm[(total.fgm[["birth.year"]] <= current.birth.year + instr.cohort.range) & 
                          (total.fgm[["birth.year"]] >= current.birth.year - instr.cohort.range), ]

  if (is.null(u.cluster)) 
    grp.geo <- grp.country[(grp.country[["governorate"]] == current.governorate), ]
  else
  {
    grp.geo <- df
    
    # This is very important, otherwise I will end up returning all the neighboring observations
    df <- df[df$unique.cluster == u.cluster,]
  }

  if (!is.null(knetwork))
  {
    current.knetwork <- df[1, knetwork]

    grp <- grp.geo[(grp.geo[[knetwork]] == current.knetwork), ]
    grp.larger <- grp.country[(grp.country[[knetwork]] == current.knetwork), ]
    inst.grp <- inst.grp[(inst.grp[[knetwork]] == current.knetwork), ]
  }
  else
  {
    grp <- grp.geo 
    grp.larger <- grp.country
  }

  grp.circum <- grp[grp$circum == 1, ]
  grp.larger.circum <- grp.larger[grp.larger$circum == 1, ]
  grp.geo.circum <- grp.geo[grp.geo$circum == 1, ]
  grp.country.circum <- grp.country[grp.country$circum == 1, ]

  df$grp.size <- nrow(grp)
  df$grp.larger.size <- nrow(grp.larger)
  df$grp.geo.size <- nrow(grp.geo)
  df$grp.country.size <- nrow(grp.country)

  grp.mean <- function(rowid, grp.data, column)
  {
    grp.data <- grp.data[-rowid,]
    
    if (nrow(grp.data) == 0)
      return(NA)

    mean(grp.data[, column], na.rm = TRUE)
  }

  weighted.grp.mean <- function(rowid, grp.data, column) 
  {
    grp.data <- grp.data[-rowid,]

    if (nrow(grp.data) == 0) 
      return(NA)

    w.total <- sum(grp.data$weight)
    grp.data$weight <- grp.data$weight / w.total
    weighted.mean(grp.data[,column], grp.data$weight, na.rm = TRUE)
  }

  if ('has.or.intends.circum' %in% names(df)) 
    df$grpavg.has.or.intends.circum <- vapply(1:nrow(df), grp.mean, 0, grp, "has.or.intends.circum")

  df$grpavg.circum <- vapply(1:nrow(df), grp.mean, 0, grp, "circum") 
  df$grpavg.circum2 <- vapply(1:nrow(df), grp.mean, 0, grp.larger, "circum") 
  df$grpavg.circum3 <- vapply(1:nrow(df), grp.mean, 0, grp.geo, "circum") 
  df$grpavg.circum4 <- vapply(1:nrow(df), grp.mean, 0, grp.country, "circum") 
  df$wgrpavg.circum <- vapply(1:nrow(df), weighted.grp.mean, 0, grp, "circum")

  df$grpavg.med <- vapply(1:nrow(df), grp.mean, 0, grp.circum, "med.circum") 
  df$grpavg.med2 <- vapply(1:nrow(df), grp.mean, 0, grp.larger.circum, "med.circum") 
  df$grpavg.med3 <- vapply(1:nrow(df), grp.mean, 0, grp.geo.circum, "med.circum") 
  df$grpavg.med4 <- vapply(1:nrow(df), grp.mean, 0, grp.country.circum, "med.circum") 
  df$grpavg.med5 <- vapply(1:nrow(df), grp.mean, 0, grp, "med.circum") 
  df$wgrpavg.med <- vapply(1:nrow(df), weighted.grp.mean, 0, grp.circum, "med.circum") 
  df$wgrpavg.med2 <- vapply(1:nrow(df), weighted.grp.mean, 0, grp.larger.circum, "med.circum") 

  df$grpavg.circum_med <- df$grpavg.circum * df$grpavg.med
  df$grpavg.circum_med2 <- df$grpavg.circum * df$grpavg.med2
  df$grpavg.circum_med3 <- df$grpavg.circum * df$grpavg.med3
  df$grpavg.circum_med4 <- df$grpavg.circum * df$grpavg.med4
  df$wgrpavg.circum_med2 <- df$wgrpavg.circum * df$wgrpavg.med2

  if ('has.or.intends.circum' %in% names(df)) 
    df$inst.grpavg.has.or.intends.circum <- mean(inst.grp$has.or.intends.circum, na.rm = TRUE) 

  df$inst.grpavg.circum <- mean(inst.grp$circum, na.rm = TRUE)
  df$inst.grpavg.med <- mean(inst.grp$med.circum, na.rm = TRUE)
  df$inst.grpavg.circum_med <- df$inst.grpavg.circum * df$inst.grpavg.med

  return(df)
}

byrad.calc.grpavg.daughters <- function(u.cluster, df, total.fgm, ...)
{
  cat('.')
  calc.grpavg.daughters(df@data, total.fgm = total.fgm@data, ..., u.cluster = u.cluster)
}

subset.to.regress.daughters <- function(fgm.data, youngest.cohort = 1996, oldest.cohort = NULL, cohort.range = 1, knetwork = "urban.rural", na.rows = NULL, radius = NULL)
{
  if (!is.null(youngest.cohort)) fgm.data <- subset(fgm.data, birth.year <= youngest.cohort)
  if (!is.null(oldest.cohort)) fgm.data <- subset(fgm.data, birth.year >= oldest.cohort)
  if (!is.null(na.rows)) fgm.data <- fgm.data[-na.rows,]

  fgm.data <- subset(fgm.data, circum <= 1)
  
  if (is.null(radius))
    return(do.call(rbind, by(fgm.data, 
                             fgm.data[c(c("birth.year.fac", "governorate"), knetwork)], 
                             calc.grpavg.daughters, 
                             fgm.data, 
                             cohort.range, 
                             knetwork)))
  else
    return(do.call(rbind, by.radius(fgm.data, 
                                    radius, 
                                    indices = fgm.data@data[c(c("birth.year.fac"), knetwork)], # Don't include governorate! We're using GPS instead
                                    byrad.calc.grpavg.daughters, 
                                    total.fgm = fgm.data, 
                                    cohort.range = cohort.range, 
                                    knetwork = knetwork)))
}

dep.var <- c("has.or.intends.circum", "circum") #, "med.circum")
co.var <- c("wealth.index.2", "urban.rural", "educ.lvl", "med.help.permission.fac", "med.help.distance.fac", "med.help.transportation.fac", "marital.age", "mother.circum.fac", "occupation.2.fac", "religion", "partner.educlvl.fac", "order.fac", "hh.head.sex")
se.var <- list("grpavg.med", "grpavg.circum", "grpavg.has.or.intends.circum")


covar.dict <- list(wealth.index.2 = "Wealth Index", urban.rural = NULL, religion = "Household\\\\\nReligion", 
                   educ.lvl = "Mother's\\\\\nEducation Level", partner.educlvl.fac = "Father's\\\\\nEducation Level",
                   med.help.permission.fac = "Difficulty\\\\\nGetting Permission\\\\\nfor Medical Care", 
                   med.help.distance.fac = "Distance\\\\\nto Medical Care", 
                   med.help.transportation.fac = "Difficulty\\\\\nof Transportation\\\\\nto Medical Care", 
                   marital.age = "Mother's Marital Age", mother.circum.fac = "Mother's FGM\\\\\nStatus", occupation.2.fac = "Mother's Occupation",
                   order.fac = "Order of Birth", hh.head.sex = "Sex of \\\\\nHead of Household") 

knetworks <- c("wealth.index.2", "urban.rural", "religion")

fgm.regress <- function(fgm.data, dep.var, co.var = NULL, se.var, knetworks, get.vcov = TRUE, include.govern = TRUE, govern.cohort.interact = TRUE, knet.cohort.interact = TRUE, se.cohort.interact = TRUE, wls = FALSE, oldest.cohort = NULL, radius = NULL, gen.data.only = FALSE, clean.missing = is.null(radius) && !gen.data.only, use.existing = NULL)
{
  if (is.null(use.existing))
  {
    results <- vector("list", length(dep.var))
    names(results) <- dep.var 

    attr(results, "dep.var") <- dep.var
    attr(results, "co.var") <- co.var
    attr(results, "se.var") <- se.var
    attr(results, "knetworks") <- knetworks
    attr(results, "vcov") <- get.vcov
    attr(results, "wls") <- wls

    if (!is.null(radius))
      attr(results, "radius") <- radius
  }
  else
  {
    dep.var <- names(use.existing)
    radius <- attr(use.existing, "radius") 
    wls <- attr(use.existing, "wls")
    if (is.null(co.var)) co.var <- attr(use.existing, "co.var")
  }

  for (dep in dep.var)
  {
    if (is.null(use.existing))
    {
      results[[dep]] <- vector("list", length(knetworks))
      names(results[[dep]]) <- knetworks
    }
    else
    {
      knetworks <- names(use.existing[[dep]])
    }

    for (k in knetworks)
    {
      if (is.null(use.existing))
      {
        results[[dep]][[k]] <- vector("list") 
        k.data <- NULL
      }
      else
      {
        se.var <- strsplit(names(use.existing[[dep]][[k]]), split = ',')
      }

      for (se in se.var)
      {
        se.name <- paste(se, collapse = ",")

        co.var.formula <- if (!is.null(co.var)) paste(co.var[(co.var != k)], collapse = " + ") 
        se.formula <- if (!is.null(se)) 
                        paste(" + ", paste(if (se.cohort.interact) 
                                             paste("birth.year.fac", se, sep = "*") 
                                           else 
                                             se, 
                                           collapse = " + "), 
                                     sep = "") 
                      else 
                        ""

        govern.formula <- ""

        if (include.govern)
          govern.formula <- sprintf("%sgovernorate", if (govern.cohort.interact) "birth.year.fac*" else "")

        reg.formula <- sprintf("%s ~ %s + %s%s %s %s",
                               dep, 
                               govern.formula,
                               if (knet.cohort.interact) "birth.year.fac*" else "",
                               k,
                               se.formula, 
                               if (!is.null(co.var.formula)) paste("+ ", co.var.formula) else "")

        cat(sprintf("\nRegression formula: %s\n\n", reg.formula))

        fgm <- NULL

        if (!is.null(use.existing) && !is.null(use.existing[[dep]][[k]][[se.name]]$fgm))
        {
          fgm <- use.existing[[dep]][[k]][[se.name]]$fgm
        }
        else if (!clean.missing && !is.null(k.data))
        {
          cat("Reusing data subset\n")
          fgm <- k.data
        }
        else
        {
          cat("Retrieving initial data subset...")
          fgm <- subset.to.regress.daughters(fgm.data, knetwork = k, oldest.cohort = oldest.cohort, radius = radius)
          cat("done\n")
        }

        p <- NULL
        
        if (!gen.data.only)
        {
          cat("Running initial regression...")
          p <- if (wls) lm(formula(reg.formula), data = fgm, weights = fgm$weight) else lm(formula(reg.formula), data = fgm)
          cat("done\n")
        }

        if (clean.missing && !is.null(p) && !is.null(na.action(p)))
        {
          cat("Removing rows with missingness...")
          fgm <- subset.to.regress.daughters(fgm, knetwork = k, na.rows = na.action(p), oldest.cohort = oldest.cohort, radius = radius)
          cat("done\n")

          if (!gen.data.only)
          {
            cat("Running main regression...")
            p <- if (wls) lm(formula(reg.formula), data = fgm, weights = fgm$weight) else lm(formula(reg.formula), data = fgm)
            cat("done\n")
          }
        }


        if (is.null(use.existing))
          results[[dep]][[k]][[se.name]] <- list(formula = reg.formula, lm = p, fgm = fgm)
        else
          use.existing[[dep]][[k]][[se.name]] <- list(formula = reg.formula, lm = p, fgm = fgm)

        k.data <- fgm

        if (get.vcov && !gen.data.only)
        {
          cat("Calculating HAC vcov...")
          if (is.null(use.existing))
            results[[dep]][[k]][[se.name]]$vcov <- tryCatch(vcovHAC(p), error = function(e) { print("Could not get vcov") })
          else
            use.existing[[dep]][[k]][[se.name]]$vcov <- tryCatch(vcovHAC(p), error = function(e) { print("Could not get vcov") })      
          cat("done\n")
        }
      }
    }
  }

  if (is.null(use.existing)) results else use.existing
}
