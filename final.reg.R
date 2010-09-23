library(plm)
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

print.cohorts <- function(results, coef.name = NULL, indent = 0, pvalue = TRUE, which.alter = 2)
{
  row.names <- NULL
  
  for (i in 1:length(results))
  {
    results[[i]][[which.alter]]$sum.coefs <- sum.coefs(results[[i]][[which.alter]]$lm, 
                                                       results[[i]][[which.alter]]$fgm, 
                                                       "birth.year.fac", 
                                                       results[[i]][[which.alter]]$vcov,
                                                       coef.name)

    if (is.null(row.names) & (is.matrix(results[[i]][[which.alter]]$sum.coefs)))
      row.names <- rownames(results[[i]][[which.alter]]$sum.coefs)
  }

  for (i in 1:length(results))
    if (is.matrix(results[[i]][[which.alter]]$sum.coefs))
      results[[i]][[which.alter]]$sum.coefs.signif <- symnum(results[[i]][[which.alter]]$sum.coefs[, 3], 
                                                             corr = FALSE, 
                                                             na = FALSE, 
                                                             cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                                             symbols = c("***", "**", "*", ".", " "))
    else
      results[[i]][[which.alter]]$sum.coefs.signif <- symnum(results[[i]][[which.alter]]$sum.coefs[3], 
                                                             corr = FALSE, 
                                                             na = FALSE, 
                                                             cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                                             symbols = c("***", "**", "*", ".", " "))

  for (r in row.names)
  {
    if (indent > 0) for (i in 1:indent) cat("  ")

    cat(sub(".*birth\\.year\\.fac_(\\d+)", "\\1", r))

    for (model.refgrp in names(results))
    {
      if (is.matrix(results[[model.refgrp]][[which.alter]]$sum.coefs))
      {
          est <- results[[model.refgrp]][[which.alter]]$sum.coefs[r, 1] #coef(model)[regname]
          #se <- results[[model.refgrp]][[which.alter]]$coefs[regname, 2] #sqrt(diag(vcov(model)))[regname]
          pval <- results[[model.refgrp]][[which.alter]]$sum.coefs[r, 3]

          if (!is.na(est))
            cat(paste("   &   ", round(est,3), results[[model.refgrp]][[which.alter]]$sum.coefs.signif[r]))
          else
            cat ("   &   ")
      }
      else
        cat ("   &   ")
    }

    cat (" \\\\\n ")

    if (pvalue)
      for (model.refgrp in names(results))
      {
        if (is.matrix(results[[model.refgrp]][[which.alter]]$sum.coefs))
        {
            est <- results[[model.refgrp]][[which.alter]]$sum.coefs[r, 1] #coef(model)[regname]
            pval <- results[[model.refgrp]][[which.alter]]$sum.coefs[r, 3]

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

print.factors <- function(results, regname, desc, pvalue = TRUE, which.alter = 2)
{
  if (length(levels(results[[1]][[which.alter]]$fgm[[regname]])) <= 2)
  {
    lvl <- levels(results[[1]][[which.alter]]$fgm[[regname]])[2]
    fac <- sub("(.)(.+)", "\\U\\1\\E\\2", lvl, perl = TRUE)
    coef.name <- sprintf("%s%s", regname, lvl)

    cat(fac)

    if (!is.null(desc))
      cat(sprintf(" (%s)", desc))

    for (model.refgrp in names(results))
    {
      if (regname != model.refgrp)
      {
          est <- results[[model.refgrp]][[which.alter]]$coefs[coef.name, 1] #coef(model)[regname]
          pval <- results[[model.refgrp]][[which.alter]]$coefs[coef.name, 4]

          if (!is.na(est))
            cat(paste("   &   ", round(est,3), results[[model.refgrp]][[which.alter]]$signif[coef.name]))
          else
            cat ("   &   ")
      }
      else
        cat ("   &   ")
    }

    cat (" \\\\\n ")

    if (pvalue)
      for (model.refgrp in names(results))
      {
        if (regname != model.refgrp)
        {
            est <- results[[model.refgrp]][[which.alter]]$coefs[coef.name, 1] #coef(model)[regname]
            pval <- results[[model.refgrp]][[which.alter]]$coefs[coef.name, 4]

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
    lvls <- levels(results[[1]][[which.alter]]$fgm[[regname]])

    cat(sprintf("%s \\\\\n", desc))

    for (lvl in lvls[2:length(lvls)])
    {
      if (summary(results[[1]][[which.alter]]$fgm[[regname]])[[lvl]] < 1)
        next

      lvl.name <- sub("(.)(.+)", "\\U\\1\\E\\2", lvl, perl = TRUE) 
      coef.name <- sprintf("%s%s", regname, lvl)

      cat(sprintf("  %s", lvl.name))

      for (model.refgrp in names(results))
      {
        #if (regname != model.refgrp)
        {
            est <- results[[model.refgrp]][[which.alter]]$coefs[coef.name, 1] #coef(model)[regname]
            pval <- results[[model.refgrp]][[which.alter]]$coefs[coef.name, 4]

            if (!is.na(est))
              cat(paste("   &   ", round(est,3), results[[model.refgrp]][[which.alter]]$signif[coef.name]))
            else
              cat ("   &   ")
        }
        #else
        #  cat ("   &   ")
      }

      cat (" \\\\\n ")

      if (pvalue)
        for (model.refgrp in names(results))
        {
          #if (regname != model.refgrp)
          {
              est <- results[[model.refgrp]][[which.alter]]$coefs[coef.name, 1] #coef(model)[regname]
              pval <- results[[model.refgrp]][[which.alter]]$coefs[coef.name, 4]

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

fgm.outreg <- function(results, covar.dict, title="My Regression", label="", pvalue = TRUE, time.reg = "birth.year.fac", dummies = c("governorate")) # modelLabel = NULL, varLabels = NULL, showAIC=TRUE, lyx=TRUE, varCallback = NULL, isPresentFuncs = NULL, vcov = NULL, tight=TRUE, longtable = FALSE
{
  which.alter <- 2 # I tried different alternative regressions.  Sticking to the second one for now.
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
  
  cat("            ")
  for (i in 1:num.models) 
    cat (" & (S.E.) ")

  cat(" \\\\\n")

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
  print.cohorts(results, indent = 1)

  cat("Cohort \\times Rich (Household Wealth Index)\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "wealth.index.2rich")

  cat("Cohort \\times Rural\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "urban.ruralrural")

  cat("Cohort \\times Christian (Household Religion)\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "religionchristian")

  cat("Cohort \\times \\overline{med_{j(-i),k,t(n)}}\\\\\n")
  print.cohorts(results, indent = 1, coef.name = "grpavg.med")

  for (regname in names(covar.dict))
  {
    if (is.factor(results[[1]][[which.alter]]$fgm[[regname]]))
    {
      print.factors(results, regname, covar.dict[regname])
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

        cat (" \\\\\n ")
      }

      for (model.refgrp in names(results))
      {
        if (model.refgrp != regname)
        {
            est <- results[[model.refgrp]][[which.alter]]$coefs[regname, 1] #coef(model)[regname]
            se <- results[[model.refgrp]][[which.alter]]$coefs[regname, 2] #sqrt(diag(vcov(model)))[regname]

            if (!is.na(est))
              cat(paste("   &   (",round(se, 3), ")", sep=""))
            else
              cat ("   &   ")
        }
        else
          cat ("   &   ")

        cat (" \\\\\n ")
      }

      if (pvalue)
        for (model.refgrp in names(results))
        {
          if (model.refgrp != regname)
          {
              est <- results[[model.refgrp]][[which.alter]]$coefs[regname, 1] #coef(model)[regname]
              pval <- results[[model.refgrp]][[which.alter]]$coefs[regname, 4]

              if (!is.na(est))
                cat(paste("   &   [", round(pval, 3), "]", sep=""))
              else
                cat ("   &   ")
          }
          else
            cat ("   &   ")

          cat (" \\\\\n ")
        }
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
    cat( paste("       &", if (is.numeric(results[[i]][[which.alter]]$lm$r.square)) round(results[[i]][[which.alter]]$lm$r.square, 3)))

  cat ("  \\\\\n ")

  cat("\\hline\\hline\n")

  cat("\\end{longtable}\n")

  cat("\\end{center}\n")
}


calc.grpavg.daughters <- function(df, total.fgm, cohort.range, knetwork, instr.cohort.range = cohort.range)
{
  current.birth.year <- df$birth.year[1]
  current.governorate <- df$governorate[1]
  current.urban.rural <- df$urban.rural[1]

  grp.country <- total.fgm[(total.fgm[["birth.year"]] <= current.birth.year + cohort.range) & 
                          (total.fgm[["birth.year"]] >= current.birth.year - cohort.range), ]

  inst.grp <- total.fgm[(total.fgm[["birth.year"]] <= current.birth.year + instr.cohort.range) & 
                          (total.fgm[["birth.year"]] >= current.birth.year - instr.cohort.range), ]

  grp.geo <- grp.country[(grp.country[["governorate"]] == current.governorate), ]

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

  if ('has.or.intends.circum' %in% names(df)) 
    df$grpavg.has.or.intends.circum <- vapply(1:nrow(df), function(rowid) mean(grp[-rowid, "has.or.intends.circum"], na.rm = TRUE), 0) 

  grp.mean <- function(rowid, grp.data, column) mean(grp.data[-rowid, column], na.rm = TRUE)

  df$grpavg.circum <- vapply(1:nrow(df), grp.mean, 0, grp, "circum") 
  df$grpavg.circum2 <- vapply(1:nrow(df), grp.mean, 0, grp.larger, "circum") 
  df$grpavg.circum3 <- vapply(1:nrow(df), grp.mean, 0, grp.geo, "circum") 
  df$grpavg.circum4 <- vapply(1:nrow(df), grp.mean, 0, grp.country, "circum") 
  df$grpavg.med <- vapply(1:nrow(df), grp.mean, 0, grp, "med.circum") 
  df$grpavg.med2 <- vapply(1:nrow(df), grp.mean, 0, grp.larger, "med.circum") 
  df$grpavg.med3 <- vapply(1:nrow(df), grp.mean, 0, grp.geo, "med.circum") 
  df$grpavg.med4 <- vapply(1:nrow(df), grp.mean, 0, grp.country, "med.circum") 
  df$grpavg.circum_med <- df$grpavg.circum * df$grpavg.med
  df$grpavg.circum_med2 <- df$grpavg.circum * df$grpavg.med2
  df$grpavg.circum_med3 <- df$grpavg.circum * df$grpavg.med3
  df$grpavg.circum_med4 <- df$grpavg.circum * df$grpavg.med4
  df$grpavg.circum3_med <- df$grpavg.circum3 * df$grpavg.med

  if ('has.or.intends.circum' %in% names(df)) 
    df$inst.grpavg.has.or.intends.circum <- mean(inst.grp$has.or.intends.circum, na.rm = TRUE) 

  df$inst.grpavg.circum <- mean(inst.grp$circum, na.rm = TRUE)
  df$inst.grpavg.med <- mean(inst.grp$med.circum, na.rm = TRUE)
  df$inst.grpavg.circum_med <- df$inst.grpavg.circum * df$inst.grpavg.med

  return(df)
}

subset.to.regress.daughters <- function(fgm.data, youngest.cohort = 1996, oldest.cohort = NULL, cohort.range = 1, knetwork = "urban.rural", na.rows = NULL)
{
  if (!is.null(youngest.cohort)) fgm.data <- subset(fgm.data, birth.year <= youngest.cohort)
  if (!is.null(oldest.cohort)) fgm.data <- subset(fgm.data, birth.year >= oldest.cohort)
  if (!is.null(na.rows)) fgm.data <- fgm.data[-na.rows,]

  fgm.data <- subset(fgm.data, circum <= 1)
  
  do.call(rbind, by(fgm.data, fgm.data[c(c("birth.year.fac", "governorate"), knetwork)], calc.grpavg.daughters, fgm.data, cohort.range, knetwork))
}


dep.var <- "circum"
co.var <- c("wealth.index.2", "urban.rural", "educ.lvl", "med.help.permission.fac", "med.help.distance.fac", "med.help.transportation.fac", "marital.age", "mother.circum.fac", "occupation.2.fac", "partner.occupation.2.fac", "religion", "partner.educlvl.fac")
se.var <- "grpavg.med"


covar.dict <- list(wealth.index.2 = "Wealth Index", urban.rural = NULL, religion = "Household Religion", 
                   educ.lvl = "Mother's Education Level", partner.educlvl.fac = "Father's Education Level",
                   med.help.permission.fac = "Difficult Getting Permission for Medical Care", 
                   med.help.distance.fac = "Distance to Medical Care", 
                   med.help.transportation.fac = "Difficulty of Transportation to Medical Care", 
                   marital.age = "Mother's Marital Age", mother.circum.fac = "Mother's FGM Status", occupation.2.fac = "Mother's Occupation") 

knetworks <- c("wealth.index.2", "urban.rural", "religion")
#knetworks <- c("wealth.index.2")

results <- vector("list", length(knetworks))
names(results) <- knetworks

num.alter <- 2

for (k in names(results))
{
  results[[k]] <- vector("list", num.alter)

  for (alter in 1:num.alter)
  {
    co.var.formula <- paste(co.var[(co.var != k) & ((alter == 1) | (co.var != "partner.occupation.2.fac"))], collapse = " + ")

    reg.formula <- sprintf("%s ~ birth.year.fac*governorate + birth.year.fac*%s + birth.year.fac*%s + %s",
                           dep.var,
                           k,
                           se.var,
                           co.var.formula)

    print(reg.formula)

    fgm <- subset.to.regress.daughters(fgm.data.daughters.08@data, knetwork = k)
    p <- lm(formula(reg.formula), data = fgm)

    if (!is.null(na.action(p)))
    {
      fgm <- subset.to.regress.daughters(fgm, knetwork = k, na.rows = na.action(p))
      p <- lm(formula(reg.formula), data = fgm)
    }

    vac <- vcovHAC(p)

    results[[k]][[alter]] <- list(formula = reg.formula, lm = p, vcov = vac, fgm = fgm)
  }
}

for (k in names(results))
{
  coef.names <- names(coef(results[[k]][[1]]$lm))
  print(linearHypothesis(results[[k]][[1]]$lm, coef.names[grep("partner.occupation", coef.names)], vcov = results[[k]][[1]]$vcov))
}

for (k in names(results))
{
  print(sum.coefs(results[[k]][[2]]$lm, results[[k]][[2]]$fgm, "birth.year.fac", vcov = results[[k]][[2]]$vcov, "grpavg.med"))
}
