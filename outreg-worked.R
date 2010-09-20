### Paul Johnson
### Adapted from ideas in post in r-help by Dave Armstrong May 8, 2006


###tight means one column per fitted model
###not tight means 2 columns per fitted model

###incoming= either one regression model or a list of regresion models
###title = a string
###modelLabels= a VECTOR of character strings
### varLabels= a LIST of labels linked to variable names (see examples)
### tight= BOOLEAN, indicates results should be on one tight column or two for each model
### showAIC= BOOLEAN should the AIC be displayed for each model?
### lyx=create a table suitable for inclusion in a lyx float.

require(lmtest)

sum.coefs <- function(reg.res, data, time.dummy, vcov = NULL, to.sum = NULL)
{
  ret.mat <- NULL
  row.names <- NULL
  time.lvls <- levels(data[, time.dummy])

  coefs <- coeftest(reg.res, vcov = vcov)

  timeinvar.estimate <- ifelse(!is.null(to.sum), coefs[to.sum, 1], coefs["(Intercept)", 1])

  for (time.lvl in time.lvls)
  {
    lvl.name <- sprintf("%s%s%s",  time.dummy, time.lvl, ifelse(is.null(to.sum), "", paste(":", to.sum, sep = '')))
    where.index <- grep(sprintf("^%s$", gsub("\\.", "\\\\.", lvl.name)), rownames(coefs))

    stopifnot(length(where.index) < 2)

    if (length(where.index) == 0)
      next

    estimate.sum <- coefs[lvl.name, 1] + timeinvar.estimate

    lh <- linearHypothesis(reg.res, sprintf("%s + %s", ifelse(is.null(to.sum), "(Intercept)", to.sum), lvl.name), vcov = vcov)
    fstat.sum <- lh[2,3]
    pval.sum <- lh[2,4]

    ret.mat <- rbind(ret.mat, c(estimate.sum, fstat.sum, pval.sum))
    row.names <- c(row.names, sprintf("%s%s_%s", ifelse(is.null(to.sum), "", paste(to.sum, ":", sep = '')), time.dummy, time.lvl))
  }

  rownames(ret.mat) <- row.names
  colnames(ret.mat) <- c("Estimate", "F statistic", "p-value")
  return(ret.mat)
}

outreg <- function(incoming, title="My Regression", label="", modelLabels=NULL, varLabels=NULL, tight=TRUE, showAIC=TRUE, lyx=TRUE, pvalue = FALSE, longtable = FALSE, varCallback = NULL, isPresentFuncs = NULL, vcov = NULL){

  modelList <- NULL
  
  ## was input just one model, or a list of models?  ###
  if ( "lm" %in% class(incoming)) { ##just one model input
    nmodels <- 1
    modelList <- list(modl1=incoming)
    if (is.null(vcov))
      vcovList <- NULL
    else
      vcovList <- list(vcov1 = vcov)
  } else {
    nmodels <- length(incoming)
    modelList <- incoming
    vcovList <- vcov

    stopifnot(modelLabels == nmodels)
    stopifnot(is.null(vcov) || (length(vcov) == nmodels))
  } 
  
  ## Get a regression summary object for each fitted model
  summaryList <- list()
  fixnames <- vector()
  myModelClass <- vector()
  
  i <-  1
  for (model in modelList){
    summaryList[[i]] <- summary(model)
    
    fixnames <- unique( c( fixnames, names(coef(model))))
    myModelClass[i] <- class(model)[1]
    i <- i+1
  }
  
  nColumns <- ifelse(tight, 1+nmodels, 1 + 2*nmodels) 

  ###If you are just using LaTeX, you need these
  if (lyx == FALSE){
    if (!longtable)
    {
      cat("\\begin{table}\n ")
      cat("\\caption{",title,"}\\label{",label,"}\n ")
    }
  #  else
  #    cat("\\begin{longtable}{*{\n ")
  }

  cat("\\begin{center}\n ")

  if (!longtable)
  {
    cat(paste("\\begin{tabular}{*{",nColumns,"}{l}}\n ", sep=""))
    cat("\\hline\n ")
  }
  else
  {
    cat(paste("\\begin{longtable}{*{",nColumns,"}{l}}\n ", sep=""))
    cat("\\caption{",title," ", "\\label{", label, "}}\\\\\n ") # TODO \\label{",label,"}\n ")
  }

  ### Put model labels on top of each model column, if modelLabels were given
  if (!is.null(modelLabels)){
    cat("     ")
    for (modelLabel in modelLabels){
      if (tight == T) {
        cat(paste("&", modelLabel))
      }else{
        cat(paste("&\\multicolumn{2}{c}{",modelLabel,"}",sep=""))
      }
    }
    cat (" \\\\\n ")
  }
  
  ### Print the headers "Estimate" and "(S.E.)", output depends on tight or other format 
  if (tight == T){
    cat("             ")
    for (i in 1:nmodels) { cat (" & Estimate ") }
    cat(" \\\\\n")
    
    cat("             ")
    for (i in 1:nmodels) {  cat (" & (S.E.) ") }
    cat(" \\\\\n")

    if (pvalue == T) {
      cat("             ")
      for (i in 1:nmodels) {  cat (" & [p-value] ") }
      cat(" \\\\\n")
    }
  }else{
    if (pvalue == T) {
      cat("             ")
      for (i in 1:nmodels) {  cat (" & Estimate & S.E. & P-value") }
      cat(" \\\\\n")
    } else {
      cat("             ")
      for (i in 1:nmodels) { cat (" & Estimate & S.E.") }
      cat(" \\\\\n")
    }
  }
  
  cat("\\hline \n \\hline\n ")

  model.coefs <- as.list(rep(0, length(modelList)))
  signif <- as.list(rep(0, length(modelList)))

  for (i in 1:length(model.coefs)) {
    if (!is.null(vcovList))
      model.coefs[[i]] <- coeftest(modelList[[i]], vcov = vcovList[[i]])
    else
      model.coefs[[i]] <- coeftest(modelList[[i]])

    signif[[i]] <- symnum(model.coefs[[i]][, 4], corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                                                           symbols = c("***", "**", "*", ".", " "))
    #rownames(signif[[i]]) <- rownames(model.coefs[[i]])
  }

  ### Here come the regression coefficients
  for (regname in fixnames) {
    exclude.reg <- FALSE
    if ( !is.null(varLabels[[regname]]) ) 
    { 
      cat(paste("",varLabels[[regname]]), sep="")
    } 
    else if (!is.null(varCallback)) 
    {
      var <- varCallback(regname)
      if (is.null(var)) 
      {
        exclude.reg <- TRUE
      }
      else 
      {
        cat(paste("", var), sep="") 
      }
    } 
    else 
    {
      cat(paste("", regname), sep="")
    }
    
    if (exclude.reg == FALSE)
    {
      #for (model in modelList) {
      i <- 0
      for (coefs in model.coefs) {
        i <- i + 1
        est <- NA
        
        if (regname %in% rownames(coefs))
        {
          est <- coefs[regname, 1] #coef(model)[regname]
          se <- coefs[regname, 2] #sqrt(diag(vcov(model)))[regname]
          pval <- coefs[regname, 4]
        }

        if ( !is.na(est) ) {
          cat (paste("   &   ", round(est,3), signif[[i]][regname]))
          #pval <- pt(abs(est/se), lower.tail=F, df = model$df.residual)
          #if (pval < 0.025) cat("*")
          
          if (tight == F) {
            cat (paste("   &   (", round(se,3),")",sep=""))

            if (pvalue)
              cat(paste(" [", round(pval, 3), "]", sep=""))
          }
        } else {
          cat ("   &   ")
          if (tight == F) 
          {
            cat (" & " )
            if (pvalue)
              cat(" & ")
          }
        }
      }

      cat (" \\\\\n ")
      
      if (tight == T) {
        #for (model in modelList) {
        for (coefs in model.coefs) {
          est <- NA
          
          if (regname %in% rownames(coefs))
          {
            est <- coefs[regname, 1] #coef(model)[regname]
            se <- coefs[regname, 2] #sqrt(diag(vcov(model)))[regname]
          }

          #if (!is.na(est)) cat (paste("   &   (",round(sqrt(diag(vcov(model)))[regname],3)),")",sep="")
          if (!is.na(est))
          {
            cat(paste("   &   (",round(se, 3), ")", sep=""))
          }
          else 
          {
            cat("   &  ")
          }
        }
        cat (" \\\\\n ")

        if (pvalue)
        {
          for (coefs in model.coefs) {
            est <- NA
            
            if (regname %in% rownames(coefs))
            {
              est <- coefs[regname, 1] #coef(model)[regname]
              pval <- coefs[regname, 4]
            }
            #if (!is.na(est)) cat (paste("   &   (",round(sqrt(diag(vcov(model)))[regname],3)),")",sep="")
            if (!is.na(est))
            {
              cat(paste("   &   [", round(pval, 3), "]", sep=""))
            }
            else 
            {
              cat("   &  ")
            }
          }

          cat (" \\\\\n ")
        }
      }
    } 
  }

  cat("\\hline \n")

  if (!is.null(isPresentFuncs))
  {
    for (func in isPresentFuncs)
    {
      printedName <- FALSE

      for (coefs in model.coefs) {
        ret <- func(coefs)

        if (!printedName)
        {
          printedName <- TRUE
          cat(paste("", ret[1], sep=""))
        }

        cat(paste("   &   ", ret[2], sep=""))
      }

      cat("\\\\\n")
    }

    cat("\\hline \n")
  }

  ### Print a row for the number of cases
  cat(paste("N"), sep="")
  for (model in summaryList) {
    myDF <- sum( model$df[-3] ) #omit third value from df vector
    cat (paste("   &   ", myDF))
    if (tight == F) cat("    &")
  }
  cat (" \\\\\n ")


  ### Print a row for the root mean square error
  if ("lm" %in% myModelClass) {
       cat(paste("$RMSE$"),sep="")
       for (model in summaryList) {
         cat( paste("       &", if(is.numeric(model$sigma)) round(model$sigma,3)))
         if (tight == F) cat("    &")
       }
       cat ("  \\\\\n ")
     }
  
 
  ### Print a row for the R-square
  if ("lm" %in% myModelClass) {
     cat(paste("$R^2$"),sep="")
     for (model in summaryList) {
       cat( paste("       &", if(is.numeric(model$r.square))round(model$r.square,3)))
       if (tight == F) cat("    &")
     }
     cat ("  \\\\\n ")
   }

 
  ## Print a row for the model residual deviance
   if ("glm" %in% myModelClass) {
    cat(paste("$Deviance$"),sep="")
    for (model in summaryList) {
      cat (paste("      &", if(is.numeric(model$deviance))round(model$deviance,3)))
      if (tight == F) cat("      &")
    }
    cat ("  \\\\\n ")
  }

  ### Print a row for the model's fit, as -2LLR
  if ("glm" %in% myModelClass) {    
    cat (paste("$-2LLR (Model \\chi^2)$"),sep="")
    for (model in modelList) {
      if (is.numeric(model$deviance)){
        n2llr <- model$null.deviance - model$deviance 
        cat (paste("      &", round(n2llr,3)))
        gmdf <- model$df.null - model$df.residual + 1
       
        if (pchisq(n2llr, df= gmdf, lower.tail=F) < 0.05) {cat ("*")}
      }
    
      else {
        cat ("    &")
      }
      if (tight == F) cat("      &")
    }
    cat ("  \\\\\n ")
  }



  ## Print a row for the model's fit, as -2 LLR
  ### Can't remember why I was multiplying by -2
  
  if (showAIC == T) {
    cat(paste("$AIC$"),sep="")
    for (model in modelList) {
      cat (paste("      &", if(is.numeric(AIC(model)))round(AIC(model),3)))
      if (tight == F) cat("      &")
    }
    cat ("  \\\\\n ")
  }


  
   cat("\\hline\\hline\n")
   #cat ("* $p \\le 0.05$")
   if (!longtable)
   {
     cat("\\end{tabular}\n")
   }
   else
   {
     cat("\\end{longtable}\n")
   }

   cat("\\end{center}\n")

   if (lyx == FALSE){ 
      if (!longtable)
        cat("\\end{table}\n")
   }
 }



# x1 <- rnorm(100)
# x2 <- rnorm(100)
# y1 <- 5*rnorm(100)+3*x1 + 4*x2
# 
# y2 <- rnorm(100)+5*x2
# m1 <- lm (y1~x1)
# m2 <- lm (y1~x2)
# m3 <- lm (y1 ~ x1 + x2)
# gm1 <- glm(y1~x1)
# 
# 
# 
#  outreg(m1,title="My One Tightly Printed Regression", lyx=F )
# 
# 
#  outreg(m1,tight=F,modelLabels=c("Fingers"), title="My Only Spread Out Regressions" ,lyx=F)
#  
# 
#         
#  outreg(list(m1,m2),modelLabels=c("Mine","Yours"),varLabels=list(x1="Billie"), title="My Two Linear Regressions Tightly Printed" ,lyx=F)
# 
# 
#         
#  outreg(list(m1,m2),modelLabels=c("Whatever","Whichever"), title="My Two Linear Regressions Not Tightly  Printed", showAIC=F, lyx=F)
# 
# 
#  outreg(list(m1,m2,m3),title="My Three Linear Regressions", lyx=F)
#         
#         
#  outreg(list(m1,m2,m3),tight=F,modelLabels=c("I Love love love really long titles","Hate Long","Medium"), lyx=F)
# 
#  outreg(list(gm1),modelLabels=c("GLM"), lyx=F)
# 
# 
#  outreg(list(m1,gm1),modelLabels=c("OLS","GLM"), lyx=F)
# 

