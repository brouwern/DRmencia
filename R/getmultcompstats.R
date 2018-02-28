#extracts info needed for x and x^2 tests from multcomp
print("Dist = Poisson; coefficients will be exp.  If normal data must change Dist = ")

getmultcompstats <- function(mc.out,
                             dist = "poisson"){

  if(dist == "poisson"){print("if dist = Poisson; coefficients will be exp")}

  #extract each part of output for a single model
  cs<-mc.out$test$coefficients
  ts<-mc.out$test$tstat
  ps<-mc.out$test$pvalues
  ss<-mc.out$test$sigma #standard errors

  #put into vector
  x <- c(cs,ts,ps,ss)

  #name vector components
  names(x) <- paste( names(x),
                     c("B","B","t","t","p","p","SE","SE"),sep = ".")

  #exp poisson data
  if(dist == "poisson"){

    #means on real scale
    mc.means <- exp(mc.out$coef)

    #calcualte CIs around mean
    ## need to use model summary table
    mod.summ.tab <- summary(mc.out$model)$coefficients

    beta <- mod.summ.tab[,"Estimate"]
    se <- mod.summ.tab[,"Std. Error"]

    ci.lo <- exp(beta - 1.96*se)
    ci.hi <- exp(beta + 1.96*se)

    names(ci.lo) <- gsub("site","CI.lo",names(ci.lo))
    names(ci.hi) <- gsub("site","CI.hi",names(ci.hi))
  }

  if(dist != "poisson"){
    mc.means <- mc.out$coef

    #calcualte CIs around mean
    ## need to use model summary table
    mod.summ.tab <- summary(mc.out$model)$coefficients

    beta <- mod.summ.tab[,"Estimate"]
    se <- mod.summ.tab[,"Std. Error"]

    ci.lo <- (beta - 1.96*se)
    ci.hi <- (beta + 1.96*se)

    names(ci.lo) <- gsub("site","CI.lo",names(ci.lo))
    names(ci.hi) <- gsub("site","CI.hi",names(ci.hi))
  }


  #vector of ALL site names
  all.sites <- c("siteLa Cueva","siteLa Caoba",
                 "siteMorelia","siteEl Corral",
                 "siteAceitillar" )
  #browser()
  all.sites2 <- gsub("^site","",all.sites)

  #blank vector
  y1 <- rep(NA,5)
  names(y1) <- all.sites

  #blank vector
  y2 <- rep(NA,5)
  names(y2) <- paste("CI.lo",all.sites2,sep = "")
  #blank vector
  y3 <- rep(NA,5)
  names(y3) <- paste("CI.hi",all.sites2,sep = "")

  #align means
  #match up output (has to be done b/c sometimes there are 4 sites
  ## and sometimes there are 5 sites)
  ## very clunky but gets it done
  for(i in 1:length(mc.means)){
    j <- which(names(mc.means)[i] == names(y1))
    y1[j] <-  mc.means[i]
  }

  #align CIs
  #match up output (has to be done b/c sometimes there are 4 sites
  ## and sometimes there are 5 sites)
  ## very clunky but gets it done
  for(i in 1:length(ci.lo)){
    j <- which(names(ci.lo)[i] == names(y2))
    y2[j] <-  ci.lo[i]
  }

  #align CIs
  #match up output (has to be done b/c sometimes there are 4 sites
  ## and sometimes there are 5 sites)
  ## very clunky but gets it done
  for(i in 1:length(ci.hi)){
    j <- which(names(ci.hi)[i] == names(y3))
    y3[j] <-  ci.hi[i]
  }

  return(c(x,   #trend test stats etc
           y1,  #means
           y2,  #ci lo
           y3)) #ci hi
}
