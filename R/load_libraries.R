## Libraries used for main analyses of Mencia-Aceitillar data

load_libraries <- function(){
  #base glmm function
  library(lme4)

  #allows models with mildly informative priors
  ## improves convergned
  library(blme)

  # for checking convergence warnings from lme4
  library(numDeriv)

  library(afex)

  library(foreach)

  library(multcomp)

  library(lmerTest)

  library(reshape2)

  library(vegan)

  # AIC calculation
  library(bbmle)

  # Estiamted SEs of BLUPs
  library(arm)

  # Confidence intervals for predicitons from lme4
  library(merTools)

  # Plotting
  library(ggplot2)
  library(cowplot)

  # Plotting glmms
  library(sjPlot)
}

