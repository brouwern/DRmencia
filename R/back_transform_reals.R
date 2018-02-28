## transform fitted values to real scale
### Use for plotting output from merTools::predictInterval()
back_transform_reals <- function(dat,fnxn = "exp"){
  if(fnxn == "exp"){
    dat$fit.real <- exp(dat$fit)
    dat$lwr.real <- exp(dat$lwr)
    dat$upr.real <- exp(dat$upr)

  }

  if(fnxn == "invlogit" | fnxn == "inv.logit"){
    library(arm)
    dat$fit.real <- invlogit(dat$fit)
    dat$lwr.real <- invlogit(dat$lwr)
    dat$upr.real <- invlogit(dat$upr)

  }

  if(fnxn == "none"){
        dat$fit.real <- (dat$fit)
    dat$lwr.real <- (dat$lwr)
    dat$upr.real <- (dat$upr)

  }

  return(dat)
}

