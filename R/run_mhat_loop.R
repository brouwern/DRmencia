
run_mhat_loop <- function(spp = focals,
        dat = condition2,
        L0 = "mean"){

  parms <- expand.grid(spp = focals,
                       N = NA,
                       L0. = NA,
                       b.SMA. = NA)

  for(i in 1:length(spp)){

    # print(i)
    # print(spp[i])

    i.wrkng <- which(dat$spp.code == spp[i])

    if(length(i.wrkng) == 0){next}

    #print(dim(dat[, ]))

    sma.i <- sma(mass ~ wing , data = dat[i.wrkng, ],
                 log="xy",
                 method=c("SMA"),
                 type=c("elevation"),
                 multcomp=FALSE,
                 multcompmethod=c("default","adjusted"),
                 robust=FALSE)



    #parameters
    Mi. <- dat[i.wrkng, "mass"]
    Li. <- log(dat[i.wrkng, "wing"])

    # L0 - for each spp or held constant
    L0. <- log(mean(dat[i.wrkng, "wing"]))
    if(L0 == "mean" ){
      L0. <- log(mean(dat$wing))
    }

    b.SMA. <- coef(sma.i)[["slope"]]


    #save parameters



    parms[i, "N"] <- length(i.wrkng)
    parms[i, "L0."] <- L0.
    parms[i, "b.SMA."] <- b.SMA.

    #calculate M.hat
    M.hat.i <- M.hat(Mi = Mi.,
                     Li = Li.,
                     L0 = L0.,
                     b.SMA = b.SMA.)

    #combine orig data with M.hat
    dat$M.hat.i[i.wrkng] <- M.hat.i

    #Calculat residuals
    dat$e.i[i.wrkng]  <- resid(lm(log(mass) ~ log(wing),
                                         data =     dat[i.wrkng, ]))



  }

  return(list(condition = dat,
              params = parms))
}
