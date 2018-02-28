



## Build ANOVA plot
### for poisson, so takes exp() of fitted values
plot_ANOVA_pois_comm <- function(dat,
                                  mod,
                                  mod.dat.i,
                                  #mean.of.covar,
                                  xlab. = "",
                                  ylab. = "",
                                  alpha. = 0.125,
                                 ymax. = 35,
                                 ymin. = 10,
                                 point.sz = 5){

  ## main plot
  gg.out <-  ggplot(data = dat,
                    aes(y = fit.real,
                        x = site)) +
    geom_line(aes(),
              size = 1) +
    ylim(10,35) +
    xlab(xlab.) +
    ylab(ylab.) +
    geom_errorbar(aes(ymax = upr.real,
                    ymin = lwr.real),
                width = 0)  +
    geom_point(size = point.sz) +
    theme(legend.position="none") +
    ylim(ymin., ymax.)

  ## add raw data
  pd <- position_dodge(0.25)
  gg.out <- gg.out + geom_jitter(data = mod@frame[mod.dat.i,],
                                #position = pd,
                                width  = 0.125,
                                shape = 10,
                                aes(y = mod@frame[mod.dat.i,1],
                                    x = site)) +
    theme(axis.line.y=element_blank(),
          #axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          #axis.title.x=element_blank(),
          axis.title.y=element_blank())


  return(gg.out)
}




