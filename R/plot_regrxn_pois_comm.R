## Build plot
### for poisson, so takes exp() of fitted values
plot_regrxn_pois_comm <- function(dat,
                                  ANOVA.dat,
                                  mod,
                                  mean.of.covar,
                                  alpha. = 0.05,
                                  xlab. = "Site age",
                                  ylab. = "...",
                                  point.size = 2,
                                  ymin. = 0,
                                  ymax. = 30){

  ## main plot
  gg.out <-  ggplot(data = dat,
         aes(y = fit.real,
             x = site.age.cent+mean.of.covar,
             color = site,
             shape = site)) +
    geom_line(aes()) +
    ylim(10,35) +
    xlab(xlab.) +
    ylab(ylab.) +
    scale_shape_manual(values=c(4,0, 1, 2, 5)) +

    geom_ribbon(aes(ymax = upr.real,
                    ymin = lwr.real),
                alpha = alpha.,
                linetype = 0)  +
    ylim(ymin., ymax.) +
    theme(legend.position="none")

  ## add raw data
  gg.out1 <- gg.out + geom_point(data = mod@frame,
                      aes(y = mod@frame[,1],
                          x = site.age.cent+mean.of.covar),
                      size = point.size)

  ## add ANOVA dat
  pd <- position_dodge(0.25)
  gg.out2 <- gg.out1 + geom_point(data = ANOVA.dat,
                                aes(y = fit.real,
                                    x =  site.age.median,
                                    shape = site),
                                size = 5,
                                position = pd) +
                 geom_errorbar(data = ANOVA.dat,
                               aes(x = site.age.median,
                                  ymax = upr.real,
                                   ymin = lwr.real),
                               width = 0)


  return(gg.out2)
}
