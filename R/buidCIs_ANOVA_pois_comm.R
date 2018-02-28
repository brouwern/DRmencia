
## Build prediction interval for 1-way ANOVA data
### Poisson model (has "i" for overdisp)
### community data
### 1-way ANOVA model, means parameteriztion
buidCIs_ANOVA_pois_comm <- function(mod, mod.type = "poisson"){
  #new data

  if(mod.type == "poisson"){
    newdat <- expand.grid(
      i = mod@frame$i[1]

      ,year = unique(mod@frame$year)[1]
      ,site = unique(mod@frame$site)
      #,site.age.cent = unique(mod@frame$site.age.cent)
    )
  }



  if(mod.type != "poisson"){
    newdat <- expand.grid(
      #i = mod@frame$i[1]

      year = unique(mod@frame$year)[1]
      ,site = unique(mod@frame$site)
      #,site.age.cent = unique(mod@frame$site.age.cent)
    )
  }

  #predict output
  out <- merTools::predictInterval(mod,
                                   newdata =   newdat,
                                   which = "fixed",
                                   level = 0.95,
                                   #n.sims = 10,
                                   stat = "median",
                                   include.resid.var = FALSE)
  #combine
  out <- cbind(newdat,out)

  temp <- data.frame(site = c("La Cueva","La Caoba",
                              "Morelia","El Corral",
                              "Aceitillar"),
                     site.age.median = c(4,
                                         7,
                                         12,
                                         22,
                                         NA))

  out2 <- merge(out, temp)
  return(out2)
}
