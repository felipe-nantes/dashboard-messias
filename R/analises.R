# Reprocessamento das versões automáticas. A versão recebida nunca é sobrescrita.
bayes_local <- function(casos, pop, nb) {
  vapply(seq_along(casos), function(i) {
    viz <- unique(c(i, nb[[i]][nb[[i]] > 0]))
    media <- sum(casos[viz]) / sum(pop[viz])
    variancia <- sum(pop[viz] * (casos[viz]/pop[viz]-media)^2)/sum(pop[viz]) - media/mean(pop[viz])
    if (!is.finite(variancia) || variancia <= 0) return(media)
    peso <- max(0,min(1,variancia/(variancia+media/pop[i])))
    peso * casos[i]/pop[i] + (1-peso)*media
  }, numeric(1))
}
tendencia_mk <- function(y, anos) {
  t <- suppressWarnings(stats::cor.test(anos,y,method="kendall"))
  tau <- unname(t$estimate); p <- t$p.value
  if (!is.finite(tau)) { tau <- 0; p <- 1 }
  pares <- combn(seq_along(y), 2)
  sen <- median((y[pares[2,]]-y[pares[1,]])/(anos[pares[2,]]-anos[pares[1,]]))
  data.frame(MK_TAU=round(tau,4), MK_PVAL=round(p,5), SEN_SLOPE=round(sen,4),
    TENDENCIA=if(p<.05 && tau>0) "Aumento Significativo" else if(p<.05 && tau<0) "Queda Significativa" else "Estável / Sem Tendência")
}
recalcular <- function(estudo, perfis, anos) {
  ids <- estudo$espacial$CD_MUN
  grid <- expand.grid(CD_MUN=ids, ANO=sort(anos), stringsAsFactors=FALSE)
  grid$NM_MUN <- estudo$espacial$NM_MUN[match(grid$CD_MUN,ids)]
  cont <- aggregate(N ~ CD_MUN+ANO, perfis, sum)
  key <- function(d) paste(d$CD_MUN,d$ANO,sep="_")
  grid$CASOS <- cont$N[match(key(grid),key(cont))]; grid$CASOS[is.na(grid$CASOS)] <- 0
  grid$POP_15_29 <- estudo$painel$POP_15_29[match(key(grid),key(estudo$painel))]
  grid$TAXA_BRUTA <- round(grid$CASOS/grid$POP_15_29*100000,2)
  grid$TAXA_EBS <- NA_real_
  lw <- spdep::nb2listw(estudo$nb,style="W",zero.policy=TRUE)
  morans <- list()
  for (ano in sort(unique(grid$ANO))) {
    ii <- which(grid$ANO==ano)
    stopifnot(identical(grid$CD_MUN[ii],ids))
    if (anyNA(grid$POP_15_29[ii])) next
    eb <- round(bayes_local(grid$CASOS[ii],grid$POP_15_29[ii],estudo$nb)*100000,2)
    grid$TAXA_EBS[ii] <- eb
    m <- spdep::moran.test(eb,lw,zero.policy=TRUE)
    morans[[as.character(ano)]] <- data.frame(ANO=ano,I_Moran_Global=round(unname(m$estimate[1]),4),
      p_valor=format.pval(m$p.value,digits=4), Media_Taxa_Bruta=round(mean(grid$TAXA_BRUTA[ii]),2),Media_Taxa_EBS=round(mean(eb),2))
  }
  sp <- estudo$espacial
  p5 <- grid[grid$ANO %in% 2021:2025,]
  stopifnot(nrow(p5)==399*5, !anyNA(p5$POP_15_29))
  for (ano in 2021:2025) {
    a <- p5[p5$ANO==ano,]
    sp[[paste0("C1529_",ano)]] <- a$CASOS[match(ids,a$CD_MUN)]
    j<-match(ids,a$CD_MUN)
    sp[[paste0("POP",substr(ano,3,4))]]<-a$POP_15_29[j]
    sp[[paste0("TAX1529_",substr(ano,3,4))]]<-a$CASOS[j]/a$POP_15_29[j]*100000
  }
  # O derivado de 15–29 anos não transporta contagens de outra faixa etária.
  sp[c("CAS3049_TOT","CAS3049_MED")]<-NULL
  n <- aggregate(CASOS~CD_MUN,p5,sum)
  pop <- aggregate(POP_15_29~CD_MUN,p5,mean)
  sp$CAS1529_TOT <- n$CASOS[match(ids,n$CD_MUN)]
  sp$CAS1529_MED <- sp$CAS1529_TOT/5
  sp$POP1529_MED <- pop$POP_15_29[match(ids,pop$CD_MUN)]
  sp$TAX1529_MED <- sp$CAS1529_MED/sp$POP1529_MED*100000
  x <- spdep::EBlocal(sp$CAS1529_MED,sp$POP1529_MED,estudo$nb,zero.policy=TRUE,geoda=TRUE)$est
  sp$TXMDCMDPO <- x; sp$TXS10 <- x*10000; sp$TAXA_BAYES_100MIL <- x*100000
  z <- as.vector(scale(x)); zl <- as.vector(scale(spdep::lag.listw(lw,x,zero.policy=TRUE)))
  quad <- ifelse(z>0,ifelse(zl>0,"HH","HL"),ifelse(zl>0,"LH","LL"))
  ana <- spdep::localmoran(x,lw,zero.policy=TRUE)
  set.seed(42); prm <- spdep::localmoran_perm(x,lw,nsim=999,zero.policy=TRUE)
  sp$LISA_I <- round(ana[,1],6); sp$LISA_P_ANA <- round(ana[,5],5)
  sp$LISA_P_PRM <- round(prm[,"Pr(folded) Sim"],5)
  sp$LISA_P_BID <- round(prm[,"Pr(z != E(Ii)) Sim"],5)
  sp$QUAD_ANA <- ifelse(sp$LISA_P_ANA<.05,quad,"NS")
  sp$QUAD_PRM <- ifelse(sp$LISA_P_PRM<.05,quad,"NS")
  sp$QUAD_OFIC <- NA_character_
  divergente<-sp$QUAD_ANA!=sp$QUAD_PRM
  sp$DIF_ANA_PRM<-ifelse(divergente,paste0("Sim (",sum(divergente)," mun)"),"Não (Concordante)")
  sp$DIF_ANA_HIST<-NA_character_
  sp$DIF_PRM_HIST<-NA_character_
  set.seed(42); m <- spdep::moran.mc(x,lw,nsim=999,zero.policy=TRUE)
  mg <- spdep::moran.test(x,lw,zero.policy=TRUE)
  tr <- do.call(rbind,lapply(ids,function(id) {
    a <- grid[grid$CD_MUN==id & grid$ANO %in% 2014:2025,]
    a <- a[order(a$ANO),]; stopifnot(nrow(a)==12,!anyNA(a$TAXA_EBS))
    cbind(data.frame(CD_MUN=id,NM_MUN=sp$NM_MUN[match(id,ids)]),tendencia_mk(a$TAXA_EBS,a$ANO))
  }))
  out <- estudo
  out$painel <- grid; out$perfis <- perfis; out$espacial <- sp; out$tendencias <- tr
  out$moran <- do.call(rbind,morans)
  out$meta$moran_I <- unname(mg$estimate[1]); out$meta$moran_p <- m$p.value
  out
}
