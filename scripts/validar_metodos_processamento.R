source("R/comum.R",encoding="UTF-8")
source("R/analises.R",encoding="UTF-8")
e<-readRDS(arquivo_dados("estudo.rds"));validar_dados(e,estudo=TRUE)
cat("Conferindo a reprodução dos métodos antes de processar novas extrações...\n")
r<-recalcular(e,e$perfis,2014:2025)
chave<-function(x)paste(x$CD_MUN,x$ANO)
a<-r$painel[match(chave(e$painel),chave(r$painel)),]
stopifnot(identical(as.numeric(a$CASOS),as.numeric(e$painel$CASOS)),
  max(abs(a$TAXA_EBS-e$painel$TAXA_EBS))<.011,
  max(abs(r$espacial$TXMDCMDPO-e$espacial$TXMDCMDPO))<1e-12,
  identical(r$espacial$QUAD_PRM,e$espacial$QUAD_PRM),
  max(abs(r$espacial$LISA_P_PRM-e$espacial$LISA_P_PRM))<1e-7,
  identical(r$tendencias$TENDENCIA,e$tendencias$TENDENCIA),
  abs(r$meta$moran_I-e$meta$moran_I)<1e-10)
cat("OK: contagens, taxas, Bayes, LISA por permutação, Moran e tendências reproduzem o estudo.\n")
