source("R/comum.R",encoding="UTF-8")
source("R/analises.R",encoding="UTF-8")
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1)
job <- jsonlite::read_json(args[1],simplifyVector=TRUE)
estudo <- readRDS(arquivo_dados("estudo.rds"))
perfis <- list()
for (i in seq_len(nrow(job$arquivos))) {
  f <- job$arquivos[i,]
  cat("Lendo",f$ano,basename(f$arquivo),"\n"); flush.console()
  stopifnot(identical(digest::digest(f$arquivo,algo="sha256",file=TRUE),f$sha256))
  d <- read.dbc::read.dbc(f$arquivo)
  exigidos <- c("CIRCUNSTAN","ID_MN_RESI","AGENTE_TOX","NU_IDADE_N","CS_SEXO","EVOLUCAO","NU_ANO")
  if (!all(exigidos %in% names(d))) stop("Mudança de esquema no SINAN: faltam campos essenciais.")
  idade <- suppressWarnings(as.integer(as.character(d$NU_IDADE_N)))
  agente <- sprintf("%02d",suppressWarnings(as.integer(as.character(d$AGENTE_TOX))))
  mun <- substr(trimws(as.character(d$ID_MN_RESI)),1,6)
  sel <- which(trimws(as.character(d$CIRCUNSTAN))=="10" & substr(mun,1,2)=="41" &
    idade>=4015 & idade<=4029 & agente %in% names(agentes))
  a <- d[sel,exigidos,drop=FALSE]
  if (nrow(a)) {
    stopifnot(all(as.integer(as.character(a$NU_ANO))==f$ano),all(mun[sel] %in% estudo$espacial$CD_MUN))
    a$IDADE_ANOS <- idade[sel]-4000; a$CD_MUN <- mun[sel]; a$ANO <- as.integer(f$ano)
    perfis[[as.character(f$ano)]] <- agregar_perfis(a)
  }
  rm(d,a); invisible(gc())
}
p <- do.call(rbind,perfis)
stopifnot(all(2014:2025 %in% job$arquivos$ano),nrow(p)>0)
out <- recalcular(estudo,p,job$arquivos$ano)
out$meta <- modifyList(out$meta,list(id=job$id,tipo="sinan",titulo="Atualização automática SINAN",
  extraido_em=job$consultado_em, extracao_informada=NULL, periodo=range(job$arquivos$ano),
  criado=format(Sys.time(),"%Y-%m-%d %H:%M:%S %z"),lisa_pendente=FALSE,
  referencia_lisa=REFERENCIA_LISA,status_lisa="recalculo_exploratorio_com_metodo_de_referencia_confirmado",
  descricao="Agregados e análises exploratórias recalculados; referência acadêmica preservada."))
out$origens <- job$arquivos
validar_dados(out)
# Comparação de revisões por ano, sem sobrescrever a referência acadêmica.
antes <- aggregate(CASOS~ANO,estudo$painel,sum); names(antes)[2] <- "ESTUDO"
depois <- aggregate(CASOS~ANO,out$painel,sum); names(depois)[2] <- "ATUALIZADO"
comparacao <- merge(antes,depois,by="ANO",all=TRUE)
comparacao$DIFERENCA <- comparacao$ATUALIZADO-comparacao$ESTUDO
out$comparacao <- comparacao
saveRDS(out,file.path(job$diretorio,"dados.rds"),compress="xz")
write.csv(comparacao,file.path(job$diretorio,"revisoes_por_ano.csv"),row.names=FALSE,fileEncoding="UTF-8")
jsonlite::write_json(out$meta,file.path(job$diretorio,"manifesto.json"),pretty=TRUE,auto_unbox=TRUE)
cat("VALIDADO:",sum(out$painel$CASOS),"notificações;",length(job$arquivos$ano),"anos.\n")
