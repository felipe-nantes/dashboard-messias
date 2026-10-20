source("R/comum.R",encoding="UTF-8")
if(AMBIENTE!="publicacao")stop("Execute com DASH_AMBIENTE=publicacao na pasta da distribuição.")
if(pode_atualizar_local())stop("O pacote público não pode oferecer atualização local.")
if(file.exists(file.path(PAINEL,"config.local.json")))stop("Configuração deste computador encontrada no pacote.")
inv<-jsonlite::read_json(file.path(PAINEL,"publicacao/inventario_dados.json"),simplifyVector=TRUE)
for(i in seq_len(nrow(inv$arquivos))) {
  f<-file.path(PAINEL,inv$arquivos$arquivo[i])
  stopifnot(file.exists(f),identical(digest::digest(f,algo="sha256",file=TRUE),inv$arquivos$sha256[i]))
}
e<-readRDS(arquivo_dados("estudo.rds"));a<-ler_atualizacao()
validar_dados(e,estudo=TRUE);validar_dados(a)
stopifnot(!is.null(a),sum(e$painel$CASOS)==47662,
  identical(e$painel$POP_15_29,a$painel$POP_15_29[match(paste(e$painel$CD_MUN,e$painel$ANO),paste(a$painel$CD_MUN,a$painel$ANO))]),
  !any(grepl("^([A-Za-z]:|/)",rownames(e$origens))),
  !any(grepl("^([A-Za-z]:|/)",a$origens$arquivo)),
  all(is.na(a$painel$TAXA_BRUTA[a$painel$ANO>2025])))
proibidos<-list.files(PAINEL,recursive=TRUE,all.files=TRUE,pattern="\\.(dbc|part|docx|pdf|zip)$",ignore.case=TRUE)
if(length(proibidos))stop("Arquivos fora do pacote público: ",paste(proibidos,collapse=", "))
lock<-jsonlite::read_json(file.path(PAINEL,"renv.lock"),simplifyVector=FALSE)
source("publicacao/dependencias.R",encoding="UTF-8")
stopifnot(all(DEPENDENCIAS_APP %in% names(lock$Packages)))
cat("OK: pacote público íntegro, caminhos portáveis, população preservada e atualização externa.\n")
