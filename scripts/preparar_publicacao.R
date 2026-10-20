source("R/comum.R",encoding="UTF-8")
args<-commandArgs(trailingOnly=TRUE)
destino<-if(length(args))caminho_configurado(args[1]) else file.path(PAINEL,"publicacao/pacotes",paste0("dashboard_",format(Sys.time(),"%Y%m%d_%H%M%S")))
if(dir.exists(destino) && length(list.files(destino,all.files=TRUE,no..=TRUE)))stop("O destino precisa ser uma pasta nova ou vazia. Nenhuma distribuição existente é sobrescrita.")
originais<-c(arquivo_dados("estudo.rds"),arquivo_dados("atualizacao_atual.json"))
hashes_antes<-vapply(originais,digest::digest,"",algo="sha256",file=TRUE)
estudo<-readRDS(arquivo_dados("estudo.rds"));validar_dados(estudo,estudo=TRUE)
atual<-ler_atualizacao()
if(is.null(atual))stop("Prepare uma atualização válida antes de gerar a distribuição.")
ponteiro<-jsonlite::read_json(arquivo_dados("atualizacao_atual.json"),simplifyVector=TRUE)
dir.create(destino,recursive=TRUE,showWarnings=FALSE)
copiar<-function(origem,alvo=origem) {
  entrada<-file.path(PAINEL,origem);saida<-file.path(destino,alvo)
  if(!file.exists(entrada))stop("Arquivo necessário ausente: ",origem)
  dir.create(dirname(saida),recursive=TRUE,showWarnings=FALSE)
  if(!file.copy(entrada,saida,overwrite=FALSE))stop("Não foi possível copiar ",origem)
}
arquivos<-c("app.R","config.json","R/comum.R","R/analises.R","R/graficos.R","R/exploracao.R",
  "R/servidor_exploracao.R","R/ui.R","R/servidor.R","www/app.css","www/app.js",
  "www/acessibilidade.css","www/exploracao.css","www/ficha.css","data/referencia_cientifica.json",
  "scripts/atualizar_sinan.py","scripts/processar_atualizacao.R","scripts/preparar_publicacao.R","scripts/validar_metodos_processamento.R",
  "scripts/validar_pacote_publicacao.R","scripts/entregar_publicacao.py",
  "publicacao/HOSPEDAGEM.txt","publicacao/dependencias.R","publicacao/renv.app.lock","publicacao/renv.processamento.lock",
  "publicacao/workflows/verificar.yml","publicacao/workflows/atualizar_sinan.yml")
for(f in arquivos)copiar(f)
cfg<-jsonlite::read_json(file.path(destino,"config.json"),simplifyVector=TRUE)
cfg$agendamento<-"Planejamento de nuvem: semanal, segundas-feiras às 08:17 em Brasília. O estado efetivo consta em data/agendamento.json."
cfg$publicacao<-list(provedor="Posit Connect Cloud",processamento_externo="GitHub Actions",fuso="America/Sao_Paulo")
jsonlite::write_json(cfg,file.path(destino,"config.json"),auto_unbox=TRUE,pretty=TRUE)
copiar("publicacao/renv.app.lock","renv.lock")
copiar("publicacao/workflows/verificar.yml",".github/workflows/verificar.yml")
copiar("publicacao/workflows/atualizar_sinan.yml",".github/workflows/atualizar_sinan.yml")
limpar_origens<-function(d) {
  o<-d$origens
  if("url"%in%names(o))o$arquivo<-basename(gsub("\\\\","/",o$arquivo))
  rownames(o)<-NULL;d$origens<-o;d
}
# Somente metadados de caminho são normalizados; tabelas e resultados permanecem iguais.
estudo_publico<-limpar_origens(estudo);atual_publico<-limpar_origens(atual)
salvar<-function(d,rel) {
  p<-file.path(destino,"data",rel);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE)
  saveRDS(d,p,compress="xz");p
}
invisible(salvar(estudo_publico,"estudo.rds"))
rel_atual<-paste0("atualizacoes/",atual$meta$id,"/dados.rds")
arq_atual<-salvar(atual_publico,rel_atual)
ponteiro$arquivo<-rel_atual;ponteiro$sha256<-digest::digest(arq_atual,algo="sha256",file=TRUE)
gravar_json<-function(x,nome)jsonlite::write_json(x,file.path(destino,"data",nome),auto_unbox=TRUE,pretty=TRUE,na="null")
gravar_json(ponteiro,"atualizacao_atual.json")
status<-jsonlite::read_json(arquivo_dados("atualizacao_status.json"),simplifyVector=TRUE)
# Mensagens de erro podem conter caminhos do executor; os detalhes ficam no log administrativo.
if(identical(status$estado,"falha"))status$mensagem<-"A última consulta não foi concluída. A versão válida anterior foi preservada."
gravar_json(status,"atualizacao_status.json")
gravar_json(list(arquivos=atual_publico$origens),"catalogo_sinan.json")
agendamento_ativo<-identical(Sys.getenv("DASH_AGENDAMENTO_NUVEM_ATIVO"),"true")
gravar_json(list(ativo=agendamento_ativo,provedor="GitHub Actions",frequencia="Semanal · segundas-feiras, 08:17 (Brasília)",
  requisito=if(agendamento_ativo)"Execução externa ao dashboard. A data efetiva da consulta aparece acima." else "Rotina de nuvem preparada; ativação e teste na hospedagem ainda pendentes."),"agendamento.json")
registros<-list.files(file.path(destino,"data"),recursive=TRUE,full.names=TRUE)
inventario<-data.frame(arquivo=substring(registros,nchar(destino)+2),
  sha256=vapply(registros,digest::digest,"",algo="sha256",file=TRUE),bytes=file.info(registros)$size,row.names=NULL)
jsonlite::write_json(list(gerado_em=format(Sys.time(),"%Y-%m-%dT%H:%M:%S%z"),
  ambiente="publicacao",estudo=estudo$meta$id,atualizacao=atual$meta$id,arquivos=inventario,
  agendamento_ativado=agendamento_ativo),file.path(destino,"publicacao/inventario_dados.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(c("/.runtime/","/config.local.json","/publicacao/pacotes/","/publicacao/.dependencias/",".Renviron",".Rhistory",".RData","*.dbc","*.part","__pycache__/","*.pyc"),file.path(destino,".gitignore"))
writeLines(c(".git",".github",".runtime","config.local.json","publicacao/pacotes",".Renviron",".Rhistory",".RData"),file.path(destino,".rscignore"))
stopifnot(identical(hashes_antes,vapply(originais,digest::digest,"",algo="sha256",file=TRUE)))
cat("PACOTE_PREPARADO:",normalizePath(destino,winslash="/"),"\n")
cat("Somente código, dependências, agregados e metadados. Publicação externa não executada.\n")
