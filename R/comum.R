options(encoding = "UTF-8")
raiz_painel <- function() {
  candidatos <- c(Sys.getenv("DASH_PAINEL_DIR"),getwd(),file.path(getwd(),"painel"))
  for(p in candidatos[nzchar(candidatos)]) {
    if(file.exists(file.path(p,"app.R")) && file.exists(file.path(p,"R/comum.R")))
      return(normalizePath(p,winslash="/",mustWork=TRUE))
  }
  stop("Raiz do dashboard não encontrada. Execute na pasta do app.R ou configure DASH_PAINEL_DIR.")
}
PAINEL <- raiz_painel()
PROJETO <- dirname(PAINEL)
caminho_absoluto <- function(p) grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)",p)
caminho_configurado <- function(p,base=PAINEL) {
  if(!caminho_absoluto(p)) p <- file.path(base,p)
  normalizePath(p,winslash="/",mustWork=FALSE)
}
CONFIG_LOCAL <- if(file.exists(file.path(PAINEL,"config.local.json")))
  jsonlite::read_json(file.path(PAINEL,"config.local.json"),simplifyVector=TRUE) else list()
AMBIENTE <- Sys.getenv("DASH_AMBIENTE",if(is.null(CONFIG_LOCAL$ambiente))"publicacao" else CONFIG_LOCAL$ambiente)
if(!AMBIENTE %in% c("local","publicacao","processamento"))stop("DASH_AMBIENTE inválido.")
if(AMBIENTE=="publicacao")CONFIG_LOCAL<-list()
libs <- c(CONFIG_LOCAL$bibliotecas_r,strsplit(Sys.getenv("DASH_R_LIBS"),.Platform$path.sep,fixed=TRUE)[[1]])
libs <- vapply(libs[nzchar(libs)],caminho_configurado,"")
.libPaths(c(libs[dir.exists(libs)],.libPaths()))
FONTE_ESTUDO <- caminho_configurado(Sys.getenv("DASH_FONTE_ESTUDO",if(is.null(CONFIG_LOCAL$fonte_estudo))"origens_estudo" else CONFIG_LOCAL$fonte_estudo))
DADOS_DIR <- caminho_configurado(Sys.getenv("DASH_DATA_DIR","data"))
RUNTIME_DIR <- caminho_configurado(Sys.getenv("DASH_RUNTIME_DIR",if(AMBIENTE=="publicacao")file.path(tempdir(),"dashboard_messias") else ".runtime"))
REFERENCIA_LISA <- jsonlite::read_json(file.path(PAINEL,"data/referencia_cientifica.json"),simplifyVector=TRUE)
dir.create(RUNTIME_DIR,recursive=TRUE,showWarnings=FALSE)
Sys.setenv(R_USER_CACHE_DIR=file.path(RUNTIME_DIR,"cache"),
           R_USER_DATA_DIR=file.path(RUNTIME_DIR,"user_data"),
           XDG_CACHE_HOME=file.path(RUNTIME_DIR,"cache"))
arquivo_dados <- function(nome)file.path(DADOS_DIR,nome)
pode_atualizar_local <- function() AMBIENTE=="local" && isTRUE(CONFIG_LOCAL$permitir_atualizacao_local) && .Platform$OS.type=="windows"
resolver_arquivo_atualizacao <- function(p) {
  if(is.null(p$arquivo) || length(p$arquivo)!=1 || !is.character(p$arquivo))stop("Ponteiro de dados inválido.")
  if(AMBIENTE=="publicacao" && caminho_absoluto(p$arquivo))stop("A publicação exige caminho relativo para os dados.")
  destino<-normalizePath(caminho_configurado(p$arquivo,DADOS_DIR),winslash="/",mustWork=TRUE)
  raiz<-paste0(normalizePath(DADOS_DIR,winslash="/",mustWork=TRUE),"/")
  comparar<-if(.Platform$OS.type=="windows")tolower else identity
  if(!startsWith(comparar(destino),comparar(raiz)))stop("A atualização deve estar dentro do diretório de dados.")
  if(AMBIENTE=="publicacao" && is.null(p$sha256))stop("A atualização publicada precisa de hash SHA-256.")
  if(!is.null(p$sha256) && !identical(digest::digest(destino,algo="sha256",file=TRUE),p$sha256))stop("Integridade da atualização não confere.")
  destino
}
ler_atualizacao <- function() {
  p<-arquivo_dados("atualizacao_atual.json")
  if(!file.exists(p))return(NULL)
  ponteiro<-jsonlite::read_json(p,simplifyVector=TRUE)
  d<-readRDS(resolver_arquivo_atualizacao(ponteiro))
  if(!identical(d$meta$id,ponteiro$id))stop("Versão e ponteiro não conferem.")
  validar_dados(d)
  d
}
validar_inventario_publicacao <- function() {
  if(AMBIENTE!="publicacao")return(invisible(TRUE))
  p<-file.path(PAINEL,"publicacao/inventario_dados.json")
  if(!file.exists(p))stop("Inventário da publicação ausente. Gere o pacote antes de iniciar.")
  inv<-jsonlite::read_json(p,simplifyVector=TRUE)
  if(is.null(inv$arquivos) || !nrow(inv$arquivos))stop("Inventário de dados vazio.")
  for(i in seq_len(nrow(inv$arquivos))) {
    rel<-inv$arquivos$arquivo[i]
    if(!startsWith(rel,"data/") || grepl("(^|/)\\.\\.(/|$)",rel))stop("Caminho inválido no inventário.")
    f<-file.path(DADOS_DIR,substring(rel,6))
    if(!file.exists(f) || !identical(digest::digest(f,algo="sha256",file=TRUE),inv$arquivos$sha256[i]))
      stop("Integridade do pacote não confere: ",rel)
  }
  invisible(TRUE)
}
ler_csv <- function(p) {
  d <- read.csv(p, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE)
  names(d)[1] <- sub("^\ufeff", "", names(d)[1])
  if ("CD_MUN" %in% names(d)) d$CD_MUN <- substr(as.character(d$CD_MUN), 1, 6)
  d
}
fmt <- function(x, digitos = 0) {
  v <- formatC(x, format = "f", digits = digitos, big.mark = ".", decimal.mark = ",")
  v[is.na(x)] <- "—"
  v
}
esc <- function(x) htmltools::htmlEscape(as.character(x))
classes_lisa <- c("HH" = "Alto–Alto", "LL" = "Baixo–Baixo", "HL" = "Alto–Baixo", "LH" = "Baixo–Alto", "NS" = "Não significante")
cores_lisa <- c("HH" = "#B44742", "LL" = "#4F7FAA", "HL" = "#E9A16F", "LH" = "#8FB7D2", "NS" = "#DDE3E5")
codigo_lisa <- function(x) {
  v <- tolower(iconv(x, to = "ASCII//TRANSLIT"))
  ifelse(grepl("^alto-alto", v), "HH", ifelse(grepl("^baixo-baixo", v), "LL",
    ifelse(grepl("^alto-baixo", v), "HL", ifelse(grepl("^baixo-alto", v), "LH", "NS"))))
}
agentes <- c("01"="Medicamento", "02"="Agrotóxico agrícola", "03"="Agrotóxico doméstico",
  "04"="Agrotóxico de saúde pública", "05"="Raticida", "06"="Produto veterinário",
  "07"="Produto de uso domiciliar", "08"="Cosmético / higiene", "09"="Produto químico industrial",
  "10"="Metal", "11"="Drogas de abuso", "12"="Planta tóxica", "13"="Alimento e bebida", "14"="Outro", "99"="Ignorado")
agregar_perfis <- function(d) {
  d$CD_MUN <- substr(as.character(d$CD_MUN), 1, 6)
  d$SEXO <- ifelse(d$CS_SEXO == "F", "Feminino", ifelse(d$CS_SEXO == "M", "Masculino", "Ignorado / outro"))
  d$SEXO[is.na(d$SEXO)] <- "Ignorado / outro"
  d$FAIXA <- cut(as.numeric(d$IDADE_ANOS), breaks = c(14,19,24,29), labels = c("15–19 anos", "20–24 anos", "25–29 anos"))
  d$AGENTE <- sprintf("%02d", suppressWarnings(as.integer(as.character(d$AGENTE_TOX))))
  d$EVOLUCAO <- as.character(d$EVOLUCAO)
  d$EVOLUCAO[is.na(d$EVOLUCAO) | d$EVOLUCAO == ""] <- "9"
  aggregate(rep(1L, nrow(d)), d[c("CD_MUN","ANO","SEXO","FAIXA","AGENTE","EVOLUCAO")], sum) |>
    setNames(c("CD_MUN","ANO","SEXO","FAIXA","AGENTE","EVOLUCAO","N"))
}
validar_dados <- function(d, estudo = FALSE) {
  stopifnot(nrow(d$espacial) == 399L, length(unique(d$espacial$CD_MUN)) == 399L,
    !anyDuplicated(d$painel[c("CD_MUN","ANO")]), all(d$painel$CASOS >= 0),
    all(is.finite(d$painel$CASOS)), all(d$painel$CASOS == floor(d$painel$CASOS)),
    all(d$painel$CD_MUN %in% d$espacial$CD_MUN),
    sum(d$perfis$N) == sum(d$painel$CASOS))
  com_pop <- !is.na(d$painel$POP_15_29)
  stopifnot(all(d$painel$POP_15_29[com_pop] > 0))
  cont <- aggregate(CASOS ~ ANO, d$painel, sum)
  prf <- aggregate(N ~ ANO, d$perfis, sum)
  stopifnot(identical(cont$ANO, prf$ANO), identical(as.numeric(cont$CASOS), as.numeric(prf$N)))
  if (estudo) stopifnot(nrow(d$painel) == 4788, sum(d$painel$CASOS) == 47662,
    sum(d$painel$CASOS[d$painel$ANO >= 2021]) == 24287,
    sum(d$perfis$N[d$perfis$ANO >= 2021 & d$perfis$EVOLUCAO == "3"]) == 77,
    sum(d$espacial$QUAD_PRM == "HH") == 84, sum(d$espacial$QUAD_PRM == "LL") == 84,
    sum(d$espacial$QUAD_PRM != d$espacial$QUAD_OFIC) == 33)
  invisible(TRUE)
}

# A procedência acompanha a base selecionada, sem modificar os dados recebidos.
metodo_populacao <- function(ano=NULL) {
  geral <- "Método do estudo: 2014–2020 = população anual total × proporção municipal de jovens em 2021, arredondada ao habitante; 2021–2025 = soma anual das faixas 15–19 e 20–29 anos. Após 2025, sem denominador validado, não há taxa."
  if(is.null(ano)) return(geral)
  v <- rep(geral,length(ano))
  v[!is.na(ano) & ano>=2014 & ano<=2020] <- "Estimativa do estudo: população anual total × proporção municipal de jovens em 2021, arredondada ao habitante."
  v[!is.na(ano) & ano>=2021 & ano<=2025] <- "Denominador do estudo: soma das populações anuais de 15–19 e 20–29 anos recebidas do DATASUS / TabNet."
  v[!is.na(ano) & (ano<2014 | ano>2025)] <- "Sem denominador validado nesta versão; taxas indisponíveis."
  v
}
procedencia_dados <- function(d) {
  estudo <- identical(d$meta$tipo,"estudo")
  list(
    versao=d$meta$id,
    tipo_base=if(estudo) "Estudo recebido e confirmado na resposta 1.5" else "Atualização automática SINAN; recálculo exploratório",
    notificacoes=if(estudo) "SINAN / DATASUS — dados recebidos no pacote Pasta_Complementar_2.0 do estudo" else "SINAN / DATASUS — coleta direta dos arquivos oficiais IEXOBRyy.dbc, pastas FINAIS e PRELIM",
    data_extracao=if(estudo) d$meta$extracao_informada else d$meta$extraido_em,
    natureza_data=if(estudo) "Data informada pelo responsável do estudo" else "Data da consulta/coleta automática registrada pelo painel",
    referencia_estudo=REFERENCIA_LISA$id,
    pacote_estudo="Pasta_Complementar_2.0.zip e Resposta1.5.docx, recebidos em 19/10/2026",
    resultados=if(estudo) "Tabelas e resultados do estudo enviado; LISA final QUAD_PRM / LISA_P_PRM" else "Recalculados pelo painel com os métodos do estudo; resultados exploratórios",
    populacao="DATASUS / TabNet — denominadores recebidos no estudo; não são atualizados pela coleta do SINAN",
    metodo_populacao=metodo_populacao(),
    arquivos_populacao=paste0("03_GeoDa_LISA_e_Bayes/",c("populacao_parana.csv","pop_15_19.csv","pop_20_29.csv")),
    diretorio_estudo="Pasta_Complementar_2.0 (pacote recebido do estudo)",
    portal_oficial="https://datasus.saude.gov.br/transferencia-de-arquivos/",
    unidade="Notificações por município de residência e ano de notificação; não pessoas únicas",
    periodo_disponivel=range(d$painel$ANO),
    periodo_espacial=c(2021L,2025L),
    periodo_tendencias=c(2014L,2025L))
}
anexar_procedencia <- function(a,d) {
  p <- procedencia_dados(d)
  campos <- list(VERSAO_DADOS=p$versao,TIPO_BASE=p$tipo_base,FONTE_NOTIFICACOES=p$notificacoes,
    DATA_EXTRACAO_NOTIFICACOES=p$data_extracao,NATUREZA_DATA_EXTRACAO=p$natureza_data,
    FONTE_POPULACAO=p$populacao,REFERENCIA_ESTUDO=p$referencia_estudo,
    MANIFESTO_FONTES=paste0("metadados_",p$versao,".json"))
  for(n in names(campos)) a[[n]] <- rep(campos[[n]],nrow(a))
  a$METODO_POPULACAO <- if("ANO" %in% names(a)) metodo_populacao(a$ANO) else rep(p$metodo_populacao,nrow(a))
  a
}
manifesto_dados <- function(d,estado=NULL) {
  p <- procedencia_dados(d)
  fontes_estudo <- if(identical(d$meta$tipo,"estudo")) d$origens else readRDS(arquivo_dados("estudo.rds"))$origens
  meta <- d$meta; meta$lisa_pendente <- NULL
  list(versao=meta,procedencia=p,referencia_cientifica_atual=REFERENCIA_LISA,
    status_lisa=if(identical(d$meta$tipo,"estudo")) "resultado do estudo confirmado" else "recalculo exploratorio; metodo de referencia confirmado",
    fontes=d$origens,
    fontes_populacao_do_estudo=fontes_estudo[fontes_estudo$arquivo %in% p$arquivos_populacao,,drop=FALSE],
    atualizacao=estado,
    agendamento=if(file.exists(arquivo_dados("agendamento.json")))jsonlite::read_json(arquivo_dados("agendamento.json"),simplifyVector=TRUE) else NULL,
    regras=jsonlite::read_json(file.path(PAINEL,"config.json"),simplifyVector=TRUE))
}
