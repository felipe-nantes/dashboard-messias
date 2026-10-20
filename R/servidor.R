`%ou%` <- function(x,y) if(is.null(x) || length(x)==0 || identical(x,"")) y else x
json_seguro <- function(p) tryCatch(jsonlite::read_json(p,simplifyVector=TRUE),error=function(e) NULL)
data_pt <- function(x) {
  if(is.null(x) || !length(x) || is.na(x))return("Ainda não executada")
  if(nchar(x)==10)return(paste0(substr(x,9,10),"/",substr(x,6,7),"/",substr(x,1,4)))
  iso<-sub("Z$","+0000",x)
  iso<-sub("([+-][0-9]{2}):([0-9]{2})$","\\1\\2",iso)
  instante<-as.POSIXct(iso,format="%Y-%m-%dT%H:%M:%OS%z",tz="UTC")
  if(is.na(instante))return("Data não informada")
  paste0(format(instante,"%d/%m/%Y às %H:%M",tz="America/Sao_Paulo")," (Brasília)")
}
servidor_painel <- function(input,output,session) {
  estudo <- readRDS(arquivo_dados("estudo.rds"))
  ultima_base_valida <- NULL
  atual <- reactivePoll(10000,session,
    checkFunc=function() {p <- arquivo_dados("atualizacao_atual.json"); if(file.exists(p)) as.character(file.info(p)$mtime) else ""},
    valueFunc=function() {
      tryCatch({d<-ler_atualizacao();if(!is.null(d))ultima_base_valida<<-d;ultima_base_valida},
        error=function(e){warning("Atualização não carregada: ",conditionMessage(e));ultima_base_valida})
    })
  estado <- reactivePoll(10000,session,
    checkFunc=function() {p <- arquivo_dados("atualizacao_status.json"); if(file.exists(p)) as.character(file.info(p)$mtime) else ""},
    valueFunc=function() json_seguro(arquivo_dados("atualizacao_status.json")))
  inicial <- TRUE
  observe({
    a <- atual()
    escolhas <- c("Estudo confirmado · 2014–2025"="estudo")
    if(!is.null(a)) escolhas <- c("SINAN · atualização automática"="sinan",escolhas)
    escolhido <- isolate(input$versao)
    if(inicial) { escolhido <- json_seguro("config.json")$versao_inicial %ou% "estudo";if(!escolhido %in% escolhas)escolhido<-"estudo"; inicial <<- FALSE }
    updateSelectInput(session,"versao",choices=escolhas,selected=escolhido)
  })
  dados <- reactive({ if(identical(input$versao,"sinan") && !is.null(atual())) atual() else estudo })
  ultimo_tipo <- NULL
  ultimo_fim <- NULL
  observe({
    d <- dados()
    escolhas <- c("Todo o Paraná"="todos",setNames(d$espacial$CD_MUN,d$espacial$NM_MUN))
    updateSelectizeInput(session,"municipio",choices=escolhas,selected=isolate(input$municipio) %ou% "todos",server=TRUE)
    fim <- max(d$painel$ANO)
    intervalo <- isolate(input$anos)
    if(is.null(ultimo_tipo) || ultimo_tipo!=d$meta$tipo || is.null(intervalo)) intervalo<-c(2021,fim)
    else {
      if(!is.null(ultimo_fim) && intervalo[2]==ultimo_fim) intervalo[2]<-fim
      intervalo<-pmax(min(d$painel$ANO),pmin(fim,intervalo))
    }
    updateSliderInput(session,"anos",max=fim,value=intervalo)
    ultimo_tipo <<- d$meta$tipo; ultimo_fim <<- fim
    metodos <- c(setNames("PRM",if(d$meta$tipo=="estudo") "Permutação em R · oficial do estudo" else "Permutação em R · método de referência"),"Analítico em R · comparação"="ANA")
    if(d$meta$tipo=="estudo") metodos <- c(metodos,"Referência histórica · mapa prévio"="OFIC")
    m <- isolate(input$metodo_lisa) %ou% "PRM"
    if(!m %in% metodos) m <- "PRM"
    updateSelectInput(session,"metodo_lisa",choices=metodos,selected=m)
  })
  observeEvent(input$limpar,{
    updateSelectizeInput(session,"municipio",selected="todos")
    updateSliderInput(session,"anos",value=c(2021,max(dados()$painel$ANO)))
  })
  filtrar <- function(d,municipal=TRUE) {
    anos <- input$anos %ou% c(2021,2025)
    sel <- d$ANO>=anos[1] & d$ANO<=anos[2]
    mun <- input$municipio %ou% "todos"
    if(municipal && mun!="todos") sel <- sel & d$CD_MUN==mun
    d[sel,,drop=FALSE]
  }
  painel <- reactive(filtrar(dados()$painel))
  perfis <- reactive(filtrar(dados()$perfis))
  serie <- reactive({
    d <- painel()
    a <- aggregate(CASOS~ANO,d,sum)
    a$POP <- vapply(a$ANO,function(y) { p <- d$POP_15_29[d$ANO==y]; if(anyNA(p)) NA_real_ else sum(p) },numeric(1))
    a$TAXA <- a$CASOS/a$POP*100000
    if((input$municipio %ou% "todos")!="todos") a$BAYES <- d$TAXA_EBS[match(a$ANO,d$ANO)]
    a
  })
  output$titulo_pagina <- renderUI({
    titulos <- list(visao=c("O panorama das notificações","Explore o tempo, o território e o perfil dos jovens."),
      territorio=c("O território em perspectiva","Taxas observadas e suavizadas para os municípios do Paraná."),
      espacial=c("Os padrões de associação espacial","Moran global e LISA: uma leitura do conjunto e da vizinhança."),
      tempo=c("Como os padrões evoluem","Séries anuais e tendências na dimensão espaço-temporal."),
      municipal=c("Um município em perspectiva","Explore os resultados locais e compare com o Paraná."),
      metodo=c("Dados com contexto e procedência","Entenda as fontes, os cálculos e a atualização do painel."))
    a <- titulos[[input$pagina %ou% "visao"]] %ou% titulos$visao
    div(span(class="eyebrow","INTOXICAÇÕES AUTOPROVOCADAS"),h1(a[1]),p(a[2]))
  })
  output$selo_base <- renderUI({d<-dados(); span(class="selo-base",span(class="ponto-status"),if(d$meta$tipo=="sinan") paste0("Coleta · ",data_pt(d$meta$extraido_em)) else "Estudo confirmado · 19/10/2026")})
  output$aviso_versao <- renderUI({
    d <- dados()
    if(d$meta$tipo=="sinan") div(class="aviso aviso-discreto",strong("Atualização automática"),span("Novas extrações e análises exploratórias. A versão acadêmica pode ser selecionada ao lado."))
    else div(class="aviso aviso-discreto",strong("Estudo confirmado"),span("Extração informada: 19/06/2026 · referência final recebida em 19/10/2026. A coleta automática do SINAN permanece disponível no seletor de dados."))
  })
  output$fonte_dados <- renderUI({
    p <- procedencia_dados(dados())
    div(class="fonte-dados",`aria-label`="Origem dos dados e método populacional",
      tags$p(strong("Notificações: "),paste0(p$notificacoes,".")),
      tags$p(strong("População e taxas: "),"denominadores DATASUS / TabNet do estudo. 2014–2020: estimativa pela proporção de jovens em 2021; 2021–2025: faixas etárias anuais. Sem denominador validado, não há taxa."),
      tags$details(tags$summary("Ver datas e arquivos de origem"),
        tags$p(strong(p$natureza_data,": "),data_pt(p$data_extracao)," · Versão: ",p$versao),
        tags$p(strong("Referência metodológica: "),p$pacote_estudo,". ",p$resultados,"."),
        tags$p(strong("População: "),p$populacao,". ",p$metodo_populacao),
        tags$p("Arquivos populacionais: populacao_parana.csv, pop_15_19.csv e pop_20_29.csv."),
        tags$p("O manifesto em Método e dados identifica os arquivos utilizados e seus hashes SHA-256.")))
  })
  output$kpis <- renderUI({
    a <- painel(); p <- perfis(); s <- sum(a$CASOS)
    taxa <- if(anyNA(a$POP_15_29)) NA_real_ else s/sum(a$POP_15_29)*100000
    local <- if((input$municipio %ou% "todos")=="todos") "Paraná" else a$NM_MUN[1]
    kpi <- function(label,valor,nota,classe="") div(class=paste("kpi",classe),span(class="kpi-label",label),strong(valor),span(class="kpi-nota",nota))
    div(class="kpis",kpi("NOTIFICAÇÕES",fmt(s),paste(local,"·",paste(input$anos %ou% c(2021,2025),collapse="–")),"kpi-principal"),
      kpi("TAXA ANUAL MÉDIA",fmt(taxa,1),if(is.na(taxa)) "Denominador não disponível em todo o recorte" else "Por 100 mil jovens · taxa bruta"),
      kpi("MUNICÍPIOS COM REGISTROS",fmt(length(unique(a$CD_MUN[a$CASOS>0]))),"Município de residência"),
      kpi("ÓBITOS POR INTOXICAÇÃO",fmt(sum(p$N[p$EVOLUCAO=="3"])),"Evolução registrada no SINAN · código 3"))
  })
  output$grafico_notificacoes <- renderUI(svg_grafico(serie(),"ANO","CASOS",barras=TRUE,altura=285,legenda="Número de notificações por ano"))
  output$contexto_serie <- renderUI({a<-serie(); span(paste0(fmt(sum(a$CASOS))," notificações no período selecionado"))})
  perfil_output <- function(campo,limite=Inf) {
    p <- perfis()
    if(!nrow(p)) return(tags$p("Sem notificações neste recorte."))
    a <- aggregate(p$N,list(p[[campo]]),sum); names(a)<-c("GRUPO","N")
    if(campo=="AGENTE") a$GRUPO <- unname(agentes[as.character(a$GRUPO)])
    if(campo=="FAIXA") a<-a[order(a$GRUPO),] else a<-a[order(-a$N),]
    a <- head(a,limite)
    # Os percentuais usam todo o recorte, inclusive categorias fora das seis mostradas.
    total <- sum(p$N)
    if(!nrow(a) || total==0) return(tags$p("Sem notificações neste recorte."))
    tagList(lapply(seq_len(nrow(a)),function(i) div(class="perfil-linha",div(class="perfil-label",span(a$GRUPO[i]),tags$b(fmt(a$N[i]),tags$small(paste0(" · ",fmt(a$N[i]/total*100,1),"%")))),div(class="barra-fundo",div(class="barra-valor",style=paste0("width:",a$N[i]/max(a$N)*100,"%"))))))
  }
  output$perfil_sexo <- renderUI(perfil_output("SEXO"))
  output$perfil_idade <- renderUI(perfil_output("FAIXA"))
  output$perfil_agentes <- renderUI(perfil_output("AGENTE",6))
  mapa_numerico <- function(valores,titulo,dec=1,intervalo=NULL) {
    d<-dados(); sp<-d$espacial; geo<-d$mapa
    sem_taxa<-all(is.na(valores))
    pal<-leaflet::colorBin(c("#D8E7E1","#ACCFC2","#77B39F","#438F7D","#1B665A","#15463F"),domain=if(sem_taxa)c(0,1) else intervalo %ou% valores,bins=6,na.color="#E3E7E9")
    labels<-lapply(seq_along(valores),function(i) HTML(paste0("<strong>",esc(sp$NM_MUN[i]),"</strong><br>",esc(titulo),": ",fmt(valores[i],dec))))
    m<-leaflet::leaflet(geo,options=leaflet::leafletOptions(minZoom=5,maxZoom=12,zoomControl=TRUE,attributionControl=TRUE)) |>
      leaflet::addPolygons(layerId=sp$CD_MUN,fillColor=pal(valores),fillOpacity=.94,color="#FFFFFF",weight=.65,
        label=labels,popup=labels,highlightOptions=leaflet::highlightOptions(weight=2,color="#C8794E",bringToFront=TRUE))
    if(sem_taxa) m<-leaflet::addControl(m,"Sem denominador populacional para todo o período selecionado.",position="bottomleft")
    else m<-leaflet::addLegend(m,position="bottomleft",pal=pal,values=valores,title=titulo,opacity=1,na.label="Sem denominador",labFormat=function(type,cuts,p) paste(fmt(head(cuts,-1),dec),fmt(tail(cuts,-1),dec),sep=" – "))
    leaflet::addControl(m,"Malha IBGE 2022 · exibição simplificada",position="bottomright")
  }
  output$mapa_visao <- leaflet::renderLeaflet({
    d<-filtrar(dados()$painel,FALSE); sp<-dados()$espacial
    a<-aggregate(CASOS~CD_MUN,d,sum); vals<-a$CASOS[match(sp$CD_MUN,a$CD_MUN)]
    if(identical(input$medida_mapa,"TAXA")) {
      pops<-vapply(sp$CD_MUN,function(id) {x<-d$POP_15_29[d$CD_MUN==id];if(anyNA(x)) NA_real_ else sum(x)},numeric(1))
      vals<-vals/pops*100000
      mapa_numerico(vals,"Taxa por 100 mil",1)
    } else mapa_numerico(vals,"Notificações",0)
  })
  output$mapa_territorio <- leaflet::renderLeaflet({
    sp<-dados()$espacial; ind<-input$indicador_territorio %ou% "TAXA_BAYES_100MIL"
    titulo<-if(ind=="CAS1529_TOT") "Notificações · 2021–2025" else "Taxa por 100 mil · 2021–2025"
    mapa_numerico(sp[[ind]],titulo,if(ind=="CAS1529_TOT")0 else 1,
      if(ind!="CAS1529_TOT") c(sp$TAX1529_MED,sp$TAXA_BAYES_100MIL) else NULL)
  })
  for(id in c("mapa_visao","mapa_territorio","mapa_lisa","mapa_tendencia")) local({
    nome<-paste0(id,"_shape_click")
    observeEvent(input[[nome]],{
      v<-input[[nome]]$id
      if(!is.null(v)) updateSelectizeInput(session,"municipio",selected=v)
    },ignoreInit=TRUE)
  })
  tabela <- function(d,pagina=7) {
    tab<-DT::datatable(d,rownames=FALSE,selection="none",escape=TRUE,
      options=list(pageLength=pagina,lengthChange=FALSE,dom="ftip",scrollX=TRUE,autoWidth=TRUE,
        language=list(search="Buscar:",zeroRecords="Nenhum resultado neste recorte",emptyTable="Sem resultados",info="_START_–_END_ de _TOTAL_",infoEmpty="0 registros",infoFiltered="(de _MAX_)",paginate=list(previous="Anterior",`next`="Próxima"))))
    for(nome in names(d)[vapply(d,is.numeric,TRUE)]) {
      dig<-if(nome=="τ") 4 else if(nome=="Sen") 2 else if(all(d[[nome]]==floor(d[[nome]]),na.rm=TRUE)) 0 else 1
      tab<-DT::formatRound(tab,nome,digits=dig,mark=if(toupper(nome)=="ANO") "" else ".",dec.mark=",")
    }
    tab
  }
  territorio <- reactive({s<-dados()$espacial; data.frame(Município=s$NM_MUN,Notificações=s$CAS1529_TOT,`Taxa bruta`=round(s$TAX1529_MED,1),`Taxa Bayes`=round(s$TAXA_BAYES_100MIL,1),check.names=FALSE)})
  output$tabela_territorio <- DT::renderDT(tabela(territorio()[order(-territorio()[[4]]),]),server=FALSE)
  output$aviso_lisa <- renderUI({
    texto<-if(dados()$meta$tipo=="estudo") "Resultado final confirmado na resposta 1.5: QUAD_PRM com LISA_P_PRM. O mapa anterior permanece apenas como referência histórica." else "Recálculo automático pelo método de referência confirmado no estudo. Os valores desta extração são exploratórios; a confirmação acadêmica se refere à versão do estudo."
    div(class="aviso aviso-discreto",strong(if(dados()$meta$tipo=="estudo") "LISA do estudo · confirmado" else "LISA · método de referência confirmado"),span(texto))
  })
  output$moran_valor <- renderUI(strong(class="moran-numero",fmt(dados()$meta$moran_I,4)))
  output$moran_p <- renderUI(strong(paste0("Pseudo-p = ",fmt(dados()$meta$moran_p,3))))
  output$dispersao_moran <- renderUI(svg_moran(dados()$espacial,dados()$nb,classes=lisa()$CLASSE))
  metodo_atual <- reactive({
    m<-input$metodo_lisa %ou% "PRM"
    if(!m %in% c("ANA","PRM","OFIC") || (m=="OFIC" && dados()$meta$tipo!="estudo")) "PRM" else m
  })
  lisa <- reactive({
    sp<-dados()$espacial; m<-metodo_atual()
    sp$CLASSE<-sp[[paste0("QUAD_",m)]]
    sp$P_EXIBIDO<-if(m=="OFIC") NA_real_ else sp[[paste0("LISA_P_",m)]]
    sp
  })
  output$mapa_lisa <- leaflet::renderLeaflet({
    s<-lisa(); rot<-lapply(seq_len(nrow(s)),function(i) HTML(paste0("<strong>",esc(s$NM_MUN[i]),"</strong><br>",classes_lisa[s$CLASSE[i]],"<br>",if(is.na(s$P_EXIBIDO[i])) "p: não disponível no mapa histórico" else if(s$P_EXIBIDO[i]==0) "p &lt; 0,00001 (arredondamento da fonte)" else paste0("p = ",fmt(s$P_EXIBIDO[i],5)))))
    leaflet::leaflet(dados()$mapa,options=leaflet::leafletOptions(minZoom=5,maxZoom=12)) |>
      leaflet::addPolygons(layerId=s$CD_MUN,fillColor=unname(cores_lisa[s$CLASSE]),fillOpacity=.92,color="white",weight=.6,label=rot,
        highlightOptions=leaflet::highlightOptions(weight=2,color="#223F43",bringToFront=TRUE)) |>
      leaflet::addLegend("bottomleft",colors=unname(cores_lisa),labels=unname(classes_lisa),title="LISA · 2021–2025",opacity=1) |>
      leaflet::addControl("Malha IBGE 2022",position="bottomright")
  })
  output$resumo_lisa <- renderUI({s<-lisa();n<-table(factor(s$CLASSE,levels=names(classes_lisa)))
    tagList(lapply(names(classes_lisa),function(c) div(class="classe-lisa",span(class="classe-nome",span(class="classe-cor",style=paste0("background:",cores_lisa[c])),classes_lisa[c]),div(strong(fmt(n[c])),tags$small(paste0(fmt(n[c]/399*100,1),"%"))))))})
  output$detalhe_lisa <- renderUI({s<-dados()$espacial;m<-metodo_atual()
    div(class="nota-lisa",if(m=="OFIC") p("Mapa histórico recebido. Os p-valores correspondentes não foram enviados; o pseudo-p do R não é associado a essas classes.") else p(if(m=="PRM") "Permutação condicional em R · 999 simulações · semente 42 · pseudo-p folded." else "Aproximação analítica em R · p-valores analíticos."),
      if(dados()$meta$tipo=="estudo") p(strong(fmt(sum(s$QUAD_PRM!=s$QUAD_OFIC)))," municípios diferem entre o mapa histórico e a permutação em R; ",strong(fmt(sum(s$QUAD_ANA!=s$QUAD_PRM)))," diferem entre analítico e permutação."),
      p("Alto e baixo são relativos à distribuição municipal. O padrão não representa o risco de uma pessoa individual."))})
  output$grafico_taxas <- renderUI({a<-serie();svg_grafico(a,"ANO","TAXA",altura=270,serie2=if("BAYES"%in%names(a)) "BAYES" else NULL,sufixo=" por 100 mil",legenda="Taxas anuais por 100 mil jovens")})
  output$legenda_taxas <- renderUI(div(class="legenda-grafico",span("● Taxa bruta"),if((input$municipio %ou% "todos")!="todos") span(class="legenda-bayes","┄ Taxa Bayes municipal"),if(anyNA(serie()$TAXA)) span("Sem taxa nos anos sem denominador validado.")))
  output$grafico_moran_anual <- renderUI({a<-filtrar(dados()$moran,FALSE);svg_grafico(a,"ANO","I_Moran_Global",cor="#A87450",altura=270,legenda="Índice I de Moran por ano")})
  output$mapa_tendencia <- leaflet::renderLeaflet({
    tr<-dados()$tendencias; geo<-dados()$mapa; tr<-tr[match(geo$CD_MUN,tr$CD_MUN),]
    cats<-c("Aumento Significativo"="#B5794F","Queda Significativa"="#5585A5","Estável / Sem Tendência"="#D6DFDC")
    labs<-lapply(seq_len(nrow(tr)),function(i)HTML(paste0("<strong>",esc(tr$NM_MUN[i]),"</strong><br>",esc(tr$TENDENCIA[i]),"<br>τ = ",fmt(tr$MK_TAU[i],4)," · p = ",fmt(tr$MK_PVAL[i],5),"<br>Sen = ",fmt(tr$SEN_SLOPE[i],2)," por 100 mil/ano")))
    leaflet::leaflet(geo,options=leaflet::leafletOptions(minZoom=5,maxZoom=12)) |>
      leaflet::addPolygons(layerId=geo$CD_MUN,fillColor=unname(cats[tr$TENDENCIA]),fillOpacity=.95,color="white",weight=.6,label=labs,highlightOptions=leaflet::highlightOptions(weight=2,color="#223F43")) |>
      leaflet::addLegend("bottomleft",colors=unname(cats),labels=names(cats),title="Mann–Kendall · 2014–2025",opacity=1)
  })
  output$kpis_tendencia <- renderUI({t<-dados()$tendencias;div(class="tendencia-contagens",div(strong(fmt(sum(t$TENDENCIA=="Aumento Significativo"))),span("em aumento")),div(strong(fmt(sum(t$TENDENCIA=="Estável / Sem Tendência"))),span("estáveis / sem tendência")),div(strong(fmt(sum(t$TENDENCIA=="Queda Significativa"))),span("em queda")))})
  output$tabela_tendencias <- DT::renderDT({t<-dados()$tendencias;sel<-input$municipio %ou% "todos";if(sel!="todos")t<-t[t$CD_MUN==sel,]
    tabela(data.frame(Município=t$NM_MUN,`τ`=t$MK_TAU,`p`=ifelse(t$MK_PVAL==0,"< 0,00001",fmt(t$MK_PVAL,5)),Sen=t$SEN_SLOPE,Tendência=t$TENDENCIA,check.names=FALSE),5)},server=FALSE)
  output$estado_atualizacao <- renderUI({s<-estado();if(is.null(s))return(p("A primeira verificação ainda não foi executada."))
    div(class="status-atualizacao",span(class=paste("badge-estado",s$estado),switch(s$estado,atualizado="Base atualizada",sem_alteracoes="Fonte sem alterações",em_andamento="Atualização em andamento",falha="Última tentativa falhou",s$estado)),p(s$mensagem),tags$dl(tags$dt("Última consulta"),tags$dd(data_pt(s$ultima_consulta)),tags$dt("Última atualização válida"),tags$dd(data_pt(s$ultima_atualizacao))))})
  output$controle_atualizacao <- renderUI({
    if(pode_atualizar_local()) actionButton("atualizar_agora","Verificar SINAN agora",class="botao-primario")
    else p(class="texto-apoio","As atualizações são gerenciadas pela rotina externa. Esta página mostra a última consulta registrada.")
  })
  output$estado_agendamento <- renderUI({s<-json_seguro(arquivo_dados("agendamento.json"));if(is.null(s))return(p(class="texto-apoio","Agendamento externo ainda não configurado."))
    div(class="agendamento",strong(if(isFALSE(s$ativo))"Agendamento em preparação" else s$frequencia),p(s$requisito))})
  output$proveniencia <- renderUI({
    d<-dados();p<-procedencia_dados(d)
    tags$dl(tags$dt("Versão"),tags$dd(d$meta$id),tags$dt("Notificações"),tags$dd(p$notificacoes),
       tags$dt("Período disponível"),tags$dd(paste(range(d$painel$ANO),collapse="–")),
       tags$dt("Total de notificações"),tags$dd(fmt(sum(d$painel$CASOS))),
       tags$dt(if(d$meta$tipo=="estudo") "Extração informada" else "Coleta automática"),
       tags$dd(data_pt(if(d$meta$tipo=="estudo") d$meta$extracao_informada else d$meta$extraido_em)),
       tags$dt("Referência recebida"),tags$dd(p$pacote_estudo),
       tags$dt("Resultados"),tags$dd(p$resultados),
       tags$dt("Origem da população"),tags$dd(p$populacao),
       tags$dt("Método populacional"),tags$dd(p$metodo_populacao),
       tags$dt("Unidade"),tags$dd("Município de residência × ano de notificação"))
  })
  output$aviso_comparacao <- renderUI(if(is.null(atual()))p("A comparação estará disponível após a primeira coleta validada."))
  output$tabela_revisoes <- DT::renderDT({req(atual());tabela(atual()$comparacao,13)},server=FALSE)
  observeEvent(input$atualizar_agora,{
    if(!pode_atualizar_local()) {showNotification("Atualização gerenciada externamente.",type="message");return()}
    if(identical(estado()$estado,"em_andamento")) {showNotification("Uma atualização já está em execução.",type="message");return()}
    system2("powershell.exe",c("-NoProfile","-NonInteractive","-WindowStyle","Hidden","-ExecutionPolicy","Bypass","-File",shQuote(file.path(PAINEL,"scripts/atualizar.ps1"))),
      wait=FALSE,stdout=file.path(PAINEL,".runtime/ultima_execucao.log"),stderr=file.path(PAINEL,".runtime/ultima_execucao_erro.log"))
    showNotification("Consulta iniciada em segundo plano. O estado será atualizado nesta página.",type="message",duration=8)
  })
  exportar <- function(id,func,nome) output[[id]] <- downloadHandler(
    filename=function()paste0(nome,"_",dados()$meta$id,".csv"),
    content=function(file){a<-anexar_procedencia(func(),dados());write.csv2(a,file,row.names=FALSE,fileEncoding="UTF-8",na="")})
  exportar("baixar_serie",serie,"serie_anual")
  exportar("baixar_territorio",function(){a<-dados()$espacial[c("CD_MUN","NM_MUN","CAS1529_TOT","CAS1529_MED","POP1529_MED","TAX1529_MED","TXMDCMDPO","TAXA_BAYES_100MIL")];a$PERIODO<-"2021-2025";a$UNIDADE_TAXAS<-"por 100 mil jovens; TXMDCMDPO original por habitante";a},"territorio_2021_2025")
  exportar("baixar_lisa",function(){s<-lisa();m<-metodo_atual();status<-if(dados()$meta$tipo!="estudo") "recalculo exploratorio" else if(m=="PRM") "resultado final do estudo confirmado na resposta 1.5" else if(m=="OFIC") "referencia historica; p indisponivel" else "comparacao analitica";data.frame(CD_MUN=s$CD_MUN,NM_MUN=s$NM_MUN,METODO=m,PERIODO="2021-2025",CLASSE=s$CLASSE,P_VALOR=s$P_EXIBIDO,LISA_I=if(m=="OFIC")NA_real_ else s$LISA_I,STATUS=status,REFERENCIA_METODO=REFERENCIA_LISA$id)},"lisa_2021_2025")
  exportar("baixar_cubo",painel,"painel_municipio_ano")
  exportar("baixar_tendencias",function()dados()$tendencias,"tendencias_2014_2025")
  output$baixar_manifesto <- downloadHandler(filename=function()paste0("metadados_",dados()$meta$id,".json"),
    content=function(file){jsonlite::write_json(manifesto_dados(dados(),estado()),file,auto_unbox=TRUE,pretty=TRUE,na="null")})
  exploracao<-servidor_exploracao(input,output,session,dados,filtrar,tabela,exportar)
}
