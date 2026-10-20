servidor_exploracao <- function(input,output,session,dados,filtrar,tabela,exportar) {
  ficha<-shiny::reactive(obter_ficha(dados(),input$municipio %ou% "todos",input$anos %ou% c(2021,2025)))
  pagina<-shiny::reactiveVal(1L)
  celula<-shiny::reactiveVal(NULL)
  cubo<-shiny::reactive(preparar_cubo(dados(),input$anos %ou% c(2021,2025),
    input$busca_cubo %ou% "",input$ordem_cubo %ou% "casos",input$medida_cubo %ou% "CASOS"))
  shiny::observeEvent(list(input$busca_cubo,input$ordem_cubo,input$medida_cubo,input$anos,input$versao),{pagina(1L)},ignoreInit=TRUE)
  shiny::observeEvent(input$pagina_cubo,{
    n<-max(1L,ceiling(length(cubo()$ids)/20))
    if(input$pagina_cubo=="proxima")pagina(min(n,pagina()+1L))
    if(input$pagina_cubo=="anterior")pagina(max(1L,pagina()-1L))
  },ignoreInit=TRUE)
  shiny::observeEvent(input$escolher_municipio,{
    id<-as.character(input$escolher_municipio)
    if(length(id)==1 && id %in% dados()$espacial$CD_MUN) shiny::updateSelectizeInput(session,"municipio",selected=id)
  },ignoreInit=TRUE)
  shiny::observeEvent(input$celula_cubo,{
    c<-input$celula_cubo
    if(is.null(c$municipio) || is.null(c$ano) || length(c$municipio)!=1 || length(c$ano)!=1) return()
    ano<-suppressWarnings(as.integer(c$ano));id<-as.character(c$municipio)
    if(!is.na(ano) && id %in% cubo()$ids && ano %in% cubo()$anos) {
      celula(list(municipio=id,ano=ano));shiny::updateSelectizeInput(session,"municipio",selected=id)
    }
  },ignoreInit=TRUE)
  shiny::observeEvent(input$municipio,{
    c<-celula()
    if(!is.null(c) && !identical(input$municipio,c$municipio))celula(NULL)
  },ignoreInit=TRUE)
  shiny::observe({
    c<-celula()
    if(!is.null(c) && (!c$municipio %in% cubo()$ids || !c$ano %in% cubo()$anos))celula(NULL)
  })
  output$municipio_em_foco<-shiny::renderUI({
    f<-ficha()
    if(is.null(f))return(div(class="foco-neutro",icone("perfil"),span("Selecione um município na busca ou no mapa para abrir sua ficha.")))
    div(class="municipio-foco",div(span(class="eyebrow","MUNICÍPIO EM FOCO"),strong(f$nome),span(class="foco-classe",classes_lisa[f$espacial$QUAD_PRM]," · LISA 2021–2025")),
      tags$button(type="button",class="botao-exemplo",`data-pagina`="municipal","Abrir ficha municipal →"))
  })
  output$cabecalho_ficha<-shiny::renderUI({
    f<-shiny::req(ficha())
    div(class="ficha-cabecalho",div(span(class="eyebrow",paste("CÓDIGO MUNICIPAL",f$id)),h2(f$nome),p("Resultados municipais com referência no Paraná · ",paste(f$anos,collapse="–"))),
      span(class="badge-ficha",if(dados()$meta$tipo=="estudo")"Estudo confirmado" else "Recálculo exploratório"))
  })
  output$kpis_ficha<-shiny::renderUI({
    f<-shiny::req(ficha())
    k<-function(t,v,n,c="")div(class=paste("kpi",c),span(class="kpi-label",t),strong(v),span(class="kpi-nota",n))
    div(class="kpis",k("NOTIFICAÇÕES",fmt(f$casos),paste(f$anos,collapse="–"),"kpi-principal"),
      k("TAXA ANUAL MÉDIA",fmt(f$taxa,1),if(is.na(f$taxa))"Há ano sem população validada" else "Taxa bruta por 100 mil jovens"),
      k("PARTICIPAÇÃO NO PARANÁ",paste0(fmt(f$participacao,2),"%"),"Parcela das notificações no mesmo recorte"),
      k("ÓBITOS POR INTOXICAÇÃO",fmt(f$obitos),"Evolução registrada · código 3"))
  })
  output$grafico_comparacao_municipal<-shiny::renderUI({f<-shiny::req(ficha());svg_grafico(f$serie,"ANO","TAXA",serie2="TAXA_PR",altura=285,sufixo=" por 100 mil",legenda="Taxa bruta municipal e taxa bruta do Paraná")})
  output$comparacao_municipal<-shiny::renderUI({
    f<-shiny::req(ficha())
    if(is.na(f$taxa) || is.na(f$taxa_PR))return(p(class="contexto-ficha","Taxa do recorte indisponível: há ano sem população validada. O gráfico mostra somente os valores anuais disponíveis."))
    dif<-f$taxa-f$taxa_PR
    p(class="contexto-ficha","No recorte selecionado, a taxa bruta estadual é ",strong(fmt(f$taxa_PR,1))," por 100 mil jovens. A taxa municipal está ",strong(paste0(fmt(abs(dif),1)," por 100 mil ",if(dif>=0)"acima" else "abaixo"))," desse valor. A comparação é descritiva.")
  })
  output$comparacao_bayes<-shiny::renderUI({
    f<-shiny::req(ficha());s<-f$espacial;maximo<-max(s$TAX1529_MED,s$TAXA_BAYES_100MIL,1)
    barra<-function(t,v,c)div(class="bayes-medida",div(span(t),strong(fmt(v,2))),div(class="bayes-trilho",span(style=paste0("width:",v/maximo*100,"%;background:",c))))
    tagList(barra("Taxa bruta",s$TAX1529_MED,"#A3BCAF"),barra("Bayes empírico espacial",s$TAXA_BAYES_100MIL,"#327C72"),
      p(class="contexto-ficha",fmt(s$CAS1529_TOT)," notificações em 2021–2025 · população anual média de ",fmt(s$POP1529_MED,1)," jovens."))
  })
  output$metodos_municipais<-shiny::renderUI({f<-shiny::req(ficha());tagList(html_metodos(f),p(class="contexto-ficha","A seleção municipal não recalcula o LISA. Ele usa a vizinhança e a distribuição dos 399 municípios. O histórico não recebe os p-valores do R."))})
  output$tendencia_municipal<-shiny::renderUI({
    f<-shiny::req(ficha());t<-f$tendencia
    div(class="ficha-tendencia",strong(t$TENDENCIA),
      tags$dl(tags$dt("τ de Kendall"),tags$dd(fmt(t$MK_TAU,4)),tags$dt("p-valor"),tags$dd(p_legivel(t$MK_PVAL)),tags$dt("Inclinação de Sen"),tags$dd(paste(fmt(t$SEN_SLOPE,2),"por 100 mil/ano"))),
      p(class="contexto-ficha","Calculada sobre as taxas Bayes da série completa 2014–2025. O filtro de período não refaz esse teste."))
  })
  output$tabela_ficha<-DT::renderDT({
    f<-shiny::req(ficha());s<-f$serie
    tabela(data.frame(Ano=s$ANO,Notificações=s$CASOS,`Taxa bruta municipal`=fmt(s$TAXA,2),`Bayes municipal`=fmt(s$BAYES,2),`Taxa bruta do Paraná`=fmt(s$TAXA_PR,2),check.names=FALSE),13)
  },server=FALSE)
  output$matriz_cubo<-shiny::renderUI(html_cubo(cubo(),pagina(),input$municipio %ou% "todos",celula()))
  output$paginacao_cubo<-shiny::renderUI({
    n<-length(cubo()$ids);pg<-min(pagina(),max(1,ceiling(n/20)));maxpg<-max(1,ceiling(n/20))
    div(class="paginacao-cubo",span(if(n)paste0((pg-1)*20+1,"–",min(pg*20,n)," de ",n,if(n==1)" município" else " municípios") else "0 municípios"),
      tags$button(type="button",`data-cubo-pagina`="anterior",disabled=if(pg<=1)"disabled" else NULL,`aria-label`="Página anterior da matriz","←"),
      span(paste0(pg," / ",maxpg)),tags$button(type="button",`data-cubo-pagina`="proxima",disabled=if(pg>=maxpg)"disabled" else NULL,`aria-label`="Próxima página da matriz","→"))
  })
  output$detalhe_celula<-shiny::renderUI({
    c<-celula();if(is.null(c))return(NULL)
    d<-dados();a<-d$painel[d$painel$CD_MUN==c$municipio & d$painel$ANO==c$ano,,drop=FALSE]
    if(!nrow(a))return(NULL)
    div(class="detalhe-celula",div(span(class="eyebrow","CÉLULA SELECIONADA"),strong(a$NM_MUN," · ",a$ANO)),
      div(span("Notificações"),strong(fmt(a$CASOS))),div(span("Taxa bruta / 100 mil"),strong(fmt(a$TAXA_BRUTA,2))),div(span("Bayes / 100 mil"),strong(fmt(a$TAXA_EBS,2))),
      tags$button(type="button",class="botao-exemplo",`data-pagina`="municipal","Ver ficha →"))
  })
  exportar("baixar_matriz",function(){
    c<-cubo();a<-c$nomes[c("CD_MUN","NM_MUN")]
    for(ano in c$anos) {v<-c$dados[c$dados$ANO==ano,];a[[as.character(ano)]]<-v[[c$medida]][match(a$CD_MUN,v$CD_MUN)]}
    a$INDICADOR<-rep(c$medida,nrow(a));a$UNIDADE<-rep(if(c$medida=="CASOS")"notificacoes" else "por 100 mil jovens",nrow(a));a
  },"matriz_municipio_ano")
  output$baixar_ficha_csv<-shiny::downloadHandler(
    filename=function(){f<-shiny::req(ficha());paste0("serie_municipal_",f$id,"_",dados()$meta$id,".csv")},
    content=function(arquivo){f<-shiny::req(ficha());a<-f$serie;a$CD_MUN<-f$id;a$NM_MUN<-f$nome;a$UNIDADE_TAXAS<-"por 100 mil jovens";a<-anexar_procedencia(a,dados());write.csv2(a,arquivo,row.names=FALSE,fileEncoding="UTF-8",na="")})
  output$baixar_ficha_html<-shiny::downloadHandler(
    filename=function(){f<-shiny::req(ficha());paste0("ficha_municipal_",f$id,"_",dados()$meta$id,".html")},contentType="text/html; charset=utf-8",
    content=function(arquivo){f<-shiny::req(ficha());writeLines(c("<!DOCTYPE html>",ficha_html_exportavel(f,REFERENCIA_LISA)),arquivo,useBytes=TRUE)})
  list(ficha=ficha,cubo=cubo,pagina=shiny::reactive(pagina()),celula=shiny::reactive(celula()))
}
