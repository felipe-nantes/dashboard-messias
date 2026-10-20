# Exploração dos agregados: não recalcula métodos espaciais nem altera fontes.
taxa_periodo <- function(d) {
  if(!nrow(d) || anyNA(d$POP_15_29) || sum(d$POP_15_29)<=0) return(NA_real_)
  sum(d$CASOS)/sum(d$POP_15_29)*100000
}
serie_agregada <- function(d) {
  a<-aggregate(CASOS~ANO,d,sum)
  a$POP<-vapply(a$ANO,function(y) {v<-d$POP_15_29[d$ANO==y];if(anyNA(v)) NA_real_ else sum(v)},numeric(1))
  a$TAXA<-a$CASOS/a$POP*100000
  a
}
p_legivel <- function(p) ifelse(is.na(p),"Não disponível",ifelse(p==0,"< 0,00001",fmt(p,5)))
obter_ficha <- function(d,id,anos) {
  if(length(id)!=1 || !id %in% d$espacial$CD_MUN) return(NULL)
  sp<-d$espacial[d$espacial$CD_MUN==id,,drop=FALSE]
  periodo<-d$painel[d$painel$ANO>=anos[1] & d$painel$ANO<=anos[2],]
  mun<-periodo[periodo$CD_MUN==id,]
  if(!nrow(mun)) return(NULL)
  perfis<-d$perfis[d$perfis$CD_MUN==id & d$perfis$ANO>=anos[1] & d$perfis$ANO<=anos[2],]
  anual<-serie_agregada(mun);pr<-serie_agregada(periodo)
  anual$TAXA_PR<-pr$TAXA[match(anual$ANO,pr$ANO)]
  anual$BAYES<-mun$TAXA_EBS[match(anual$ANO,mun$ANO)]
  classes<-c(sp$QUAD_PRM,sp$QUAD_ANA)
  metodos<-data.frame(Método=c(if(d$meta$tipo=="estudo")"Permutação R · oficial" else "Permutação R · recálculo","Analítico R · comparação"),
    Classe=unname(classes_lisa[classes]),p=c(sp$LISA_P_PRM,sp$LISA_P_ANA),CODIGO=classes,check.names=FALSE)
  if(!is.na(sp$QUAD_OFIC)) metodos<-rbind(metodos,data.frame(Método="Mapa prévio · histórico",Classe=unname(classes_lisa[sp$QUAD_OFIC]),p=NA_real_,CODIGO=sp$QUAD_OFIC,check.names=FALSE))
  list(id=id,nome=sp$NM_MUN,anos=anos,municipal=mun,espacial=sp,perfis=perfis,serie=anual,
    casos=sum(mun$CASOS),casos_PR=sum(periodo$CASOS),taxa=taxa_periodo(mun),taxa_PR=taxa_periodo(periodo),
    participacao=if(sum(periodo$CASOS)>0)sum(mun$CASOS)/sum(periodo$CASOS)*100 else NA_real_,
    obitos=sum(perfis$N[perfis$EVOLUCAO=="3"]),metodos=metodos,
    tendencia=d$tendencias[d$tendencias$CD_MUN==id,,drop=FALSE],versao=d$meta,procedencia=procedencia_dados(d))
}
tabela_html <- function(d,classe="tabela-simples") {
  htmltools::tags$table(class=classe,
    htmltools::tags$thead(htmltools::tags$tr(lapply(names(d),function(n)htmltools::tags$th(scope="col",n)))),
    htmltools::tags$tbody(lapply(seq_len(nrow(d)),function(i)htmltools::tags$tr(lapply(d,function(v)htmltools::tags$td(as.character(v[i])))))))
}
html_metodos <- function(f) {
  m<-f$metodos[c("Método","Classe","p")];m$p<-p_legivel(m$p)
  names(m)[3]<-"p / pseudo-p"
  tabela_html(m)
}
nomes_medidas <- c(CASOS="Notificações",TAXA_BRUTA="Taxa bruta por 100 mil",TAXA_EBS="Taxa Bayes por 100 mil")
normalizar_busca <- function(x) tolower(iconv(x,to="ASCII//TRANSLIT"))
preparar_cubo <- function(d,anos,busca="",ordem="casos",medida="CASOS") {
  if(!medida %in% names(nomes_medidas)) medida<-"CASOS"
  a<-d$painel[d$painel$ANO>=anos[1] & d$painel$ANO<=anos[2],]
  anos_disponiveis<-sort(unique(a$ANO))
  nomes<-d$espacial[c("CD_MUN","NM_MUN")]
  busca<-trimws(busca)
  if(nzchar(busca)) nomes<-nomes[grepl(normalizar_busca(busca),normalizar_busca(nomes$NM_MUN),fixed=TRUE) | grepl(busca,nomes$CD_MUN,fixed=TRUE),]
  a<-a[a$CD_MUN %in% nomes$CD_MUN,]
  ids<-nomes$CD_MUN
  if(length(ids)) {
    valores<-vapply(ids,function(id) {
      v<-a[[if(ordem=="valor") medida else "CASOS"]][a$CD_MUN==id]
      if(ordem=="valor") {if(all(is.na(v))) -Inf else max(v,na.rm=TRUE)} else sum(v)
    },numeric(1))
    ii<-if(ordem=="nome") order(nomes$NM_MUN) else order(-valores,nomes$NM_MUN)
    ids<-ids[ii];nomes<-nomes[ii,]
  }
  list(dados=a,ids=ids,nomes=nomes,anos=anos_disponiveis,medida=medida)
}
html_cubo <- function(cubo,pagina=1,selecionado=NULL,celula=NULL) {
  n<-length(cubo$ids)
  if(!n) return(htmltools::tags$div(class="estado-vazio",htmltools::tags$strong("Nenhum município encontrado"),htmltools::tags$p("Busque outro nome ou código municipal.")))
  pagina<-max(1,min(pagina,ceiling(n/20)))
  ids<-cubo$ids[seq.int((pagina-1)*20+1,min(pagina*20,n))]
  v<-cubo$dados[[cubo$medida]];fin<-v[is.finite(v)]
  dominio<-if(length(fin))range(fin) else c(0,1)
  if(diff(dominio)==0)dominio[2]<-dominio[1]+1
  pal<-grDevices::colorRampPalette(c("#EEF5F0","#C1DCCE","#80B5A0","#287564","#14483E"))(101)
  key<-paste(cubo$dados$CD_MUN,cubo$dados$ANO,sep="_")
  dig<-if(cubo$medida=="CASOS")0 else 1
  rows<-lapply(ids,function(id) {
    nome<-cubo$nomes$NM_MUN[match(id,cubo$nomes$CD_MUN)]
    htmltools::tags$tr(class=if(identical(id,selecionado))"linha-em-foco" else NULL,
      htmltools::tags$th(scope="row",htmltools::tags$button(type="button",class="cubo-municipio",`data-municipio`=id,nome)),
      lapply(cubo$anos,function(ano) {
        valor<-cubo$dados[[cubo$medida]][match(paste(id,ano,sep="_"),key)]
        frac<-if(is.na(valor)) 0 else max(0,min(1,(valor-dominio[1])/diff(dominio)))
        bg<-if(is.na(valor))"#E5E9EB" else pal[1+round(frac*100)]
        fg<-if(!is.na(valor) && frac>.6)"#FFFFFF" else "#244B40"
        ativa<-!is.null(celula) && identical(celula$municipio,id) && identical(as.integer(celula$ano),as.integer(ano))
        label<-paste0(nome,", ",ano," · ",nomes_medidas[cubo$medida],": ",if(is.na(valor))"indisponível, sem denominador validado" else fmt(valor,dig))
        htmltools::tags$td(htmltools::tags$button(type="button",class=paste("cubo-celula",if(ativa)"celula-ativa" else ""),
          `data-cubo-mun`=id,`data-cubo-ano`=ano,`aria-label`=label,title=label,
          style=paste0("background:",bg,";color:",fg),fmt(valor,dig)))
      }))
  })
  htmltools::tagList(htmltools::tags$div(class="cubo-scroll",tabindex="0",`aria-label`="Matriz município por ano, com rolagem horizontal",
    htmltools::tags$table(class="tabela-cubo",htmltools::tags$caption(class="visually-hidden","Matriz de ",nomes_medidas[cubo$medida]," por município e ano"),
      htmltools::tags$thead(htmltools::tags$tr(htmltools::tags$th(scope="col","Município"),lapply(cubo$anos,function(y)htmltools::tags$th(scope="col",y)))),htmltools::tags$tbody(rows))),
    htmltools::tags$div(class="legenda-cubo",htmltools::tags$span(nomes_medidas[cubo$medida]),
      if(length(fin) && diff(range(fin))>0) htmltools::tagList(htmltools::tags$span(fmt(min(fin),dig)),htmltools::tags$span(class="gradiente-cubo"),htmltools::tags$span(fmt(max(fin),dig)))
      else if(length(fin)) htmltools::tags$span(paste("Valor único:",fmt(fin[1],dig))),
      if(anyNA(v)) htmltools::tags$span(class="cubo-sem-dado","— sem denominador validado")))
}
ficha_html_exportavel <- function(f,referencia) {
  versao<-if(f$versao$tipo=="estudo") "Estudo confirmado na resposta 1.5" else "Atualização SINAN · recálculo exploratório"
  fonte<-f$procedencia
  css<-paste(readLines(file.path(PAINEL,"www/ficha.css"),warn=FALSE,encoding="UTF-8"),collapse="\n")
  serie<-f$serie[c("ANO","CASOS","TAXA","BAYES","TAXA_PR")]
  names(serie)<-c("Ano","Notificações","Taxa bruta municipal","Taxa Bayes municipal","Taxa bruta do Paraná")
  for(n in names(serie)[3:5])serie[[n]]<-fmt(serie[[n]],2)
  t<-f$tendencia
  corpo<-htmltools::tagList(
    htmltools::tags$header(htmltools::tags$p(class="sobretitulo","INTOXICAÇÕES AUTOPROVOCADAS · PARANÁ · 15–29 ANOS"),
      htmltools::tags$h1(f$nome),htmltools::tags$p("Ficha municipal · código ",f$id," · período selecionado ",paste(f$anos,collapse="–")),
      htmltools::tags$p(class="versao",versao)),
    htmltools::tags$main(
      htmltools::tags$section(class="metricas",lapply(list(c("Notificações",fmt(f$casos)),c("Taxa anual média por 100 mil",fmt(f$taxa,1)),c("Participação nas notificações do PR",paste0(fmt(f$participacao,2),"%")),c("Óbitos por intoxicação",fmt(f$obitos))),function(v)htmltools::tags$div(htmltools::tags$span(v[1]),htmltools::tags$strong(v[2])))),
      htmltools::tags$h2("Evolução das taxas no período selecionado"),
      svg_grafico(f$serie,"ANO","TAXA",serie2="TAXA_PR",altura=270,sufixo=" por 100 mil",legenda="Taxas brutas do município e do Paraná"),
      htmltools::tags$p(class="legenda","Verde: município · linha tracejada: Paraná. Taxas calculadas com as populações de jovens de cada ano. Ausência de denominador é mostrada como traço, sem imputação."),
      htmltools::tags$h2("Série municipal"),tabela_html(serie),
      htmltools::tags$h2("Bayes e LISA · recorte fixo de 2021–2025"),
      htmltools::tags$p("Taxa bruta anual média: ",fmt(f$espacial$TAX1529_MED,2)," · Bayes empírico espacial: ",fmt(f$espacial$TAXA_BAYES_100MIL,2),". Ambas por 100 mil jovens."),html_metodos(f),
      htmltools::tags$p("O mapa histórico não possui seus p-valores originais. O resultado do estudo confirmado usa QUAD_PRM com LISA_P_PRM, 999 permutações, semente 42 e p<0,05. Novas extrações permanecem exploratórias."),
      htmltools::tags$h2("Tendência · série completa de 2014–2025"),
      htmltools::tags$p(t$TENDENCIA," · τ = ",fmt(t$MK_TAU,4)," · p: ",p_legivel(t$MK_PVAL)," · Sen = ",fmt(t$SEN_SLOPE,2)," por 100 mil/ano. Calculada sobre as taxas Bayes anuais."),
      htmltools::tags$h2("Fonte e interpretação"),
      htmltools::tags$p("Fonte das notificações: ",fonte$notificacoes,". ",fonte$natureza_data,": ",fonte$data_extracao,"."),
      htmltools::tags$p("Referência do estudo: ",fonte$pacote_estudo,". ",fonte$resultados,"."),
      htmltools::tags$p("População: ",fonte$populacao,". ",fonte$metodo_populacao),
      htmltools::tags$p("Arquivos populacionais: populacao_parana.csv, pop_15_19.csv e pop_20_29.csv, do diretório 03_GeoDa_LISA_e_Bayes do pacote recebido. O manifesto metadados_",fonte$versao,".json identifica os arquivos de origem e seus hashes SHA-256."),
      htmltools::tags$p("Município de residência, idade de 15–29 anos, circunstância 10 e agentes 01–14 ou 99. As contagens representam notificações, não pessoas únicas; não houve deduplicação adicional. Dados sujeitos a revisão; anos recentes podem estar incompletos."),
      htmltools::tags$p("A suavização Bayes não é uma contagem observada. Associação espacial e tendência não demonstram causalidade nem risco individual.")),
    htmltools::tags$footer("Versão dos dados: ",f$versao$id," · Referência: ",referencia$id,
      htmltools::tags$br(),"Gerado em ",format(Sys.time(),"%d/%m/%Y %H:%M",tz="America/Sao_Paulo")," · Painel de pesquisa"))
  # A conversão direta de tags$html extrai <head> para metadados do Shiny.
  # O documento portátil precisa gravar esse cabeçalho no próprio arquivo.
  cabecalho<-htmltools::tagList(htmltools::tags$meta(charset="utf-8"),htmltools::tags$meta(name="viewport",content="width=device-width, initial-scale=1"),htmltools::tags$title(paste("Ficha municipal —",f$nome)),htmltools::tags$style(htmltools::HTML(css)))
  paste0('<html lang="pt-BR">\n<head>\n',as.character(cabecalho),'\n</head>\n<body>\n',as.character(corpo),'\n</body>\n</html>')
}
ui_cubo <- function() card("Explorador município × ano",
  "Compare as células da série espaço-temporal. Clique em um valor para examinar o município naquele ano.",
  div(class="cubo-controles",
    div(textInput("busca_cubo","BUSCAR MUNICÍPIO OU CÓDIGO",placeholder="Nome ou código IBGE")),
    div(selectInput("medida_cubo","VALOR DAS CÉLULAS",setNames(names(nomes_medidas),unname(nomes_medidas)))),
    div(selectInput("ordem_cubo","ORDENAR MUNICÍPIOS",c("Mais notificações no período"="casos","Ordem alfabética"="nome","Maior valor anual do indicador"="valor")))),
  uiOutput("matriz_cubo"),
  div(class="cartao-rodape cubo-rodape",uiOutput("paginacao_cubo"),downloadLink("baixar_matriz","↓ Baixar matriz filtrada")),
  uiOutput("detalhe_celula"),
  tags$details(class="ajuda-exploracao",tags$summary("Como interpretar esta matriz"),
    p("Cada linha representa um município e cada coluna, um ano de notificação. A escala de cores usa todos os municípios encontrados na busca, para o período e o indicador selecionados; ela permanece igual ao trocar de página."),
    p("Zero significa ausência de notificações no agregado. O traço indica taxa indisponível por falta de denominador validado. O painel organiza a dimensão espaço-temporal dos dados; não representa uma análise Emerging Hot Spot nem um cubo NetCDF.")))
ui_ficha <- function() tagList(
  conditionalPanel("!input.municipio || input.municipio == 'todos'",
    div(class="estado-vazio cartao",icone("perfil"),h3("Escolha um município para começar"),
      p("Use a busca acima ou selecione um dos exemplos para explorar os resultados confirmados."),
      div(class="exemplos-municipios",lapply(list(c("410690","Curitiba"),c("411370","Londrina"),c("411520","Maringá")),function(a)tags$button(type="button",class="botao-exemplo",`data-municipio`=a[1],a[2]))))),
  conditionalPanel("input.municipio && input.municipio != 'todos'",
    uiOutput("cabecalho_ficha"),uiOutput("kpis_ficha"),
    div(class="grade grade-principal",
      card("Município e Paraná","Taxas brutas na mesma escala de 100 mil jovens.",uiOutput("grafico_comparacao_municipal"),div(class="legenda-grafico",span("● Município"),span(class="legenda-bayes","┄ Paraná")),uiOutput("comparacao_municipal")),
      card("Taxa bruta e Bayes","Recorte fixo de 2021–2025 · taxas anuais médias por 100 mil jovens.",uiOutput("comparacao_bayes"),tags$details(class="ajuda-exploracao",tags$summary("Por que os valores são diferentes?"),p("A suavização Bayes combina a taxa municipal com informações da população e da vizinhança. Ela reduz oscilações em pequenas populações e pode produzir estimativa positiva mesmo onde não há registros. Não representa uma contagem observada.")))),
    div(class="grade grade-principal",
      card("LISA e versões do método","Associação espacial local em 2021–2025, calculada no conjunto dos 399 municípios.",uiOutput("metodos_municipais")),
      card("Tendência municipal","Resultado da série completa de taxas Bayes de 2014–2025.",uiOutput("tendencia_municipal"))),
    card("Série anual do município","As taxas sem denominador validado aparecem como indisponíveis.",DT::DTOutput("tabela_ficha"),
      div(class="cartao-rodape",downloadLink("baixar_ficha_csv","↓ Baixar série municipal"),downloadLink("baixar_ficha_html","↓ Exportar ficha para apresentação")))))
