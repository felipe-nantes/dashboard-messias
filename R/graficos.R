svg_grafico <- function(d, x, y, cor="#327C72", barras=FALSE, altura=260, sufixo="", serie2=NULL, legenda=NULL) {
  if (!nrow(d)) return(htmltools::tags$p("Sem observações neste recorte."))
  w <- 760; h <- altura; l <- 65; r <- 25; t <- 22; b <- 45
  vx <- d[[x]]; vy <- d[[y]]
  ymax <- max(c(vy, if(!is.null(serie2)) d[[serie2]],0),na.rm=TRUE)
  if (!is.finite(ymax) || ymax<=0) ymax <- 1
  ymax <- ymax*1.15
  px <- seq(l,w-r,length.out=length(vx))
  if(length(vx)==1) px <- (l+w-r)/2
  py <- function(v) h-b-v/ymax*(h-t-b)
  out <- sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s" class="grafico-svg">',w,h,esc(if(is.null(legenda)) "Série anual" else legenda))
  for (tick in pretty(c(0,ymax),n=4)) if (tick>=0 && tick<=ymax) out <- paste0(out,
    sprintf('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="#E9EDEF"/><text x="%d" y="%.1f" text-anchor="end" class="eixo">%s</text>',l,w-r,py(tick),py(tick),l-12,py(tick)+4,fmt(tick,if(ymax<2) 2 else 0)))
  for (i in seq_along(vx)) out <- paste0(out,sprintf('<text x="%.1f" y="%d" text-anchor="middle" class="eixo">%s</text>',px[i],h-16,esc(vx[i])))
  if (barras) {
    bw <- min(45,(w-l-r)/length(vx)*.57)
    for (i in which(is.finite(vy))) out <- paste0(out,sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="5" fill="%s" opacity="%s"><title>%s: %s%s</title></rect>',px[i]-bw/2,py(vy[i]),bw,h-b-py(vy[i]),cor,if(i==length(vx)) "1" else ".72",esc(vx[i]),fmt(vy[i]),sufixo))
  } else {
    linha <- function(v,color,dashed=FALSE) {
      pts <- which(is.finite(v))
      if(!length(pts)) return("")
      # Valores indisponíveis não são tratados como zero.
      segmentos <- split(pts,cumsum(c(1,diff(pts)!=1)))
      res <- paste(vapply(segmentos,function(ii) sprintf('<polyline points="%s" fill="none" stroke="%s" stroke-width="3" %s/>',paste(sprintf('%.1f,%.1f',px[ii],py(v[ii])),collapse=" "),color,if(dashed) 'stroke-dasharray="6 4"' else ''),""),collapse="")
      for (i in pts) res <- paste0(res,sprintf('<circle cx="%.1f" cy="%.1f" r="4.5" fill="%s" stroke="white" stroke-width="2"><title>%s: %s%s</title></circle>',px[i],py(v[i]),color,esc(vx[i]),fmt(v[i],if(ymax<2) 4 else 2),sufixo))
      res
    }
    out <- paste0(out,linha(vy,cor))
    if(!is.null(serie2)) out <- paste0(out,linha(d[[serie2]],"#C78050",TRUE))
  }
  htmltools::HTML(paste0(out,"</svg>"))
}
barras_perfil <- function(rotulos,valores,cor="#4B8C82") {
  total <- sum(valores)
  if(!length(valores) || total==0) return(htmltools::tags$p("Sem notificações neste recorte."))
  htmltools::tagList(lapply(seq_along(valores),function(i) htmltools::tags$div(class="perfil-linha",
    htmltools::tags$div(class="perfil-label",htmltools::tags$span(rotulos[i]),htmltools::tags$b(fmt(valores[i]),htmltools::tags$small(paste0(" · ",fmt(valores[i]/total*100,1),"%")))),
    htmltools::tags$div(class="barra-fundo",htmltools::tags$div(style=paste0("width:",valores[i]/max(valores)*100,"%;background:",cor),class="barra-valor")))))
}
svg_moran <- function(sp,nb,classes=sp$QUAD_PRM) {
  x <- sp$TXMDCMDPO
  lw <- spdep::nb2listw(nb,style="W",zero.policy=TRUE)
  z <- as.vector(scale(x)); lag <- spdep::lag.listw(lw,z,zero.policy=TRUE)
  lim <- max(abs(c(z,lag)))*1.08
  posx <- function(v) 55+(v+lim)/(lim*2)*640
  posy <- function(v) 315-(v+lim)/(lim*2)*280
  res <- '<svg viewBox="0 0 760 360" class="grafico-svg" role="img" aria-label="Diagrama de dispersão de Moran">'
  res <- paste0(res,sprintf('<line x1="55" x2="695" y1="%f" y2="%f" stroke="#CBD5D7"/><line x1="%f" x2="%f" y1="35" y2="315" stroke="#CBD5D7"/>',posy(0),posy(0),posx(0),posx(0)))
  beta <- as.numeric(crossprod(z,lag)/crossprod(z))
  res <- paste0(res,sprintf('<line x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f" stroke="#829991" stroke-dasharray="5 5"/>',posx(-lim),posx(lim),posy(-lim*beta),posy(lim*beta)))
  for(i in seq_along(z)) res <- paste0(res,sprintf('<circle cx="%.1f" cy="%.1f" r="4" opacity=".75" fill="%s"><title>%s · z: %s · defasagem: %s · %s</title></circle>',posx(z[i]),posy(lag[i]),cores_lisa[classes[i]],esc(sp$NM_MUN[i]),fmt(z[i],2),fmt(lag[i],2),esc(classes_lisa[classes[i]])))
  htmltools::HTML(paste0(res,'<text x="375" y="350" text-anchor="middle" class="eixo">Taxa Bayes padronizada (z)</text><text x="16" y="178" transform="rotate(-90 16 178)" text-anchor="middle" class="eixo">Defasagem espacial de z</text></svg>'))
}
