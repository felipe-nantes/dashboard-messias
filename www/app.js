(() => {
  const ativar = (pagina) => {
    document.querySelectorAll('[data-pagina]').forEach((el) => {
      const ativo = el.dataset.pagina === pagina;
      el.classList.toggle('ativo', ativo);
      if (ativo) el.setAttribute('aria-current', 'page'); else el.removeAttribute('aria-current');
    });
    if (window.Shiny) Shiny.setInputValue('pagina', pagina, {priority:'event'});
    document.body.classList.remove('menu-aberto');
    window.scrollTo({top:0, behavior:'smooth'});
    // Leaflet precisa recalcular a área ao sair de um painel oculto.
    window.dispatchEvent(new Event('resize'));
    setTimeout(() => window.dispatchEvent(new Event('resize')), 150);
  };
  document.addEventListener('click', (e) => {
    const nav = e.target.closest('[data-pagina]');
    if (nav) ativar(nav.dataset.pagina);
    const mun = e.target.closest('[data-municipio]');
    if (mun && window.Shiny) Shiny.setInputValue('escolher_municipio', mun.dataset.municipio, {priority:'event'});
    const celula = e.target.closest('[data-cubo-mun]');
    if (celula && window.Shiny) Shiny.setInputValue('celula_cubo', {municipio:celula.dataset.cuboMun,ano:Number(celula.dataset.cuboAno)}, {priority:'event'});
    const pag = e.target.closest('[data-cubo-pagina]');
    if (pag && !pag.disabled && window.Shiny) Shiny.setInputValue('pagina_cubo',pag.dataset.cuboPagina,{priority:'event'});
    if (e.target.closest('#menu-mobile')) document.body.classList.toggle('menu-aberto');
  });
  document.addEventListener('keydown', (e) => { if (e.key === 'Escape') document.body.classList.remove('menu-aberto'); });
  const conectar = () => {
    if (!window.jQuery) return setTimeout(conectar, 30);
    window.jQuery(document).on('shiny:connected', () => ativar('visao'));
  };
  conectar();
})();
