#' Baixar consulta processual de primeiro grau
#'
#' @param processos Número do processo (cnj)
#' @param diretorio Diretório onde serão armazenados os htmls
#'
#' @return html com dados processuais
#' @export
#'
tjam_baixar_cpopg <- function (processos = NULL, diretorio = ".")
{
  httr::set_config(httr::config(ssl_verifypeer = FALSE))


  processos <- stringr::str_remove_all(processos, "\\D+") %>%
    stringr::str_pad(width = 20, "left", "0") %>%
    abjutils::build_id()




  uri1 <- "https://consultasaj.tjam.jus.br/cpopg/search.do?"



  purrr::map_dfr(processos, purrr::possibly(purrrogress::with_progress(~{

    u <- "https://consultasaj.tjam.jus.br/cpopg/open.do?gateway=true"

  b<-  httr::GET(u)

  #d <- httr::GET(url2,httr::set_cookies(unlist(b$cookies)))


     p <- .x
    unificado <- p %>% stringr::str_extract(".{15}")
    foro <- p %>% stringr::str_extract("\\d{4}$")

    query1 <-
      list(
        gateway="true",
        conversationId = "",
        cbPesquisa = "NUMPROC",
        numeroDigitoAnoUnificado = unificado,
        foroNumeroUnificado = foro,
        dadosConsulta.valorConsultaNuUnificado = p,
        dadosConsulta.valorConsultaNuUnificado = "UNIFICADO",
        dadosConsulta.valorConsulta = "",
        dadosConsulta.tipoNuProcesso = "UNIFICADO"
      )

    resposta1 <- httr::RETRY("GET", url = uri1, query = query1,
                             quiet = TRUE, httr::timeout(2))

    conteudo1 <- httr::content(resposta1)

    if (xml2::xml_find_first(conteudo1, "boolean(//div[@id='listagemDeProcessos'])")) {
      conteudo1 <- xml2::xml_find_all(conteudo1, "//a[@class='linkProcesso']") %>%
        xml2::xml_attr("href") %>% xml2::url_absolute("https://conulstasaj.tjam.jus.br") %>%
        purrr::map(~httr::RETRY("GET", .x, httr::timeout(2)) %>%
                     httr::content())
    }
    else
      conteudo1 <- list(conteudo1)

    p <- stringr::str_remove_all(p, "\\D+")
    arquivo <- file.path(diretorio, paste0(format(Sys.Date(),
                                                  "%Y_%m_%d_"), p, ".html"))
    xml2::write_html(conteudo1[[1]], arquivo)

  }), NULL))
}
