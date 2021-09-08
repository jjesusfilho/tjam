tjam_cpopg_baixar_tabela_docs <- function (processos = NULL, diretorio = ".")
{

  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  processos <- stringr::str_remove_all(processos, "\\D+") %>%
    stringr::str_pad(width = 20, "left", "0") %>%
    abjutils::build_id()

  uri1 <- "https://consultasaj.tjam.jus.br/cpopg/search.do?"

  u <- "https://consultasaj.tjam.jus.br/cpopg/open.do?gateway=true"


  pb <- progress::progress_bar$new(total = length(processos))

  purrr::walk(processos, purrr::possibly(~{

    pb$tick()


    b <-  httr::GET(u)


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
        xml2::xml_attr("href") %>% xml2::url_absolute("https://consultasaj.tjam.jus.br") %>%
        purrr::map(~httr::RETRY("GET", .x, httr::timeout(2)) %>%
                     httr::content())
    } else {
      conteudo1 <- list(conteudo1)
    }



    purrr::walk(conteudo1,purrr::possibly(~{

      arquivo <- file.path(diretorio,paste0("tabela_cpopg_docs_processo_",stringr::str_remove_all(p,"\\D"),".html"))


      url1 <- .x %>%
        xml2::xml_find_first("//a[@id='linkPasta']") %>%
        xml2::xml_attr("href") %>%
        paste0("https://consultasaj.tjam.jus.br",.)


    httr::GET(url1,
              httr::write_disk(arquivo,overwrite = TRUE))


    },NULL))
  }, NULL))
}



url2 <- "https://consultasaj.tjam.jus.br/pastadigital/getPDF.do?nuSeqRecurso=00000&nuProcesso=0244292-42.2016.8.04.0001&cdDocumentoOrigem=0&cdDocumento=131770460&conferenciaDocEdigOriginal=false&nmAlias=PG5DS&origemDocumento=P&nuPagina=0&numInicial=151&tpOrigem=2&cdTipoDocDigital=36&flOrigem=P&deTipoDocDigital=Certid%E3o&cdProcesso=01001V5V30000&cdFormatoDoc=5&cdForo=1&idDocumento=131770460-151-0&numFinal=151&sigiloExterno=N"

url3 <- httr::parse_url(url2)

url3$query

