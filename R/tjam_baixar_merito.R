#' Baixa pdfs com decisões de mérito
#'
#' @param df Dataframe com processos e urls dos méritos
#' @param diretorio Diretório onde baixar os pdfs
#'
#' @return pdfs
#' @export
#'
tjam_baixar_merito <- function(df, diretorio = "."){

  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  uri1 <- "https://consultasaj.tjam.jus.br/cpopg/search.do?"

  pb <- progress::progress_bar$new(total = nrow(df))

  purrr::walk2(df$processo, df$url,purrr::possibly(~{

    pb$tick()

  ### Este trecho apenas realiza a busca processual ####

    u <- "https://consultasaj.tjam.jus.br/cpopg/open.do?gateway=true"

    b<-  httr::GET(u)

    processo   <- stringr::str_remove_all(.x, "\\D+")

    p <- processo %>%
      stringr::str_pad(width = 20, "left", "0") %>%
      abjutils::build_id()

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


    ### Este trecho busca informações do documento, extrai os parâmetros e forma a url do pdf ####

    url_pdf <- httr::GET(.y) %>%
           httr::content("text") %>%
      stringr::str_extract("(?<=requestScope \\= )\\X+?(?=;)") %>%
      jsonlite::fromJSON() %>%
      purrr::pluck("children",1,"data","parametros") %>%
      paste0("https://consultasaj.tjam.jus.br/pastadigital/getPDF.do?",.)


    ### Este trecho baixa o pdf ####

    arquivo <- file.path(diretorio, paste0("processo_",processo,"_cdDocumento_",stringr::str_extract(url_pdf,"(?<=cdDocumento=)\\d+"),".pdf"))

    httr::GET(url_pdf,httr::write_disk(arquivo,overwrite = TRUE))


  },NULL))





}






