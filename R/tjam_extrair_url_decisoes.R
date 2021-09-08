#' Extrair urls das decisões dos andamentos do cpopg
#'
#' @param arquivos Informar arquivos
#' @param diretorio Informar diretório se não informar arquivos
#'
#' @return tibble com processo, data_movimentacao, descricao e url.
#' @export
#'
tjam_extrair_url_decisoes <- function(arquivos = NULL,
           diretorio = ".") {

    if (is.null(arquivos)) {
      arquivos <-
        list.files(diretorio, full.names = TRUE, pattern = "html$")
    }

    purrr::map_dfr(arquivos,
                   purrr::possibly(purrrogress::with_progress( ~ {
                     processo <- stringr::str_extract(.x, "\\d{20}")
                     x <- xml2::read_html(.x)
                     data_movimentacao <- x %>%
                       xml2::xml_find_all(
                         "//a[contains(text(),'Mérito')]/ancestor::td/preceding::td[@class='dataMovimentacao'][1]"
                       ) %>%
                       xml2::xml_text(trim = TRUE) %>%
                       lubridate::dmy()
                     url <- x %>%
                       xml2::xml_find_all("//a[contains(text(),'Mérito')]") %>%
                       xml2::xml_attr("href") %>%
                       xml2::url_absolute("https://consultasaj.tjam.jus.br")
                     descricao <- x %>%
                       xml2::xml_find_all("//a[contains(text(),'Mérito')]") %>%
                       xml2::xml_text(trim = TRUE)
                     tibble::tibble(
                       processo = processo,
                       data_movimentacao = data_movimentacao,
                       descricao = descricao,
                       url = url
                     ) %>%
                       dplyr::distinct()
                   }), NULL))
  }
