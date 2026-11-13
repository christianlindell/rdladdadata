#' Räkna arbetsställen per kommun i SCB:s sökpåver-API
#'
#' Frågar SCB:s sökpåver-API hur många arbetsställen som finns i varje
#' kommun, utan att hämta dem. Ett anrop görs per kommun. Bra för att se hur
#' stort ett uttag med [scb_hamta_arbetsstallen()] blir.
#'
#' @inheritParams scb_rakna_foretag_i_kommuner
#' @param arbetsstallestatus Arbetsställestatus som text. Standard är `"1"`
#'   (verksamma).
#'
#' @return En tibble med kolumnerna `kommun` och `antal_arbetsstallen`.
#' @export
#' @example inst/examples/scb_rakna_arbetsstallen_i_kommuner.R
scb_rakna_arbetsstallen_i_kommuner <- function(
    kommunkoder,
    arbetsstallestatus = "1",
    extra_kategorier = NULL,
    cert_thumb = scb_hamta_cert_thumb(),
    visa_konsol = TRUE
){
  rakna_url <- "https://privateapi.scb.se/nv0101/v1/sokpavar/api/ae/raknaarbetsstallen/"
  kommunkoder <- expandera_kommunkoder(kommunkoder)
  .api_calls <- new.env(); .api_calls$timestamps <- numeric(0)

  map_dfr(kommunkoder, function(kom){
    kat <- list(list(Kategori = "Kommun", Kod = list(kom)))
    if (!is.null(extra_kategorier)) kat <- c(kat, extra_kategorier)
    payload <- list(Arbetsställestatus = arbetsstallestatus, Kategorier = kat)
    n <- rakna_api(rakna_url, payload, cert_thumb, .api_calls)
    if (visa_konsol) message(glue("📏 {kom}: {n %||% NA} arbetsställen"))
    tibble(kommun = kom, antal_arbetsstallen = as.integer(n))
  })
}
