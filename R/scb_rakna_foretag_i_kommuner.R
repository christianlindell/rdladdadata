#' Räkna företag per kommun i SCB:s sökpåver-API
#'
#' Frågar SCB:s sökpåver-API hur många företag som har säte i varje kommun,
#' utan att hämta dem. Ett anrop görs per kommun. Bra för att se hur stort
#' ett uttag med [scb_hamta_foretag()] blir.
#'
#' @param kommunkoder Läns- och kommunkoder som text. En länskod räknas om
#'   till länets kommuner och `"00"` till alla kommuner i Sverige.
#'   Obligatorisk.
#' @param foretagsstatus Företagsstatus som text. Standard är `"1"`
#'   (verksamma). `NULL` filtrerar inte.
#' @param registreringsstatus Registreringsstatus som text. Standard är
#'   `"1"`. `NULL` filtrerar inte.
#' @param extra_kategorier Lista med fler kategorifilter i API:ets format,
#'   t.ex. `list(list(Kategori = "Juridisk form", Kod = list("61")))`.
#'   `NULL` (standard) lägger inte till några.
#' @param cert_thumb Tumavtryck för SCB-certifikatet, som text. Standard
#'   letas certifikatet upp med [scb_hamta_cert()].
#' @param visa_konsol `TRUE` (standard) skriver ut antalet per kommun.
#'
#' @return En tibble med kolumnerna `kommun` och `antal_foretag`.
#' @export
#' @example inst/examples/scb_rakna_foretag_i_kommuner.R
scb_rakna_foretag_i_kommuner <- function(
    kommunkoder,
    foretagsstatus = "1",
    registreringsstatus = "1",
    extra_kategorier = NULL,
    cert_thumb = scb_hamta_cert_thumb(),
    visa_konsol = TRUE
){
  rakna_url <- "https://privateapi.scb.se/nv0101/v1/sokpavar/api/je/raknaforetag/"
  kommunkoder <- expandera_kommunkoder(kommunkoder)
  .api_calls <- new.env(); .api_calls$timestamps <- numeric(0)

  map_dfr(kommunkoder, function(kom){
    kat <- list(list(Kategori = "SätesKommun", Kod = list(kom)))
    if (!is.null(extra_kategorier)) kat <- c(kat, extra_kategorier)
    payload <- list(Kategorier = kat)
    if (!is.null(foretagsstatus))      payload$Företagsstatus      <- foretagsstatus
    if (!is.null(registreringsstatus)) payload$Registreringsstatus <- registreringsstatus
    n <- rakna_api(rakna_url, payload, cert_thumb, .api_calls)
    if (visa_konsol) message(glue("📏 {kom}: {n %||% NA} företag"))
    tibble(kommun = kom, antal_foretag = as.integer(n))
  })
}
