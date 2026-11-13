#' Hämta kategorier och kodtabeller från SCB:s sökpåver-API
#'
#' Hämtar de kategorier (t.ex. "Juridisk form", "2-siffrig bransch 1",
#' "Anställda") som går att filtrera på i SCB:s sökpåver-API för
#' företagsregistret, med deras koder och klartexter. Koderna används i
#' `grundfilter_kategorier` och i filterparametrarna till
#' [scb_hamta_foretag()] och [scb_hamta_arbetsstallen()].
#'
#' @param tabell `"foretag"` (standard) för företag (juridiska enheter).
#'   Andra värden ger arbetsställen.
#' @param med_varden `TRUE` (standard) ger en rad per kod. `FALSE` ger en
#'   rad per kategori med antal koder.
#' @param cert_thumb Tumavtryck för SCB-certifikatet, som text. Standard
#'   letas certifikatet upp med [scb_hamta_cert()].
#'
#' @return En tibble med kolumnerna `kategori_id`, `kategori_typ`, `kod` och
#'   `text`, eller `kategori_id` och `antal_unika_varden` om
#'   `med_varden = FALSE`.
#' @export
#' @example inst/examples/scb_kategorier_med_kodtabeller.R
scb_kategorier_med_kodtabeller <- function(
    tabell = "foretag",
    med_varden = TRUE,
    cert_thumb = scb_hamta_cert_thumb()) {

  del <- if_else(tolower(tabell) %in% c("ftg", "foretag", "företag"), "Je", "Ae")
  url <- paste0("https://privateapi.scb.se/nv0101/v1/sokpavar/api/", del, "/KategorierMedKodtabeller")

  r <- httr::GET(url, config = httr::config(sslcert = paste0(scb_cert_info()$store, "\\MY\\", cert_thumb), sslcerttype = "Schannel"))
  httr::stop_for_status(r)
  kats <- httr::content(r, as = "parsed", encoding = "UTF-8")

  retur_df <- tibble(
    kategori_id  = map_chr(kats, ~ .x$Id_Kategori_JE %||% .x$Id_Kategori_AE %||% .x$Id_Kategori %||% .x$Id),
    kategori_typ = map_chr(kats, ~ .x$Datatyp %||% NA_character_),
    varde_lista  = map(kats, ~ .x$VardeLista %||% list())
  ) |>
    mutate(varde_lista = map(varde_lista, ~ keep(.x, ~ is.list(.x) && (!is.null(.x$Varde) || !is.null(.x$Kod))))) |>
    unnest_longer(varde_lista, keep_empty = TRUE) |>
    mutate(
      kod  = map_chr(varde_lista, ~ as.character(.x$Varde %||% .x$Kod %||% NA_character_)),
      text = map_chr(varde_lista, ~ as.character(.x$Text  %||% .x$Beskrivning %||% NA_character_))
    ) |>
    select(-varde_lista)

  if (!med_varden) {
    retur_df <- retur_df %>%
      group_by(kategori_id) %>%
      summarise(antal_unika_varden = n(), .groups = "drop")
  }
  retur_df
}
