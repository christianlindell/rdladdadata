#' Hämta arbetsställen från SCB:s företagsregister
#'
#' Hämtar arbetsställen från SCB:s API för det allmänna företagsregistret
#' (apiafr.scb.se). API:et lämnar 5 000 rader per sida och funktionen hämtar
#' alla sidor, med en förloppsindikator och en sammanfattning när den är
#' klar. Anropen begränsas till fyra per sekund och görs om vid tillfälliga
#' fel.
#'
#' API:et kan inte filtrera på status, så statusfiltreringen görs i R efter
#' hämtningen. Det kan heller bara filtrera på en sak åt gången, så ange
#' högst en av `regionkod`, `naringsgren` och `anstalldaklass`. API:et är
#' nere 04.00–04.30 varje natt.
#'
#' API-nyckeln läses från miljövariabeln `SCB_AFR_API_KEY` eller, om den
#' saknas, från keyring (tjänsten `"SCB"`, användaren `"Foretagsregister"`).
#'
#' @inheritParams hamta_antal
#' @param full `TRUE` hämtar alla variabler. Det går bara för hela Sverige,
#'   utan urval. Standard är `FALSE`.
#' @param status Status som ska behållas, som text: `"1"` (standard) är
#'   verksamma, `"9"` ej längre verksamma och `"0"` aldrig verksamma. Flera
#'   går att ange. `NULL` behåller alla.
#' @param meddelande `TRUE` (standard) skriver ut en sammanfattning av
#'   uttaget.
#' @param visa_progress `TRUE` (standard) visar en förloppsindikator.
#' @param tid_per_chunk `TRUE` skriver ut tiden för varje sida. Standard är
#'   `FALSE`.
#' @param ... Skickas vidare till den interna sidhämtningen, t.ex. `limit`
#'   (antal rader per sida, standard 5000).
#'
#' @return En tibble med en rad per arbetsställe och kolumnen `uttag_datum`.
#' @seealso [hamta_antal()] för att räkna innan du hämtar och
#'   [hamta_kodtabell()] för koderna. [scb_hamta_arbetsstallen()] använder
#'   SCB:s äldre sökpåver-API.
#' @export
#' @example inst/examples/hamta_arbetsstallen.R
hamta_arbetsstallen <- function(regionkod = NULL,
                                 naringsgren = NULL,
                                 anstalldaklass = NULL,
                                 full = FALSE,
                                 status = "1",
                                 meddelande = TRUE,
                                 visa_progress = TRUE,
                                 tid_per_chunk = FALSE,
                                 api_key = afr_nyckel(), ...) {
  afr_hamta("arbetsstallen", "arbetsstallen", "aeStat", regionkod,
            list(naringsgren = naringsgren, anstalldaklass = anstalldaklass),
            full, status, meddelande, visa_progress, tid_per_chunk, api_key, ...)
}
