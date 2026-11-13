#' Hämta juridiska enheter (företag) från SCB:s företagsregister
#'
#' Hämtar juridiska enheter från SCB:s API för det allmänna
#' företagsregistret (apiafr.scb.se), på samma sätt som
#' [hamta_arbetsstallen()] hämtar arbetsställen. För juridiska enheter
#' avser `regionkod` troligen säteskommun eller säteslän.
#'
#' API:et kan bara filtrera på en sak åt gången, så ange högst en av
#' `regionkod`, `naringsgren`, `anstalldaklass`, `omsattningsklass` och
#' `juridiskform`. Statusfiltreringen görs i R efter hämtningen.
#'
#' @inheritParams hamta_arbetsstallen
#' @inheritParams hamta_antal
#'
#' @return En tibble med en rad per juridisk enhet och kolumnen
#'   `uttag_datum`.
#' @seealso [hamta_antal()], [hamta_kodtabell()]. [scb_hamta_foretag()]
#'   använder SCB:s äldre sökpåver-API.
#' @export
#' @example inst/examples/hamta_foretag.R
hamta_foretag <- function(regionkod = NULL,
                           naringsgren = NULL,
                           anstalldaklass = NULL,
                           omsattningsklass = NULL,
                           juridiskform = NULL,
                           full = FALSE,
                           status = "1",
                           meddelande = TRUE,
                           visa_progress = TRUE,
                           tid_per_chunk = FALSE,
                           api_key = afr_nyckel(), ...) {
  afr_hamta("juridiskaenheter", "jes", "ftgStat", regionkod,
            list(naringsgren = naringsgren, anstalldaklass = anstalldaklass,
                 omsattningsklass = omsattningsklass, juridiskform = juridiskform),
            full, status, meddelande, visa_progress, tid_per_chunk, api_key, ...)
}
