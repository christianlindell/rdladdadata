#' Hämta en kodtabell från SCB:s företagsregister
#'
#' Hämtar en kodtabell, t.ex. för näringsgrenar eller storleksklasser, från
#' SCB:s API för det allmänna företagsregistret (apiafr.scb.se). Koderna
#' används som urval i [hamta_arbetsstallen()], [hamta_foretag()] och
#' [hamta_antal()].
#'
#' @param kodtabell Kodtabellens namn i API:et, som text. Obligatorisk.
#' @param api_key API-nyckel som text. Standard läses den från
#'   miljövariabeln `SCB_AFR_API_KEY` eller, om den saknas, från keyring
#'   (tjänsten `"SCB"`, användaren `"Foretagsregister"`).
#'
#' @return API:ets svar, tolkat från JSON till en lista eller data frame.
#' @export
#' @example inst/examples/hamta_kodtabell.R
hamta_kodtabell <- function(kodtabell, api_key = afr_nyckel()) {
  afr_request(c("kodtabeller", kodtabell), api_key = api_key) |>
    req_perform() |>
    afr_las_svar()
}
