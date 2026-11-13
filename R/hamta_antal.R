#' Räkna arbetsställen eller juridiska enheter i SCB:s företagsregister
#'
#' Frågar SCB:s API för företagsregistret (apiafr.scb.se) hur många
#' arbetsställen eller juridiska enheter som finns för ett urval, utan att
#' hämta dem. Antalet gäller alla statusar, eftersom API:et inte kan filtrera
#' på status. Funktionen kan också uppskatta hur lång tid en hämtning med
#' [hamta_arbetsstallen()] eller [hamta_foretag()] skulle ta. Uppskattningen
#' bygger på den senaste hämtningen i sessionen, eller på ett standardvärde.
#'
#' API-nyckeln läses från miljövariabeln `SCB_AFR_API_KEY` eller, om den
#' saknas, från keyring (tjänsten `"SCB"`, användaren `"Foretagsregister"`).
#'
#' API:et kan bara filtrera på en sak åt gången, så ange högst en av
#' `regionkod`, `naringsgren`, `anstalldaklass`, `omsattningsklass` och
#' `juridiskform`.
#'
#' @param objekt `"arbetsstallen"` (standard) eller `"juridiskaenheter"`.
#' @param regionkod Läns- och kommunkoder som text, t.ex. `"20"` eller
#'   `c("20", "2180")`. `NULL` (standard) eller `"00"` gäller hela Sverige.
#' @param naringsgren Näringsgrenskod (SNI) som text. `NULL` (standard)
#'   filtrerar inte.
#' @param anstalldaklass Storleksklass för antal anställda, som text. `NULL`
#'   (standard) filtrerar inte.
#' @param omsattningsklass Omsättningsklass, som text. Bara för
#'   `"juridiskaenheter"`. `NULL` (standard) filtrerar inte.
#' @param juridiskform Juridisk form, som text. Bara för
#'   `"juridiskaenheter"`. `NULL` (standard) filtrerar inte.
#' @param full `TRUE` uppskattar tiden för en hämtning med alla variabler
#'   (bara hela Sverige). Standard är `FALSE`.
#' @param uppskatta_tid `TRUE` (standard) skriver ut en uppskattning av
#'   hämtningstiden.
#' @param limit Antal rader per sida i uppskattningen. Standard är 5000,
#'   samma som vid hämtning.
#' @param api_key API-nyckel som text. Standard läses den från
#'   `SCB_AFR_API_KEY` eller keyring.
#'
#' @return Ett heltal, eller en namngiven heltalsvektor med ett antal per kod
#'   när flera koder anges.
#' @export
#' @example inst/examples/hamta_antal.R
hamta_antal <- function(objekt = c("arbetsstallen", "juridiskaenheter"),
                         regionkod = NULL,
                         naringsgren = NULL,
                         anstalldaklass = NULL,
                         omsattningsklass = NULL,
                         juridiskform = NULL,
                         full = FALSE,
                         uppskatta_tid = TRUE,
                         limit = 5000,
                         api_key = afr_nyckel()) {
  objekt <- match.arg(objekt)
  if (objekt == "arbetsstallen" && !(is.null(omsattningsklass) && is.null(juridiskform))) {
    stop("omsattningsklass och juridiskform finns bara för juridiskaenheter.", call. = FALSE)
  }
  
  sokvagar <- afr_bygg_sokvagar(
    objekt, regionkod,
    list(naringsgren = naringsgren, anstalldaklass = anstalldaklass,
         omsattningsklass = omsattningsklass, juridiskform = juridiskform),
    full = full
  )
  antal <- map_int(sokvagar, \(s) afr_antal_sokvag(s, api_key))
  if (length(sokvagar[[1]]) > 2) {
    antal <- set_names(antal, map_chr(sokvagar, \(s) tail(s, 1)))
  }
  
  if (uppskatta_tid) {
    typ   <- if (full) "full" else "urval"
    kalla <- afr_matning$kalla[[typ]]
    sek   <- afr_matning$sek_per_sida[[typ]]
    if (is.na(sek)) {                                 # /full ej uppmätt: använd urval
      sek   <- afr_matning$sek_per_sida[["urval"]]
      kalla <- "uppmätt för urvalsvarianten, /full är troligen långsammare"
    }
    # Varje kod pagineras för sig, plus ett count-anrop per kod för progress bar
    sidor <- sum(pmax(ceiling(antal / limit), 1))
    tid   <- sidor * sek + length(antal) * 0.5
    
    cli::cli_inform(c(
      "i" = paste0("Uppskattad hämtningstid: ca ", afr_formatera_tid(tid), " (",
                   afr_formatera_antal(sidor), " sidor à ", sprintf("%.1f", sek),
                   " s, ", kalla, ")."),
      " " = "Grov uppskattning: svarstiden varierar med belastningen hos SCB."
    ))
  }
  
  antal
}
