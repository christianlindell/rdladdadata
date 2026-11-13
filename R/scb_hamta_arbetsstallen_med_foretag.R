#' Hämta arbetsställen och deras företag från SCB:s sökpåver-API
#'
#' Hämtar arbetsställen med [scb_hamta_arbetsstallen()] och sedan de
#' företag som äger dem med [scb_hamta_foretag()], ett anrop per unikt
#' organisationsnummer (grupperat efter prefix). Resultatet är två tabeller
#' som kan slås ihop på organisationsnumret:
#' `dplyr::left_join(res$arbetsstallen, res$foretag, by = "OrgNr")`.
#'
#' Blir det fler unika organisationsnummer än `riks_varning_trosklel`
#' varnar funktionen och pausar en stund, eftersom det då ofta går
#' snabbare att hämta alla företag i Sverige för sig och slå ihop
#' tabellerna själv. Det sättet får också med moderbolag med säte i en
#' annan kommun.
#'
#' Med `cache_path` (t.ex. `"scb.rds"`) skapas två cachefiler, en per steg
#' (`scb_arbst.rds` och `scb_ftg.rds`).
#'
#' @inheritParams scb_hamta_arbetsstallen
#' @inheritParams scb_hamta_foretag
#' @param orgnr Organisationsnummer med tio siffror, som text. Anges de
#'   hämtas arbetsställena för dessa företag i stället för för
#'   `kommunkoder`. Standard är `NULL`.
#' @param grundfilter_variabler_arbst,grundfilter_kategorier_arbst Variabel-
#'   och kategorifilter för arbetsställena, i API:ets format. Standard är
#'   tomma listor.
#' @param grundfilter_variabler_ftg,grundfilter_kategorier_ftg Variabel- och
#'   kategorifilter för företagen, i API:ets format. Standard är tomma
#'   listor.
#' @param riks_varning_trosklel Antal unika organisationsnummer över vilket
#'   funktionen varnar. Standard är 10000. `NULL` varnar aldrig.
#' @param riks_varning_paus Antal sekunder att pausa efter varningen, så att
#'   du hinner avbryta. Standard är 5; 0 pausar inte.
#'
#' @return En lista med två tibbles: `arbetsstallen` och `foretag`.
#' @export
#' @example inst/examples/scb_hamta_arbetsstallen_med_foretag.R
scb_hamta_arbetsstallen_med_foretag <- function(
    kommunkoder = NULL,
    orgnr       = NULL,
    grundfilter_variabler_arbst  = list(),
    grundfilter_kategorier_arbst = list(),
    anstallda  = NULL,
    branscher  = NULL,
    vald_arbetsstallestatus = "1",
    grundfilter_variabler_ftg  = list(),
    grundfilter_kategorier_ftg = list(),
    vald_foretagsstatus      = "1",
    vald_registreringsstatus = "1",
    auto_loopa_kommuner = FALSE,
    auto_loopa_trosklel = 10000L,
    cache_path   = NULL,
    behall_cache = FALSE,
    max_forsok   = 3L,
    batch_via_prefix = TRUE,
    prefix_start     = 4L,
    riks_varning_trosklel = 10000L,
    riks_varning_paus     = 5L,
    cert_thumb = scb_hamta_cert_thumb(),
    visa_meddelanden_konsol          = FALSE,
    visa_resultat_meddelanden_konsol = TRUE,
    visa_progress                    = TRUE
) {
  total_start <- Sys.time()

  # Två separata cachefiler (en per fas) härledda från cache_path
  cache_arbst <- if (!is.null(cache_path)) sub("\\.rds$", "", cache_path, ignore.case = TRUE) %>% paste0("_arbst.rds") else NULL
  cache_ftg   <- if (!is.null(cache_path)) sub("\\.rds$", "", cache_path, ignore.case = TRUE) %>% paste0("_ftg.rds")   else NULL

  # ── Fas 1: Arbetsställen ────────────────────────────────────────────────
  if (visa_resultat_meddelanden_konsol)
    message("━━━ Fas 1/2: Hämtar arbetsställen ━━━")

  arbst <- scb_hamta_arbetsstallen(
    kommunkoder = kommunkoder,
    orgnr       = orgnr,
    grundfilter_variabler  = grundfilter_variabler_arbst,
    grundfilter_kategorier = grundfilter_kategorier_arbst,
    anstallda  = anstallda,
    branscher  = branscher,
    vald_arbetsstallestatus = vald_arbetsstallestatus,
    auto_loopa_kommuner = auto_loopa_kommuner,
    auto_loopa_trosklel = auto_loopa_trosklel,
    cache_path   = cache_arbst,
    behall_cache = behall_cache,
    max_forsok   = max_forsok,
    cert_thumb = cert_thumb,
    visa_meddelanden_konsol          = visa_meddelanden_konsol,
    visa_resultat_meddelanden_konsol = visa_resultat_meddelanden_konsol,
    visa_progress                    = visa_progress
  )

  if (is.null(arbst) || !NROW(arbst)) {
    message("⚠ Inga arbetsställen hittades — hoppar över företagsfasen")
    return(list(arbetsstallen = arbst %||% tibble(), foretag = tibble()))
  }

  # Plocka ut unika orgnr ur arbetsställeresultatet
  orgnr_kol <- hitta_orgnr_kolumn(arbst)
  if (is.na(orgnr_kol)) {
    message("⚠ Hittade ingen OrgNr-kolumn i arbetsställeresultatet — kan inte hämta företag")
    return(list(arbetsstallen = arbst, foretag = tibble()))
  }

  orgnr_unika <- unique(as.character(arbst[[orgnr_kol]]))
  orgnr_unika <- orgnr_unika[!is.na(orgnr_unika) & nzchar(orgnr_unika)]

  # Varning för stora uttag: prefix-batchningen ger 20–80× bara om orgnren
  # ligger tätt inom prefix-grupperna. För glesa, breda listor (typ alla orgnr
  # i ett län) är det oftast snabbare att hämta hela riket separat och joina
  # i databasen — det fångar dessutom moderbolag med säte i annan kommun.
  if (!is.null(riks_varning_trosklel) &&
      length(orgnr_unika) > riks_varning_trosklel &&
      visa_resultat_meddelanden_konsol) {
    message("")
    message(glue("⚠ {scb_format(length(orgnr_unika))} unika orgnr — för stora uttag är riks-strategin ofta snabbare:"))
    message("     ftg   <- scb_hamta_foretag(kommunkoder = \"00\", cache_path = \"ftg.rds\")")
    message("     arbst <- scb_hamta_arbetsstallen(kommunkoder = \"00\", cache_path = \"arbst.rds\")")
    message("   Den vägen fångar dessutom moderbolag med säte i annan kommun än arbetsstället.")
    if (isTRUE(riks_varning_paus > 0L)) {
      message(glue("   Fortsätter ändå om {riks_varning_paus} sek — Ctrl+C för att avbryta."))
      Sys.sleep(riks_varning_paus)
    }
    message("")
  }

  # ── Fas 2: Företag ──────────────────────────────────────────────────────
  if (visa_resultat_meddelanden_konsol)
    message(glue("━━━ Fas 2/2: Hämtar {scb_format(length(orgnr_unika))} unika företag ━━━"))

  ftg <- scb_hamta_foretag(
    orgnr = orgnr_unika,
    grundfilter_variabler  = grundfilter_variabler_ftg,
    grundfilter_kategorier = grundfilter_kategorier_ftg,
    vald_foretagsstatus      = vald_foretagsstatus,
    vald_registreringsstatus = vald_registreringsstatus,
    cache_path   = cache_ftg,
    behall_cache = behall_cache,
    max_forsok   = max_forsok,
    batch_via_prefix = batch_via_prefix,
    prefix_start     = prefix_start,
    cert_thumb = cert_thumb,
    visa_meddelanden_konsol          = visa_meddelanden_konsol,
    visa_resultat_meddelanden_konsol = visa_resultat_meddelanden_konsol,
    visa_progress                    = visa_progress
  )

  if (visa_resultat_meddelanden_konsol)
    message(glue("📦 Klart: {scb_format(NROW(arbst))} arbetsställen + {scb_format(NROW(ftg))} företag på {scb_tid(total_start)}"))

  list(arbetsstallen = arbst, foretag = ftg %||% tibble())
}
