#' Hämta arbetsställen från SCB:s sökpåver-API
#'
#' Hämtar arbetsställen från SCB:s sökpåver-API för företagsregistret. Som
#' [scb_hamta_foretag()] delas stora urval upp automatiskt, här efter
#' organisationsnummer (prefix), postnummer (intervall), antal anställda och
#' bransch. Med `orgnr` hämtas arbetsställena för en lista med
#' organisationsnummer, ett anrop per nummer.
#'
#' Funktionen kräver SCB:s certifikat för sökpåver-API:t i Windows
#' certifikatförråd, se [scb_hamta_cert()].
#'
#' @inheritParams scb_hamta_foretag
#' @param orgnr Organisationsnummer med tio siffror, som text. Anges de
#'   hämtas bara arbetsställena för dessa företag. Standard är `NULL`.
#' @param anstallda Koder för antal anställda som text, t.ex.
#'   `c("6", "7", "8", "9")` för 50 anställda eller fler. `NULL` (standard)
#'   filtrerar inte.
#' @param vald_arbetsstallestatus Arbetsställestatus som text. Standard är
#'   `"1"` (verksamma). `NULL` filtrerar inte.
#'
#' @return En tibble med en rad per arbetsställe. Hämtas kommun för kommun
#'   får den också kolumnen `kod_kommun`.
#' @seealso [scb_rakna_arbetsstallen_i_kommuner()],
#'   [scb_hamta_arbetsstallen_med_foretag()] och [hamta_arbetsstallen()] för
#'   SCB:s nya API.
#' @export
#' @example inst/examples/scb_hamta_arbetsstallen.R
scb_hamta_arbetsstallen <- function(
    kommunkoder = NULL,
    orgnr = NULL,
    grundfilter_variabler  = list(),
    grundfilter_kategorier = list(),
    anstallda = NULL,
    branscher = NULL,
    vald_arbetsstallestatus = "1",
    auto_loopa_kommuner     = FALSE,
    auto_loopa_trosklel     = 10000L,
    cache_path              = NULL,
    behall_cache            = FALSE,
    max_forsok              = 3L,
    cert_thumb = scb_hamta_cert_thumb(),
    visa_meddelanden_konsol          = FALSE,
    visa_resultat_meddelanden_konsol = TRUE,
    visa_progress                    = TRUE
) {
  total_start <- Sys.time()
  base_url  <- "https://privateapi.scb.se/nv0101/v1/sokpavar/api/"
  rakna_url <- paste0(base_url, "ae/raknaarbetsstallen/")
  hamta_url <- paste0(base_url, "ae/hamtaarbetsstallen/")
  max_rader <- 2000L

  # Normalisera: NULL och "00" betyder båda "hela riket" — låt rakna+auto_loopa-
  # logiken nedan avgöra om det blir single-shot eller kommun-loop.
  if (is.null(kommunkoder) || "00" %in% as.character(kommunkoder)) {
    kommunkoder <- NULL
  } else {
    kommunkoder <- expandera_kommunkoder(kommunkoder)
  }

  .api_calls <- new.env(); .api_calls$timestamps <- numeric(0)

  rakna <- function(p) rakna_api(rakna_url, p, cert_thumb, .api_calls, visa_meddelanden_konsol, max_forsok)
  hamta <- function(p) hamta_api(hamta_url, p, cert_thumb, .api_calls, visa_meddelanden_konsol, max_forsok)

  ctx <- list(rakna = rakna, hamta = hamta, max_rader = max_rader,
              visa_medd = visa_meddelanden_konsol)

  bygg_baspayload <- function(extra_kategorier = list()) {
    kat <- grundfilter_kategorier
    if (length(extra_kategorier)) kat <- c(kat, extra_kategorier)
    if (!is.null(anstallda)) kat <- c(kat, list(list(Kategori = "Anställda",             Kod = as.list(anstallda))))
    if (!is.null(branscher)) kat <- c(kat, list(list(Kategori = "2-siffrig bransch 1",   Kod = as.list(branscher))))
    skapa_baspayload(
      typ = "arbetsstalle",
      grundfilter_kategorier = kat,
      grundfilter_variabler  = grundfilter_variabler,
      arbetsstallestatus     = vald_arbetsstallestatus
    )
  }

  # OrgNr-prefix först → 100% täckning. Kategori-strategier som backup.
  strategier <- list(
    strategi_orgnr,
    strategi_postnr,
    strategi_kategori("Anställda",
                      varden_fn = function() hamta_kategorivarden("arbetsstalle", "Anställda", cert_thumb)),
    strategi_kategori("2-siffrig bransch 1",
                      varden_fn = function() hamta_kategorivarden("arbetsstalle", "2-siffrig bransch 1", cert_thumb))
  )

  # ── Orgnr-läge ────────────────────────────────────────────────────────────
  if (!is.null(orgnr) && length(orgnr)) {
    # Ladda cache om finns (checkpoint var 100:e orgnr)
    resultat_lista <- list()
    done_orgnr <- character()
    if (!is.null(cache_path) && file.exists(cache_path)) {
      cache <- tryCatch(readRDS(cache_path), error = function(e) NULL)
      if (!is.null(cache) && is.list(cache)) {
        resultat_lista <- cache$resultat   %||% list()
        done_orgnr     <- cache$done_orgnr %||% character()
        if (visa_resultat_meddelanden_konsol)
          message(glue("♻ Återupptar från cache: {length(done_orgnr)} orgnr redan klara"))
      }
    }
    todo_orgnr <- setdiff(orgnr, done_orgnr)

    if (visa_resultat_meddelanden_konsol)
      message(glue("🔎 Hämtar arbetsställen för {scb_format(length(todo_orgnr))} orgnr..."))

    pb <- if (visa_progress) progress::progress_bar$new(
      total = length(orgnr),
      format = ":current/:total [:bar] :percent :message", clear = FALSE
    ) else list(tick = function(...) NULL)
    if (length(done_orgnr)) suppressWarnings(pb$tick(length(done_orgnr)))

    # Ett orgnr kan teoretiskt ha >2000 arbetsställen — fall tillbaka på
    # postnr-halvering om så är fallet.
    inre_strategier <- list(strategi_postnr)

    spara_var <- 100L
    for (idx in seq_along(todo_orgnr)) {
      o <- todo_orgnr[idx]
      pb$tick(tokens = list(message = o))
      p <- bygg_baspayload()
      p <- satt_variabel(p, "OrgNr (10 siffror)", o, "", "ArLikaMed")
      r <- hamta_rekursivt(p, inre_strategier, ctx, djup = 0L)
      if (!is.null(r)) resultat_lista[[length(resultat_lista) + 1]] <- r
      done_orgnr <- c(done_orgnr, o)

      if (!is.null(cache_path) && (idx %% spara_var == 0L || idx == length(todo_orgnr))) {
        tryCatch(
          saveRDS(list(resultat = resultat_lista, done_orgnr = done_orgnr), cache_path),
          error = function(e) message(glue("⚠ Kunde inte spara cache: {conditionMessage(e)}"))
        )
      }
    }

    df <- resultat_lista %>% compact() %>% list_rbind()

    if (!is.null(cache_path) && !behall_cache && file.exists(cache_path)) {
      tryCatch(file.remove(cache_path), error = function(e) NULL)
    }

    if (visa_resultat_meddelanden_konsol)
      message(glue("📊 Hämtade {scb_format(NROW(df))} arbetsställen för {length(orgnr)} orgnr på {scb_tid(total_start)}"))
    return(df)
  }

  # ── Räkna totalen (om ingen kommunavgränsning) ──────────────────────────
  # Samma logik som för företag: räkna alltid totalen när kommun är NULL,
  # för progress-bar + sanity check. Auto-loopen aktiveras bara om över tröskeln.
  n_tot_auto <- NA_integer_
  if (is.null(kommunkoder)) {
    n_tot_auto <- rakna(bygg_baspayload())
    if (!is.na(n_tot_auto) && n_tot_auto > auto_loopa_trosklel && auto_loopa_kommuner) {
      if (visa_resultat_meddelanden_konsol)
        message(glue("🌍 Hela uttaget = {scb_format(n_tot_auto)} arbetsställen — loopar Sveriges kommuner automatiskt"))
      kommunkoder <- hamta_alla_kommunkoder()
    } else if (visa_resultat_meddelanden_konsol && !is.na(n_tot_auto)) {
      message(glue("ℹ Hela uttaget = {scb_format(n_tot_auto)} arbetsställen — hämtar utan kommun-loop"))
    }
  }

  # Räknare för bortskippade rader
  skip_env <- new.env(); skip_env$rader <- 0L; skip_env$celler <- 0L
  ctx$skip_env <- skip_env

  # ── Kommunloop ────────────────────────────────────────────────────────────
  kommun_lista <- if (is.null(kommunkoder)) list(NULL) else as.list(kommunkoder)

  pb <- if (visa_progress && length(kommun_lista) > 1) progress::progress_bar$new(
    total = length(kommun_lista),
    format = ":current/:total [:bar] :percent :message", clear = FALSE
  ) else list(tick = function(...) NULL)

  # Single-shot-läge: progress-bar som tickar per hämtad rad mot totalen
  ctx$fetch_pb <- if (visa_progress && length(kommun_lista) == 1 &&
                      !is.na(n_tot_auto) && n_tot_auto > 0) {
    progress::progress_bar$new(
      total = n_tot_auto,
      format = "  📥 hämtade :current/:total [:bar] :percent eta :eta",
      clear = FALSE, show_after = 0
    )
  } else NULL

  # Ladda cache om finns (per-kommun checkpoint)
  resultat <- list()
  done_kommuner <- character()
  if (!is.null(cache_path) && file.exists(cache_path)) {
    cache <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(cache) && is.list(cache)) {
      resultat      <- cache$resultat      %||% list()
      done_kommuner <- cache$done_kommuner %||% character()
      if (visa_resultat_meddelanden_konsol)
        message(glue("♻ Återupptar från cache: {length(done_kommuner)} kommuner redan klara ({scb_format(sum(map_int(resultat, NROW)))} rader)"))
    }
  }

  for (kom in kommun_lista) {
    # Hoppa över redan klara kommuner från cache
    if (!is.null(kom) && kom %in% done_kommuner) {
      pb$tick(tokens = list(message = paste(kom, "(cache)")))
      next
    }

    kom_start <- Sys.time()
    pb$tick(tokens = list(message = kom %||% "(alla)"))

    extra_kat <- if (!is.null(kom)) list(list(Kategori = "Kommun", Kod = list(kom))) else list()
    payload <- bygg_baspayload(extra_kategorier = extra_kat)

    df <- hamta_rekursivt(payload, strategier, ctx, djup = 0L)
    if (!is.null(df) && nrow(df)) {
      if (!is.null(kom)) df$kod_kommun <- kom
      resultat[[length(resultat) + 1]] <- df
    }

    # Spara checkpoint efter varje klar kommun
    if (!is.null(kom)) done_kommuner <- c(done_kommuner, kom)
    if (!is.null(cache_path)) {
      tryCatch(
        saveRDS(list(resultat = resultat, done_kommuner = done_kommuner), cache_path),
        error = function(e) message(glue("⚠ Kunde inte spara cache: {conditionMessage(e)}"))
      )
    }

    if (visa_resultat_meddelanden_konsol) {
      n_kom <- if (!is.null(df)) nrow(df) else 0L
      message(glue("🏁 {kom %||% 'alla kommuner'}: hämtade {scb_format(n_kom)} arbetsställen på {scb_sec(kom_start)} sek"))
    }
  }

  retur_df <- resultat %>% list_rbind()

  # Bortskippade celler (alla strategier slut)
  if (skip_env$celler > 0L) {
    message(glue("⚠ Hoppade över {skip_env$celler} celler ({scb_format(skip_env$rader)} räknade rader) — strategierna räckte inte hela vägen"))
  }

  # Sanity check: jämför resultat med auto-totalen.
  if (!is.na(n_tot_auto) && n_tot_auto > nrow(retur_df)) {
    gap <- n_tot_auto - nrow(retur_df)
    if (length(kommun_lista) > 1) {
      message(glue("⚠ {scb_format(gap)} arbetsställen saknades jfr totalen — sannolikt poster utan registrerad Kommun"))
    } else {
      message(glue("⚠ {scb_format(gap)} arbetsställen saknades jfr totalen ({scb_format(n_tot_auto)} räknade, {scb_format(nrow(retur_df))} hämtade)"))
    }
  }

  # Städa bort cachefilen vid lyckad körning (om användaren inte begärt att behålla den)
  if (!is.null(cache_path) && !behall_cache && file.exists(cache_path)) {
    tryCatch(file.remove(cache_path), error = function(e) NULL)
  }

  if (visa_resultat_meddelanden_konsol) {
    kom_txt <- if (length(kommun_lista) > 1) "kommuner" else "kommun"
    message(glue("📊 Totalt {scb_format(nrow(retur_df))} arbetsställen från {length(kommun_lista)} {kom_txt} på {scb_tid(total_start)}"))
  }
  retur_df
}
