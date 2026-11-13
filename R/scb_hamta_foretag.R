#' Hämta företag från SCB:s sökpåver-API
#'
#' Hämtar företag (juridiska enheter) från SCB:s sökpåver-API för
#' företagsregistret. API:t lämnar högst 2 000 rader per anrop, så
#' funktionen räknar först hur många rader ett urval ger och delar upp det
#' i mindre delar tills varje del ryms. Uppdelningen görs efter
#' organisationsnummer (prefix), postnummer (intervall), omsättningsklass,
#' bransch och juridisk form, i den ordningen. Prefix kommer först eftersom
#' alla företag har ett organisationsnummer. Kategoriuppdelningarna tappar
#' poster som saknar värde i kategorin.
#'
#' Med `orgnr` hämtas en lista med organisationsnummer i stället. Numren
#' grupperas då efter prefix, så att ett anrop kan hämta många nummer.
#'
#' Anropen begränsas till tio per tio sekunder och görs om vid tillfälliga
#' fel. Med `cache_path` sparas en kontrollpunkt efter varje kommun (eller
#' grupp av organisationsnummer), så att en avbruten hämtning kan fortsätta
#' där den slutade.
#'
#' Funktionen kräver SCB:s certifikat för sökpåver-API:t i Windows
#' certifikatförråd, se [scb_hamta_cert()]. Inga betalda tilläggsgrupper
#' används.
#'
#' @param kommunkoder Läns- och kommunkoder som text, t.ex.
#'   `c("2080", "2081")` (Falun och Borlänge), `"20"` (alla kommuner i
#'   Dalarna) eller `c("21", "2080")`. `NULL` (standard) eller `"00"` gäller
#'   hela Sverige.
#' @param orgnr Organisationsnummer med tio siffror, som text. Anges de
#'   hämtas bara dessa företag och `kommunkoder` används inte. Standard är
#'   `NULL`.
#' @param grundfilter_variabler Lista med variabelfilter i API:ets format,
#'   t.ex. `list(list(Varde1 = "ask", Varde2 = "", Operator = "Innehaller",
#'   Variabel = "Firma"))`. Skickas direkt till API:t. Standard är en tom
#'   lista.
#' @param grundfilter_kategorier Lista med kategorifilter i API:ets format,
#'   t.ex. `list(list(Kategori = "Juridisk form", Kod = list("61")))`.
#'   Standard är en tom lista.
#' @param juridisk_kod Koder för juridisk form som text, t.ex.
#'   `c("10", "31")`. `NULL` (standard) filtrerar inte.
#' @param oms_klass Koder för omsättningsklass som text, t.ex.
#'   `c("00", "02")`. `NULL` (standard) filtrerar inte.
#' @param branscher Tvåsiffriga branschkoder (SNI) som text, t.ex.
#'   `c("10", "11")`. `NULL` (standard) filtrerar inte.
#' @param vald_foretagsstatus Företagsstatus som text. Standard är `"1"`
#'   (verksamma). `NULL` filtrerar inte.
#' @param vald_registreringsstatus Registreringsstatus som text. Standard
#'   är `"1"`. `NULL` filtrerar inte.
#' @param auto_loopa_kommuner Gäller hela Sverige. `TRUE` hämtar kommun för
#'   kommun om uttaget är större än `auto_loopa_trosklel`. `FALSE`
#'   (standard) hämtar allt i ett svep.
#' @param auto_loopa_trosklel Antal rader över vilket hela Sverige hämtas
#'   kommun för kommun när `auto_loopa_kommuner = TRUE`. Standard är 10000.
#' @param cache_path Sökväg till en .rds-fil för kontrollpunkter. `NULL`
#'   (standard) sparar inga.
#' @param behall_cache `TRUE` behåller cachefilen när hämtningen lyckats.
#'   Standard är `FALSE`, som tar bort den.
#' @param max_forsok Antal försök per API-anrop vid nätverksfel. Standard
#'   är 3.
#' @param batch_via_prefix Gäller `orgnr`. `TRUE` (standard) grupperar
#'   numren efter prefix, vilket ofta ger 20–80 gånger färre anrop. `FALSE`
#'   gör ett anrop per nummer.
#' @param prefix_start Gäller `orgnr`. Prefixlängd att börja gruppera på.
#'   Standard är 4; 5 ger finare grupper.
#' @param cert_thumb Tumavtryck för SCB-certifikatet, som text. Standard
#'   letas certifikatet upp med [scb_hamta_cert()].
#' @param visa_meddelanden_konsol `TRUE` skriver ut detaljer om varje
#'   uppdelning. Standard är `FALSE`.
#' @param visa_resultat_meddelanden_konsol `TRUE` (standard) skriver ut en
#'   sammanfattning per kommun och för hela hämtningen.
#' @param visa_progress `TRUE` (standard) visar en förloppsindikator.
#'
#' @return En tibble med en rad per företag. Hämtas kommun för kommun får
#'   den också kolumnen `kod_kommun`.
#' @seealso [scb_rakna_foretag_i_kommuner()] för att räkna innan du hämtar,
#'   [scb_kategorier_med_kodtabeller()] för koderna och [hamta_foretag()]
#'   för SCB:s nya API.
#' @export
#' @example inst/examples/scb_hamta_foretag.R
scb_hamta_foretag <- function(
    kommunkoder = NULL,
    orgnr = NULL,
    grundfilter_variabler  = list(),
    grundfilter_kategorier = list(),
    juridisk_kod = NULL,
    oms_klass    = NULL,
    branscher    = NULL,
    vald_foretagsstatus      = "1",
    vald_registreringsstatus = "1",
    auto_loopa_kommuner      = FALSE,
    auto_loopa_trosklel      = 10000L,
    cache_path               = NULL,
    behall_cache             = FALSE,
    max_forsok               = 3L,
    batch_via_prefix         = TRUE,
    prefix_start             = 4L,
    cert_thumb = scb_hamta_cert_thumb(),
    visa_meddelanden_konsol           = FALSE,
    visa_resultat_meddelanden_konsol  = TRUE,
    visa_progress                     = TRUE
) {
  total_start <- Sys.time()
  base_url  <- "https://privateapi.scb.se/nv0101/v1/sokpavar/api/"
  rakna_url <- paste0(base_url, "je/raknaforetag/")
  hamta_url <- paste0(base_url, "je/hamtaforetag/")
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

  # Bygg statisk del av baspayload (gemensamt för alla anrop i denna körning)
  bygg_baspayload <- function(extra_kategorier = list()) {
    kat <- grundfilter_kategorier
    if (length(extra_kategorier)) kat <- c(kat, extra_kategorier)
    if (!is.null(juridisk_kod)) kat <- c(kat, list(list(Kategori = "Juridisk form",         Kod = as.list(juridisk_kod))))
    if (!is.null(oms_klass))    kat <- c(kat, list(list(Kategori = "Omsättningsklass grov", Kod = as.list(oms_klass))))
    if (!is.null(branscher))    kat <- c(kat, list(list(Kategori = "2-siffrig bransch 1",   Kod = as.list(branscher))))
    skapa_baspayload(
      typ = "foretag",
      grundfilter_kategorier = kat,
      grundfilter_variabler  = grundfilter_variabler,
      foretagsstatus         = vald_foretagsstatus,
      registreringsstatus    = vald_registreringsstatus
    )
  }

  # ── Orgnr-läge ────────────────────────────────────────────────────────────
  if (!is.null(orgnr) && length(orgnr)) {
    orgnr <- unique(as.character(orgnr))
    orgnr <- orgnr[!is.na(orgnr) & nzchar(orgnr)]

    # Ladda cache om finns
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

    pb <- if (visa_progress) progress::progress_bar$new(
      total = length(orgnr),
      format = ":current/:total [:bar] :percent :message eta :eta", clear = FALSE
    ) else list(tick = function(...) NULL)
    if (length(done_orgnr)) suppressWarnings(pb$tick(length(done_orgnr)))

    if (batch_via_prefix && length(todo_orgnr) >= 3L) {
      # ── Adaptiv prefix-batchning ──────────────────────────────────────────
      if (visa_resultat_meddelanden_konsol)
        message(glue("🔎 Hämtar {scb_format(length(todo_orgnr))} orgnr via prefix-batchning (startprefix={prefix_start})..."))

      todo_orgnr <- sort(todo_orgnr)
      payload_bas <- bygg_baspayload()
      ctx_orgnr <- list(
        rakna = rakna, hamta = hamta, max_rader = max_rader,
        pb = if (visa_progress) pb else NULL,
        visa_medd = visa_meddelanden_konsol
      )

      # Top-level: gruppera på prefix_start och hantera en grupp i taget så
      # att vi kan checkpoint:a cachen efter varje stor batch.
      start_prefixar <- substr(todo_orgnr, 1, prefix_start)
      top_grupper <- split(todo_orgnr, start_prefixar)

      for (pf_idx in seq_along(top_grupper)) {
        grupp <- top_grupper[[pf_idx]]
        sub <- hamta_orgnr_batch(grupp, prefix_start, payload_bas, ctx_orgnr)
        if (!is.null(sub$df) && NROW(sub$df) > 0)
          resultat_lista[[length(resultat_lista) + 1L]] <- sub$df
        done_orgnr <- c(done_orgnr, sub$klara_orgnr)

        if (!is.null(cache_path)) {
          tryCatch(
            saveRDS(list(resultat = resultat_lista, done_orgnr = done_orgnr), cache_path),
            error = function(e) message(glue("⚠ Kunde inte spara cache: {conditionMessage(e)}"))
          )
        }
      }
    } else {
      # ── Fallback: en API-call per orgnr ──────────────────────────────────
      if (visa_resultat_meddelanden_konsol)
        message(glue("🔎 Hämtar {scb_format(length(todo_orgnr))} orgnr (en API-call per orgnr)..."))

      spara_var <- 100L
      for (idx in seq_along(todo_orgnr)) {
        o <- todo_orgnr[idx]
        pb$tick(tokens = list(message = o))
        p <- bygg_baspayload()
        p <- satt_variabel(p, "OrgNr (10 siffror)", o, "", "ArLikaMed")
        r <- hamta(p)
        if (!is.null(r)) resultat_lista[[length(resultat_lista) + 1]] <- r
        done_orgnr <- c(done_orgnr, o)

        if (!is.null(cache_path) && (idx %% spara_var == 0L || idx == length(todo_orgnr))) {
          tryCatch(
            saveRDS(list(resultat = resultat_lista, done_orgnr = done_orgnr), cache_path),
            error = function(e) message(glue("⚠ Kunde inte spara cache: {conditionMessage(e)}"))
          )
        }
      }
    }

    df <- resultat_lista %>% compact() %>% list_rbind()

    if (!is.null(cache_path) && !behall_cache && file.exists(cache_path)) {
      tryCatch(file.remove(cache_path), error = function(e) NULL)
    }

    if (visa_resultat_meddelanden_konsol)
      message(glue("📊 Hämtade {scb_format(NROW(df))} företag av {length(orgnr)} efterfrågade på {scb_tid(total_start)}"))
    return(df)
  }

  # ── Räkna totalen (om ingen kommunavgränsning) ──────────────────────────
  # Vi räknar alltid hela uttaget när användaren inte angivit kommuner.
  # Är det fler rader än tröskeln OCH auto_loopa_kommuner är på byter vi till
  # att loopa alla kommunkoder. Annars hämtas allt i single-shot via
  # strategierna och totalen används för progress-bar + sanity check.
  n_tot_auto <- NA_integer_
  if (is.null(kommunkoder)) {
    n_tot_auto <- rakna(bygg_baspayload())
    if (!is.na(n_tot_auto) && n_tot_auto > auto_loopa_trosklel && auto_loopa_kommuner) {
      if (visa_resultat_meddelanden_konsol)
        message(glue("🌍 Hela uttaget = {scb_format(n_tot_auto)} företag — loopar Sveriges kommuner automatiskt"))
      kommunkoder <- hamta_alla_kommunkoder()
    } else if (visa_resultat_meddelanden_konsol && !is.na(n_tot_auto)) {
      message(glue("ℹ Hela uttaget = {scb_format(n_tot_auto)} företag — hämtar utan kommun-loop"))
    }
  }

  # Räknare för bortskippade rader (när alla strategier slut)
  skip_env <- new.env(); skip_env$rader <- 0L; skip_env$celler <- 0L
  ctx$skip_env <- skip_env

  # ── Strategilista för uppdelning ─────────────────────────────────────────
  # OrgNr-prefix ligger först eftersom kategori-strategierna tappar poster som
  # saknar värde i kategorin (gav ~7,6% gap för hela riket). OrgNr finns på
  # alla företag → 100% täckning. Övriga strategier ligger kvar som backup.
  strategier <- list(
    strategi_orgnr,
    strategi_postnr,
    strategi_kategori("Omsättningsklass grov",
                      varden_fn = function() hamta_kategorivarden("foretag", "Omsättningsklass grov", cert_thumb)),
    strategi_kategori("2-siffrig bransch 1",
                      varden_fn = function() hamta_kategorivarden("foretag", "2-siffrig bransch 1", cert_thumb)),
    strategi_kategori("Juridisk form",
                      varden_fn = function() hamta_kategorivarden("foretag", "Juridisk form", cert_thumb))
  )

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

    extra_kat <- if (!is.null(kom)) list(list(Kategori = "SätesKommun", Kod = list(kom))) else list()
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
      message(glue("🏁 {kom %||% 'alla kommuner'}: hämtade {scb_format(n_kom)} företag på {scb_sec(kom_start)} sek"))
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
      message(glue("⚠ {scb_format(gap)} företag saknades jfr totalen — sannolikt poster utan registrerad SäteKommun"))
    } else {
      message(glue("⚠ {scb_format(gap)} företag saknades jfr totalen ({scb_format(n_tot_auto)} räknade, {scb_format(nrow(retur_df))} hämtade)"))
    }
  }

  # Städa bort cachefilen vid lyckad körning (om användaren inte begärt att behålla den)
  if (!is.null(cache_path) && !behall_cache && file.exists(cache_path)) {
    tryCatch(file.remove(cache_path), error = function(e) NULL)
  }

  if (visa_resultat_meddelanden_konsol) {
    kom_txt <- if (length(kommun_lista) > 1) "kommuner" else "kommun"
    message(glue("📊 Totalt {scb_format(nrow(retur_df))} företag från {length(kommun_lista)} {kom_txt} på {scb_tid(total_start)}"))
  }
  retur_df
}
