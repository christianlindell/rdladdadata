# Interna hjälpfunktioner för SCB:s sökpåver-API (scb_hamta_foretag() m.fl.).

# Certifikatet söks upp första gången det behövs och sparas sedan för resten
# av sessionen. I skriptet söktes det upp när skriptet lästes in, men i ett
# paket skulle det ske när paketet byggs.
.cert_cache <- new.env(parent = emptyenv())
scb_cert_info <- function() {
  if (is.null(.cert_cache$info)) .cert_cache$info <- scb_hamta_cert()
  .cert_cache$info
}
scb_hamta_cert_thumb <- function() scb_cert_info()$thumb

# ───────────────────────────────────────────────────────────────────────────
# HTTP-anrop med rate-limit (max 10 anrop per 10 sek)
# ───────────────────────────────────────────────────────────────────────────

api_posta <- function(url, body, cert_thumb, .api_calls_env,
                      visa_medd = FALSE, max_forsok = 3L) {
  for (forsok in seq_len(max_forsok)) {
    # Rate-limit (max 10 anrop per 10 sek)
    now <- as.numeric(Sys.time())
    .api_calls_env$timestamps <- .api_calls_env$timestamps[.api_calls_env$timestamps > now - 10]
    if (length(.api_calls_env$timestamps) >= 10) {
      wait_time <- 10 - (now - min(.api_calls_env$timestamps))
      if (visa_medd) message(sprintf("⏳ Väntar %.1f sek pga anropsgräns...", wait_time))
      Sys.sleep(wait_time)
    }
    .api_calls_env$timestamps <- c(.api_calls_env$timestamps, as.numeric(Sys.time()))

    r <- tryCatch(
      httr::POST(
        url,
        add_headers(`Content-Type` = "application/json", Accept = "application/json"),
        body   = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "json",
        httr::config(sslcert = paste0(scb_cert_info()$store, "\\MY\\", cert_thumb), sslcerttype = "Schannel")
      ),
      error = function(e) list(status_code = -1L, error = conditionMessage(e))
    )

    # Lyckat: returnera direkt
    if (!is.null(r$status_code) && r$status_code == 200L) return(r)
    # Klient-fel (400-499): retry hjälper inte
    if (!is.null(r$status_code) && r$status_code >= 400L && r$status_code < 500L) return(r)

    # Transient fel (timeout, 5xx, nätverk): backoff och försök igen
    if (forsok < max_forsok) {
      backoff <- 2 ^ (forsok - 1L) * 2  # 2, 4, 8 sek
      status_txt <- if (is.null(r$status_code)) "okänt" else as.character(r$status_code)
      err_txt <- if (!is.null(r$error)) sprintf(" — %s", r$error) else ""
      message(sprintf("⚠ API-fel (status %s)%s försök %d/%d — väntar %d sek och försöker igen",
                      status_txt, err_txt, forsok, max_forsok, backoff))
      Sys.sleep(backoff)
    }
  }
  r  # sista (misslyckade) svaret
}

rakna_api <- function(rakna_url, payload, cert_thumb, .api_calls_env,
                      visa_medd = FALSE, max_forsok = 3L) {
  r <- api_posta(rakna_url, payload, cert_thumb, .api_calls_env, visa_medd, max_forsok)
  if (is.null(r$status_code) || r$status_code != 200) return(NA_integer_)
  suppressWarnings(as.integer(httr::content(r)))
}

hamta_api <- function(hamta_url, payload, cert_thumb, .api_calls_env,
                      visa_medd = FALSE, max_forsok = 3L) {
  r <- api_posta(hamta_url, payload, cert_thumb, .api_calls_env, visa_medd, max_forsok)
  if (is.null(r$status_code) || r$status_code != 200) return(NULL)
  innehall <- httr::content(r, simplifyVector = TRUE)
  if (is.null(innehall)) return(NULL)
  if (is.data.frame(innehall)) return(as_tibble(innehall))
  if (is.list(innehall) && length(innehall) > 0 && is.data.frame(innehall[[1]])) return(as_tibble(innehall[[1]]))
  NULL
}

.kategori_cache <- new.env(parent = emptyenv())

hamta_kategorivarden <- function(tabell, kategori_id, cert_thumb = scb_hamta_cert_thumb()) {
  nyckel <- paste(tabell, kategori_id, sep = "::")
  if (!is.null(.kategori_cache[[nyckel]])) return(.kategori_cache[[nyckel]])
  df <- scb_kategorier_med_kodtabeller(tabell = tabell, cert_thumb = cert_thumb)
  varden <- df %>% filter(kategori_id == !!kategori_id, !is.na(kod)) %>% dplyr::pull(kod) %>% unique()
  .kategori_cache[[nyckel]] <- varden
  varden
}

# ───────────────────────────────────────────────────────────────────────────
# Lazy-hämtning av Sveriges alla kommunkoder via Region Dalarnas hjälpfunktion
# (Källa: https://github.com/Region-Dalarna/funktioner). Cache:as i miljön så
# att GitHub-källningen bara görs en gång per R-session.
# ───────────────────────────────────────────────────────────────────────────

.kommun_cache <- new.env(parent = emptyenv())

hamta_alla_kommunkoder <- function() {
  if (!is.null(.kommun_cache$koder)) return(.kommun_cache$koder)

  kommuner <- hamtakommuner(tamedlan = FALSE, tamedriket = FALSE, allakommuner = TRUE)

  # Plocka ut den kolumn som innehåller 4-siffriga kommunkoder. Stödjer både
  # data.frame med kolumn (kommun/kommunkod/kod/region) och ren teckenvektor.
  if (is.data.frame(kommuner)) {
    kod_kolumn <- NULL
    for (kn in c("kommun", "kommunkod", "kod", "region")) {
      if (kn %in% names(kommuner)) {
        v <- as.character(kommuner[[kn]])
        if (all(grepl("^\\d{4}$", v))) { kod_kolumn <- kn; break }
      }
    }
    if (is.null(kod_kolumn))
      stop("Hittade ingen 4-siffrig kommunkod-kolumn i hamtakommuner()-resultatet")
    koder <- as.character(kommuner[[kod_kolumn]])
  } else {
    koder <- as.character(kommuner)
  }
  koder <- unique(koder[grepl("^\\d{4}$", koder)])

  .kommun_cache$koder <- koder
  koder
}

# Expanderar en blandad vektor av län- och kommunkoder till en vektor 4-siffriga
# kommunkoder. Regler:
#   "00"        → alla kommuner i Sverige (hela landet)
#   "20"        → alla kommuner vars 4-siffriga kod börjar med 20 (Dalarna)
#   "2080"      → behålls som den är (Falun)
#   c("21","2080") → alla Gävleborgskommuner + Falun, deduplicerat
# NULL → NULL (oförändrat; anroparen får hantera "ingen kommunavgränsning")
expandera_kommunkoder <- function(koder) {
  if (is.null(koder)) return(NULL)
  koder <- as.character(koder)
  koder <- koder[!is.na(koder) & nzchar(koder)]
  if (!length(koder)) return(NULL)

  ogiltiga <- koder[!grepl("^\\d{2}$|^\\d{4}$", koder)]
  if (length(ogiltiga)) {
    stop(sprintf("Ogiltiga koder (måste vara 2- eller 4-siffriga): %s",
                 paste(unique(ogiltiga), collapse = ", ")))
  }

  # "00" = hela Sverige — kortsluter
  if ("00" %in% koder) return(hamta_alla_kommunkoder())

  lan_koder <- unique(koder[nchar(koder) == 2L])
  kom_koder <- unique(koder[nchar(koder) == 4L])

  if (length(lan_koder)) {
    alla <- hamta_alla_kommunkoder()
    expanderade <- unlist(lapply(lan_koder, function(l) alla[startsWith(alla, l)]))
    kom_koder <- c(kom_koder, expanderade)
  }
  sort(unique(kom_koder))
}

# ───────────────────────────────────────────────────────────────────────────
# Payload-hjälpare (läs/sätt kategorier och variabler i en payload-list)
# ───────────────────────────────────────────────────────────────────────────

skapa_baspayload <- function(typ,
                             grundfilter_kategorier = list(),
                             grundfilter_variabler  = list(),
                             foretagsstatus = NULL,
                             registreringsstatus = NULL,
                             arbetsstallestatus = NULL) {
  payload <- list()
  if (typ == "foretag") {
    if (!is.null(foretagsstatus))      payload$Företagsstatus      <- unname(foretagsstatus)
    if (!is.null(registreringsstatus)) payload$Registreringsstatus <- unname(registreringsstatus)
  } else {
    if (!is.null(arbetsstallestatus))  payload$Arbetsställestatus  <- unname(arbetsstallestatus)
  }
  if (length(grundfilter_kategorier)) payload$Kategorier <- grundfilter_kategorier
  if (length(grundfilter_variabler))  payload$variabler  <- grundfilter_variabler
  payload
}

las_kategori_koder <- function(payload, kategori_namn, branschniva = NULL) {
  if (is.null(payload$Kategorier)) return(NULL)
  for (k in payload$Kategorier) {
    if (identical(k$Kategori, kategori_namn) &&
        identical(k$Branschniva %||% NULL, branschniva %||% NULL)) {
      return(unlist(k$Kod))
    }
  }
  NULL
}

satt_kategori <- function(payload, kategori_namn, koder, branschniva = NULL) {
  ny <- list(Kategori = kategori_namn, Kod = as.list(koder))
  if (!is.null(branschniva)) ny$Branschniva <- branschniva

  if (is.null(payload$Kategorier)) {
    payload$Kategorier <- list(ny)
    return(payload)
  }
  for (i in seq_along(payload$Kategorier)) {
    if (identical(payload$Kategorier[[i]]$Kategori, kategori_namn) &&
        identical(payload$Kategorier[[i]]$Branschniva %||% NULL, branschniva %||% NULL)) {
      payload$Kategorier[[i]] <- ny
      return(payload)
    }
  }
  payload$Kategorier <- c(payload$Kategorier, list(ny))
  payload
}

las_variabel <- function(payload, variabel_namn) {
  if (is.null(payload$variabler)) return(NULL)
  for (v in payload$variabler) {
    if (identical(v$Variabel, variabel_namn)) return(v)
  }
  NULL
}

satt_variabel <- function(payload, variabel_namn, varde1, varde2 = "", operator = "ArLikaMed") {
  ny <- list(Varde1 = varde1, Varde2 = varde2, Operator = operator, Variabel = variabel_namn)
  if (is.null(payload$variabler)) {
    payload$variabler <- list(ny)
    return(payload)
  }
  for (i in seq_along(payload$variabler)) {
    if (identical(payload$variabler[[i]]$Variabel, variabel_namn)) {
      payload$variabler[[i]] <- ny
      return(payload)
    }
  }
  payload$variabler <- c(payload$variabler, list(ny))
  payload
}

# ───────────────────────────────────────────────────────────────────────────
# Strategier för uppdelning
# ───────────────────────────────────────────────────────────────────────────

# Kategoristrategi: binärhalvering över kodlistan. Om payload redan har
# kategorin satt halveras den befintliga listan; annars hämtas full lista
# från API:t (cache:as) och halveras.
strategi_kategori <- function(kategori_namn, varden_fn, branschniva = NULL) {
  list(
    namn = if (!is.null(branschniva)) sprintf("%s (Branschniva %s)", kategori_namn, branschniva) else kategori_namn,
    partition = function(payload) {
      nuvarande <- las_kategori_koder(payload, kategori_namn, branschniva)
      varden <- if (is.null(nuvarande)) varden_fn() else nuvarande
      if (length(varden) <= 1) return(list())
      mitt <- ceiling(length(varden) / 2)
      list(
        satt_kategori(payload, kategori_namn, varden[1:mitt], branschniva),
        satt_kategori(payload, kategori_namn, varden[(mitt + 1):length(varden)], branschniva)
      )
    }
  )
}

# Postnr (Mellan): halverar intervallet rekursivt
strategi_postnr <- list(
  namn = "Postnr (intervall)",
  partition = function(payload) {
    v <- las_variabel(payload, "Postnr")
    if (is.null(v) || !identical(v$Operator, "Mellan")) {
      start <- 0L; slut <- 99999L
    } else {
      start <- suppressWarnings(as.integer(v$Varde1))
      slut  <- suppressWarnings(as.integer(v$Varde2))
      if (is.na(start) || is.na(slut) || slut <= start) return(list())
    }
    mitt <- (start + slut) %/% 2L
    list(
      satt_variabel(payload, "Postnr", sprintf("%05d", start),      sprintf("%05d", mitt),  "Mellan"),
      satt_variabel(payload, "Postnr", sprintf("%05d", mitt + 1L),  sprintf("%05d", slut),  "Mellan")
    )
  }
)

# OrgNr (10 siffror) BorjarPa: förlänger prefixet en siffra i taget (10 barn)
strategi_orgnr <- list(
  namn = "OrgNr-prefix",
  partition = function(payload) {
    v <- las_variabel(payload, "OrgNr (10 siffror)")
    if (!is.null(v) && !identical(v$Operator, "BorjarPa")) return(list())
    prefix <- if (is.null(v)) "" else v$Varde1
    if (nchar(prefix) >= 10) return(list())
    map(as.character(0:9), function(d) {
      satt_variabel(payload, "OrgNr (10 siffror)", paste0(prefix, d), "", "BorjarPa")
    })
  }
)

# ───────────────────────────────────────────────────────────────────────────
# Rekursiv hämtning med strategilista
# ───────────────────────────────────────────────────────────────────────────

hamta_rekursivt <- function(payload, strategier, ctx, djup = 0L) {
  n <- ctx$rakna(payload)

  if (is.na(n)) {
    if (ctx$visa_medd) message(sprintf("%s⚠ NA från räkna — hoppar över denna cell", strrep("  ", djup)))
    return(NULL)
  }
  if (n == 0L) return(NULL)
  if (n <= ctx$max_rader) {
    if (ctx$visa_medd) message(sprintf("%s✅ %d rader → hämtar", strrep("  ", djup), n))
    res <- ctx$hamta(payload)
    if (!is.null(res) && !is.null(ctx$fetch_pb)) {
      suppressWarnings(ctx$fetch_pb$tick(nrow(res)))
    }
    return(res)
  }

  if (!length(strategier)) {
    # Sista chans: OrgNr-prefix om det ännu inte är uttömt. Garanterar att
    # även om kategoristrategierna ger upp tidigt så finns alltid en
    # backstop kvar (orgnr är unika → 10-siffrigt prefix = max 1 träff).
    v <- las_variabel(payload, "OrgNr (10 siffror)")
    pa_orgnr <- !is.null(v) && identical(v$Operator, "BorjarPa")
    if (!pa_orgnr || nchar(v$Varde1 %||% "") < 10) {
      if (ctx$visa_medd)
        message(sprintf("%s🆘 Sista chans: OrgNr-prefix (cell %d rader)", strrep("  ", djup), n))
      return(hamta_rekursivt(payload, list(strategi_orgnr), ctx, djup))
    }
    message(sprintf("⚠ Cell med %d rader kunde inte delas mer (alla strategier slut) — hoppar över", n))
    if (!is.null(ctx$skip_env)) {
      ctx$skip_env$rader  <- ctx$skip_env$rader + n
      ctx$skip_env$celler <- ctx$skip_env$celler + 1L
    }
    return(NULL)
  }

  strategi <- strategier[[1]]
  if (ctx$visa_medd)
    message(sprintf("%s🔀 %d rader > %d, delar via '%s'",
                    strrep("  ", djup), n, ctx$max_rader, strategi$namn))

  barn <- strategi$partition(payload)
  if (!length(barn)) {
    return(hamta_rekursivt(payload, strategier[-1], ctx, djup))
  }

  # Varje barn-payload använder samma fulla strategi-lista — strategin kan
  # behöva halveras flera gånger innan vi går till nästa dimension.
  resultat_list <- map(barn, function(bp) hamta_rekursivt(bp, strategier, ctx, djup + 1L))
  compact(resultat_list) %>% list_rbind()
}

# ───────────────────────────────────────────────────────────────────────────
# OrgNr-batchning via prefix (BorjarPa)
# ───────────────────────────────────────────────────────────────────────────
#
# När man vill hämta en lång lista 10-siffriga orgnr går det ofta mycket
# snabbare att gruppera dem på prefix och göra en BorjarPa-call per grupp
# (upp till 2000 rader per call), och sedan filtrera ner resultatet till de
# orgnr som efterfrågats. För typiska företagslistor (10 000–100 000 orgnr)
# blir det ofta 20–80x färre API-anrop än en call per orgnr.

# Hittar OrgNr-kolumnen i ett SCB-svar (med fallback för udda kolumnnamn)
hitta_orgnr_kolumn <- function(df) {
  kandidater <- c("OrgNr", "Orgnr", "orgnr", "OrgNr (10 siffror)",
                  "OrganisationsNummer", "Organisationsnummer")
  hit <- intersect(kandidater, names(df))
  if (length(hit)) return(hit[1])
  sannolika <- map_lgl(df, function(v) {
    vc <- as.character(v)
    length(vc) > 0 && mean(grepl("^\\d{10}$", head(vc, 200)), na.rm = TRUE) > 0.8
  })
  hits <- names(df)[which(sannolika)]
  if (length(hits)) hits[1] else NA_character_
}

# Rekursiv prefix-batchning:
#   - Om gruppen har 1 orgnr eller prefix-längden nått 10 → ArLikaMed direkt.
#   - Annars räkna BorjarPa för aktuellt prefix. Om ≤ max_rader → hämta
#     BorjarPa och filtrera resultatet till gruppens orgnr. Om > max_rader →
#     dela in i (max 10) undergrupper med prefix-längd+1 och rekursera.
#   - Returnerar list(df, klara_orgnr) där klara_orgnr är vektorn över de
#     orgnr som har hanterats (oavsett om de gav träff eller inte).
hamta_orgnr_batch <- function(orgnr_grupp, prefix_len, payload_bas, ctx_local) {
  if (!length(orgnr_grupp)) return(list(df = NULL, klara_orgnr = character()))

  # Bas-fall: enstaka orgnr eller maximalt prefix → ArLikaMed per orgnr
  if (length(orgnr_grupp) == 1L || prefix_len >= 10L) {
    res_lista <- list()
    for (o in orgnr_grupp) {
      p <- satt_variabel(payload_bas, "OrgNr (10 siffror)", o, "", "ArLikaMed")
      r <- ctx_local$hamta(p)
      if (!is.null(r)) res_lista[[length(res_lista) + 1L]] <- r
      if (!is.null(ctx_local$pb)) suppressWarnings(ctx_local$pb$tick(tokens = list(message = o)))
    }
    return(list(df = bind_rows(res_lista), klara_orgnr = orgnr_grupp))
  }

  # Gruppera på aktuellt prefix
  prefixar  <- substr(orgnr_grupp, 1, prefix_len)
  grupper   <- split(orgnr_grupp, prefixar)

  res_total   <- list()
  klara_total <- character()

  for (pf in names(grupper)) {
    grupp <- grupper[[pf]]

    # Singel-grupp → direkt ArLikaMed (slipper extra räknings-call)
    if (length(grupp) == 1L) {
      p <- satt_variabel(payload_bas, "OrgNr (10 siffror)", grupp, "", "ArLikaMed")
      r <- ctx_local$hamta(p)
      if (!is.null(r)) res_total[[length(res_total) + 1L]] <- r
      klara_total <- c(klara_total, grupp)
      if (!is.null(ctx_local$pb)) suppressWarnings(ctx_local$pb$tick(tokens = list(message = grupp)))
      next
    }

    p_test <- satt_variabel(payload_bas, "OrgNr (10 siffror)", pf, "", "BorjarPa")
    n <- ctx_local$rakna(p_test)

    if (is.na(n) || n == 0L) {
      # Inga företag matchar (avregistrerade, fel status etc) — markera klara
      klara_total <- c(klara_total, grupp)
      if (!is.null(ctx_local$pb))
        suppressWarnings(ctx_local$pb$tick(length(grupp), tokens = list(message = sprintf("%s (tom)", pf))))
      next
    }

    if (n <= ctx_local$max_rader) {
      r <- ctx_local$hamta(p_test)
      if (!is.null(r) && NROW(r) > 0) {
        orgnr_kol <- hitta_orgnr_kolumn(r)
        if (!is.na(orgnr_kol)) {
          r <- r[as.character(r[[orgnr_kol]]) %in% grupp, , drop = FALSE]
        }
        if (NROW(r) > 0) res_total[[length(res_total) + 1L]] <- r
      }
      klara_total <- c(klara_total, grupp)
      if (!is.null(ctx_local$pb))
        suppressWarnings(ctx_local$pb$tick(length(grupp),
                                           tokens = list(message = sprintf("%s (%d→%d)", pf, n, length(grupp)))))
    } else {
      if (ctx_local$visa_medd)
        message(sprintf("  🔀 prefix '%s' = %d företag — splittar (prefix-längd %d)",
                        pf, n, prefix_len + 1L))
      sub <- hamta_orgnr_batch(grupp, prefix_len + 1L, payload_bas, ctx_local)
      if (!is.null(sub$df) && NROW(sub$df) > 0) res_total[[length(res_total) + 1L]] <- sub$df
      klara_total <- c(klara_total, sub$klara_orgnr)
    }
  }

  list(df = bind_rows(res_total), klara_orgnr = klara_total)
}

# ───────────────────────────────────────────────────────────────────────────
# Formatering
# ───────────────────────────────────────────────────────────────────────────

scb_format <- function(x) format(x, big.mark = " ", scientific = FALSE, trim = TRUE)
scb_sec    <- function(start_time) round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)

# Formaterar förfluten tid läsbart: "12 sek", "3 min 14 sek", "1h 15m 12s"
scb_tid <- function(start_time) {
  s <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (s < 60)   return(sprintf("%.1f sek", s))
  if (s < 3600) return(sprintf("%d min %d sek", as.integer(s %/% 60), as.integer(round(s %% 60))))
  h <- as.integer(s %/% 3600); rest <- s - h * 3600
  sprintf("%dh %dm %ds", h, as.integer(rest %/% 60), as.integer(round(rest %% 60)))
}
