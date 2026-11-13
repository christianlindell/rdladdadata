#' Hitta SCB-certifikatet för sökpåver-API:t
#'
#' Letar upp det certifikat som SCB lämnar ut för sökpåver-API:t till
#' företagsregistret i Windows certifikatförråd, först hos användaren
#' (CurrentUser) och sedan på datorn (LocalMachine). Certifikatet känns igen
#' på att ämnet innehåller "sokpavar" eller "sökpåver" och att utfärdaren är
#' SCB. Funktionen använder Windows-programmet `certutil` och fungerar bara
#' på Windows.
#'
#' Paketets övriga `scb_`-funktioner anropar den automatiskt första gången
#' certifikatet behövs. Anropa den själv för att kontrollera att
#' certifikatet är installerat.
#'
#' @return En lista med `store` (`"CurrentUser"` eller `"LocalMachine"`) och
#'   `thumb` (certifikatets tumavtryck). Ger ett fel om inget certifikat
#'   hittas.
#' @export
#' @example inst/examples/scb_hamta_cert.R
scb_hamta_cert <- function() {
  parse_store <- function(cmd, store) {
    rr <- try(system(cmd, intern = TRUE), silent = TRUE)
    if (inherits(rr, "try-error")) return(NULL)
    r <- iconv(rr, from = "windows-1252", to = "UTF-8")
    df <- tibble(rad = r) |>
      tidyr::fill(rad, .direction = "down") |>
      mutate(
        subject = if_else(grepl("^Subject:", rad), sub(".*CN=", "", rad), NA_character_),
        issuer  = if_else(grepl("^Issuer:",  rad), sub("^Issuer: ", "", rad), NA_character_),
        thumb   = if_else(grepl("^Cert Hash(.*):", rad), gsub(".*: ", "", rad), NA_character_)
      ) |>
      tidyr::fill(subject, issuer) |>
      filter(!is.na(thumb)) |>
      mutate(subject_lc = tolower(subject), issuer_lc = tolower(issuer)) |>
      filter(grepl("sokpavar|sökpåver|sokpåver", subject_lc) & grepl("\\bscb\\b", issuer_lc)) |>
      transmute(store = store, thumb = gsub(" ", "", thumb))
    if (nrow(df)) df[1, ] else NULL
  }
  hit <- parse_store("certutil -user -store My", "CurrentUser")
  if (is.null(hit)) hit <- parse_store("certutil -store My", "LocalMachine")
  if (is.null(hit)) stop("Hittade inget SCB-sökpåver-cert i CurrentUser eller LocalMachine.")
  list(store = hit$store, thumb = hit$thumb)
}
