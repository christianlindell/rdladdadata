# Kräver SCB:s certifikat för sökpåver-API:t (se ?scb_hamta_cert) och
# internet. Exemplet körs bara i en interaktiv session.
har_cert <- .Platform$OS.type == "windows" &&
  !is.null(tryCatch(scb_hamta_cert(), error = function(e) NULL))
if (interactive() && har_cert && rdwebbskrapning::test_internetanslutning()) {
  # Alla verksamma företag i Falun och Borlänge
  ftg <- scb_hamta_foretag(kommunkoder = c("2080", "2081"))

  # Bara ideella föreningar (juridisk form 61) i Dalarna
  foreningar <- scb_hamta_foretag(kommunkoder = "20", juridisk_kod = "61")

  # Företag vars firma innehåller "ask" och som har e-post, i Stockholm
  ask <- scb_hamta_foretag(
    kommunkoder = "0180",
    grundfilter_variabler = list(
      list(Varde1 = "", Varde2 = "", Operator = "Finns", Variabel = "E-post"),
      list(Varde1 = "ask", Varde2 = "", Operator = "Innehaller", Variabel = "Firma")
    )
  )

  # Utvalda organisationsnummer, med kontrollpunkter i en tillfällig fil
  valda <- scb_hamta_foretag(
    orgnr = c("5560000000", "5560000001"),
    cache_path = tempfile(fileext = ".rds")
  )
}
