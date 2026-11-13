# Kräver SCB:s certifikat för sökpåver-API:t (se ?scb_hamta_cert) och
# internet. Exemplet körs bara i en interaktiv session.
har_cert <- .Platform$OS.type == "windows" &&
  !is.null(tryCatch(scb_hamta_cert(), error = function(e) NULL))
if (interactive() && har_cert && rdwebbskrapning::test_internetanslutning()) {
  # Verksamma arbetsställen i Falun
  scb_rakna_arbetsstallen_i_kommuner("2080")

  # Alla kommuner i Dalarna, utan utskrift per kommun
  scb_rakna_arbetsstallen_i_kommuner("20", visa_konsol = FALSE)
}
