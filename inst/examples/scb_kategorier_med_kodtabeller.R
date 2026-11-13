# Kräver SCB:s certifikat för sökpåver-API:t (se ?scb_hamta_cert) och
# internet. Exemplet körs bara i en interaktiv session.
har_cert <- .Platform$OS.type == "windows" &&
  !is.null(tryCatch(scb_hamta_cert(), error = function(e) NULL))
if (interactive() && har_cert && rdwebbskrapning::test_internetanslutning()) {
  # Alla kategorier för företag med koder och klartext
  kategorier <- scb_kategorier_med_kodtabeller()
  head(kategorier)

  # Antal koder per kategori för arbetsställen
  scb_kategorier_med_kodtabeller(tabell = "arbetsstalle", med_varden = FALSE)
}
