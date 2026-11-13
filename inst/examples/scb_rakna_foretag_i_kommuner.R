# Kräver SCB:s certifikat för sökpåver-API:t (se ?scb_hamta_cert) och
# internet. Exemplet körs bara i en interaktiv session.
har_cert <- .Platform$OS.type == "windows" &&
  !is.null(tryCatch(scb_hamta_cert(), error = function(e) NULL))
if (interactive() && har_cert && rdwebbskrapning::test_internetanslutning()) {
  # Verksamma företag i Falun och Borlänge
  scb_rakna_foretag_i_kommuner(c("2080", "2081"))

  # Ideella föreningar (juridisk form 61) i alla kommuner i Dalarna
  scb_rakna_foretag_i_kommuner(
    "20",
    extra_kategorier = list(list(Kategori = "Juridisk form", Kod = list("61")))
  )
}
