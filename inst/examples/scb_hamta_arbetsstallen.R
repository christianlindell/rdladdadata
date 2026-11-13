# Kräver SCB:s certifikat för sökpåver-API:t (se ?scb_hamta_cert) och
# internet. Exemplet körs bara i en interaktiv session.
har_cert <- .Platform$OS.type == "windows" &&
  !is.null(tryCatch(scb_hamta_cert(), error = function(e) NULL))
if (interactive() && har_cert && rdwebbskrapning::test_internetanslutning()) {
  # Alla verksamma arbetsställen i Falun
  arbst <- scb_hamta_arbetsstallen(kommunkoder = "2080")

  # Arbetsställen med 50 anställda eller fler i Falun
  stora <- scb_hamta_arbetsstallen(kommunkoder = "2080", anstallda = c("6", "7", "8", "9"))
}
