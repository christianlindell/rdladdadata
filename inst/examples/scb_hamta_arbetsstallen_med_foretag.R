# Kräver SCB:s certifikat för sökpåver-API:t (se ?scb_hamta_cert) och
# internet. Exemplet körs bara i en interaktiv session.
har_cert <- .Platform$OS.type == "windows" &&
  !is.null(tryCatch(scb_hamta_cert(), error = function(e) NULL))
if (interactive() && har_cert && rdwebbskrapning::test_internetanslutning()) {
  # Arbetsställen i Falun och företagen som äger dem
  res <- scb_hamta_arbetsstallen_med_foretag(kommunkoder = "2080")
  bred <- dplyr::left_join(res$arbetsstallen, res$foretag, by = "OrgNr")

  # Bara arbetsställen med 50 anställda eller fler, i Falun och Borlänge
  res_stora <- scb_hamta_arbetsstallen_med_foretag(
    kommunkoder = c("2080", "2081"),
    anstallda = c("6", "7", "8", "9")
  )
}
