# Kräver en API-nyckel till SCB:s företagsregister i miljövariabeln
# SCB_AFR_API_KEY eller i keyring, och internet. Exemplet körs bara i en
# interaktiv session.
if (interactive() && nzchar(Sys.getenv("SCB_AFR_API_KEY")) &&
    rdwebbskrapning::test_internetanslutning()) {
  # Verksamma arbetsställen i Falun
  ae_falun <- hamta_arbetsstallen("2080")

  # Ett län och en kommun i samma anrop
  ae_blandat <- hamta_arbetsstallen(c("20", "2180"))

  # Alla statusar, utan förloppsindikator
  ae_alla <- hamta_arbetsstallen("2080", status = NULL, visa_progress = FALSE)
}
