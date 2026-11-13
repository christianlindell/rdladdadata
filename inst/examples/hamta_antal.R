# Kräver en API-nyckel till SCB:s företagsregister i miljövariabeln
# SCB_AFR_API_KEY eller i keyring, och internet. Exemplet körs bara i en
# interaktiv session.
if (interactive() && nzchar(Sys.getenv("SCB_AFR_API_KEY")) &&
    rdwebbskrapning::test_internetanslutning()) {
  # Antal arbetsställen i Sverige, med uppskattad hämtningstid
  hamta_antal()

  # Antal per län, utan tidsuppskattning
  hamta_antal("arbetsstallen", regionkod = c("20", "21"), uppskatta_tid = FALSE)

  # Antal juridiska enheter i en kommun
  hamta_antal("juridiskaenheter", regionkod = "2080")
}
