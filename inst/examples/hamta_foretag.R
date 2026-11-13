# Kräver en API-nyckel till SCB:s företagsregister i miljövariabeln
# SCB_AFR_API_KEY eller i keyring, och internet. Exemplet körs bara i en
# interaktiv session.
if (interactive() && nzchar(Sys.getenv("SCB_AFR_API_KEY")) &&
    rdwebbskrapning::test_internetanslutning()) {
  # Verksamma juridiska enheter med säte i Falun
  je_falun <- hamta_foretag("2080")

  # Urval på näringsgren i stället för region
  je_handel <- hamta_foretag(naringsgren = "47110")
}
