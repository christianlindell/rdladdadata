# Kräver en API-nyckel till SCB:s företagsregister i miljövariabeln
# SCB_AFR_API_KEY eller i keyring, och internet. Exemplet körs bara i en
# interaktiv session.
if (interactive() && nzchar(Sys.getenv("SCB_AFR_API_KEY")) &&
    rdwebbskrapning::test_internetanslutning()) {
  juridiska_former <- hamta_kodtabell("juridiskform")
  head(juridiska_former)
}
