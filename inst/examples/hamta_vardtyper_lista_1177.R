# Läser en sida på 1177.se. Exemplet körs bara i en interaktiv session, så
# att 1177:s server inte belastas vid varje paketkontroll.
if (interactive() && rdwebbskrapning::test_internetanslutning()) {
  vardtyper <- hamta_vardtyper_lista_1177()
  head(vardtyper)
}
