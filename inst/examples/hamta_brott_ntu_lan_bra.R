# Laddar ner en Excel-fil från bra.se. Exemplet körs bara i en interaktiv
# session, så att BRÅ:s server inte belastas vid varje paketkontroll.
if (interactive() && rdwebbskrapning::test_internetanslutning()) {
  ntu <- hamta_brott_ntu_lan_bra()

  # En tabell per indikator
  names(ntu)
  head(ntu[[1]])
}
