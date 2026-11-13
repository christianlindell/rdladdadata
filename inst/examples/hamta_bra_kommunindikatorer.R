# Styr Edge eller Chrome mot bra.se och laddar ner en fil per kommun, vilket
# tar några minuter. Exemplet körs därför bara i en interaktiv session.
if (interactive() && rdwebbskrapning::test_internetanslutning()) {
  # Alla kommuner i Dalarna (standard)
  bra_dalarna <- hamta_bra_kommunindikatorer()
  head(bra_dalarna)

  # Bara Falun och Borlänge
  bra_tva <- hamta_bra_kommunindikatorer(region_vekt = c("2080", "2081"))
}
