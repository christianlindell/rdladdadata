# Laddar ner Excel-filer från rf.se. Exemplet körs bara i en interaktiv
# session, så att RF-SISU:s server inte belastas vid varje paketkontroll.
if (interactive() && rdwebbskrapning::test_internetanslutning()) {
  lokstod <- hamta_lokstod_kommun_regso_kon_rf()
  head(lokstod)

  # Bara kommunerna i Dalarna
  lokstod[lokstod$Lanskod %in% "20" & is.na(lokstod$`RegSO-kod`), ]
}
