# Laddar ner Excel-filer från Skolverket. Exemplet körs bara i en interaktiv
# session, så att Skolverkets server inte belastas vid varje paketkontroll.
if (interactive() && rdwebbskrapning::test_internetanslutning()) {
  # Grundskola och förskoleklass, per kommun och per län
  elever <- hamta_elever_grundskola_forskoleklass_skolverket()
  head(elever)

  # Bara grundskolan per län, utan meddelande om tidsåtgång
  elever_lan <- hamta_elever_grundskola_forskoleklass_skolverket(
    hamta_kommuner = FALSE,
    hamta_forskoleklass = FALSE,
    meddelande_tid = FALSE
  )
}
