# Styr Edge eller Chrome mot rf.se och laddar ner en fil per år, vilket tar
# några minuter. Exemplet körs därför bara i en interaktiv session.
if (interactive() && rdwebbskrapning::test_internetanslutning()) {
  lok <- hamta_lok_per_forening()
  head(lok)

  # Spara Excel-filerna i en egen mapp och visa webbläsarfönstret
  mapp <- file.path(tempdir(), "lok_filer")
  lok <- hamta_lok_per_forening(nedladdningsmapp = mapp, headless = FALSE)
  list.files(mapp)
}
