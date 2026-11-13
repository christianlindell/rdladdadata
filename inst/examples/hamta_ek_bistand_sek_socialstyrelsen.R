# Styr Edge eller Chrome mot Socialstyrelsens statistikdatabas och gör tre
# exporter, vilket tar flera minuter. Exemplet körs därför bara i en
# interaktiv session.
if (interactive() && rdwebbskrapning::test_internetanslutning()) {
  belopp <- hamta_ek_bistand_sek_socialstyrelsen()
  head(belopp)

  # Summera utbetalt bistånd per år och bakgrund
  belopp |>
    dplyr::group_by(År, Bakgrund) |>
    dplyr::summarise(tkr = sum(.data[[names(belopp)[ncol(belopp)]]], na.rm = TRUE))
}
