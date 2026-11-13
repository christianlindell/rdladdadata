# Styr Edge eller Chrome mot Socialstyrelsens statistikdatabas och tar några
# minuter. Exemplet körs därför bara i en interaktiv session.
if (interactive() && rdwebbskrapning::test_internetanslutning()) {
  individer <- hamta_ek_bistand_individer_socialstyrelsen()
  head(individer)

  # Utan loggmeddelanden under körningen
  individer <- hamta_ek_bistand_individer_socialstyrelsen(verbose = FALSE)
}
