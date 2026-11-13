# Styr Edge eller Chrome mot Socialstyrelsens statistikdatabas och tar några
# minuter. Exemplet körs därför bara i en interaktiv session.
if (interactive() && rdwebbskrapning::test_internetanslutning()) {
  hushall <- hamta_ek_bistand_hushall_socialstyrelsen()
  head(hushall)

  # Visa webbläsarfönstret och vänta längre, t.ex. vid felsökning
  hushall <- hamta_ek_bistand_hushall_socialstyrelsen(headless = FALSE, timeout = 60)
}
