# Gör ett API-anrop per skolenhet till Skolverket, vilket tar några minuter
# även för ett län. Exemplet körs därför bara i en interaktiv session.
if (interactive() && rdwebbskrapning::test_internetanslutning()) {
  # Aktiva skolenheter i Falun, som sf-objekt i SWEREF 99 TM
  skolor_falun <- hamta_skolenheter_api_skolverket(region_vekt = "2080")
  head(skolor_falun)

  # Både aktiva och vilande skolenheter i Dalarna, i WGS 84
  skolor_dalarna <- hamta_skolenheter_api_skolverket(
    region_vekt = "20",
    skolstatus = c("AKTIV", "VILANDE"),
    geografi = "wgs84"
  )
}
