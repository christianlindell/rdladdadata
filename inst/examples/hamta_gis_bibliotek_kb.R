# Hämtar data från KB:s biblioteksdatabas. Exemplet körs bara i en
# interaktiv session, så att KB:s server inte belastas vid varje
# paketkontroll.
if (interactive() && rdwebbskrapning::test_internetanslutning()) {
  # Biblioteken i Dalarna som kartpunkter
  bibliotek_dalarna <- hamta_gis_bibliotek_kb("20")
  plot(sf::st_geometry(bibliotek_dalarna), pch = 20)

  # Hela landet som vanlig data frame med latitud och longitud
  bibliotek <- hamta_gis_bibliotek_kb(returnera_geo = FALSE)
  table(bibliotek$library_type)
}
