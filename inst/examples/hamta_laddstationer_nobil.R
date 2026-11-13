# Kräver en API-nyckel från NOBIL sparad i keyring (se Detaljer) och
# internet. Exemplet körs bara i en interaktiv session.
nyckel_finns <- tryCatch(
  "nobil_laddstationer" %in% keyring::key_list()$service,
  error = function(e) FALSE
)
if (interactive() && nyckel_finns && rdwebbskrapning::test_internetanslutning()) {
  laddstationer <- hamta_laddstationer_nobil()
  nrow(laddstationer)

  # Rita stationerna på en karta
  plot(sf::st_geometry(laddstationer), pch = 20, cex = 0.3)
}
