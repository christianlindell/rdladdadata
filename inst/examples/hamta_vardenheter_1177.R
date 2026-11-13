# Läser 1177.se sida för sida och tar någon minut. Exemplet körs därför bara
# i en interaktiv session. Hämta helst en region åt gången.
if (interactive() && rdwebbskrapning::test_internetanslutning()) {
  # Alla mottagningar i Dalarna
  vard_dalarna <- hamta_vardenheter_1177("20")
  table(vard_dalarna$kategori, useNA = "ifany")

  # Bara vårdcentraler. Giltiga vårdtyper visas av hamta_vardtyper_lista_1177().
  vardcentraler <- hamta_vardenheter_1177("20", vardtyp = "Vårdcentraler")
}
