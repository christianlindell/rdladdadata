# Kolumnnamn som funktionerna använder utan citattecken i dplyr- och
# tidyr-anrop. Listan gör att R CMD check inte rapporterar dem som okända
# variabler. Namn med å, ä och ö skrivs med \u-koder.
utils::globalVariables(c(
  ".", "Arskurser", "Gymnasieprogram", "Gymnasieprogram_kod", "Heading",
  "Idrott", "IsCompareableOn1177", "Kommun", "Kommunkod", "Koordinat_SweRef_E",
  "Koordinat_SweRef_N", "Koordinat_WGS84_Lat", "Koordinat_WGS84_Lon",
  "Longitude", "Regionkod", "Skolform", "Skolform_kod", "Value", "ar", "code",
  "dept", "enhet", "foreningsnr", "geografi", "huvudman", "idrott", "issuer",
  "issuer_lc", "kategori", "kategori_id", "kod", "lanskod", "lanskod_bokstav",
  "lasar", "latitude", "library_subtype", "library_type", "longitude",
  "match_group", "municipality_code", "name", "plot_group", "rad", "region",
  "regionkod", "row_id", "sigel", "skoltyp", "soknamn_region", "studyPaths",
  "studyPaths_code", "studyPaths_name", "subject", "subject_lc", "thumb",
  "varde", "varde_lista", "variabel",
  "Kommun", "adress", "epost", "geometry", "kom_kod", "kommunkod", "kommunnamn",
  "meta", "position", "postnummer", "postort", "telefonnummer", "title", "url",
  "Föreningsnr.", "Län", "Månad", "Månad_num", "År"
))
