#' Hämta bygdegårdar i Sverige från Bygdegårdarnas riksförbund
#'
#' Hämtar alla bygdegårdar från sidan "Hitta bygdegård" på bygdegardarna.se
#' och returnerar dem som ett sf-objekt med punkter i SWEREF 99 TM
#' (EPSG:3006). Koordinaterna finns i sidans kartdata (JavaScript), som tolkas
#' med paketet V8. Varje bygdegård kopplas till kommun och län genom att
#' punkterna läggs mot kommunytorna i VGR:s geodatabas (tabellen
#' `kommunytor`, hämtad med [funcVGR::func_gis_hamta_geoobjekt()]).
#' Bygdegårdar som finns i sidans tabell men saknar koordinater tas med utan
#' geometri.
#'
#' Funktionen kräver paketen V8 och funcVGR, en anslutning till VGR:s
#' nätverk och inloggning till geodata.vgregion.se (se
#' [funcVGR::func_gis_hamta_con()]).
#'
#' @param returnera_sf `TRUE` (standard) returnerar ett sf-objekt. `FALSE`
#'   returnerar en data frame utan geometri.
#'
#' @return Ett sf-objekt (eller en data frame) med en rad per bygdegård och
#'   kolumnerna `namn`, `adress`, `postnummer`, `postort`, `telefonnummer`,
#'   `url`, `epost`, `kommunkod`, `kommun`, `lanskod` och `lan`, samt de
#'   kolumner som finns i sidans tabell.
#' @export
#' @example inst/examples/hamta_bygdegardar.R
hamta_bygdegardar <- function(returnera_sf = TRUE) {

  rlang::check_installed(c("V8", "funcVGR"), reason = "för att hämta bygdegårdar")

  # hämta kommunpolygoner för att bestämma kommun. I skriptet hämtades de med
  # hamta_karta("kommuner_lm") från Region Dalarnas func_GIS.R.
  kommuner_sf <- funcVGR::func_gis_hamta_geoobjekt(table_name = "kommunytor") %>%
    select(kommunkod = kom_kod, kommun = kommunnamn)

  lansnyckel <- hamtaregtab() %>%
    filter(nchar(regionkod) == 2 & regionkod != "00") %>%
    rename(lanskod = regionkod,
           lan = region)

  kommunnyckel <- hamtaregtab() %>%
    filter(nchar(regionkod) == 4) %>%
    rename(kommunkod = regionkod,
           kommun = region)

  bygdeg_url <- "https://bygdegardarna.se/hitta-bygdegard/?namn=&distrikt=&kommun=&storlek=&sok=S%C3%96K"

  res <- GET(bygdeg_url)

  res_content <- httr::content(res, as = "text", encoding = "UTF-8")

  html_obj <- read_html(res_content)

  # 2. Extrahera alla script-taggar som text
  scripts <- html_obj %>% html_elements("script") %>% html_text()

  # 3. Hitta den som innehåller markerData
  script_data <- scripts[str_detect(scripts, "var markerData")] [1]

  # 4. Plocka ut själva JS-arrayen
  js_array <- str_extract(script_data, "(?s)markerData\\s*=\\s*\\[.*?\\]")

  # 5. Rensa bort "markerData =" och semikolon
  js_array_clean <- js_array %>%
    str_replace("markerData\\s*=\\s*", "") %>%
    str_replace(";$", "")

  ctx <- V8::v8()

  # Ladda in JS-arrayen
  ctx$eval(paste0("var markerData = ", js_array_clean, ";"))

  # Konvertera JS-objektet till JSON-string i JS-engine
  ctx$eval("var jsonMarkerData = JSON.stringify(markerData);")

  # Hämta JSON som text
  json_data <- ctx$get("jsonMarkerData")

  # Konvertera JSON → R-objekt
  marker_data <- fromJSON(json_data)

  # konvertera HTML-entity till vanlig text
  marker_data$title <- sapply(marker_data$title, function(x) {
    xml2::xml_text(xml2::read_html(paste0("<x>", x, "</x>")))
  })

  bygdegardar_sf <- marker_data %>%
    mutate(lat = position$lat,
           lng = position$lng,
           adress = meta$address,
           postnummer = meta$postalCode,
           postort = meta$city,
           telefonnummer = meta$phone,
           url = meta$permalink,
           epost = meta$email,
           ) %>%
    st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
    st_transform(3006) %>%
    select(namn = title, adress, postnummer, postort, telefonnummer,
           url, epost, geometry) %>%
    st_join(
      kommuner_sf[, c("kommunkod", "kommun")], # välj bara relevanta kolumner
      left = TRUE,
      join = st_within
    ) %>%
    mutate(lanskod = str_sub(kommunkod, 1, 2)) %>%
    left_join(lansnyckel, by = "lanskod")

  tabell_utan_geo <- html_obj %>%
    html_element("table") %>%
    html_table() %>%
    rename(namn = "Bygdegård", kommun = Kommun)

  har_geo <- bygdegardar_sf %>%
    left_join(tabell_utan_geo, by = c("namn", "kommun"))

  saknar_geo <- tabell_utan_geo %>%
    anti_join(bygdegardar_sf, by = c("namn", "kommun"))

  saknas_i_karta <- bygdegardar_sf %>% anti_join(tabell_utan_geo, by = c("namn", "kommun"))
  saknar_geo <- saknar_geo[!saknar_geo$namn %in% saknas_i_karta$namn,] %>%
    left_join(kommunnyckel, by = "kommun") %>%
    mutate(lanskod = str_sub(kommunkod, 1, 2)) %>%
    left_join(lansnyckel, by = "lanskod")

  bygdegardar_sf <- bind_rows(har_geo, saknar_geo)

  if (!returnera_sf) bygdegardar_sf <- bygdegardar_sf %>% st_drop_geometry()
  return(bygdegardar_sf)

}
