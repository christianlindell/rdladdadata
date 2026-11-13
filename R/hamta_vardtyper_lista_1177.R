#' Lista vårdtyperna i 1177:s "Hitta vård"
#'
#' Läser listan över vårdtyper (t.ex. "Vårdcentraler" och
#' "Akutmottagningar") från sidan "Hitta vård" på 1177.se. Vårdtyperna kan
#' användas som `vardtyp` i [hamta_vardenheter_1177()].
#'
#' @return En data frame med en kolumn, `vardtyp`.
#' @seealso [hamta_vardenheter_1177()]
#' @export
#' @example inst/examples/hamta_vardtyper_lista_1177.R
hamta_vardtyper_lista_1177 <- function() {
  # url till hitta vård-sidan på 1177.se
  url_1177 <- "https://www.1177.se/hitta-vard/?st=208656b8-211b-4df9-ba72-912c7e02fca7&nearby=false&s=&g=&lat=&lng=&location=&caretype=&q=&openN=&openE=&openW=&openTN=&openTE=&openTW=&ow=&es=&l=&lq="

  html_grund <- read_html(url_1177)
  scripts <- html_elements(html_grund, "script")  %>%     # Hämta alla script-element
    html_text()
  geo_script <- scripts[str_detect(scripts, "__PRELOADED_STATE__")]
  json_text <- str_extract(geo_script, "\\{.*\\}")                              # Plocka ut JSON-delen
  geo_data <- fromJSON(json_text)                                               # Konvertera JSON-texten till en R-lista

  vardtyper_df <- geo_data$`__PRELOADED_STATE__`$Content$AOCareTypes$Categories %>%
    unlist() %>%
    unique() %>%
    as.data.frame() %>%
    setNames("vardtyp")
  return(vardtyper_df)
}
