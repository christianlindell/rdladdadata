#' Hämta bibliotek från Kungliga bibliotekets biblioteksdatabas
#'
#' Hämtar svenska bibliotek med koordinater från KB:s biblioteksdatabas
#' (bibdb.libris.kb.se) och returnerar dem som kartpunkter i SWEREF 99 TM
#' (EPSG:3006) eller som en data frame. Bibliotek utan koordinater tas bort.
#'
#' En länskod översätts till KB:s länsbokstav med hjälp av tabellen över
#' ISO 3166-2:SE på svenska Wikipedia.
#'
#' @param skickad_lanskod Länskod som text, t.ex. `"20"`. `NA` (standard)
#'   hämtar hela landet.
#' @param returnera_geo `TRUE` (standard) returnerar ett sf-objekt. `FALSE`
#'   returnerar en data frame med kolumnerna `latitude` och `longitude`.
#'
#' @return Ett sf-objekt (eller en data frame) med en rad per bibliotek och
#'   kolumnerna `sigel`, `name`, `dept`, `library_type`, `library_subtype`,
#'   `lanskod` och `kommunkod`.
#' @export
#' @example inst/examples/hamta_gis_bibliotek_kb.R
hamta_gis_bibliotek_kb <- function(skickad_lanskod = NA, returnera_geo = TRUE) {
  
  if (!all(is.na(skickad_lanskod))){
  
  # nyckeltabell för att översätta mellan länskod och länsbokstav
  wiki_url <- "https://sv.wikipedia.org/wiki/ISO_3166-2:SE"
  wiki_html <- read_html(wiki_url)

  lanskoder_tabell <- html_table(html_nodes(wiki_html, "table"), fill = TRUE)[[1]] %>% 
    rename(lanskod_bokstav = 1, Lan = 2, lanskod = 3) %>%
    mutate(lanskod = lanskod %>% str_remove("SE-"),
           lanskod_bokstav = lanskod_bokstav %>% str_remove("SE\\-"),
           lanskod_bokstav = ifelse(lanskod_bokstav == "M", "LM", lanskod_bokstav))
  
  # sätt ihop en textsträng som skickas med api-sökningen, den är tom om man inte skickat med en länskod
  lan_api_txt <- lanskoder_tabell %>% 
    filter(lanskod %in% skickad_lanskod) %>%
    dplyr::pull(lanskod_bokstav) %>% 
    paste0(., collapse = ",") %>% 
    paste0("&county=", .)
  
 } else lan_api_txt <- ""
  
  if (length(skickad_lanskod) > 1) lan_api_txt <- ""
  
  # hämta bibliotek från KB - loopa så länge det finns data
  start_rad <- 0
  max_varde <- 200
  varv <- 0
  bib_alla_df <- data.frame()
  repeat {
    api_url <- paste0("https://bibdb.libris.kb.se/api/lib?start=", start_rad, "&limit=", max_varde, "&country_code=SE", lan_api_txt)
    
    resp <- GET(api_url) %>%                           # hämta data och bearbeta till läsbart format 
      httr::content(as = "text", encoding = "UTF-8") %>% 
      fromJSON(., flatten = TRUE)
    
    bib_varv_df <- resp$libraries %>%                      # hämta bibliotek från svaret
      mutate(lanskod = str_sub(municipality_code,1,2)) %>% 
      select(sigel, name, dept, library_type, library_subtype, lanskod, kommunkod = municipality_code, latitude, longitude) %>% 
      filter(!is.na(latitude),
             !is.na(longitude),
             latitude != 0,
             longitude != 0,
             !sigel %in% c("SEK", "SEK2"))
    
    # Kontrollera om data finns i API-uttaget och lägg till i bib_alla_df i så fall, annar bryter vi loopen
    if (length(bib_varv_df) == 0) {
      break
    } else {
      
      bib_alla_df <- bib_alla_df %>% bind_rows(bib_varv_df)       # sätt ihop data från olika varv
      varv <- varv + 1                                               # lägg på ett på variabeln varv
      start_rad <- max_varde * varv                                  # beräkna ny start-rad
      if(nrow(resp$libraries) < max_varde) break                     # om antalet rader är mindre än max_varde, bryt loopen
    } # slut kontroll om det finns data i uttaget
  } # slut repeat
  
 if (length(skickad_lanskod) > 1) bib_alla_df <- bib_alla_df %>% filter(lanskod %in% skickad_lanskod) 
  
  if (returnera_geo) {
    # skapa sf-objekt
    bibliotek_geo <- st_as_sf(bib_alla_df, coords = c("longitude", "latitude"), crs = 4326) %>% 
      st_transform(crs = 3006)
    return(bibliotek_geo)                    # returnera sf-objekt
    
  } else return(bib_alla_df)
}
