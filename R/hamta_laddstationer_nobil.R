#' Hämta publika laddstationer i Sverige från NOBIL
#'
#' Laddar ner alla publika laddstationer för elbilar i Sverige från NOBIL:s
#' API och returnerar dem som ett sf-objekt med punkter i SWEREF 99 TM
#' (EPSG:3006).
#'
#' Funktionen kräver en API-nyckel från NOBIL, som är kostnadsfri (ansök på
#' <https://info.nobil.no/api>). Spara nyckeln en gång i keyring under
#' tjänsten `"nobil_laddstationer"`:
#'
#' ```
#' keyring::key_set("nobil_laddstationer", username = "<ditt användarnamn>")
#' ```
#'
#' `key_set()` frågar efter nyckeln, så den hamnar inte i ditt skript.
#' Saknas tjänsten avbryts funktionen med ett felmeddelande som förklarar
#' vad som behövs.
#'
#' @param returnera_sf `TRUE` (standard) returnerar ett sf-objekt. `FALSE`
#'   returnerar en data frame med koordinaterna i WGS 84 i kolumnerna `lat`
#'   och `lon`.
#'
#' @return Ett sf-objekt med en rad per laddstation och punktgeometri i
#'   SWEREF 99 TM, eller en data frame om `returnera_sf = FALSE`.
#' @export
#' @example inst/examples/hamta_laddstationer_nobil.R
hamta_laddstationer_nobil <- function(returnera_sf = TRUE) {

  # Kontrollera att keyring är installerat ------------------------------
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop("Paketet 'keyring' är inte installerat. Installera det med install.packages('keyring').")
  }
  
  # Kontrollera att tjänsten 'nobil_laddstationer' finns ----------------
  services <- keyring::key_list()
  
  if (!"nobil_laddstationer" %in% services$service) {
    stop("Ingen tjänst med namnet 'nobil_laddstationer' hittades i keyring. Du måste ha ett API från Nobil som du kan få kostnadsfritt här: https://info.nobil.no/api. Lägg in användare och API-nyckel som service 'nobil_laddstationer' i keyring-paketet så kommer detta skript att fungera.")
  }
  
  laddst_sv <- GET(paste0("https://nobil.no/api/server/datadump.php?apikey=", 
                          key_get("nobil_laddstationer", key_list(service = "nobil_laddstationer")$username), 
                          "&countrycode=SWE&fromdate=2012-06-02&format=json&file=false"))
  laddst_sv_resp <- fromJSON(httr::content(laddst_sv, as = "text"), flatten = FALSE)
  laddst_sv_df <- laddst_sv_resp$chargerstations$csmd
  
  #ta bort parenteser
  
  laddst_sv_df$Position <- gsub("[()]", "", as.character(laddst_sv_df$Position))
  
  #splitta kolumnen position
  laddstolpar_sf <- laddst_sv_df %>% separate_wider_delim(Position, ",", names = c("lat", "lon")) #WGS84 Decimal (lat, lon)

  if (!returnera_sf) {
    laddstolpar_sf$lat <- as.numeric(laddstolpar_sf$lat)
    laddstolpar_sf$lon <- as.numeric(laddstolpar_sf$lon)
    return(laddstolpar_sf)
  }

  laddstolpar_sf = st_as_sf(laddstolpar_sf, coords = c("lon", "lat"), 
                            crs = 4326, agr = "constant") %>% 
    st_transform(crs = 3006)
  
  return(laddstolpar_sf)
  
}
