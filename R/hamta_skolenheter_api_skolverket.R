#' Hämta skolenheter från Skolverkets skolenhetsregister
#'
#' Hämtar skolenheter med adresser, koordinater, skolformer, årskurser,
#' gymnasieprogram, huvudman och kontaktuppgifter från Skolverkets API för
#' skolenhetsregistret. Skolformer och gymnasieprogram får klartext från
#' Skolverkets API:er för läroplaner och planerade utbildningar.
#'
#' Varje skolenhet kräver ett eget API-anrop, så hela landet tar lång tid.
#' Med `region_vekt` hämtas länsvis, så att webbadresserna inte blir för
#' långa.
#'
#' @param region_vekt Läns- och kommunkoder som text. En länskod hämtar alla
#'   kommuner i länet, och läns- och kommunkoder får blandas. `NA`
#'   (standard) hämtar hela landet.
#' @param skolstatus Skolenheternas status, som text: `"AKTIV"` (standard),
#'   `"VILANDE"`, `"UPPHORT"` eller `"PLANERAD"`. Flera går att ange.
#' @param geografi Koordinatsystem för den geometri som läggs till:
#'   `"sweref99"` (standard) eller `"sweref"` ger ett sf-objekt i SWEREF 99
#'   TM (EPSG:3006), `"wgs84"` ett sf-objekt i WGS 84 (EPSG:4326). `NA`
#'   eller ett annat värde ger en vanlig data frame utan geometri.
#' @param listkolumner_till_strang_avdelare Text som skiljer värdena åt när
#'   listkolumnerna (skolformer, årskurser, gymnasieprogram) görs om till
#'   text. Standard är `", "`, som passar för att spara i en databas.
#'   `"<br>"` passar för mapview. `NA` behåller listkolumnerna.
#'
#' @return Ett sf-objekt (eller en data frame, se `geografi`) med en rad per
#'   skolenhet.
#' @export
#' @example inst/examples/hamta_skolenheter_api_skolverket.R
hamta_skolenheter_api_skolverket <- function(
    region_vekt = NA,
    skolstatus = "AKTIV",
    geografi = "sweref99",
    listkolumner_till_strang_avdelare = ", "
  ) {

  skolenheter_basurl <- "https://api.skolverket.se/skolenhetsregistret/v2/school-units"        # bas-url för Skolverkets API

  api_hamta_json <- possibly(function(url) {
    resp <- GET(url); stop_for_status(resp)
    fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
  }, otherwise = NULL)

  # används för att lägga på klartext på skolformskoderna (har inte hittat nyckel så listat ut själv, inte 100 % säkert att de stämmer)
  skolverket_skolform <- api_hamta_json("https://api.skolverket.se/syllabus/v1/valuestore/schooltypes")$schoolTypes

  hamta_skolenhetsinfo <- function(skolenhetskod) {
    res <- api_hamta_json(glue("{skolenheter_basurl}/{skolenhetskod}"))
    adr <- pluck(res, "data", "attributes", "addresses", .default = tibble())
    besok <- adr %>% dplyr::filter(.data$type == "BESOKSADRESS") %>% dplyr::slice(1)
    post  <- adr %>% dplyr::filter(.data$type == "POSTADRESS")  %>% dplyr::slice(1)
    geo   <- if (nrow(besok) > 0) if(any(str_detect(names(besok), "geoCoordinates"))) TRUE else FALSE else FALSE

    tibble::tibble(
      Skolenhetskod        = pluck(res, "data", "schoolUnitCode", .default = NA_character_),
      Namn                 = pluck(res, "data", "attributes", "schoolName", .default = NA_character_),
      Namn_kort            = pluck(res, "data", "attributes", "displayName", .default = NA_character_),
      Kommunkod            = pluck(res, "data", "attributes", "municipalityCode", .default = NA_character_),

      Adress               = pluck(besok, "streetAddress", 1, .default = NA_character_),
      Postnummer           = pluck(besok, "postalCode",   1, .default = NA_character_),
      Postort              = pluck(besok, "locality",     1, .default = NA_character_),

      Utdelningsadress     = pluck(post,  "streetAddress", 1, .default = NA_character_),
      Utdelningspostnummer = pluck(post,  "postalCode",    1, .default = NA_character_),
      Utdelningspostort    = pluck(post,  "locality",      1, .default = NA_character_),

      Koordinat_SweRef_E   = if (geo) pluck(besok, "geoCoordinates.coordinateSweRefE", .default = NA_character_) else NA_character_,
      Koordinat_SweRef_N   = if (geo) pluck(besok, "geoCoordinates.coordinateSweRefN", .default = NA_character_) else NA_character_,
      Koordinat_WGS84_Lat  = if (geo) pluck(besok, "geoCoordinates.latitude",         .default = NA_character_) else NA_character_,
      Koordinat_WGS84_Lon  = if (geo) pluck(besok, "geoCoordinates.longitude",        .default = NA_character_) else NA_character_,

      Status               = pluck(res, "data", "attributes", "status", .default = NA_character_),
      Skolform_kod         = list(pluck(res, "data", "attributes", "schoolTypes",
                                               .default = NA_character_)),
      Arskurser            = list(pluck(res, "data", "attributes", "schoolTypeProperties", "gr", "grades",
                                               .default = NA_character_)),
      Gymnasieprogram_kod  = list(pluck(res, "data", "attributes", "schoolTypeProperties", "gy", "programmes",
                                        .default = NA_character_)),

      Specialskola         = pluck(res, "data", "attributes", "specialSupportSchool", .default = NA),
      Sjukhusskola         = pluck(res, "data", "attributes", "hospitalSchool",      .default = NA),
      Startdatum           = pluck(res, "data", "attributes", "startdate",           .default = NA_character_),
      Webbsida             = pluck(res, "data", "attributes", "url",                 .default = NA_character_),
      Epost                = pluck(res, "data", "attributes", "email",               .default = NA_character_),
      Telefonnummer        = pluck(res, "data", "attributes", "phoneNumber",         .default = NA_character_),
      Rektor               = pluck(res, "data", "attributes", "headMaster",          .default = NA_character_),
      Skolenhetstyp        = pluck(res, "data", "attributes", "schoolUnitType",      .default = NA_character_),

      Organisationstyp     = pluck(res, "included", "attributes", "organizerType",   .default = NA_character_),
      Organisationsnummer  = pluck(res, "included", "organizationNumber",            .default = NA_character_),
      Huvudman             = pluck(res, "included", "attributes", "displayName",     .default = NA_character_)
    )
  }

  # ====== hämta kommunkoder och kontrollera giltighet ============

  kommunnyckel <- hamtaregtab() %>%                              # hämta alla kommunkoder
    filter(nchar(regionkod) == 4) %>%
    rename(Kommun = region,
           Kommunkod = regionkod)
  
  if (!all(is.na(region_vekt))) {
    
    region_kommuner <- region_vekt[nchar(region_vekt) == 4]            # lägg alla kommunkoder i en vektor
    region_lan <- region_vekt[nchar(region_vekt) == 2]                 # lägg alla länskoder i en vektor
    kommunkoder <- hamtakommuner(region_lan, F, F) %>%                 # hämta alla kommuner som finns i län med länskod
      c(., region_kommuner)                                            # och lägg ihop med skickade kommunkoder

    kommunkoder <- kommunkoder[kommunkoder %in% kommunnyckel$Kommunkod]      # behåll bara kommunkoder som är giltiga
    unika_lan <- kommunkoder %>% str_sub(1, 2) %>% unique()
  }
  # ======= nyckel för gymnasieprogramkoder - hämtas via API från Skolverket =============

  gymnprg_nyckel_api <- api_hamta_json("https://api.skolverket.se/planned-educations/v3/support/programs")$body$gy

  inriktningar <- gymnprg_nyckel_api %>%
    unnest(cols = c(studyPaths), names_sep = "_") %>%
    select(Gymnasieprogram_kod = studyPaths_code,
           Gymnasieprogram = studyPaths_name)

  gymnprg_nyckel_old <- gymnprg_nyckel_api %>%                # liten speciallösning, vi tar bort "25" från koderna
    select(Gymnasieprogram_kod = code,                        # och "IB0" för att kunna koppla mot äldre gymnasie-
           Gymnasieprogram = name) %>%                        # programskoder
    mutate(Gymnasieprogram_kod = Gymnasieprogram_kod %>%
             str_remove_all("25") %>%
             str_remove_all("IB0"))

  gymnprg_nyckel <- gymnprg_nyckel_api %>%                    # lägg ihop inriktningar, gymnasieprogram och även
    select(Gymnasieprogram_kod = code,                        # gymnasieprogramkoder utan "25" eller "IB0"
           Gymnasieprogram = name) %>%
    bind_rows(gymnprg_nyckel_old, inriktningar) %>%
    distinct(Gymnasieprogram_kod, .keep_all = TRUE)

  # ===================== hämta data =============================

  # vi kör länsvis för att inte få för långa url:er
  if (all(is.na(region_vekt))) {
    skolor_url <- glue("{skolenheter_basurl}?status={paste0(skolstatus, collapse = ',')}")   # skapa url
    resp <- api_hamta_json(skolor_url)                                                       # hämta alla skolenheter
    skolenhetskoder <- resp$data$attributes$schoolUnitCode       # lägg alla skolenhetskoder i en variabel (för att lättare följa vad som händer i koden)
    skolor_df <- map(skolenhetskoder, ~ hamta_skolenhetsinfo(.x), .progress = TRUE) %>%
      list_rbind()

  } else {
    skolor_df <- map(unika_lan, ~{

      kommunkoder_lansvis <- kommunkoder[str_sub(kommunkoder, 1,2) %in% .x]
      # skapa en url för att hämta samtliga skolenheter för de kommuner som skickats med i sökningen
      skolor_url <- glue("{skolenheter_basurl}?status={paste0(skolstatus, collapse = ',')}&municipality_code={paste0(kommunkoder_lansvis, collapse = ',')}")

      resp <- api_hamta_json(skolor_url)                  # hämta alla skolenheter

      skolenhetskoder <- resp$data$attributes$schoolUnitCode       # lägg alla skolenhetskoder i en variabel (för att lättare följa vad som händer i koden)

      if (length(skolenhetskoder) > 0) {                   # hämta skolenhetsinformation om det finns skolenheter
        # hämta ut information om alla skolenheter - det måste göras en och en, vi använder map() från purrr för att göra det smidigt
        retur_df <- map(skolenhetskoder, ~ hamta_skolenhetsinfo(.x)) %>%
          list_rbind()
        return(retur_df)
      }
    }, .progress = TRUE) %>%
      list_rbind()
  }
  # här gör vi om nycklar för gymnasieprogram samt skolform som vi hämtar vi API från Skolverket till vektorer
  # så att det blir smidigare att koppla på dem i de listkolumner vi har över skolformer och gymnasieprogram
  gymnprg_vektor <- setNames(gymnprg_nyckel$Gymnasieprogram, gymnprg_nyckel$Gymnasieprogram_kod)
  skolform_vektor <- setNames(skolverket_skolform$name, skolverket_skolform$code)

  skolor_df <- skolor_df %>%
    mutate(Skolform = map(Skolform_kod, ~ {if (length(.x) == 0) list() else unname(skolform_vektor[.x])}),
           Gymnasieprogram = map(Gymnasieprogram_kod, ~ {if (length(.x) == 0) list() else unname(gymnprg_vektor[.x])})) %>%
    relocate(Gymnasieprogram, .after = Gymnasieprogram_kod) %>%
    relocate(Skolform, .after = Skolform_kod) %>%
    left_join(kommunnyckel, by = "Kommunkod") %>%
    relocate(Kommunkod, .before = Kommun)

  # om man vill omvandla listkolumner till vanliga kolumner där alla värden ligger åtskilda av avdelaren som valts ovan
  if (!is.na(listkolumner_till_strang_avdelare)) {
    collapse_or_na <- \(x) if (length(x)) paste(x, collapse = listkolumner_till_strang_avdelare) else NA_character_

    skolor_df <- skolor_df %>%
      mutate(
        Skolform_kod        = map_chr(Skolform_kod, collapse_or_na),
        Skolform            = map_chr(Skolform, collapse_or_na),
        Arskurser           = map_chr(Arskurser, collapse_or_na),
        Gymnasieprogram_kod = map_chr(Gymnasieprogram_kod, collapse_or_na),
        Gymnasieprogram     = map_chr(Gymnasieprogram, collapse_or_na),
        Skolform_kod        = ifelse(Skolform_kod == "NA", NA_character_, Skolform_kod),
        Skolform        = ifelse(Skolform == "NA", NA_character_, Skolform),
        Arskurser        = ifelse(Arskurser == "NA", NA_character_, Arskurser),
        Gymnasieprogram_kod        = ifelse(Gymnasieprogram_kod == "NA", NA_character_, Gymnasieprogram_kod),
        Gymnasieprogram        = ifelse(Gymnasieprogram == "NA", NA_character_, Gymnasieprogram)
      )
  }

  if (isTRUE(tolower(geografi) %in% c("sweref99", "sweref"))) {

    skolor_df <- skolor_df %>%
      mutate(
        geom = pmap(
          list(Koordinat_SweRef_E, Koordinat_SweRef_N),
          ~ {
            x <- suppressWarnings(as.numeric(..1))
            y <- suppressWarnings(as.numeric(..2))
            if (is.na(x) || is.na(y)) st_geometrycollection()
            else st_point(c(x, y))
          }
        )
      ) %>%
      st_as_sf(sf_column_name = "geom", crs = 3006)

  } else if (isTRUE(tolower(geografi) == "wgs84")) {

    skolor_df <- skolor_df %>%
      mutate(
        geom = pmap(
          list(Koordinat_WGS84_Lon, Koordinat_WGS84_Lat),
          ~ {
            lon <- suppressWarnings(as.numeric(..1))
            lat <- suppressWarnings(as.numeric(..2))
            if (is.na(lon) || is.na(lat)) st_geometrycollection()
            else st_point(c(lon, lat))
          }
        )
      ) %>%
      st_as_sf(sf_column_name = "geom", crs = 4326)

  }
  return(skolor_df)
}
