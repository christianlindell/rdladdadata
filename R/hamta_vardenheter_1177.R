#' Hämta vårdenheter från 1177 som kartpunkter
#'
#' Hämtar vårdenheterna i en region från "Hitta vård" på 1177.se och
#' returnerar dem som ett sf-objekt med punkter i SWEREF 99 TM (EPSG:3006).
#' Informationen finns bara på sidor med tio enheter per sida, så funktionen
#' läser sida för sida. För att inte belasta 1177:s servrar hämtas bara en
#' region per anrop. Behövs flera regioner, lägg in en paus mellan anropen.
#'
#' Enheterna får en kolumn `kategori` (t.ex. "Vårdcentral", "Akutmottagning",
#' "Tandvård") som räknas fram från namnen. Vårdcentraler som 1177 visar som
#' flera enheter (t.ex. en mottagning och dess barnavårdscentral) grupperas
#' ihop med hjälp av namnen.
#'
#' @param regionkod Länskod för regionen, som text, t.ex. `"20"` för Dalarna.
#'   Anges flera används bara den första. Obligatorisk.
#' @param vardtyp Vårdtyp som ska hämtas, som text. Standard är
#'   `"Alla mottagningar"`. Giltiga vårdtyper listas av
#'   [hamta_vardtyper_lista_1177()], t.ex. `"Vårdcentraler"`.
#'
#' @return Ett sf-objekt med en rad per vårdenhet, med bl.a. kolumnerna
#'   `Vardenhetsnamn`, `kategori` och punktgeometri i SWEREF 99 TM.
#' @seealso [hamta_vardtyper_lista_1177()] för vilka vårdtyper som finns.
#' @export
#' @example inst/examples/hamta_vardenheter_1177.R
hamta_vardenheter_1177 <- function(
    regionkod,
    vardtyp = "Alla mottagningar"
    ) {

  starttid <- Sys.time()

  if (length(regionkod) > 1) {
    regionkod <- regionkod[1]
    message("Endast en regionkod kan skickas till funktionen och därför kommer enbart den första regionkoden att användas: ", regionkod)
  }

  # url till hitta vård-sidan på 1177.se
  url_1177 <- "https://www.1177.se/hitta-vard/?st=208656b8-211b-4df9-ba72-912c7e02fca7&nearby=false&s=&g=&lat=&lng=&location=&caretype=&q=&openN=&openE=&openW=&openTN=&openTE=&openTW=&ow=&es=&l=&lq="
  url_1177 <- str_replace(url_1177, "caretype=", paste0("caretype=", vardtyp)) %>% URLencode()

  html_grund <- read_html(url_1177)
  scripts <- html_elements(html_grund, "script")  %>%     # Hämta alla script-element
    html_text()
  geo_script <- scripts[str_detect(scripts, "__PRELOADED_STATE__")]
  json_text <- str_extract(geo_script, "\\{.*\\}")                              # Plocka ut JSON-delen
  geo_data <- fromJSON(json_text)                                               # Konvertera JSON-texten till en R-lista

  regioner_df <- geo_data$`__PRELOADED_STATE__`$Regionalization$Regions$Options

  reg_nyckel <- hamtaregion_kod_namn(hamtaAllaLan())

  regioner_df <- regioner_df %>%
    left_join(reg_nyckel %>% select(regionkod, soknamn_region = region), by = c("Value" = "regionkod"))

  region_sok <- regioner_df %>%
    filter(Value == regionkod) %>%
    dplyr::pull(soknamn_region)

  # läs in hitta vård-sidan på 1177.se
  url_region <- url_1177 %>% str_replace("location=", paste0("location=", URLencode(region_sok, reserved = TRUE)))
  html_1177 <- read_html(url_region)
  raw_text <- rvest::html_text(rvest::html_element(html_1177, "body"))
  total_hits <- as.integer(regmatches(raw_text, regexpr('(?<="TotalHits":)\\d+', raw_text, perl = TRUE)))
  n_pages <- ceiling(total_hits / 10)

  vardenheter_df <- map(1:n_pages, ~ {
    if (.x > 1) {
      url_pag <- str_c(url_region, "&p=", .x)

    } else url_pag <- url_region

    # läs in paginerad sida
    html <- read_html(url_pag)

    # Hämta alla script-element
    scripts <- html_elements(html, "script")  %>%
      html_text()

    # Hitta script som innehåller latitude
    geo_script <- scripts[str_detect(scripts, "__PRELOADED_STATE__")]

    # Plocka ut JSON-delen
    json_text <- str_extract(geo_script, "\\{.*\\}")

    # Konvertera JSON-texten till en R-lista
    geo_data <- fromJSON(json_text)

    # Plocka ut relevant data
    rader_df <- geo_data$`__PRELOADED_STATE__`$
      Content$
      SearchResult$
      SearchHits

    return(rader_df)

  }, .progress = TRUE) %>%
    list_rbind()

  # Hitta grupper där en rad är en delsträng av en annan
  df <- vardenheter_df %>%
    distinct() %>%
    mutate(row_id = row_number(), match_group = 0L)

  headings <- df$Heading

  for (i in seq_along(headings)) {
    # Hitta alla rader där heading[i] är en delsträng (men inte identisk)
    children <- which(
      str_detect(headings, regex(paste0("\\b", str_replace_all(fixed(headings[i]), "([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1"), "\\b"), ignore_case = TRUE)) &
        headings != headings[i] &
        nchar(headings) > nchar(headings[i])
    )

    if (length(children) > 0 && any(df$IsCompareableOn1177[c(i, children)]) && all(sapply(children, function(j) {
      parent_words <- str_to_lower(str_split(str_squish(str_remove_all(headings[i], regex("mottagning|,", ignore_case = TRUE))), "[\\s-]+")[[1]])
      child_words  <- str_to_lower(str_split(str_squish(str_remove_all(headings[j], regex("mottagning|,", ignore_case = TRUE))), "[\\s-]+")[[1]])
      str_detect(headings[j], regex("\\bmottagning\\b", ignore_case = TRUE)) && all(child_words %in% parent_words)
    }))) {

      group_id <- i
      df$match_group[i]        <- group_id      # förälder: bara siffra
      df$match_group[children] <- -group_id     # barn: negativ = får bokstav
    }
  }

  # Omvandla till läsbart format: 0, "1", "1a", "1b", ...
  letters_vec <- c(letters, paste0(letters, letters))  # a-z, aa-bb...

  df <- df %>%
    group_by(match_group) %>%
    mutate(
      plot_group = case_when(
        match_group == 0  ~ "0",
        match_group > 0   ~ as.character(match_group),  # förälder: siffra
        match_group < 0   ~ paste0(abs(match_group), letters_vec[row_number()])  # barn: siffra+bokstav
      )
    ) %>%
    ungroup()  %>%
    select(-row_id, -match_group)

  vard_med_kategori_df <- df %>%
    mutate(kategori = if_else(plot_group == 0 & IsCompareableOn1177, "Vårdcentral", NA),
           kategori = if_else(plot_group != 0 & str_detect(plot_group, "[a-z]"), "Vårdcentral", kategori),
           kategori = if_else(str_detect(tolower(Heading), "akutmottagning|närakut") & !str_detect(tolower(Heading), "psykiatrisk"), "Akutmottagning", kategori),
           kategori = if_else(str_detect(tolower(Heading), "psykiatrisk akutmottagning"), "Psykiatrisk akutmottagning", kategori),
           kategori = if_else(str_detect(tolower(Heading), "barnavårdscentral"), "Barnavårdscentral", kategori),
           kategori = if_else(str_detect(tolower(Heading), "familjecentral"), "Familjecentral", kategori),
           kategori = if_else(str_detect(tolower(Heading), "tand"), "Tandvård", kategori)
           )

  # konvertera till sf-objekt
  vardenheter_sf <- vard_med_kategori_df %>%
    filter(!is.na(Longitude)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(crs = 3006) %>%
    rename(Vardenhetsnamn = Heading) %>%
    select(-plot_group)

  tid_diff <- round(as.numeric(difftime(Sys.time(), starttid, units = "secs")))
  message(paste0("Det tog ", tid_diff ," sekunder att hämta vårdenheterna."))

  return(vardenheter_sf)
}
