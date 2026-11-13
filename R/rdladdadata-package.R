#' rdladdadata: hämta data från myndigheter och organisationer till R
#'
#' Funktioner som hämtar statistik och registerdata via API:er,
#' nedladdningsbara Excel-filer och webbskrapning. Funktionerna kommer från
#' skripten i repot hamta_data. De använder hjälpfunktioner från paketen
#' rdverktyg (regionkoder och nedladdning) och rdwebbskrapning (styrning av
#' Edge eller Chrome).
#'
#' @section Socialstyrelsen (ekonomiskt bistånd per månad):
#' [hamta_ek_bistand_hushall_socialstyrelsen()],
#' [hamta_ek_bistand_individer_socialstyrelsen()],
#' [hamta_ek_bistand_sek_socialstyrelsen()]
#'
#' @section Brottsförebyggande rådet:
#' [hamta_bra_kommunindikatorer()], [hamta_brott_ntu_lan_bra()]
#'
#' @section Skolverket:
#' [hamta_elever_grundskola_forskoleklass_skolverket()],
#' [hamta_skolenheter_api_skolverket()]
#'
#' @section RF-SISU (LOK-stöd):
#' [hamta_lok_per_forening()], [hamta_lokstod_kommun_regso_kon_rf()]
#'
#' @section Platser som kartpunkter:
#' [hamta_gis_bibliotek_kb()], [hamta_bygdegardar()], [hamta_laddstationer_nobil()],
#' [hamta_vardtyper_lista_1177()], [hamta_vardenheter_1177()]
#'
#' @section SCB:s företagsregister, nya API:t (apiafr.scb.se):
#' [hamta_arbetsstallen()], [hamta_foretag()], [hamta_antal()],
#' [hamta_kodtabell()]
#'
#' @section SCB:s företagsregister, sökpåver-API:t (kräver certifikat):
#' [scb_hamta_foretag()], [scb_hamta_arbetsstallen()],
#' [scb_hamta_arbetsstallen_med_foretag()], [scb_rakna_foretag_i_kommuner()],
#' [scb_rakna_arbetsstallen_i_kommuner()], [scb_kategorier_med_kodtabeller()],
#' [scb_hamta_cert()]
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom dplyr %>% .data across all_of any_of anti_join arrange as_tibble bind_rows filter
#'   case_when distinct group_by if_any if_else last_col left_join
#'   matches mutate n na_if relocate rename row_number select slice
#'   starts_with summarise tibble transmute ungroup where
#' @importFrom glue glue
#' @importFrom httr add_headers GET stop_for_status write_disk
#' @importFrom httr2 iterate_with_cursor req_headers req_perform req_perform_iterative
#'   req_retry req_throttle req_timeout req_url_path_append
#'   req_url_query request resp_body_json resp_body_raw
#' @importFrom jsonlite fromJSON
#' @importFrom keyring key_get key_list
#' @importFrom purrr compact imap keep list_rbind map map_chr map_dbl map_dfr map_int
#'   map_lgl map2 pluck pmap possibly set_names
#' @importFrom rdverktyg filhamtning_med_url_och_sokord hamtaAllaLan hamtakommuner
#'   hamtaregion_kod_namn hamtaregtab suppress_specific_warning
#' @importFrom rdwebbskrapning hamta_attribut hamta_nedladdning hamta_select_options
#'   hamta_select_varden hitta klicka_via_id
#'   klicka_via_klass_och_text klicka_via_text kor_js
#'   loggning_initiera stang_skrapsession starta_skrapsession
#'   textruta_inmatning valj_i_lista valj_option_utan_event
#'   vanta_pa_js
#' @importFrom readr parse_number
#' @importFrom readxl excel_sheets read_excel read_xlsx
#' @importFrom rlang := %||%
#' @importFrom rvest html_attr html_element html_elements html_nodes html_table html_text read_html
#' @importFrom selenider elem_select is_visible
#' @importFrom sf st_as_sf st_drop_geometry st_geometrycollection st_join st_point st_transform st_within
#' @importFrom stringr fixed regex str_c str_detect str_extract str_remove str_remove_all
#'   str_replace str_replace_all str_split str_squish str_sub
#'   str_to_lower str_to_sentence str_trim
#' @importFrom tidyr fill pivot_longer separate_wider_delim unnest unnest_longer
#' @importFrom utils head tail URLdecode URLencode
#' @importFrom stats setNames
NULL
