# rdladdadata

Funktioner som hämtar statistik och registerdata från bland annat
Socialstyrelsen, Brottsförebyggande rådet, Skolverket, RF-SISU, 1177,
Kungliga biblioteket, NOBIL och SCB:s företagsregister. Funktionerna kommer
från skripten i repot [hamta_data](https://github.com/christianlindell/hamta_data)
och hämtar data via API:er, nedladdningsbara Excel-filer och webbskrapning.

## Installation

Kräver R 4.1 eller senare. Paketet använder Region Dalarnas paket
[rdverktyg](https://github.com/Region-Dalarna/rdpaket) och
[rdwebbskrapning](https://github.com/christianlindell/rdwebbskrapning), som
installeras automatiskt från GitHub via fältet `Remotes`.

```r
install.packages("remotes")
remotes::install_local("H:/R/R_claude/rdladdadata", dependencies = TRUE, build_vignettes = TRUE)
```

`build_vignettes = TRUE` gör att vignetten följer med; öppna den med
`vignette("rdladdadata")`.

### Det här behöver vissa funktioner

| Behövs för | Vad |
|---|---|
| Socialstyrelsen, `hamta_bra_kommunindikatorer()`, `hamta_lok_per_forening()` | Edge eller Chrome. Kontrollera med `rdwebbskrapning::testa_skrapmiljo()`. |
| `hamta_laddstationer_nobil()` | En kostnadsfri API-nyckel från [NOBIL](https://info.nobil.no/api), sparad med `keyring::key_set("nobil_laddstationer", username = "<användarnamn>")`. |
| `hamta_arbetsstallen()`, `hamta_foretag()`, `hamta_antal()`, `hamta_kodtabell()` | En API-nyckel till SCB:s företagsregister, i `.Renviron` som `SCB_AFR_API_KEY` (öppna filen med `usethis::edit_r_environ()`) eller i keyring (tjänsten `"SCB"`, användaren `"Foretagsregister"`). |
| `scb_`-funktionerna | SCB:s certifikat för sökpåver-API:t i Windows certifikatförråd. Kontrollera med `scb_hamta_cert()`. |
| `hamta_bygdegardar()` | Paketen V8 och [funcVGR](https://github.com/Samhallsanalys/funcVGR), VGR:s nätverk och inloggning till geodata.vgregion.se (se `?funcVGR::func_gis_hamta_con`). |

## Kom igång

```r
library(rdladdadata)

# BRÅ:s Nationella trygghetsundersökning per län och kön
ntu <- hamta_brott_ntu_lan_bra()
names(ntu)

# Vårdcentraler i Dalarna som kartpunkter
vardcentraler <- hamta_vardenheter_1177("20", vardtyp = "Vårdcentraler")
```

Alla funktioner hämtar data från andras servrar. Hämta det du behöver och
spara resultatet, i stället för att hämta samma data om och om igen. Fler
exempel finns på varje funktions hjälpsida och i vignetten.

## Funktioner

| Funktion | Vad den gör | Obligatoriska parametrar |
|---|---|---|
| `hamta_ek_bistand_hushall_socialstyrelsen()` | Hämta antal hushåll med ekonomiskt bistånd per månad från Socialstyrelsen | – |
| `hamta_ek_bistand_individer_socialstyrelsen()` | Hämta antal biståndsmottagare per månad från Socialstyrelsen | – |
| `hamta_ek_bistand_sek_socialstyrelsen()` | Hämta utbetalt ekonomiskt bistånd per månad från Socialstyrelsen | – |
| `hamta_bra_kommunindikatorer()` | Hämta BRÅ:s indikatorer för kommuners lägesbild | – |
| `hamta_brott_ntu_lan_bra()` | Hämta Nationella trygghetsundersökningen per län och kön från BRÅ | – |
| `hamta_elever_grundskola_forskoleklass_skolverket()` | Hämta antal elever i grundskola och förskoleklass från Skolverket | – |
| `hamta_skolenheter_api_skolverket()` | Hämta skolenheter från Skolverkets skolenhetsregister | – |
| `hamta_lok_per_forening()` | Hämta utbetalt LOK-stöd per förening från RF-SISU | – |
| `hamta_lokstod_kommun_regso_kon_rf()` | Hämta andel barn och unga med LOK-stöd per kommun och RegSO från RF-SISU | – |
| `hamta_gis_bibliotek_kb()` | Hämta bibliotek från Kungliga bibliotekets biblioteksdatabas | – |
| `hamta_bygdegardar()` | Hämta bygdegårdar i Sverige från Bygdegårdarnas riksförbund | – |
| `hamta_laddstationer_nobil()` | Hämta publika laddstationer i Sverige från NOBIL | – |
| `hamta_vardtyper_lista_1177()` | Lista vårdtyperna i 1177:s "Hitta vård" | – |
| `hamta_vardenheter_1177()` | Hämta vårdenheter från 1177 som kartpunkter | `regionkod` |
| `hamta_antal()` | Räkna arbetsställen eller juridiska enheter i SCB:s företagsregister | – |
| `hamta_arbetsstallen()` | Hämta arbetsställen från SCB:s företagsregister | – |
| `hamta_foretag()` | Hämta juridiska enheter (företag) från SCB:s företagsregister | – |
| `hamta_kodtabell()` | Hämta en kodtabell från SCB:s företagsregister | `kodtabell` |
| `scb_hamta_cert()` | Hitta SCB-certifikatet för sökpåver-API:t | – |
| `scb_hamta_foretag()` | Hämta företag från SCB:s sökpåver-API | – |
| `scb_hamta_arbetsstallen()` | Hämta arbetsställen från SCB:s sökpåver-API | – |
| `scb_hamta_arbetsstallen_med_foretag()` | Hämta arbetsställen och deras företag från SCB:s sökpåver-API | – |
| `scb_rakna_foretag_i_kommuner()` | Räkna företag per kommun i SCB:s sökpåver-API | `kommunkoder` |
| `scb_rakna_arbetsstallen_i_kommuner()` | Räkna arbetsställen per kommun i SCB:s sökpåver-API | `kommunkoder` |
| `scb_kategorier_med_kodtabeller()` | Hämta kategorier och kodtabeller från SCB:s sökpåver-API | – |

## Byta från skripten

Ta bort `source()`-anropen till skripten i hamta_data och lägg till
`library(rdladdadata)`. Funktionerna har samma namn, argument och beteende som
i skripten, med dessa skillnader:

- Funktionerna laddar inte längre paket med `pacman::p_load()`,
  `library()` eller `require()`, och installerar inga paket. Tidigare
  laddades t.ex. tidyverse och selenider in i din session när en funktion
  kördes. Använder ditt skript dplyr, sf eller selenider direkt behöver du
  ladda dem själv med `library()`.
- `func_API.R` och `func_webbskrapning.R` läses inte längre in från
  Region-Dalarnas repo. Funktionerna använder i stället paketen rdverktyg
  och rdwebbskrapning, som har samma funktioner.
- `hamta_lokstod_kommun_regso_kon_rf()` skapar inte längre objektet
  `kommun_nyckel` i den globala miljön.
- SCB:s sökpåver-API: certifikatet letas upp första gången det behövs, inte
  när skriptet läses in. Hjälpfunktionerna (t.ex. `hamta_api()`,
  `api_posta()`, `expandera_kommunkoder()` och `scb_hamta_cert_thumb()`)
  syns inte längre. Behöver ett skript anropa dem direkt:
  `rdladdadata:::expandera_kommunkoder("20")`.
- `hamta_elever_grundskola_forskoleklass_skolverket()` och
  `hamta_vardenheter_1177()` skriver ut sina meddelanden med `message()` i
  stället för `cat()`, så att de kan stängas av med `suppressMessages()`.
- `hamta_bygdegardar()` hämtar kommunytorna från VGR:s geodatabas med
  funcVGR i stället för med `hamta_karta()` från Region Dalarnas
  `func_GIS.R`. Den kräver därför VGR:s nätverk.
- Paketet kräver R 4.1 eller senare och httr2 1.1.0 eller senare.

Skripten finns kvar i repot hamta_data. Rättningar som görs i paketet kommer
inte med i skripten.

## Versionshistorik

### 0.2.0 (2026-11-13)

#### Nytt
* `hamta_bygdegardar()`: hämtar alla bygdegårdar i Sverige från
  bygdegardarna.se som kartpunkter. Funktionen fanns i skriptet
  `bygdegardar_geo.R`. Kommunytorna hämtas nu från VGR:s geodatabas med
  funcVGR i stället för med `hamta_karta()`, så funktionen kräver VGR:s
  nätverk och paketen funcVGR och V8.

### 0.1.1 (2026-11-13)

#### Rättat
* `hamta_ek_bistand_hushall_socialstyrelsen()` och
  `hamta_ek_bistand_individer_socialstyrelsen()` gav felet "object 'tmpdir'
  not found", eftersom den tillfälliga mappen aldrig skapades. Fanns det en
  `tmpdir` i den globala miljön användes den i stället, och mappen raderades
  när funktionen var klar. Nu används alltid en ny tillfällig mapp.
* `hamta_ek_bistand_hushall_socialstyrelsen()` och
  `hamta_ek_bistand_individer_socialstyrelsen()`: `verbose = FALSE` stänger nu
  av loggmeddelandena.
* Socialstyrelsens tre funktioner: `Månad_num` blev tom när datorns
  språkinställning inte var svensk. Månadsnamnen är nu fasta.
* `hamta_bra_kommunindikatorer()` hittade inte sökrutan efter att bra.se
  ändrats. Sökrutan pekas nu ut med sin platshållartext i stället för ett id
  som byts ut. Väntetiden för sökrutan följer nu `timeout`.
* `hamta_skolenheter_api_skolverket()` gav fel med flera regionkoder, t.ex.
  `c("20", "21")`, och med `geografi = NA`. `geografi = NA` ger nu en data
  frame utan geometri, och `"sweref"` fungerar som `"sweref99"`.
* `hamta_gis_bibliotek_kb()` gav fel med flera länskoder.
* `hamta_laddstationer_nobil()`: `returnera_sf = FALSE` returnerar nu en data
  frame med koordinaterna i kolumnerna `lat` och `lon`. Tidigare returnerades
  alltid ett sf-objekt.

### 0.1.0 (2026-11-13)

* Första versionen som paket. Funktionerna i skripten i hamta_data, utom
  `hamta_bygdegardar()`, har samlats i paketet med samma namn och argument,
  och varje funktion har en hjälpsida med exempel.
