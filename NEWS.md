# rdladdadata 0.2.0

## Nytt
* `hamta_bygdegardar()`: hämtar alla bygdegårdar i Sverige från
  bygdegardarna.se som kartpunkter. Funktionen fanns i skriptet
  `bygdegardar_geo.R`. Kommunytorna hämtas nu från VGR:s geodatabas med
  funcVGR i stället för med `hamta_karta()`, så funktionen kräver VGR:s
  nätverk och paketen funcVGR och V8.

# rdladdadata 0.1.1

## Rättat
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

# rdladdadata 0.1.0

* Första versionen som paket. Funktionerna i skripten i hamta_data, utom
  `hamta_bygdegardar()`, har samlats i paketet med samma namn och argument,
  och varje funktion har en hjälpsida med exempel.
