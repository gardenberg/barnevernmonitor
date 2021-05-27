#Script for transformering av økonomi-data til kommunemonitor barnevern
##14	Antall fosterhjemstiltak utenom familie og nære nettverk	https://www.ssb.no/statbank/table/10660/ (F, L), spesialbestilling fra SSB for K
#TIL #1G: Andel av fosterhjemstiltak i familie og nære nettverk, fylker og landet

#ANTALL - KOSTRA FOR FYLKER OG LANDET + SPESIALBESTILTE KOMMUNETALL LEVERT AV SSB

#opprettet 25. februar 2021
#versjon 001

#FORBEDRINGSPUNKTER
#Første versjon er veldig omfangsrik for å være forståelig. Andre versjon kan komprimeres.
#Det er også mye manuell testing her, kobling av lister for inspeksjon. Det kan tas ut og legges andre steder, heller enn inline
#Bakgrunnsdataene som lastes inn kan strømlinjeformes, slik at jeg får inn kun det jeg trenger.

#PROSESS
#det er to forskjellige prosesser - en for antall som kan summeres, og en for andeler som ikke kan summeres, men må deles.
#Det er også behov for ulik håndtering av KOSTRA-data og befolkningsdata, tall som vi lager sjøl, og filleveranser fra SSB og fylkesnemda

#BIBLIOTEKER
library(tidyverse)
library(readr)
library(janitor) #kjekk for funksjonen get_dupes()

#SETTINGS
#for å unngå vitenskapelig notering på visse høye eller lave tall 
options(scipen = 999)

#OPPSLAGSLISTER / BAKGRUNNSDATA

#metadata med informasjon om alle godkjente geografiske enheter pr. 23. februar 2021, med følgende historikk
#Bydeler: januar 2004 -> | #kommuner: januar 2013 ->  | #interkommunale samarbeid: kun gjeldende pr. februar 2021 
#fylker: januar 1972 ->  | #kode for hele landet: 0 |#regionale spesialkoder:  kun gjeldende for Longyearbyen pr februar 2021

metadata_geografi_alle <- read_delim("metadata_geografi/metadata_geografi_alle.csv", ";", escape_double = FALSE, 
                                     locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)

#liste over alle kommunekoder fra og med januar 2020 (fortsatt gjeldende 2021), med historiske kommunenummer tilbake til 2013.
#NB! Merk at kommunene 1850 Tysfjord ble delt fra 2019 til 2020 mellom 1806 Narvik og 1875 Hamarøy
#NB! Merk at kommunen 5012 Snillfjord fra 2019 til 2020 ble delt mellom 5055 Heim, 5056 Hitra og 5059 Orkland

historiske_kommunekoder <- read_delim("metadata_geografi/kommunenummer_fra2013_til2020_long.csv", ";", escape_double = FALSE, 
                                      locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)

#DEN OVER ERSTATTES MED DENNE
historiske_kommunekoder_gyldig <- read_delim("metadata_geografi/kommunenummer_fra2013_til2020_lang_gyldighet.csv", ";", escape_double = FALSE, 
                                             col_types = cols(gyldig_år = col_character()),
                                             locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE) %>%
  filter(gyldig_år %in% c("2015", "2016", "2017", "2018", "2019")) #dette kan gjøres i kilden


#metadata med informasjon om geografiske enheter vi ønsker å ha i outputen vår
#samme som metadata_geograf_alle, men kun gyldige enheter.

metadata_episerver <- read_delim("metadata_geografi/metadata_geografi_episerver.csv", ";", escape_double = FALSE, 
                                     locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)

#metadata med informasjon om hver enkelt kommunes IKS

metadata_iks <- read_delim("metadata_geografi/iks/iks_23-februar 2021.csv", ";", escape_double = FALSE, 
                           locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE) %>%
  filter(is.na(Samarbeidskommunenr) == FALSE) %>%
  mutate(
    Samarbeidskommunenr = str_replace_all(Samarbeidskommunenr, fixed(" "), "")
  )

#liste over bydelsnummer som må endres
#i motsetning til kommunelista over er det kun bydelene som må endres som er med her.

historiske_bydelsnr <- read_delim("metadata_geografi/bydelsnummer_fra2004_til2020_long.csv", ";", escape_double = FALSE, 
                                  col_types = cols(bydelsnr = col_character(),
                                                   historisk_nummer = col_character()),
                                  locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)

#liste over fylkesnummer som må endres
#i motsetning til kommunelista over er det kun fylkene som må endres som er med her.

historiske_fylkesnr <- read_delim("metadata_geografi/fylkesnummer_fra1972_til2020_long.csv", ";", escape_double = FALSE,
                                  col_types = cols(fylkesnr = col_character(),
                                                   historisk_nummer = col_character()),
                                  locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)


#STEG 1 1 laste inn data
tabell_14_fylker <- read_delim("datauttrekk_raw/tabell_14_fylker.csv", ";", escape_double = FALSE, 
                               col_types = cols(
                                 Region = col_character(),
                                 Barnevernstiltak = col_character(),
                                 ContentsCode = col_character(),
                                 Tid =  col_character(),
                                 value = col_number()
                               ),
                               locale = locale(decimal_mark = ",", grouping_mark = " "), 
                               trim_ws = TRUE)

#kommunedataene for både tabell 13 og 14 er i egne spesialleveranser fra SSB
#egen fil for historiske data: "data_manuelle_leveranser/tabell_13_14_kommuner_2013_2018.csv"
#eksempelfil: "data_manuelle_leveranser/tabell_13_14_eksempel dataleveranse fra SSB - fosterhjem i slekt og nettverk 2019.csv"

tabell_14_kommuner_2013_2018 <- read_delim("data_manuelle_leveranser/tabell_13_14_kommuner_2013_2018.csv", ";", escape_double = FALSE, 
                                           locale = locale(decimal_mark = ",", grouping_mark = "|", encoding = "ISO-8859-1"),
                                           col_types = cols(
                                             Tid = col_character(),
                                             Region = col_character(),
                                             fosterheim_familie_nare_nettverk = col_skip(),
                                             fosterheim_utenom_familie_nare_nettverk = col_number()
                                           ), 
                                           trim_ws = TRUE)

tabell_14_kommuner_2019 <- read_delim("data_manuelle_leveranser/tabell_13_14_eksempel dataleveranse fra SSB - fosterhjem i slekt og nettverk 2019.csv", ";", escape_double = FALSE, 
                                           locale = locale(decimal_mark = ",", grouping_mark = "|", encoding = "ISO-8859-1"),
                                           col_types = cols(
                                             kommunenummer = col_character(),
                                             kommune = col_skip(),
                                             fosterheim_familie_nettverk_ilaaaret_2019 = col_skip(),
                                             fosterheim_utenom_familie_nettverk_ilaaaret_2019 = col_number(), #verdt å sjekke i kilden hvordan NA, : . er håndtert fra SSB
                                             fosterheim_familie_nettverk_3112_2019 = col_skip(),
                                             fosterheim_utenom_familie_nettverk_3112_2019 = col_skip()
                                           ), 
                                           trim_ws = TRUE) %>%
  mutate(Tid = "2019")

#SØRGE FOR FELLES STRUKTUR OG BINDE SAMMEN
#REGION - TID - VALUE
#binder her sammen litt ulik koding

temp = select(tabell_14_fylker, Region, Tid, Value = value) %>%
  bind_rows(., select(tabell_14_kommuner_2013_2018, Region, Tid, Value = fosterheim_utenom_familie_nare_nettverk)) %>%
  bind_rows(., select(tabell_14_kommuner_2019, Region = kommunenummer, Tid, Value = fosterheim_utenom_familie_nettverk_ilaaaret_2019))

#4 FJERNER OVERFLØDIGE OBSERVASJONER STEG 1
#for tabell 1 returnerer API-et 0 for enheter som ikke lenger finnes.
#f.eks. 1601 i 2018 og 2019 er 0, mens 5001 er 0 i 2015-2017). 
#Disse kan trygt fjernes, gjør datasettet mer håndterlig før omkoding. 
# det ser også ut til å være delvis tilfelle for tabell 13, der fylke 50 har 0 2013-2017, og 16 og 17 i 2018-2019. Men 21 har også 0, og den finnes.
#dette er ikke tilfellet for KOSTRA-datasettene (16, 17, 18, ...), som i stedet returnerer NA-verdier for ikke-aktuelle 
#men KOSTRA har også NA for kommuner som finnes, men som har for få personer - altså observasjoner som vi må ta med oss.

#HVIS KOSTRA-DATA
#KOSTRA-DATA HAR KOSTRA-GRUPPER EKG, EAB - alle bydeler i Oslo og EAKUO - landet uten Oslo, som ikke trengs. Disse fjernes

#HVIS BEFOLKNINGSDATA
#temp = filter(temp, Value != 0)

temp = filter(temp, !(Region %in% c("16", "17", "50") & Value == 0))

#HVIS BARNEVERNSSTATISTIKK PÅ FYLKESNIVÅ

#hvilke koder finnes i datasettet, som ikke finnes i mal-listene?

test = distinct(temp, Region) %>%
  anti_join(., metadata_geografi_alle, by = c("Region" = "code"))

#SSB-tabell 10660 har litt ulike koder for hele landet og ulike barnevernsregioner (0N, R1-R7, 667200-667600)
#NB! Svalbard er koda 21

temp = filter(temp, !grepl("667", Region, fixed = TRUE)) %>%
  filter(., !grepl("R", Region, fixed = TRUE)) %>%
  filter(., !grepl("888", Region, fixed = TRUE)) %>%
  filter(., !grepl("0300", Region, fixed = TRUE))  #Oslo finnes både som 03, 0301 og 0300 - og er identisk


#5 OMKODING DEL 1 
test = distinct(temp, Region) %>%
  anti_join(., metadata_geografi_alle, by = c("Region" = "code"))

#HVIS AVVIKENDE BYDELSKODING (KKKKBBa i stedet for KKKKBB)
#KORREKT I KOSTRA, IKKE BEFOLKNING
#temp = mutate(temp, Region = gsub("a", "", Region))

#HVIS KOSTRA
#KOSTRA HAR EAK og er hele landet  | #EKA01, EKA02 osv som er fylkene - skal omkodes |

#HVIS BARNEVERNSDATA PÅ FYLKESNIVÅ
temp = mutate(temp,
              Region = ifelse(Region == "0N", "0", Region),
              Region = ifelse(Region == "21", "2111", Region))

test = distinct(temp, Region) %>%
  anti_join(., metadata_geografi_alle, by = c("Region" = "code"))

#HÅNDTERE DUPLIKATER

#her bør det ikke være noen duplikater? 
test = janitor::get_dupes(temp, Region, Tid)

#er dette duplikater også av verdi?
test2 = janitor::get_dupes(temp, Region, Tid, Value)
test3 = anti_join(test, test2)

fjerneliste = filter(temp, Region == "2111", is.na(Value)) %>%
  bind_rows(., filter(temp, Region == "50", is.na(Value)))

temp = anti_join(temp, fjerneliste) %>%
  distinct(., Region, Tid, Value) #siden jeg har separert håndtert duplikater der verdien var ulik, kan jeg trygt fjerne duplikater slik


#6 OMKODING DEL 2 - KOMMUNEMAPPING (FOR ANDELSVARIABEL)
#tenk på nytt m 1613/5012 og 1850 - Snillfjord og Tysfjord, splittekommunene.
#Disse bør droppes, ikke knotes med - inntil vi har avklart med de involverte kommunene hvordan de bruker det.

#Hvis det er et antall, kan det mappes.

#Hvis dette er en andel, er det kun kommuner som ikke er slått sammen som kan mappes. 
#sammenslåtte kommuner må beregnes sammen med IKS.

#HVIS ANTALL
temp_omkodet = rename(historiske_kommunekoder_gyldig, Tid = gyldig_år) %>%
  group_by(kommunenummer_2020, Tid) %>%
  mutate(endring = ifelse(kommunenummer_2020 != historisk_nummer, TRUE, FALSE),
         sammenslåtte = n(),
         endring = ifelse(sammenslåtte > 1, TRUE, endring)
  ) %>%
  filter(endring == TRUE) %>% #jeg vil bare ha kommuner som er sammenslåtte eller har endra nummer - (splitta er endra nr)
  left_join(., temp, by = c("historisk_nummer" = "Region", "Tid")) %>%
  mutate( 
    Value = ifelse(historisk_nummer %in% c("1613", "5012", "1850"), NA, Value) #Setter Snillfjord og Tysfjord til missing for summeringsformål
  ) %>%
  summarise(Value = sum(Value)) %>%
  rename(Region = 1)

#HVIS ANDEL 
#temp_omkodet = rename(historiske_kommunekoder_gyldig, Tid = gyldig_år) %>%
#  group_by(kommunenummer_2020, Tid) %>%
#  mutate(endring = ifelse(kommunenummer_2020 != historisk_nummer, TRUE, FALSE),
#         sammenslåtte = n(),
#         endring = ifelse(sammenslåtte > 1, FALSE, endring) #her er den FALSE for å finne kun de som har bytta nummer, ikke sammenslått
#         ) %>%
#  filter(endring == TRUE) %>% #jeg vil bare ha kommuner har endra nummer - (splitta er endra nr)
#  left_join(., temp, by = c("historisk_nummer" = "Region", "Tid")) %>%
#  mutate( 
#    Value = ifelse(historisk_nummer %in% c("1613", "5012", "1850"), NA, Value) #Setter Snillfjord og Tysfjord til missing for summeringsformål
#  ) %>%
#  select(Region = 1, Tid, Value)

#SAMME TESTING UANSETT ANTALL ELLER ANDEL
#og legger til som nye rader 
#hvis de er unike for den kombinasjonen av kode og år - sjekk?

test = bind_rows(temp, temp_omkodet) %>%
  janitor::get_dupes(., Region, Tid)

#unik kombinasjon av all variabler?
test2 = bind_rows(temp, temp_omkodet) %>%
  janitor::get_dupes(., Region, Tid, Value)

test3 = anti_join(test, test2)

#OBS! Det er noe uklart hvordan mapping er håndtert i de historiske kildene her. Jeg ignorerer det p.t.?

#fjerner duplikatene fra temp-fila
#dvs. at jeg tar ut NA-verdier, og for antall også sammenslåtte verdier?
#men tenk gjerne en gang til - er dette tilrådelig?
temp = anti_join(temp, temp_omkodet, by = c("Region", "Tid")) %>%
  bind_rows(., temp_omkodet)

test = janitor::get_dupes(temp, Region, Tid)

#7 OMKODING - FYLKEMAPPING
#OBS: NÅR DET KOMMER 2020-data, er det mulig denne vil feile og gi NA-verdier på alt 

#HVIS ANTALL
temp_fylke = left_join(historiske_fylkesnr, temp, by = c("historisk_nummer" = "Region")) %>%
  group_by(fylkesnr, Tid) %>%
  summarise(Value = sum(Value)) %>%
  filter(is.na(Value) == FALSE) %>% #på fylkesnivå er det ikke fare for å droppe manglende data - NA-verdier her skyldes kun ikke-eksisterende fylker i grunnlagsdata
  rename(Region = 1) %>%
  ungroup()

test = bind_rows(temp, temp_fylke) %>%
  janitor::get_dupes(., Region, Tid)

#fra datafila må jeg fjerne fylkene som har NA-verdier og som jeg har regnet ut verdier for her
temp = anti_join(temp, temp_fylke, by = c("Region", "Tid")) %>%
  bind_rows(., temp_fylke)

test = janitor::get_dupes(temp, Region, Tid)

#HVIS ANDEL
#Fylkesomkoding er en sammenslåing av gamle fylker, og kan ikke gjøres med andeler.

#8 OMKODING - BYDELSMAPPING
#110308 og 110309 er nye bydeler i 2020 uten historisk informasjon - vi mapper ikke fra kommune til bydel
#4601-bydelene til Bergen kan mappes, det samme kan Trondheimsbydeler fra 1601 til 5001
#OBS: Se på KOSTRA-tallene. Finnes disse overhodet for bydeler andre steder enn i Oslo?
#her er det kun kodebytte, ikke noe annet

#BÅDE ANTALL OG ANDEL
temp_bydel = left_join(historiske_bydelsnr, temp, by = c("historisk_nummer" = "Region")) %>%
  select(Region = bydelsnr, Tid, Value)

temp = bind_rows(temp, temp_bydel)

test = janitor::get_dupes(temp, Region, Tid)

#9 AGGREGERING AV MANGLENDE NIVÅER
#hvilke koder finnes i mal-listene, som ikke finnes i datasettet

test = distinct(temp, Region,  .keep_all = TRUE) %>%
  left_join(metadata_episerver, ., by = c("code" = "Region")) %>%
  filter(is.na(Tid)) #NA for Tid er de som finnes i metadata_episerver, men ikke i temp

#visuell inspeksjon viser at enhetene som ikke finnes i datasettet er en del bydeler, uoppgitt, IKS og Longyearbyen.
#for andeler vil vi også finne sammenslåtte kommuner som ikke er beregnet på nytt enda
#Longyearbyen veit jeg  ikke hva vi gjør med her.

#HVIS ANTALL
#aggregerer IKS
temp_iks = left_join(metadata_iks, temp, by = c("Kommunenummer" = "Region")) %>%
  group_by(Samarbeidskommunenr, Tid) %>%
  summarise(Value = sum(Value)) %>% #hvis NA på en av verdiene, så blir det NA på hele
  rename(Region = 1)

#binder disse inn i temp-datasettet
temp = bind_rows(temp, temp_iks)
test = janitor::get_dupes(temp, Region, Tid)

#HVIS ANDEL
#Aggregerer ikke IKS med andelsvariabel

#10 PRIKKING
#Skal vi prikke noe i grunnlagsdataene her?

#11 ANDEL
#OBS! FOR ANDELSVARIABLENE MÅ TELLER OG NEVNER LASTES INN OG OPPDATERES FØR ANDELSVARIABELEN OPPDATERES.

#v19: v16_netto_driftsutgifter_1000kr_totalt_barnevern / v18_barn_022_undersokelse_tiltak_ilaaar

#HVIS RELEVANT
#temp_tabell_16 <- read_delim("data_bearbeida/v16_netto_driftsutgifter_1000kr_totalt_barnevern.csv", ";", escape_double = FALSE, 
#                                col_types = cols(Region = col_character(),
#                                                 Tid = col_character(),
#                                                 Value = col_number(),
#                                                 Variabel = col_skip()
#                                ),
#                                locale = locale(decimal_mark = ",", grouping_mark = " "), 
#                                trim_ws = TRUE) %>%
#  rename(v16_netto_driftsutgifter_1000kr_totalt_barnevern = Value)
#
#temp_tabell_18 <- read_delim("data_bearbeida/v18_barn_022_undersokelse_tiltak_ilaaar.csv", ";", escape_double = FALSE, 
#                                 col_types = cols(Region = col_character(),
#                                                  Tid = col_character(),
#                                                  Value = col_number(),
#                                                  Variabel = col_skip()
#                                 ),
#                                 locale = locale(decimal_mark = ",", grouping_mark = " "), 
#                                 trim_ws = TRUE) %>%
#  rename(v18_barn_022_undersokelse_tiltak_ilaaar = Value)

#gjennom å bruke full_join her får jeg med alle region-tid-kombinasjoner i teller og nevner som ikke er i denne fila
#det vil blant annet si IKS, men kan også bety andre ting, f.eks. ved mismatch i lengden på tidsserien.

#temp = full_join(temp, temp_tabell_16, by = c("Region", "Tid")) %>%
#  full_join(., temp_tabell_18, by = c("Region", "Tid"))

#hvis variabelen er missing, så forsøk å beregne den 
#hvis ikke missing, behold den opprinnelige verdien
#NB! Husk samme format på tallet vi beregner her som det som allerede er her 
#TENK PÅ: Er det mest hensiktsmessig å beregne for alle NA-verdier? eller for alle IKS-verdier + kommuner og fylker som er sammenslått?

#temp = mutate(temp,
#                    Value =
#                      ifelse(is.na(Value) == TRUE, 
#                             round((v16_netto_driftsutgifter_1000kr_totalt_barnevern / v18_barn_022_undersokelse_tiltak_ilaaar)*1000, 0),
#                             Value
#                      )) %>%
#  select(Region, Tid, Value)

#test = filter(temp, is.na(Value))
#test = janitor::get_dupes(temp, Region, Tid)

#12 LAGRE-RYDDESTEG

#sjekk av om alle variabler har verdier
temp = ungroup(temp)
test = summarise_all(temp, funs(sum(is.na(.))))

#test = filter(temp, is.na(Region))
test = filter(temp, is.na(Tid))

#lager liste med manglende kombinasjoner i data (som vi skal ha) - gjelder KOSTRA-data med bydeler utenfor Oslo.
manglende_observasjoner = expand(temp, Region, Tid) %>%
  filter(is.na(Tid) == FALSE) %>%
  anti_join(temp) %>%
  semi_join(., metadata_episerver, by = c("Region" = "code"))

test = bind_rows(temp, manglende_observasjoner) %>%
  get_dupes(Region, Tid)

temp = bind_rows(temp, manglende_observasjoner) %>%
  filter(is.na(Tid) == FALSE)

#Alle Values som er NA, NaN, Inf eller -Inf skal prikkes ".."
#Sjekken for disse verdiene er is.finite - ikke is.infinite
#NB: dette konverterer også implisitt variabelen fra number til character

test = filter(temp, is.finite(Value) == FALSE)

temp = mutate(temp,
              Value = ifelse(is.finite(Value) == FALSE, "..", Value)
)

test = get_dupes(temp, Region, Tid)
test = summarise_all(temp, funs(sum(is.na(.))))


#legger på et variabelnavn som kan være id i fila
#og en variabel som forklarer enhet/tallformat
temp = mutate(temp, 
              Variabel = "v14_fosterheim_utenom_familie_nare_nettverk",
              Tallformat = "Antall"
)

#sjekk av at det er riktig antall kolonner her
#5 for de aller fleste, 6 for KHR.
ncol(temp) == 5 

#skriver ut til ferdig bearbeida data
write.csv2(temp, "data_bearbeida/v14_fosterheim_utenom_familie_nare_nettverk.csv", row.names = FALSE)

#rydder opp tabeller som ikke trenger å være i minnet lengre
rm(fjerneliste, historiske_bydelsnr, historiske_fylkesnr, historiske_kommunekoder,
   historiske_kommunekoder_gyldig, manglende_observasjoner, metadata_episerver,
   metadata_geografi_alle, metadata_iks, tabell_14_fylker, tabell_14_kommuner_2013_2018, tabell_14_kommuner_2019,
   temp, temp_bydel, temp_fylke, temp_iks, temp_omkodet, test, test2, test3
)
