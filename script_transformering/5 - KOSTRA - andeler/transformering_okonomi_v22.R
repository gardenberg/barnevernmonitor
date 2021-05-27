#SCript for transformering av økonomi-data til kommunemonitor barnevern

#opprettet 24. februar 2021
#versjon 001

#FORBEDRINGSPUNKTER
#Første versjon er veldig omfangsrik for å være forståelig. Andre versjon kan komprimeres.
#Det er også mye manuell testing her, kobling av lister for inspeksjon. Det kan tas ut og legges andre steder, heller enn inline
#Bakgrunnsdataene som lastes inn kan strømlinjeformes, slik at jeg får inn kun det jeg trenger.

#PROSESS
#det er to forskjellige prosesser - en for antall som kan summeres, og en for andeler som ikke kan summeres, men må deles.
#Det er også behov for ulik håndtering av KOSTRA-data og befolkningsdata.

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

#VARIABLER
# - 19	Netto driftsutgifter til barnevern pr. barn i barnevernet	Beregnes med nr. 16 delt på nr. 18, eller hentes fra https://www.ssb.no/statbank/table/12279/  (K) og https://www.ssb.no/statbank/table/12601/ (B)
# TIL 2B Utgifter pr. barn i barnevernet 

#ANDEL - KOSTRA

#STEG 1: Laste inn data
tabell_22_kommune <- read_delim("datauttrekk_raw/tabell_22_kommune.csv", ";", escape_double = FALSE, 
                               col_types = cols(KOKkommuneregion0000 = col_character(),
                                                ContentsCode = col_character(),
                                                Tid =  col_character(),
                                                value = col_number()
                               ),
                               locale = locale(decimal_mark = ",", grouping_mark = " "), 
                               trim_ws = TRUE)

tabell_22_bydel <- read_delim("datauttrekk_raw/tabell_22_bydel.csv", ";", escape_double = FALSE, 
                             col_types = cols(KOKbydelsregion0000 = col_character(),
                                              ContentsCode = col_character(),
                                              Tid =  col_character(),
                                              value = col_number()
                                              ), 
                             locale = locale(decimal_mark = ",", grouping_mark = " "), 
                             trim_ws = TRUE)

#2 SLÅ SAMMEN  unødvendige kategorier
# nødvendige dimensjoner er region, tid og verdi-variabelen (SSB-Apiet vil antakeligvis også ha contentscode, dvs 4)
# tabell_16_bydel har ikke aggregert over funksjoner, aggregerer

#EKSEMPEL
#temp = select(temp, -ContentsCode) %>%
#group_by(Region, Tid) %>%
#summarise(Value = sum(value)) 

#3 SLÅ SAMMEN SEPARATE SPØRRINGER ETTER SAMME VARIABEL
temp = select(tabell_22_kommune, Region = KOKkommuneregion0000, Tid, Value = value) %>%
  bind_rows(., select(tabell_22_bydel, Region = KOKbydelsregion0000, Tid, Value = value))

#4 FJERNER OVERFLØDIGE OBSERVASJONER STEG 1
#for tabell 1 returnerer API-et 0 for enheter som ikke lenger finnes.
#f.eks. 1601 i 2018 og 2019 er 0, mens 5001 er 0 i 2015-2017). 
#Disse kan trygt fjernes, gjør datasettet mer håndterlig før omkoding. 
#dette er ikke tilfellet for KOSTRA-datasettene (16, 17, 18, ...), som i stedet returnerer NA-verdier for ikke-aktuelle 
#men KOSTRA har også NA for kommuner som finnes, men som har for få personer - altså observasjoner som vi må ta med oss.

#HVIS BEFOLKNINGSDATA
#temp = filter(temp, Value != 0)

#HVIS KOSTRA-DATA
#KOSTRA-DATA HAR KOSTRA-GRUPPER EKG, EAB - alle bydeler i Oslo og EAKUO - landet uten Oslo, som ikke trengs. Disse fjernes

temp = filter(temp, !grepl("EKG", Region, fixed = TRUE)) %>%
  filter(., !grepl("EAKUO", Region, fixed = TRUE)) %>%
  filter(., !grepl("EAB", Region, fixed = TRUE))

#5 OMKODING DEL 1 
#hvilke koder finnes i datasettet, som ikke finnes i mal-listene?

test = distinct(temp, Region) %>%
  anti_join(., metadata_geografi_alle, by = c("Region" = "code"))

#HVIS AVVIKENDE BYDELSKODING (KKKKBBa i stedet for KKKKBB)
#KORREKT I KOSTRA, IKKE BEFOLKNING
#temp = mutate(temp, Region = gsub("a", "", Region))

#HVIS KOSTRA
#KOSTRA HAR EAK og er hele landet  | #EKA01, EKA02 osv som er fylkene - skal omkodes |

temp = mutate(temp,
              Region = ifelse(Region == "EAK", "0", Region),
              Region = gsub("EKA", "", Region))

#her bør det ikke være noen duplikater? 
test = janitor::get_dupes(temp, Region, Tid)

#6 OMKODING DEL 2 - KOMMUNEMAPPING (FOR ANDELSVARIABEL)
#tenk på nytt m 1613/5012 og 1850 - Snillfjord og Tysfjord, splittekommunene.
#Disse bør droppes, ikke knotes med - inntil vi har avklart med de involverte kommunene hvordan de bruker det.

#siden dette er en andel, er det kun kommuner som ikke er slått sammen som kan mappes. 
#sammenslåtte kommuner må beregnes sammen med IKS.

#HVIS ANTALL
#temp_omkodet = rename(historiske_kommunekoder_gyldig, Tid = gyldig_år) %>%
#  group_by(kommunenummer_2020, Tid) %>%
#  mutate(endring = ifelse(kommunenummer_2020 != historisk_nummer, TRUE, FALSE),
#         sammenslåtte = n(),
#         endring = ifelse(sammenslåtte > 1, TRUE, endring)
#  ) %>%
#  filter(endring == TRUE) %>% #jeg vil bare ha kommuner som er sammenslåtte eller har endra nummer - (splitta er endra nr)
#  left_join(., temp, by = c("historisk_nummer" = "Region", "Tid")) %>%
#  mutate( 
#    Value = ifelse(historisk_nummer %in% c("1613", "5012", "1850"), NA, Value) #Setter Snillfjord og Tysfjord til missing for summeringsformål
#  ) %>%
#  summarise(Value = sum(Value)) %>%
#  rename(Region = 1)

#HVIS ANDEL 
temp_omkodet = rename(historiske_kommunekoder_gyldig, Tid = gyldig_år) %>%
  group_by(kommunenummer_2020, Tid) %>%
  mutate(endring = ifelse(kommunenummer_2020 != historisk_nummer, TRUE, FALSE),
         sammenslåtte = n(),
         endring = ifelse(sammenslåtte > 1, FALSE, endring) #her er den FALSE for å finne kun de som har bytta nummer, ikke sammenslått
         ) %>%
  filter(endring == TRUE) %>% #jeg vil bare ha kommuner har endra nummer - (splitta er endra nr)
  left_join(., temp, by = c("historisk_nummer" = "Region", "Tid")) %>%
  mutate( 
    Value = ifelse(historisk_nummer %in% c("1613", "5012", "1850"), NA, Value) #Setter Snillfjord og Tysfjord til missing for summeringsformål
  ) %>%
  select(Region = 1, Tid, Value)

#SAMME TESTING UANSETT ANTALL ELLER ANDEL
#og legger til som nye rader 
#hvis de er unike for den kombinasjonen av kode og år - sjekk?

test = bind_rows(temp, temp_omkodet) %>%
  janitor::get_dupes(., Region, Tid)

#unik kombinasjon av all variabler?
test = bind_rows(temp, temp_omkodet) %>%
  janitor::get_dupes(., Region, Tid, Value)

#fjerner duplikatene fra temp-fila
#dvs. at jeg tar ut NA-verdier, og for antall også sammenslåtte verdier?
#men tenk gjerne en gang til - er dette tilrådelig?
temp = anti_join(temp, temp_omkodet, by = c("Region", "Tid")) %>%
  bind_rows(., temp_omkodet)

test = janitor::get_dupes(temp, Region, Tid)

#7 OMKODING - FYLKEMAPPING
#OBS: NÅR DET KOMMER 2020-data, er det mulig denne vil feile og gi NA-verdier på alt 

#HVIS ANTALL
#temp_fylke = left_join(historiske_fylkesnr, temp, by = c("historisk_nummer" = "Region")) %>%
#  group_by(fylkesnr, Tid) %>%
#  summarise(Value = sum(Value)) %>%
#  filter(is.na(Value) == FALSE) %>% #på fylkesnivå er det ikke fare for å droppe manglende data - NA-verdier her skyldes kun ikke-eksisterende fylker i grunnlagsdata
#  rename(Region = 1) %>%
#  ungroup()

#test = bind_rows(temp, temp_fylke) %>%
#  janitor::get_dupes(., Region, Tid)

#fra datafila må jeg fjerne fylkene som har NA-verdier og som jeg har regnet ut verdier for her
#temp = anti_join(temp, temp_fylke, by = c("Region", "Tid")) %>%
#  bind_rows(., temp_fylke)

#test = janitor::get_dupes(temp, Region, Tid)

#HVIS ANDEL
#Fylkesomkoding er en sammenslåing av gamle fylker, og kan ikke gjøres med andeler.

#8 OMKODING - BYDELSMAPPING
#110308 og 110309 er nye bydeler i 2020 uten historisk informasjon - vi mapper ikke fra kommune til bydel
#4601-bydelene til Bergen kan mappes, det samme kan Trondheimsbydeler fra 1601 til 5001
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
#temp_iks = left_join(metadata_iks, temp, by = c("Kommunenummer" = "Region")) %>%
#  group_by(Samarbeidskommunenr, Tid) %>%
#  summarise(Value = sum(Value)) %>% #hvis NA på en av verdiene, så blir det NA på hele
#  rename(Region = 1)

#binder disse inn i temp-datasettet
#temp = bind_rows(temp, temp_iks)
#test = janitor::get_dupes(temp, Region, Tid)

#HVIS ANDEL
#Aggregerer ikke IKS med andelsvariabel

#10 PRIKKING
#Skal vi prikke noe i grunnlagsdataene her?

#11 ANDEL
#OBS! FOR ANDELSVARIABLENE MÅ TELLER OG NEVNER LASTES INN OG OPPDATERES FØR ANDELSVARIABELEN OPPDATERES.

#v19: v16_netto_driftsutgifter_1000kr_totalt_barnevern / v18_barn_022_undersokelse_tiltak_ilaaar
#v22: v21_brutto_driftsutgifter_252_1000kr / v20_barn_022_252_ilaaar

#HVIS RELEVANT
temp_tabell_20 <- read_delim("data_bearbeida/v20_barn_022_252_ilaaar.csv", ";", escape_double = FALSE, 
                                col_types = cols(Region = col_character(),
                                                 Tid = col_character(),
                                                 Value = col_number(),
                                                 Variabel = col_skip(),
                                                 Tallformat = col_skip()
                                ),
                             locale = locale(decimal_mark = ",", grouping_mark = " "), 
                             na = "..", 
                                trim_ws = TRUE) %>%
  rename(v20_barn_022_252_ilaaar = Value)

temp_tabell_21 <- read_delim("data_bearbeida/v21_brutto_driftsutgifter_252_1000kr.csv", ";", escape_double = FALSE, 
                                 col_types = cols(Region = col_character(),
                                                  Tid = col_character(),
                                                  Value = col_number(),
                                                  Variabel = col_skip(),
                                                  Tallformat = col_skip()
                                 ),
                             locale = locale(decimal_mark = ",", grouping_mark = " "), 
                             na = "..",  
                                 trim_ws = TRUE) %>%
  rename(v21_brutto_driftsutgifter_252_1000kr = Value)

#gjennom å bruke full_join her får jeg med alle region-tid-kombinasjoner i teller og nevner som ikke er i denne fila
#det vil blant annet si IKS, men kan også bety andre ting, f.eks. ved mismatch i lengden på tidsserien.

temp = full_join(temp, temp_tabell_20, by = c("Region", "Tid")) %>%
  full_join(., temp_tabell_21, by = c("Region", "Tid"))

#hvis variabelen er missing, så forsøk å beregne den 
#hvis ikke missing, behold den opprinnelige verdien
#NB! Husk samme format på tallet vi beregner her som det som allerede er her 
#TENK PÅ: Er det mest hensiktsmessig å beregne for alle NA-verdier? eller for alle IKS-verdier + kommuner og fylker som er sammenslått?

temp = mutate(temp,
                    Value =
                      ifelse(is.na(Value) == TRUE, 
                             round((v21_brutto_driftsutgifter_252_1000kr / v20_barn_022_252_ilaaar)*1000, 0),
                             Value
                      )) %>%
  select(
    Region, 
    Tid, 
    Andel = Value) %>%
  pivot_longer(cols = Andel, names_to = "Tallformat", values_to = "Value")

#12 LAGRE-RYDDESTEG

temp = ungroup(temp)
test = summarise_all(temp, funs(sum(is.na(.))))

#test = filter(temp, is.na(Region))
#test = filter(temp, is.na(Halvår))
test = filter(temp, is.na(Tid))

#lager liste med manglende kombinasjoner i data (som vi skal ha)

#hvilke enheter er det som finnes i metadata_episerver, som vi skal publisere på
#men som ikke finnes i temp?
manglende_enheter = anti_join(metadata_episerver, temp, by = c("code" = "Region")) %>%
  select(Region = code)

#disse hektes ganske enkelt på Temp. Verdiene kan vi finne i neste steg

temp = bind_rows(temp, manglende_enheter)

#denne funksjonen finner alle kombinasjoner av dataene Region, Tid mm i datasettet 
#filtrerer så vekk kombinasjonene som har NA for Tid, Tallformat mm
#sjekker så mot temp, og beholder de dataene som ikke allerede finnes
#sjekker så mot metadata_episerver, og beholder kun de dataene som skal publiseres
manglende_observasjoner = expand(temp, Region, Tid, Tallformat) %>%
  filter(is.na(Tid) == FALSE, is.na(Tallformat) == FALSE) %>%
  anti_join(temp) %>%
  semi_join(., metadata_episerver, by = c("Region" = "code"))

test = bind_rows(temp, manglende_observasjoner) %>%
  get_dupes(Region, Tid, Tallformat)

temp = bind_rows(temp, manglende_observasjoner) %>%
  filter(is.na(Tid) == FALSE, is.na(Tallformat) == FALSE)

test = filter(temp, is.na(Tallformat))

#Alle Values som er NA, NaN, Inf eller -Inf skal prikkes ".."
#Sjekken for disse verdiene er is.finite - ikke is.infinite
#NB: dette konverterer også implisitt variabelen fra number til character

test = filter(temp, is.finite(Value) == FALSE)

temp = mutate(temp,
              Value = ifelse(is.finite(Value) == FALSE, "..", Value)
)

test = summarise_all(temp, funs(sum(is.na(.))))
test = janitor::get_dupes(temp, Region, Tid, Tallformat)

#legger på et variabelnavn som kan være id i fila
temp = mutate(temp, Variabel = "2C_brutto_driftsutgifter_252_prbarn_utenfor_hjemmet")

#sjekk av at det er riktig antall kolonner her
#5 for de aller fleste, 6 for KHR.
ncol(temp) == 5

#skriver ut til ferdig bearbeida data
write.csv2(temp, "data_til_pivotering/2C_brutto_driftsutgifter_252_prbarn_utenfor_hjemmet.csv", row.names = FALSE)
write.csv2(temp, "data_bearbeida/v22_brutto_driftsutgifter_252_prbarn_utenfor_hjemmet.csv", row.names = FALSE)

#rydder opp tabeller som ikke trenger å være i minnet lengre
rm(historiske_bydelsnr, historiske_fylkesnr, historiske_kommunekoder,
   historiske_kommunekoder_gyldig, manglende_observasjoner, metadata_episerver,
   metadata_geografi_alle, metadata_iks, tabell_22_bydel, tabell_22_kommune,
   temp, temp_bydel, temp_omkodet, temp_tabell_20, temp_tabell_21, test
)
