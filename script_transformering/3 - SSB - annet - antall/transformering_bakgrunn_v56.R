#SCript for transformering av data til kommunemonitor barnevern

# 4E - Andel barn i lavinntektsfamilier (EU-skala 60 prosent)
#VURDERES OM LAVINNTEKTSTALL FRA BARNEFATTIGDOM.NO SKAL BRUKES HER
#OBS TIL BEREGNING: DENNE ER LITT ANNERLEDES. Vi har andelen barn i lavinntektsfamilier, og antall barn under 18 år. 
#For å beregne for IKS m.m. må vi beregne antallet barn i lavinntektsfamilier, og så lage en ny andelsberegning.
#Er det tilrådelig?

#56	Antall «personer under 18 år» (fra statistikken for Barn 0-17 år som bor i lavinntektsfamilier)

#opprettet 4. mars 2021
#versjon 001

#FORBEDRINGSPUNKTER
#Første versjon er veldig omfangsrik for å være forståelig. Andre versjon kan komprimeres.
#Det er også mye manuell testing her, kobling av lister for inspeksjon. Det kan tas ut og legges andre steder, heller enn inline
#Bakgrunnsdataene som lastes inn kan strømlinjeformes, slik at jeg får inn kun det jeg trenger.

#PROSESS
#det er to forskjellige prosesser - en for antall som kan summeres, og en for andeler som ikke kan summeres, men må deles.

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



#1 laste inn data igjen
#TODO: legge inn eksplisitte definisjoner av typer
tabell_56 <- read_delim("datauttrekk_raw/tabell_56.csv", ";", escape_double = FALSE, 
                               col_types = cols(Region = col_character(),
                                                Tid = col_character()), 
                               locale = locale(decimal_mark = ",", grouping_mark = " "), 
                               trim_ws = TRUE)

#2 SLÅ SAMMEN SEPARATE SPØRRINGER ETTER SAMME VARIABEL

#temp = bind_rows(tabell_51_kommune, tabell_51_bydel) %>%
#  select(Region, Tid, Value = value)

#3 SLÅ SAMMEN  unødvendige kategorier
# nødvendige dimensjoner er region, tid og verdi-variabelen (SSB-Apiet vil antakeligvis også ha contentscode, dvs 4)
# aggregerer det sammen

#temp = select(temp, -ContentsCode) %>%
#  group_by(Region, Tid) %>%
#  summarise(Value = sum(value, na.rm = FALSE)) #hvis noen av cellene er missing, bli hele verdien missing

temp = select(tabell_56, Region, Tid, Value = value)

#4 FJERNER OVERFLØDIGE OBSERVASJONER STEG 1
#for tabell 1 og 45, 47, 50, 55 returnerer API-et 0 for enheter som ikke lenger finnes 
#f.eks. 1601 i 2018 og 2019 er 0, mens 5001 er 0 i 2015-2017). 
#Disse kan trygt fjernes, gjør datasettet mer håndterlig før omkoding. 

#for tabell 47 er det litt mer usikkert - kan det være 0 for små kommuner som finnes? det er ikke det - men nesten!

checksum_start = sum(filter(temp, nchar(Region) == 4)$Value, na.rm = TRUE)
test = semi_join(filter(temp, nchar(Region) == 4), historiske_kommunekoder_gyldig, by = c("Region" = "historisk_nummer", "Tid" = "gyldig_år"))
checksum_end = sum(test$Value, na.rm = TRUE)
nrow(filter(test, Value == 0))

temp = filter(temp, Value != 0)

#5 OMKODING DEL 1 
#hvilke koder finnes i datasettet, som ikke finnes i mal-listene?

test = distinct(temp, Region) %>%
  anti_join(., metadata_geografi_alle, by = c("Region" = "code"))

##ssb_07459 og ssb_10826 har avvikende bydelskoding - 030101a fra og med 2004. Utdanningstabellene også. Lavinntekt også. 
#Fra 1. januar 2004 fikk kommunene Oslo og Trondheim ny bydelsinndeling.

temp = mutate(temp, Region = gsub("a", "", Region))

#her bør det ikke være noen duplikater? 
test = get_dupes(temp, Region, Tid)

#6 OMKODING DEL 2 - KOMMUNEMAPPING (FOR SUMMERBARE VARIABLER)

#HVIS ANTALL
temp_omkodet = rename(historiske_kommunekoder_gyldig, Tid = gyldig_år) %>%
  group_by(kommunenummer_2020, Tid) %>%
  mutate(endring = ifelse(kommunenummer_2020 != historisk_nummer, TRUE, FALSE),
         sammenslåtte = n(),
         endring = ifelse(sammenslåtte > 1, TRUE, endring)
  ) %>%
  filter(endring == TRUE) %>% #jeg vil bare ha kommuner som er sammenslåtte eller har endra nummer - (splitta er endra nr)
  left_join(., temp, by = c("historisk_nummer" = "Region", "Tid")) %>%
  group_by(kommunenummer_2020, Tid) %>%
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
#  ) %>%
#  filter(endring == TRUE) %>% #jeg vil bare ha kommuner har endra nummer - (splitta er endra nr)
#  left_join(., temp, by = c("historisk_nummer" = "Region", "Tid")) %>%
#  mutate( 
#    Value = ifelse(historisk_nummer %in% c("1613", "5012", "1850"), NA, Value) #Setter Snillfjord og Tysfjord til missing for summeringsformål
#  ) %>%
#  select(Region = 1, Tid, Value)


#og legger til som nye rader 
#hvis de er unike for den kombinasjonen av kode og år - sjekk?

test = bind_rows(temp, temp_omkodet) %>%
  janitor::get_dupes(., Region, Tid)

#fjerner duplikatene fra temp-fila
#er dette tilrådelig?
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

#8 OMKODING - BYDELSMAPPING
#110308 og 110309 er nye bydeler i 2020 uten historisk informasjon - vi mapper ikke fra kommune til bydel
#4601-bydelene til Bergen kan mappes, det samme kan Trondheimsbydeler fra 1601 til 5001
#her er det kun kodebytte, ikke noe annet

temp_bydel = left_join(historiske_bydelsnr, temp, by = c("historisk_nummer" = "Region")) %>%
  select(Region = bydelsnr, Tid, Value)

temp = bind_rows(temp, temp_bydel)

test = janitor::get_dupes(temp, Region, Tid)

#9 AGGREGERING AV MANGLENDE NIVÅER
#hvilke koder finnes i mal-listene, som ikke finnes i datasettet

test = distinct(temp, Region,  .keep_all = TRUE) %>%
  left_join(metadata_episerver, ., by = c("code" = "Region")) %>%
  filter(is.na(Value))
  
#visuell inspeksjon viser at enhetne som ikke finnes i datasettet er IKS og Longyearbyen.
#Longyearbyen veit jeg  ikke hva vi gjør med her - 
#mener å huske at folk ikke folkeregistreres på Svalbard, men forblir folkeregistrert i fraflyttingskommunen?

#Andel kan ikke aggregeres

#HVIS ANTALL
#aggregerer IKS
temp_iks = left_join(metadata_iks, temp, by = c("Kommunenummer" = "Region")) %>%
  group_by(Samarbeidskommunenr, Tid) %>%
  summarise(Value = sum(Value)) %>% #hvis NA på en av verdiene, så blir det NA på hele - men ingen NA her.
  rename(Region = 1)

#binder disse inn i temp-datasettet
temp = bind_rows(temp, temp_iks)

test = janitor::get_dupes(temp, Region, Tid)

#10 PRIKKING
#Skal vi prikke noe i grunnlagsdataene her?
#11 ANDEL
#Ikke relevant for denne tabellen 1

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
              Variabel = "v56_antall_barn_totalt_fralavinntektstatistikken",
              Tallformat = "Antall"
)

#sjekk av at det er riktig antall kolonner her
#5 for de aller fleste, 6 for KHR.
ncol(temp) == 5 


#skriver ut til ferdig bearbeida data
write.csv2(temp, "data_bearbeida/v56_antall_barn_totalt_fralavinntektstatistikken.csv", row.names = FALSE)

#rydder opp tabeller som ikke trenger å være i minnet lengre
rm(historiske_bydelsnr, historiske_fylkesnr, historiske_kommunekoder,
   historiske_kommunekoder_gyldig, manglende_observasjoner, metadata_episerver,
   metadata_geografi_alle, metadata_iks, tabell_56,
   temp, temp_bydel, temp_fylke, temp_iks, temp_omkodet, test,
   checksum_end, checksum_start)