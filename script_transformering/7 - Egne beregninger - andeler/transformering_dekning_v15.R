#SCript for transformering av økonomi-data til kommunemonitor barnevern
#15	Andel av fosterhjemstiltak i familie og nære nettverk av alle fosterhjemstiltak	
#Beregnes ved å dele 13 på summen av 13 og 14

#ANDEL - EGENBEREGNET PÅ BAKGRUNN AV KOSTRADATA + SSB-LEVERANSEDATA

#opprettet 25. februar 2021
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

#BAKGRUNNSDATA
metadata_episerver <- read_delim("metadata_geografi/metadata_geografi_episerver.csv", ";", escape_double = FALSE, 
                                 locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)

#11 ANDEL
#OBS! FOR ANDELSVARIABLENE MÅ TELLER OG NEVNER LASTES INN OG OPPDATERES FØR ANDELSVARIABELEN OPPDATERES.

#v3: v2_barn_tiltak_ilaaret_ialt_0_17 / v1_antallbarn_0_17
#V5: v4_barn_undersokelse / v1_antallbarn_0_17
#v7: v6_barn_tiltak_ilaaret_252_0_17 / v1_antallbarn_0_17
#v9: v8_barn_tiltak_ilaaret_251_0_17 / v2_barn_tiltak_ilaaret_ialt_0_17
#v12: v10_barn_undersokelse_tiltak_ilaaret_0_17 / 
#v15: 
#v19: v16_netto_driftsutgifter_1000kr_totalt_barnevern / v18_barn_022_undersokelse_tiltak_ilaaar
#v22: v21_brutto_driftsutgifter_252_1000kr / v20_barn_022_252_ilaaar
#v24: v23_netto_driftsutgifter_244_1000kr / v16_netto_driftsutgifter_1000kr_totalt_barnevern (format: XX prosent)
#v26
#v29: v28_brutto_driftsutgifter_251_1000kr / v27_barn_022_251_ilaaar

#HVIS RELEVANT
temp_tabell_13 <- read_delim("data_bearbeida/v13_fosterheim_familie_nare_nettverk.csv", ";", escape_double = FALSE, 
                                col_types = cols(Region = col_character(),
                                                 Tid = col_character(),
                                                 Value = col_number(),
                                                 Variabel = col_skip(),
                                                 Tallformat = col_skip()
                                ),
                             locale = locale(decimal_mark = ",", grouping_mark = " "), 
                             na = "..",  
                                trim_ws = TRUE) %>%
  rename(v13_fosterheim_familie_nare_nettverk = Value)

temp_tabell_14 <- read_delim("data_bearbeida/v14_fosterheim_utenom_familie_nare_nettverk.csv", ";", escape_double = FALSE, 
                                 col_types = cols(Region = col_character(),
                                                  Tid = col_character(),
                                                  Value = col_number(),
                                                  Variabel = col_skip(),
                                                  Tallformat = col_skip()
                                 ),
                             locale = locale(decimal_mark = ",", grouping_mark = " "), 
                             na = "..",  
                                 trim_ws = TRUE) %>%
  rename(v14_fosterheim_utenom_familie_nare_nettverk = Value)

#gjennom å bruke full_join her får jeg med alle region-tid-kombinasjoner i teller og nevner som ikke er i denne fila
#det vil blant annet si IKS, men kan også bety andre ting, f.eks. ved mismatch i lengden på tidsserien.

temp = full_join(temp_tabell_13, temp_tabell_14, by = c("Region", "Tid"))

#hvis variabelen er missing, så forsøk å beregne den 
#hvis ikke missing, behold den opprinnelige verdien
#NB! Husk samme format på tallet vi beregner her som det som allerede er her 
#TENK PÅ: Er det mest hensiktsmessig å beregne for alle NA-verdier? eller for alle IKS-verdier + kommuner og fylker som er sammenslått?

temp = mutate(temp,
                    Value =
                      round((v13_fosterheim_familie_nare_nettverk / 
                               (v13_fosterheim_familie_nare_nettverk + v14_fosterheim_utenom_familie_nare_nettverk))*100, 1)
                      ) %>%
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
#test = filter(temp, is.na(Tid))

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
temp = mutate(temp, Variabel = "1G_andel_fosterheim_familie_nare_nettverk")

#sjekk av at det er riktig antall kolonner her
#5 for de aller fleste, 6 for KHR.
ncol(temp) == 5

#skriver ut til ferdig bearbeida data
write.csv2(temp, "data_til_pivotering/1G_andel_fosterheim_familie_nare_nettverk.csv", row.names = FALSE)
write.csv2(temp, "data_bearbeida/v15_andel_fosterheim_familie_nare_nettverk.csv", row.names = FALSE)

#rydder opp tabeller som ikke trenger å være i minnet lengre
rm(temp, temp_tabell_13, temp_tabell_14, test)
