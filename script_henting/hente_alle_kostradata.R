#HENTESCRIPT FOR ALLE DATA FRA KOSTRA

#OPPRETTET 5. mars 2021

#FORMÅL
#Siden KOSTRA-dataene oppdateres samtidig hos SSB, er det smart å ha ett script som sjekker og henter dette.
#Tid = TRUE for alle, dvs. at all tid lastes ned.

#FORBEDRINGSPUNKTER
#logging av hva som skjer
#parametrisering av evt. felles parametre - som Tid og Region, som er satt til det samme for alle.
#sjekk av om dataene er nye, nå dumpes de bare i samme folder som de gamle dataene og overskriver dem uten noen kontroll.

#funksjon for den mellomtrinnsfunksjonen.
#noe a la deparse(substitue(objekt)) ser ut til å kunne konvertere objektnavn til streng.

#BIBLIOTEKER
library(httr)
library(jsonlite)
library(PxWebApiData)
library(tidyverse)

#PARAMETRE
waiting_time = 2 #ventetid i sekunder mellom hver spørring - SSB har begrensning på 30 spørringer på 60 sekunder, og her er det ca. 70

#lager en tabell for å kunne sjekke at vi har fått med alt
all_queries = data.frame(
  tabell = c("tabell_2_kommune", "tabell_2_bydel", "tabell_3_kommune", "tabell_3_bydel", 
             "tabell_4_kommune", "tabell_4_bydel", "tabell_5_kommune", "tabell_5_bydel", 
             "tabell_6_kommune", "tabell_6_bydel", "tabell_7_kommune", "tabell_7_bydel",
             "tabell_8_kommune", "tabell_8_bydel", "tabell_10_kommune", "tabell_10_bydel",
             "tabell_11_kommune", "tabell_11_bydel", "tabell_13_fylker", "tabell_14_fylker",
             "tabell_16_kommune", "tabell_16_bydel", "tabell_17_kommune", "tabell_17_bydel",
             "tabell_18_kommune", "tabell_18_bydel", "tabell_19_kommune", "tabell_19_bydel",
             "tabell_20_kommune", "tabell_20_bydel", "tabell_21_kommune", "tabell_21_bydel",
             "tabell_22_kommune", "tabell_22_bydel", "tabell_23_kommune", "tabell_23_bydel",
             "tabell_24_kommune", "tabell_24_bydel", "tabell_25_kommune", "tabell_25_bydel",
             "tabell_27_kommune", "tabell_27_bydel", "tabell_28_kommune", "tabell_28_bydel",
             "tabell_29_kommune", "tabell_29_bydel", "tabell_34_kommune", "tabell_34_bydel", 
             "tabell_35_kommune", "tabell_35_bydel", "tabell_53_kommune", "tabell_53_bydel",
             "tabell_58_kommune", "tabell_58_bydel", "tabell_59_kommune", "tabell_59_bydel"
  )
)

temp_logg = data.frame(
  tabell = as.character(),
  antall_observasjoner = as.numeric(),
  siste_år = as.numeric()
)

#2	Antall barn 0-17 år med tiltak i alt i løpet av året	https://www.ssb.no/statbank/table/12604/ (B), https://www.ssb.no/statbank/table/12275/ (K)

#kommuner
#12275: Barn med tiltak i løpet av året og per 31. desember, etter region, alder, funksjon, statistikkvariabel og år
#statbank: https://www.ssb.no/statbank/table/12275/tableViewLayout1/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12275/

tabell_2_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12275",
  KOKkommuneregion0000 = TRUE,
  KOKalder0000 = "F000-017",
  KOKbvfunksjon0000 = "0",
  ContentsCode = "KOSbvbarntiltaka0000",
  Tid = TRUE
)

tabell_2_kommune = tabell_2_kommune$dataset
write.csv2(tabell_2_kommune, "datauttrekk_raw/tabell_2_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_2_kommune",
  antall_observasjoner = nrow(tabell_2_kommune),
  siste_år = max(parse_number(tabell_2_kommune$Tid))
))
rm(tabell_2_kommune)
Sys.sleep(waiting_time)

#bydeler
#12604: Barn med tiltak i løpet av året og per 31. desember etter funksjon, bydel, etter alder og funksjon (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12604/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12604/

tabell_2_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12604",
  KOKbydelsregion0000 = TRUE,
  KOKalder0000 = "F000-017",
  KOKbvfunksjon0000 = "0",
  ContentsCode = "KOSbvbarntiltaka0000",
  Tid = TRUE
)

tabell_2_bydel = tabell_2_bydel$dataset
write.csv2(tabell_2_bydel, "datauttrekk_raw/tabell_2_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_2_bydel",
  antall_observasjoner = nrow(tabell_2_bydel),
  siste_år = max(parse_number(tabell_2_bydel$Tid))
))
rm(tabell_2_bydel)
Sys.sleep(waiting_time)



#3	Barn med barnevernstiltak i forhold til innbyggere i aldersgruppen 0-17 år https://www.ssb.no/statbank/table/12604/ (B), https://www.ssb.no/statbank/table/12275/ (K)

#kommuner
#12275: Barn med tiltak i løpet av året og per 31. desember, etter region, alder, funksjon, statistikkvariabel og år
#statbank: https://www.ssb.no/statbank/table/12275/tableViewLayout1/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12275/

tabell_3_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12275",
  KOKkommuneregion0000 = TRUE,
  KOKalder0000 = "F000-017",
  KOKbvfunksjon0000 = "0",
  ContentsCode = "KOSbvbarntiltaka0001",
  Tid = TRUE
)

tabell_3_kommune = tabell_3_kommune$dataset
write.csv2(tabell_3_kommune, "datauttrekk_raw/tabell_3_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_3_kommune",
  antall_observasjoner = nrow(tabell_3_kommune),
  siste_år = max(parse_number(tabell_3_kommune$Tid))
))
rm(tabell_3_kommune)
Sys.sleep(waiting_time)

#bydeler
#12604: Barn med tiltak i løpet av året og per 31. desember etter funksjon, bydel, etter alder og funksjon (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12604/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12604/

tabell_3_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12604",
  KOKbydelsregion0000 = TRUE,
  KOKalder0000 = "F000-017",
  KOKbvfunksjon0000 = "0",
  ContentsCode = "KOSbvbarntiltaka0001",
  Tid = TRUE
)

tabell_3_bydel = tabell_3_bydel$dataset
write.csv2(tabell_3_bydel, "datauttrekk_raw/tabell_3_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_3_bydel",
  antall_observasjoner = nrow(tabell_3_bydel),
  siste_år = max(parse_number(tabell_3_bydel$Tid))
))
rm(tabell_3_bydel)
Sys.sleep(waiting_time)

#4 Antall barn 0-17 år med undersøkelse	https://www.ssb.no/statbank/table/12490/ (B),  https://www.ssb.no/statbank/table/12286/

#kommuner
#12286: Barn med undersøking og prosentdelen barn med undersøking frå barnevernet (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12286/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12286/

tabell_4_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12286",
  KOKkommuneregion0000 = TRUE,
  ContentsCode = "KOSbvbarnus0000",
  Tid = TRUE
)

tabell_4_kommune = tabell_4_kommune$dataset
write.csv2(tabell_4_kommune, "datauttrekk_raw/tabell_4_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_4_kommune",
  antall_observasjoner = nrow(tabell_4_kommune),
  siste_år = max(parse_number(tabell_4_kommune$Tid))
))
rm(tabell_4_kommune)
Sys.sleep(waiting_time)



#bydeler
#12490: Barn med undersøking og prosentdelen barn med undersøking frå barnevernet, bydel (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12490/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12490/

tabell_4_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12490",
  KOKbydelsregion0000 = TRUE,
  ContentsCode = "KOSbvbarnus0000",
  Tid = TRUE
)

tabell_4_bydel = tabell_4_bydel$dataset
write.csv2(tabell_4_bydel, "datauttrekk_raw/tabell_4_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_4_bydel",
  antall_observasjoner = nrow(tabell_4_bydel),
  siste_år = max(parse_number(tabell_4_bydel$Tid))
))
rm(tabell_4_bydel)
Sys.sleep(waiting_time)



#5 Barn med undersøkelse i forhold til antall innbyggere i aldersgruppen 0-17 år 	https://www.ssb.no/statbank/table/12490/ (B),  https://www.ssb.no/statbank/table/12286/ Beregnes fra nr. 4 og nr. 1

#kommuner
#12286: Barn med undersøking og prosentdelen barn med undersøking frå barnevernet (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12286/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12286/

tabell_5_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12286",
  KOKkommuneregion0000 = TRUE,
  ContentsCode = "KOSbvbarnuspinnb0000",
  Tid = TRUE
)

tabell_5_kommune = tabell_5_kommune$dataset
write.csv2(tabell_5_kommune, "datauttrekk_raw/tabell_5_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_5_kommune",
  antall_observasjoner = nrow(tabell_5_kommune),
  siste_år = max(parse_number(tabell_5_kommune$Tid))
))
rm(tabell_5_kommune)
Sys.sleep(waiting_time)


#bydeler
#12490: Barn med undersøking og prosentdelen barn med undersøking frå barnevernet, bydel (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12490/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12490/

tabell_5_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12490",
  KOKbydelsregion0000 = TRUE,
  ContentsCode = "KOSbvbarnuspinnb0000",
  Tid = TRUE
)

tabell_5_bydel = tabell_5_bydel$dataset
write.csv2(tabell_5_bydel, "datauttrekk_raw/tabell_5_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_5_bydel",
  antall_observasjoner = nrow(tabell_5_bydel),
  siste_år = max(parse_number(tabell_5_bydel$Tid))
))
rm(tabell_5_bydel)
Sys.sleep(waiting_time)

#1C Barn som bor utenfor hjemmet i løpet av året i forhold til antall innbyggere i aldersgruppen 0-17 år
#6	Antall barn 0-17 år med tiltak utenfor hjemmet i løpet av året 	https://www.ssb.no/statbank/table/12604/ (B), https://www.ssb.no/statbank/table/12275/ (K)
#7	Barn som bor utenfor hjemmet i løpet av året i forhold til antall innbyggere i aldersgruppen 0-17 år	https://www.ssb.no/statbank/table/12604/ (B), https://www.ssb.no/statbank/table/12275/ (K) Beregnes med nr. 6 og nr. 1.

#6	Antall barn 0-17 år med tiltak utenfor hjemmet i løpet av året 	https://www.ssb.no/statbank/table/12604/ (B), https://www.ssb.no/statbank/table/12275/ (K)
#barn som bor utenfor hjemmet / barn plassert av barnevernet / barn på funksjon 252

#kommuner
#12275: Barn med tiltak i løpet av året og per 31. desember, etter region, alder, funksjon, statistikkvariabel og år
#statbank: https://www.ssb.no/statbank/table/12275/tableViewLayout1/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12275/
#KOKbvfunksjon0000: 0:  "Tiltak i alt", 1:  "251 - Barnevernstiltak når barnet ikke er plassert av barnevernet", 2: "252 - Barnevernstiltak når barnet er plassert av barnevernet"

tabell_6_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12275",
  KOKkommuneregion0000 = TRUE,
  KOKalder0000 = "F000-017",
  KOKbvfunksjon0000 = "2",
  ContentsCode = "KOSbvbarntiltaka0000",
  Tid = TRUE
)

tabell_6_kommune = tabell_6_kommune$dataset
write.csv2(tabell_6_kommune, "datauttrekk_raw/tabell_6_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_6_kommune",
  antall_observasjoner = nrow(tabell_6_kommune),
  siste_år = max(parse_number(tabell_6_kommune$Tid))
))
rm(tabell_6_kommune)
Sys.sleep(waiting_time)

#bydeler
#12604: Barn med tiltak i løpet av året og per 31. desember etter funksjon, bydel, etter alder og funksjon (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12604/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12604/

tabell_6_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12604",
  KOKbydelsregion0000 = TRUE,
  KOKalder0000 = "F000-017",
  KOKbvfunksjon0000 = "2",
  ContentsCode = "KOSbvbarntiltaka0000",
  Tid = TRUE
)

tabell_6_bydel = tabell_6_bydel$dataset
write.csv2(tabell_6_bydel, "datauttrekk_raw/tabell_6_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_6_bydel",
  antall_observasjoner = nrow(tabell_6_bydel),
  siste_år = max(parse_number(tabell_6_bydel$Tid))
))
rm(tabell_6_bydel)
Sys.sleep(waiting_time)

#7	Barn som bor utenfor hjemmet i løpet av året i forhold til antall innbyggere i aldersgruppen 0-17 år	https://www.ssb.no/statbank/table/12604/ (B), https://www.ssb.no/statbank/table/12275/ (K) Beregnes med nr. 6 og nr. 1.

#kommuner
#12275: Barn med tiltak i løpet av året og per 31. desember, etter region, alder, funksjon, statistikkvariabel og år
#statbank: https://www.ssb.no/statbank/table/12275/tableViewLayout1/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12275/
#KOKbvfunksjon0000: 0:  "Tiltak i alt", 1:  "251 - Barnevernstiltak når barnet ikke er plassert av barnevernet", 2: "252 - Barnevernstiltak når barnet er plassert av barnevernet"

tabell_7_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12275",
  KOKkommuneregion0000 = TRUE,
  KOKalder0000 = "F000-017",
  KOKbvfunksjon0000 = "2",
  ContentsCode = "KOSbvbarntiltaka0001",
  Tid = TRUE
)

tabell_7_kommune = tabell_7_kommune$dataset
write.csv2(tabell_7_kommune, "datauttrekk_raw/tabell_7_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_7_kommune",
  antall_observasjoner = nrow(tabell_7_kommune),
  siste_år = max(parse_number(tabell_7_kommune$Tid))
))
rm(tabell_7_kommune)
Sys.sleep(waiting_time)

#bydeler
#12604: Barn med tiltak i løpet av året og per 31. desember etter funksjon, bydel, etter alder og funksjon (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12604/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12604/

tabell_7_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12604",
  KOKbydelsregion0000 = TRUE,
  KOKalder0000 = "F000-017",
  KOKbvfunksjon0000 = "2",
  ContentsCode = "KOSbvbarntiltaka0001",
  Tid = TRUE
)

tabell_7_bydel = tabell_7_bydel$dataset
write.csv2(tabell_7_bydel, "datauttrekk_raw/tabell_7_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_7_bydel",
  antall_observasjoner = nrow(tabell_7_bydel),
  siste_år = max(parse_number(tabell_7_bydel$Tid))
))
rm(tabell_7_bydel)
Sys.sleep(waiting_time)

#1E Andel barn med hjelpetiltak i hjemmet av alle barn med tiltak 0-17 år 
#8 Antall barn 0-17 år med hjelpetiltak i hjemmet i alt i løpet av året	https://www.ssb.no/statbank/table/12604/ (B),  https://www.ssb.no/statbank/table/12275/ (K)
#9 Andel barn med hjelpetiltak i hjemmet av alle barn med tiltak 0-17 år	Beregnes med nr. 8 og nr. 2.

#kommuner
#12275: Barn med tiltak i løpet av året og per 31. desember, etter region, alder, funksjon, statistikkvariabel og år
#statbank: https://www.ssb.no/statbank/table/12275/tableViewLayout1/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12275/
#KOKbvfunksjon0000: 0:  "Tiltak i alt", 1:  "251 - Barnevernstiltak når barnet ikke er plassert av barnevernet", 2: "252 - Barnevernstiltak når barnet er plassert av barnevernet"

tabell_8_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12275",
  KOKkommuneregion0000 = TRUE,
  KOKalder0000 = "F000-017",
  KOKbvfunksjon0000 = "1",
  ContentsCode = "KOSbvbarntiltaka0000",
  Tid = TRUE
)

tabell_8_kommune = tabell_8_kommune$dataset
write.csv2(tabell_8_kommune, "datauttrekk_raw/tabell_8_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_8_kommune",
  antall_observasjoner = nrow(tabell_8_kommune),
  siste_år = max(parse_number(tabell_8_kommune$Tid))
))
rm(tabell_8_kommune)
Sys.sleep(waiting_time)

#bydeler
#12604: Barn med tiltak i løpet av året og per 31. desember etter funksjon, bydel, etter alder og funksjon (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12604/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12604/

tabell_8_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12604",
  KOKbydelsregion0000 = TRUE,
  KOKalder0000 = "F000-017",
  KOKbvfunksjon0000 = "1",
  ContentsCode = "KOSbvbarntiltaka0000",
  Tid = TRUE
)

tabell_8_bydel = tabell_8_bydel$dataset
write.csv2(tabell_8_bydel, "datauttrekk_raw/tabell_8_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_8_bydel",
  antall_observasjoner = nrow(tabell_8_bydel),
  siste_år = max(parse_number(tabell_8_bydel$Tid))
))
rm(tabell_8_bydel)
Sys.sleep(waiting_time)

#10	Antall barn 0-17 med undersøkelser eller tiltak i løpet av året	https://www.ssb.no/statbank/table/12904/ (B), https://www.ssb.no/statbank/table/12870/ (K)

#kommuner
#12870: Barn med undersøking eller tiltak i løpet av året, etter alder (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12870/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12870/

tabell_10_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12870",
  KOKkommuneregion0000 = TRUE,
  KOKalder0000 = "F000-017",
  ContentsCode = "KOSbvbarnust0000",
  Tid = TRUE
)

tabell_10_kommune = tabell_10_kommune$dataset
write.csv2(tabell_10_kommune, "datauttrekk_raw/tabell_10_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_10_kommune",
  antall_observasjoner = nrow(tabell_10_kommune),
  siste_år = max(parse_number(tabell_10_kommune$Tid))
))
rm(tabell_10_kommune)
Sys.sleep(waiting_time)

#bydeler
#12904: Barn med undersøking eller tiltak i løpet av året, bydel, etter alder (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12904/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12904/

tabell_10_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12904",
  KOKbydelsregion0000 = TRUE,
  KOKalder0000 = "F000-017",
  ContentsCode = "KOSbvbarnust0000",
  Tid = TRUE
)

tabell_10_bydel = tabell_10_bydel$dataset
write.csv2(tabell_10_bydel, "datauttrekk_raw/tabell_10_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_10_bydel",
  antall_observasjoner = nrow(tabell_10_bydel),
  siste_år = max(parse_number(tabell_10_bydel$Tid))
))
rm(tabell_10_bydel)
Sys.sleep(waiting_time)

#11	Antall årsverk til saksbehandling (244)	https://www.ssb.no/statbank/table/12605/ (B), https://www.ssb.no/statbank/table/12305/ (K) 

#kommuner
#12305: Stillingar i barnevernstenesta etter fagutdanning og funksjon (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12305/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12305/

tabell_11_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12305",
  KOKkommuneregion0000 = TRUE,
  ContentsCode = "KOSsumfunksjon240000",
  Tid = TRUE
)

tabell_11_kommune = tabell_11_kommune$dataset
write.csv2(tabell_11_kommune, "datauttrekk_raw/tabell_11_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_11_kommune",
  antall_observasjoner = nrow(tabell_11_kommune),
  siste_år = max(parse_number(tabell_11_kommune$Tid))
))
rm(tabell_11_kommune)
Sys.sleep(waiting_time)

#bydeler
#12605: Stillingar i barnevernstenesta, bydel (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12605/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12605/

tabell_11_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12605",
  KOKbydelsregion0000 = TRUE,
  ContentsCode = "KOSsumfunksjon240000",
  Tid = TRUE
)

tabell_11_bydel = tabell_11_bydel$dataset
write.csv2(tabell_11_bydel, "datauttrekk_raw/tabell_11_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_11_bydel",
  antall_observasjoner = nrow(tabell_11_bydel),
  siste_år = max(parse_number(tabell_11_bydel$Tid))
))
rm(tabell_11_bydel)
Sys.sleep(waiting_time)

#1G: Andel av fosterhjemstiltak i familie og nære nettverk, fylker og landet
#13	Antall fosterhjemstiltak i familie og nære nettverk	https://www.ssb.no/statbank/table/10660/ (F, L), spesialbestilling fra SSB for K
#14	Antall fosterhjemstiltak utenom familie og nære nettverk	https://www.ssb.no/statbank/table/10660/ (F, L), spesialbestilling fra SSB for K
#15	Andel av fosterhjemstiltak i familie og nære nettverk	Beregnes ved å dele 13 på summen av 13 og 14

#MULIG DENNE IKKE OPPDATERES SAMMEN MED ØVRIGE KOSTRA-TALL, MEN HELLER ER EN DEL AV ØVRIG BARNEVERNSSTATISTIKK

#fylker og hele landet.
#10660: Barnevernstiltak i løpet av året, per 31. desember og for nye barn, etter tiltak (F) 2013 - 2019
#statbank: https://www.ssb.no/statbank/table/10660/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/10660/

tabell_13_fylker = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/10660",
  Region = TRUE,
  Barnevernstiltak = "2.01",
  ContentsCode = "TiltakGjennomAar",
  Tid = TRUE
)

tabell_13_fylker = tabell_13_fylker$dataset
write.csv2(tabell_13_fylker, "datauttrekk_raw/tabell_13_fylker.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_13_fylker",
  antall_observasjoner = nrow(tabell_13_fylker),
  siste_år = max(parse_number(tabell_13_fylker$Tid))
))
rm(tabell_13_fylker)
Sys.sleep(waiting_time)

#14	Antall fosterhjemstiltak utenom familie og nære nettverk	https://www.ssb.no/statbank/table/10660/ (F, L), spesialbestilling fra SSB for K

tabell_14_fylker = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/10660",
  Region = TRUE,
  Barnevernstiltak = "2.02",
  ContentsCode = "TiltakGjennomAar",
  Tid = TRUE
)

tabell_14_fylker = tabell_14_fylker$dataset
write.csv2(tabell_14_fylker, "datauttrekk_raw/tabell_14_fylker.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_14_fylker",
  antall_observasjoner = nrow(tabell_14_fylker),
  siste_år = max(parse_number(tabell_14_fylker$Tid))
))
rm(tabell_14_fylker)
Sys.sleep(waiting_time)

#KOMMUNEDATA 13 OG 14
#kommunedataene for både tabell 13 og 14 er i egne spesialleveranser fra SSB
#egen fil for historiske data: "data_manuelle_leveranser/tabell_13_14_kommuner_2013_2018.csv"
#eksempelfil: "data_manuelle_leveranser/tabell_13_14_eksempel dataleveranse fra SSB - fosterhjem i slekt og nettverk 2019.csv"
#for transformeringsforslag, se all_queries

# 16	Netto driftsutgifter til barnevern totalt (1000 kr)	https://www.ssb.no/statbank/table/12369/ (B) , https://www.ssb.no/statbank/table/12362/ (K)
#kommuner
#12362: Utgifter til tjenesteområdene, etter funksjon og art (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12362/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12362/

tabell_16_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12362",
  KOKkommuneregion0000 = TRUE,
  KOKfunksjon0000 = "FGK13",
  KOKart0000 = "AGD2",
  ContentsCode = "KOSbelop0000",
  Tid = TRUE
)


tabell_16_kommune = tabell_16_kommune$dataset
write.csv2(tabell_16_kommune, "datauttrekk_raw/tabell_16_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_16_kommune",
  antall_observasjoner = nrow(tabell_16_kommune),
  siste_år = max(parse_number(tabell_16_kommune$Tid))
))
rm(tabell_16_kommune)
Sys.sleep(waiting_time)

#bydeler
#12369: Detaljerte regnskapstall driftsregnskapet B, etter funksjon og art (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12369/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12369/

tabell_16_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12369",
  KOKbydelsregion0000 = TRUE,
  KOKfunksjon0000 = c("244", "251", "252"),
  KOKart0000 = "AGB4",
  ContentsCode = "KOSbelop0000",
  Tid = TRUE
)

tabell_16_bydel = tabell_16_bydel$dataset
write.csv2(tabell_16_bydel, "datauttrekk_raw/tabell_16_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_16_bydel",
  antall_observasjoner = nrow(tabell_16_bydel),
  siste_år = max(parse_number(tabell_16_bydel$Tid))
))
rm(tabell_16_bydel)
Sys.sleep(waiting_time)

#17	Netto driftsutgifter til barnevern pr. barn i befolkningen 0-17 år	Beregnes med nr. 16 delt på nr. 1 eller hentes fra https://www.ssb.no/statbank/table/12279/  (K) og https://www.ssb.no/statbank/table/12601/ (B)
#kommuner
#12279: Nøkkeltal utgifter barnevern (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12279/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12362/

tabell_17_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12279",
  KOKkommuneregion0000 = TRUE,
  ContentsCode = "KOSndubvinnbygge0000",
  Tid = TRUE
)

tabell_17_kommune = tabell_17_kommune$dataset
write.csv2(tabell_17_kommune, "datauttrekk_raw/tabell_17_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_17_kommune",
  antall_observasjoner = nrow(tabell_17_kommune),
  siste_år = max(parse_number(tabell_17_kommune$Tid))
))
rm(tabell_17_kommune)
Sys.sleep(waiting_time)

#bydeler
#12601: Nøkkeltal utgifter barnevern, bydel (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12601
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12601/

tabell_17_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12601",
  KOKbydelsregion0000 = TRUE,
  ContentsCode = "KOSndubvinnbygge0000",
  Tid = TRUE
)

tabell_17_bydel = tabell_17_bydel$dataset
write.csv2(tabell_17_bydel, "datauttrekk_raw/tabell_17_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_17_bydel",
  antall_observasjoner = nrow(tabell_17_bydel),
  siste_år = max(parse_number(tabell_17_bydel$Tid))
))
rm(tabell_17_bydel)
Sys.sleep(waiting_time)

#2B Utgifter pr. barn i barnevernet (2019)
# 18	Antall barn 0-22  med undersøkelser eller tiltak i løpet av året	https://www.ssb.no/statbank/table/12904/ (B), https://www.ssb.no/statbank/table/12870/ (K)
# 19	Netto driftsutgifter til barnevern pr. barn i barnevernet	 hentes fra https://www.ssb.no/statbank/table/12279/  (K) og https://www.ssb.no/statbank/table/12601/ (B)

#kommuner
#12870: Barn med undersøking eller tiltak i løpet av året, etter alder (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12870/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12870/

tabell_18_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12870",
  KOKkommuneregion0000 = TRUE,
  KOKalder0000 = "F000-022",
  ContentsCode = "KOSbvbarnust0000",
  Tid = TRUE
)

tabell_18_kommune = tabell_18_kommune$dataset
write.csv2(tabell_18_kommune, "datauttrekk_raw/tabell_18_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_18_kommune",
  antall_observasjoner = nrow(tabell_18_kommune),
  siste_år = max(parse_number(tabell_18_kommune$Tid))
))
rm(tabell_18_kommune)
Sys.sleep(waiting_time)

#bydeler
#12904: Barn med undersøking eller tiltak i løpet av året, bydel, etter alder (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12904/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12904/

tabell_18_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12904",
  KOKbydelsregion0000 = TRUE,
  KOKalder0000 = "F000-022",
  ContentsCode = "KOSbvbarnust0000",
  Tid = TRUE
)

tabell_18_bydel = tabell_18_bydel$dataset
write.csv2(tabell_18_bydel, "datauttrekk_raw/tabell_18_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_18_bydel",
  antall_observasjoner = nrow(tabell_18_bydel),
  siste_år = max(parse_number(tabell_18_bydel$Tid))
))
rm(tabell_18_bydel)
Sys.sleep(waiting_time)

# 19	Netto driftsutgifter til barnevern pr. barn i barnevernet	
# hentes fra https://www.ssb.no/statbank/table/12279/  (K) og https://www.ssb.no/statbank/table/12601/ (B)

#kommuner
#12279: Nøkkeltal utgifter barnevern (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12279/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12362/

tabell_19_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12279",
  KOKkommuneregion0000 = TRUE,
  ContentsCode = "KOSndubvbvbarnus0000",
  Tid = TRUE
)

tabell_19_kommune = tabell_19_kommune$dataset
write.csv2(tabell_19_kommune, "datauttrekk_raw/tabell_19_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_19_kommune",
  antall_observasjoner = nrow(tabell_19_kommune),
  siste_år = max(parse_number(tabell_19_kommune$Tid))
))
rm(tabell_19_kommune)
Sys.sleep(waiting_time)

#bydeler
#12601: Nøkkeltal utgifter barnevern, bydel (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12601
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12369/

tabell_19_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12601",
  KOKbydelsregion0000 = TRUE,
  ContentsCode = "KOSndubvbvbarnus0000",
  Tid = TRUE
)

tabell_19_bydel = tabell_19_bydel$dataset
write.csv2(tabell_19_bydel, "datauttrekk_raw/tabell_19_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_19_bydel",
  antall_observasjoner = nrow(tabell_19_bydel),
  siste_år = max(parse_number(tabell_19_bydel$Tid))
))
rm(tabell_19_bydel)
Sys.sleep(waiting_time)

# 2C Utgifter pr. barn som bor utenfor hjemmet (2019)

# 20	Antall barn 0-22 år som er plassert av barnevernet (252)  	https://www.ssb.no/statbank/table/12604/ (B) og https://www.ssb.no/statbank/table/12275/  (K)
# 21	Brutto driftsutgifter til barnevern funksjon 252 (1000 kr)	https://www.ssb.no/statbank/table/12369/ (B) og https://www.ssb.no/statbank/table/12362/ (K)
# 22	Utgifter pr. barn som bor utenfor hjemmet 	Beregnes med å dele nr. 21 på 20, og hentes fra hentes fra https://www.ssb.no/statbank/table/12279/  (K) og https://www.ssb.no/statbank/table/12601/ (B)


# 20	Antall barn 0-22 år som er plassert av barnevernet (252)  	https://www.ssb.no/statbank/table/12604/ (B) og https://www.ssb.no/statbank/table/12275/  (K)
#kommuner
#12275: Barn med tiltak i løpet av året og per 31. desember, etter region, alder, funksjon, statistikkvariabel og år
#statbank: https://www.ssb.no/statbank/table/12275/tableViewLayout1/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12275/
#KOKbvfunksjon0000: 0:  "Tiltak i alt", 1:  "251 - Barnevernstiltak når barnet ikke er plassert av barnevernet", 2: "252 - Barnevernstiltak når barnet er plassert av barnevernet"

tabell_20_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12275",
  KOKkommuneregion0000 = TRUE,
  KOKalder0000 = "F000-022",
  KOKbvfunksjon0000 = "2",
  ContentsCode = "KOSbvbarntiltaka0000",
  Tid = TRUE
)

tabell_20_kommune = tabell_20_kommune$dataset
write.csv2(tabell_20_kommune, "datauttrekk_raw/tabell_20_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_20_kommune",
  antall_observasjoner = nrow(tabell_20_kommune),
  siste_år = max(parse_number(tabell_20_kommune$Tid))
))
rm(tabell_20_kommune)
Sys.sleep(waiting_time)

#bydeler
#12604: Barn med tiltak i løpet av året og per 31. desember etter funksjon, bydel, etter alder og funksjon (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12604/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12604/

tabell_20_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12604",
  KOKbydelsregion0000 = TRUE,
  KOKalder0000 = "F000-022",
  KOKbvfunksjon0000 = "2",
  ContentsCode = "KOSbvbarntiltaka0000",
  Tid = TRUE
)

tabell_20_bydel = tabell_20_bydel$dataset
write.csv2(tabell_20_bydel, "datauttrekk_raw/tabell_20_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_20_bydel",
  antall_observasjoner = nrow(tabell_20_bydel),
  siste_år = max(parse_number(tabell_20_bydel$Tid))
))
rm(tabell_20_bydel)
Sys.sleep(waiting_time)

# 21	Brutto driftsutgifter til barnevern funksjon 252 (1000 kr)	https://www.ssb.no/statbank/table/12369/ (B) og https://www.ssb.no/statbank/table/12362/ (K)
#kommuner
#12362: Utgifter til tjenesteområdene, etter funksjon og art (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12362/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12362/

tabell_21_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12362",
  KOKkommuneregion0000 = TRUE,
  KOKfunksjon0000 = "252",
  KOKart0000 = "AGD10",
  ContentsCode = "KOSbelop0000",
  Tid = TRUE
)

tabell_21_kommune = tabell_21_kommune$dataset
write.csv2(tabell_21_kommune, "datauttrekk_raw/tabell_21_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_21_kommune",
  antall_observasjoner = nrow(tabell_21_kommune),
  siste_år = max(parse_number(tabell_21_kommune$Tid))
))
rm(tabell_21_kommune)
Sys.sleep(waiting_time)

#bydeler
#12369: Detaljerte regnskapstall driftsregnskapet B, etter funksjon og art (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12369/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12369/

tabell_21_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12369",
  KOKbydelsregion0000 = TRUE,
  KOKfunksjon0000 = "252",
  KOKart0000 = "AGB6",
  ContentsCode = "KOSbelop0000",
  Tid = TRUE
)

tabell_21_bydel = tabell_21_bydel$dataset
write.csv2(tabell_21_bydel, "datauttrekk_raw/tabell_21_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_21_bydel",
  antall_observasjoner = nrow(tabell_21_bydel),
  siste_år = max(parse_number(tabell_21_bydel$Tid))
))
rm(tabell_21_bydel)
Sys.sleep(waiting_time)

# 22	Utgifter pr. barn som bor utenfor hjemmet 
# hentes fra https://www.ssb.no/statbank/table/12279/  (K) og https://www.ssb.no/statbank/table/12601/ (B)
# kommuner
#12279: Nøkkeltal utgifter barnevern (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12279/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12362/

tabell_22_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12279",
  KOKkommuneregion0000 = TRUE,
  ContentsCode = "KOSbdu252bvbarnt0000",
  Tid = TRUE
)

tabell_22_kommune = tabell_22_kommune$dataset
write.csv2(tabell_22_kommune, "datauttrekk_raw/tabell_22_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_22_kommune",
  antall_observasjoner = nrow(tabell_22_kommune),
  siste_år = max(parse_number(tabell_22_kommune$Tid))
))
rm(tabell_22_kommune)
Sys.sleep(waiting_time)

#bydeler
#12601: Nøkkeltal utgifter barnevern, bydel (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12601
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12369/

tabell_22_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12601",
  KOKbydelsregion0000 = TRUE,
  ContentsCode = "KOSbdu252bvbarnt0000",
  Tid = TRUE
)

tabell_22_bydel = tabell_22_bydel$dataset
write.csv2(tabell_22_bydel, "datauttrekk_raw/tabell_22_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_22_bydel",
  antall_observasjoner = nrow(tabell_22_bydel),
  siste_år = max(parse_number(tabell_22_bydel$Tid))
))
rm(tabell_22_bydel)
Sys.sleep(waiting_time)

# 2D - Andel av barnevernsutgiftene som går til saksbehandling (2019)
# 23	Netto driftsutgifter til 244 saksbehandling (inkl. undersøkelse) drift og administrasjon	https://www.ssb.no/statbank/table/12362/ (K) og https://www.ssb.no/statbank/table/12369/ (B) 
# 24	Andel av barnevernsutgiftene som går til saksbehandling	Beregnes  ved å dele nr 23 på nr. 16, hentes fra https://www.ssb.no/statbank/table/12279/  (K) og https://www.ssb.no/statbank/table/12601/ (B)

# 23	Netto driftsutgifter til 244 saksbehandling (inkl. undersøkelse) drift og administrasjon	https://www.ssb.no/statbank/table/12362/ (K) og https://www.ssb.no/statbank/table/12369/ (B) 
#kommuner
#12362: Utgifter til tjenesteområdene, etter funksjon og art (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12362/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12362/

tabell_23_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12362",
  KOKkommuneregion0000 = TRUE,
  KOKfunksjon0000 = "244",
  KOKart0000 = "AGD2",
  ContentsCode = "KOSbelop0000",
  Tid = TRUE
)

tabell_23_kommune = tabell_23_kommune$dataset
write.csv2(tabell_23_kommune, "datauttrekk_raw/tabell_23_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_23_kommune",
  antall_observasjoner = nrow(tabell_23_kommune),
  siste_år = max(parse_number(tabell_23_kommune$Tid))
))
rm(tabell_23_kommune)
Sys.sleep(waiting_time)

#bydeler
#12369: Detaljerte regnskapstall driftsregnskapet B, etter funksjon og art (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12369/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12369/

tabell_23_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12369",
  KOKbydelsregion0000 = TRUE,
  KOKfunksjon0000 = "244",
  KOKart0000 = "AGB4",
  ContentsCode = "KOSbelop0000",
  Tid = TRUE
)

tabell_23_bydel = tabell_23_bydel$dataset
write.csv2(tabell_23_bydel, "datauttrekk_raw/tabell_23_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_23_bydel",
  antall_observasjoner = nrow(tabell_23_bydel),
  siste_år = max(parse_number(tabell_23_bydel$Tid))
))
rm(tabell_23_bydel)
Sys.sleep(waiting_time)

#24	Andel av barnevernsutgiftene som går til saksbehandling	Beregnes  ved å dele nr 23 på nr. 16, hentes fra https://www.ssb.no/statbank/table/12279/  (K) og https://www.ssb.no/statbank/table/12601/ (B)

#kommuner
#12279: Nøkkeltal utgifter barnevern (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12279/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12362/

tabell_24_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12279",
  KOKkommuneregion0000 = TRUE,
  ContentsCode = "KOSandelndu2440000",
  Tid = TRUE
)

tabell_24_kommune = tabell_24_kommune$dataset
write.csv2(tabell_24_kommune, "datauttrekk_raw/tabell_24_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_24_kommune",
  antall_observasjoner = nrow(tabell_24_kommune),
  siste_år = max(parse_number(tabell_24_kommune$Tid))
))
rm(tabell_24_kommune)
Sys.sleep(waiting_time)

#bydeler
#12601: Nøkkeltal utgifter barnevern, bydel (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12601
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12369/

tabell_24_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12601",
  KOKbydelsregion0000 = TRUE,
  ContentsCode = "KOSandelndu2440000",
  Tid = TRUE
)

tabell_24_bydel = tabell_24_bydel$dataset
write.csv2(tabell_24_bydel, "datauttrekk_raw/tabell_24_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_24_bydel",
  antall_observasjoner = nrow(tabell_24_bydel),
  siste_år = max(parse_number(tabell_24_bydel$Tid))
))
rm(tabell_24_bydel)
Sys.sleep(waiting_time)

# 2E - Andel av kommunens totale utgifter som brukes på barnevern (2019)
#25	Totale netto driftsutgifter for alle områder	https://www.ssb.no/statbank/table/12369/ (B) og https://www.ssb.no/statbank/table/12137/ (K)
#26	Andel av kommunens totale utgifter som brukes på barnevern	Beregnes  ved å dele nr. 16 på nr 25

#25	Totale netto driftsutgifter for alle områder	https://www.ssb.no/statbank/table/12369/ (B) og https://www.ssb.no/statbank/table/12137/ (K)
#kommuner
#12137: Finansielle nøkkeltall fra bevilgnings- og balanseregnskapet i kroner per innbygger, etter regnskapsbegrep (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12137
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12137

tabell_25_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12137",
  KOKkommuneregion0000 = TRUE,
  KOKartkap0000 = "AGD1",
  ContentsCode = "KOSbelop0000",
  Tid = TRUE
)

tabell_25_kommune = tabell_25_kommune$dataset 
write.csv2(tabell_25_kommune, "datauttrekk_raw/tabell_25_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_25_kommune",
  antall_observasjoner = nrow(tabell_25_kommune),
  siste_år = max(parse_number(tabell_25_kommune$Tid))
))
rm(tabell_25_kommune)
Sys.sleep(waiting_time)

#bydeler
#12369: Detaljerte regnskapstall driftsregnskapet B, etter funksjon og art (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12369/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12369/

tabell_25_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12369",
  KOKbydelsregion0000 = TRUE,
  KOKfunksjon0000 = TRUE,
  KOKart0000 = "AGB4",
  ContentsCode = "KOSbelop0000",
  Tid = TRUE
)

tabell_25_bydel = tabell_25_bydel$dataset
write.csv2(tabell_25_bydel, "datauttrekk_raw/tabell_25_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_25_bydel",
  antall_observasjoner = nrow(tabell_25_bydel),
  siste_år = max(parse_number(tabell_25_bydel$Tid))
))
rm(tabell_25_bydel)
Sys.sleep(waiting_time)

#26	Andel av kommunens totale utgifter som brukes på barnevern	Beregnes  ved å dele nr. 16 på nr 25
#tabell 26 finnes ikke for nedlasting

# 2F . Utgifter pr. barn 0-22 år som får tiltak i hjemmet (251)
#27	Antall barn 0-22 år som ikke er plassert av barnevernet (251) 	https://www.ssb.no/statbank/table/12604/ (B) og https://www.ssb.no/statbank/table/12275/ (K)
#28	Brutto driftsutgifter til barnevern funksjon 251 (1000 kr)	https://www.ssb.no/statbank/table/12369/ (B) og https://www.ssb.no/statbank/table/12362/ (K)
#29	Utgifter pr. barn som får tiltak i hjemmet 	Beregnes med å dele nr. 28 på 27, hentes fra https://www.ssb.no/statbank/table/12279/  (K) og https://www.ssb.no/statbank/table/12601/ (B)

#27	Antall barn 0-22 år som ikke er plassert av barnevernet (251) 	https://www.ssb.no/statbank/table/12604/ (B) og https://www.ssb.no/statbank/table/12275/ (K)
#kommuner
#12275: Barn med tiltak i løpet av året og per 31. desember, etter region, alder, funksjon, statistikkvariabel og år
#statbank: https://www.ssb.no/statbank/table/12275/tableViewLayout1/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12275/
#KOKbvfunksjon0000: 0:  "Tiltak i alt", 1:  "251 - Barnevernstiltak når barnet ikke er plassert av barnevernet", 2: "252 - Barnevernstiltak når barnet er plassert av barnevernet"

tabell_27_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12275",
  KOKkommuneregion0000 = TRUE,
  KOKalder0000 = "F000-022",
  KOKbvfunksjon0000 = "1",
  ContentsCode = "KOSbvbarntiltaka0000",
  Tid = TRUE
)

tabell_27_kommune = tabell_27_kommune$dataset
write.csv2(tabell_27_kommune, "datauttrekk_raw/tabell_27_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_27_kommune",
  antall_observasjoner = nrow(tabell_27_kommune),
  siste_år = max(parse_number(tabell_27_kommune$Tid))
))
rm(tabell_27_kommune)
Sys.sleep(waiting_time)

#bydeler
#12604: Barn med tiltak i løpet av året og per 31. desember etter funksjon, bydel, etter alder og funksjon (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12604/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12604/

tabell_27_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12604",
  KOKbydelsregion0000 = TRUE,
  KOKalder0000 = "F000-022",
  KOKbvfunksjon0000 = "1",
  ContentsCode = "KOSbvbarntiltaka0000",
  Tid = TRUE
)

tabell_27_bydel = tabell_27_bydel$dataset
write.csv2(tabell_27_bydel, "datauttrekk_raw/tabell_27_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_27_bydel",
  antall_observasjoner = nrow(tabell_27_bydel),
  siste_år = max(parse_number(tabell_27_bydel$Tid))
))
rm(tabell_27_bydel)
Sys.sleep(waiting_time)

#28	Brutto driftsutgifter til barnevern funksjon 251 (1000 kr)	https://www.ssb.no/statbank/table/12369/ (B) og https://www.ssb.no/statbank/table/12362/ (K)
#kommuner
#12362: Utgifter til tjenesteområdene, etter funksjon og art (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12362/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12362/

tabell_28_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12362",
  KOKkommuneregion0000 = TRUE,
  KOKfunksjon0000 = "251",
  KOKart0000 = "AGD10",
  ContentsCode = "KOSbelop0000",
  Tid = TRUE
)

tabell_28_kommune = tabell_28_kommune$dataset
write.csv2(tabell_28_kommune, "datauttrekk_raw/tabell_28_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_28_kommune",
  antall_observasjoner = nrow(tabell_28_kommune),
  siste_år = max(parse_number(tabell_28_kommune$Tid))
))
rm(tabell_28_kommune)
Sys.sleep(waiting_time)

#bydeler
#12369: Detaljerte regnskapstall driftsregnskapet B, etter funksjon og art (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12369/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12369/

tabell_28_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12369",
  KOKbydelsregion0000 = TRUE,
  KOKfunksjon0000 = "251",
  KOKart0000 = "AGB6",
  ContentsCode = "KOSbelop0000",
  Tid = TRUE
)

tabell_28_bydel = tabell_28_bydel$dataset
write.csv2(tabell_28_bydel, "datauttrekk_raw/tabell_28_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_28_bydel",
  antall_observasjoner = nrow(tabell_28_bydel),
  siste_år = max(parse_number(tabell_28_bydel$Tid))
))
rm(tabell_28_bydel)
Sys.sleep(waiting_time)

#29	Utgifter pr. barn som får tiltak i hjemmet 	Beregnes med å dele nr. 28 på 27, hentes fra https://www.ssb.no/statbank/table/12279/  (K) og https://www.ssb.no/statbank/table/12601/ (B)
#kommuner
#12279: Nøkkeltal utgifter barnevern (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12279/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12362/

tabell_29_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12279",
  KOKkommuneregion0000 = TRUE,
  ContentsCode = "KOSbdu251bvbarnt0000",
  Tid = TRUE
)

tabell_29_kommune = tabell_29_kommune$dataset
write.csv2(tabell_29_kommune, "datauttrekk_raw/tabell_29_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_29_kommune",
  antall_observasjoner = nrow(tabell_29_kommune),
  siste_år = max(parse_number(tabell_29_kommune$Tid))
))
rm(tabell_29_kommune)
Sys.sleep(waiting_time)

#bydeler
#12601: Nøkkeltal utgifter barnevern, bydel (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12601
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12369/

tabell_29_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12601",
  KOKbydelsregion0000 = TRUE,
  ContentsCode = "KOSbdu251bvbarnt0000",
  Tid = TRUE
)

tabell_29_bydel = tabell_29_bydel$dataset
write.csv2(tabell_29_bydel, "datauttrekk_raw/tabell_29_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_29_bydel",
  antall_observasjoner = nrow(tabell_29_bydel),
  siste_år = max(parse_number(tabell_29_bydel$Tid))
))
rm(tabell_29_bydel)
Sys.sleep(waiting_time)

tabell_34_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12305",
  KOKkommuneregion0000 = TRUE,
  ContentsCode = "KOSsumfagutdanni0000",
  Tid = TRUE
)

tabell_34_kommune = tabell_34_kommune$dataset
write.csv2(tabell_34_kommune, "datauttrekk_raw/tabell_34_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_34_kommune",
  antall_observasjoner = nrow(tabell_34_kommune),
  siste_år = max(parse_number(tabell_34_kommune$Tid))
))
rm(tabell_34_kommune)
Sys.sleep(waiting_time)

#bydeler
#12605: Stillingar i barnevernstenesta, bydel (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12605/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12605/

tabell_34_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12605",
  KOKbydelsregion0000 = TRUE,
  ContentsCode = "KOSsumfagutdanni0000",
  Tid = TRUE
)

tabell_34_bydel = tabell_34_bydel$dataset
write.csv2(tabell_34_bydel, "datauttrekk_raw/tabell_34_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_34_bydel",
  antall_observasjoner = nrow(tabell_34_bydel),
  siste_år = max(parse_number(tabell_34_bydel$Tid))
))
rm(tabell_34_bydel)
Sys.sleep(waiting_time)

#35	Antall stillinger i barneverntjenesten med fagutdanning pr. 1000 barn 0-17 år	https://www.ssb.no/statbank/table/12305/ (K) og https://www.ssb.no/statbank/table/12605/ (B) Beregnes / hentes

#kommuner
#12305: Stillingar i barnevernstenesta etter fagutdanning og funksjon (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12305/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12305/

tabell_35_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12305",
  KOKkommuneregion0000 = TRUE,
  ContentsCode = "KOSsumfagutd0000",
  Tid = TRUE
)

tabell_35_kommune = tabell_35_kommune$dataset
write.csv2(tabell_35_kommune, "datauttrekk_raw/tabell_35_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_35_kommune",
  antall_observasjoner = nrow(tabell_35_kommune),
  siste_år = max(parse_number(tabell_35_kommune$Tid))
))
rm(tabell_35_kommune)
Sys.sleep(waiting_time)

#bydeler
#12605: Stillingar i barnevernstenesta, bydel (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12605/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12605/

tabell_35_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12605",
  KOKbydelsregion0000 = TRUE,
  ContentsCode = "KOSsumfagutd0000",
  Tid = TRUE
)

tabell_35_bydel = tabell_35_bydel$dataset
write.csv2(tabell_35_bydel, "datauttrekk_raw/tabell_35_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_35_bydel",
  antall_observasjoner = nrow(tabell_35_bydel),
  siste_år = max(parse_number(tabell_35_bydel$Tid))
))
rm(tabell_35_bydel)
Sys.sleep(waiting_time)

# 4D - Utgifter til forebygging, helsestasjon og skolehelsetjeneste pr. innbygger 0-17 år
#53	Netto driftsutgifter til funksjon 232 1000 kr	https://www.ssb.no/statbank/table/12369/ (B) og https://www.ssb.no/statbank/table/12362/ (K)
#54	Utgifter til forebygging, helsestasjon og skolehelsetjeneste pr. innbygger 0-17 år (2019)	Beregnes

#53	Netto driftsutgifter til funksjon 232 1000 kr	https://www.ssb.no/statbank/table/12369/ (B) og https://www.ssb.no/statbank/table/12362/ (K)

#12362: Utgifter til tjenesteområdene, etter funksjon og art (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12362/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12362/

tabell_53_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12362",
  KOKkommuneregion0000 = TRUE,
  KOKfunksjon0000 = "232",
  KOKart0000 = "AGD2",
  ContentsCode = "KOSbelop0000",
  Tid = TRUE
)

tabell_53_kommune = tabell_53_kommune$dataset
write.csv2(tabell_53_kommune, "datauttrekk_raw/tabell_53_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_53_kommune",
  antall_observasjoner = nrow(tabell_53_kommune),
  siste_år = max(parse_number(tabell_53_kommune$Tid))
))
rm(tabell_53_kommune)
Sys.sleep(waiting_time)

#bydeler
#12369: Detaljerte regnskapstall driftsregnskapet B, etter funksjon og art (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12369/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12369/

tabell_53_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12369",
  KOKbydelsregion0000 = TRUE,
  KOKfunksjon0000 = "232",
  KOKart0000 = "AGB4",
  ContentsCode = "KOSbelop0000",
  Tid = TRUE
)

tabell_53_bydel = tabell_53_bydel$dataset
write.csv2(tabell_53_bydel, "datauttrekk_raw/tabell_53_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_53_bydel",
  antall_observasjoner = nrow(tabell_53_bydel),
  siste_år = max(parse_number(tabell_53_bydel$Tid))
))
rm(tabell_53_bydel)
Sys.sleep(waiting_time)

#54	Utgifter til forebygging, helsestasjon og skolehelsetjeneste pr. innbygger 0-17 år (2019)	
#Beregnes


#4F Barn med bekymringsmelding i forhold til barnebefolkningen (2019)
#58	Antall barn med bekymringsmelding 	K: https://www.ssb.no/statbank/table/12615/  B: https://www.ssb.no/statbank/table/12606/
#59 Andel barn med bekymringsmelding i forhold til barnebefolkningen Beregnes med å dele 58 på 1 – eller hentes?

#58	Antall barn med bekymringsmelding 	K: https://www.ssb.no/statbank/table/12615/  B: https://www.ssb.no/statbank/table/12606/

#12615: Barn med melding og prosentdelen barn med melding i barnevernet (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12615
#metadata:https://data.ssb.no/api/v0/no/console/meta/table/12615/

tabell_58_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12615",
  KOKkommuneregion0000 = TRUE,
  ContentsCode = "KOSbvbarnmelding0000",
  Tid = TRUE
)

tabell_58_kommune = tabell_58_kommune$dataset
write.csv2(tabell_58_kommune, "datauttrekk_raw/tabell_58_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_58_kommune",
  antall_observasjoner = nrow(tabell_58_kommune),
  siste_år = max(parse_number(tabell_58_kommune$Tid))
))
rm(tabell_58_kommune)
Sys.sleep(waiting_time)

#bydeler
#12369: Detaljerte regnskapstall driftsregnskapet B, etter funksjon og art (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12369/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12369/

tabell_58_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12606",
  KOKbydelsregion0000 = TRUE,
  ContentsCode = "KOSbvbarnmelding0000",
  Tid = TRUE
)

tabell_58_bydel = tabell_58_bydel$dataset
write.csv2(tabell_58_bydel, "datauttrekk_raw/tabell_58_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_58_bydel",
  antall_observasjoner = nrow(tabell_58_bydel),
  siste_år = max(parse_number(tabell_58_bydel$Tid))
))
rm(tabell_58_bydel)
Sys.sleep(waiting_time)

#59 Andel barn med bekymringsmelding i forhold til barnebefolkningen 
#Beregnes med å dele 58 på 1 – eller hentes? Usikker på om dette er i forhold til barnebefolkning eller befolkningen totalt?

#12615: Barn med melding og prosentdelen barn med melding i barnevernet (K) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12615
#metadata:https://data.ssb.no/api/v0/no/console/meta/table/12615/

tabell_59_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12615",
  KOKkommuneregion0000 = TRUE,
  ContentsCode = "KOSbvbarnmelding0001",
  Tid = TRUE
)

tabell_59_kommune = tabell_59_kommune$dataset
write.csv2(tabell_59_kommune, "datauttrekk_raw/tabell_59_kommune.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_59_kommune",
  antall_observasjoner = nrow(tabell_59_kommune),
  siste_år = max(parse_number(tabell_59_kommune$Tid))
))
rm(tabell_59_kommune)
Sys.sleep(waiting_time)

#bydeler
#12369: Detaljerte regnskapstall driftsregnskapet B, etter funksjon og art (B) 2015 - 2019
#statbank: https://www.ssb.no/statbank/table/12369/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/12369/

tabell_59_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/12606",
  KOKbydelsregion0000 = TRUE,
  ContentsCode = "KOSbvbarnmelding0001",
  Tid = TRUE
)

tabell_59_bydel = tabell_59_bydel$dataset
write.csv2(tabell_59_bydel, "datauttrekk_raw/tabell_59_bydel.csv", row.names = FALSE)

temp_logg = bind_rows(temp_logg, data.frame(
  tabell = "tabell_59_bydel",
  antall_observasjoner = nrow(tabell_59_bydel),
  siste_år = max(parse_number(tabell_59_bydel$Tid))
))
rm(tabell_59_bydel)
Sys.sleep(waiting_time)

#TAR UT EN LOGG OVER HVA SOM KOM MED HER?
logg = full_join(all_queries, temp_logg)
write.csv2(logg, paste0("logg/logg_", Sys.Date(), ".csv"), row.names = FALSE)
