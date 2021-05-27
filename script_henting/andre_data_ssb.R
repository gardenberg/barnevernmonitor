#HENTESCRIPT FOR ALLE ANDRE DATA FRA SSB SOM IKKE ER KOSTRA

#OPPRETTET 5. mars 2021

#FORMÅL
#Siden KOSTRA-dataene oppdateres samtidig hos SSB, er det smart å ha ett script som sjekker og henter dette.

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

#1A Barn med barnevernstiltak i forhold til innbyggere i aldersgruppen 0-17 år
#1 Antall innbyggere 0-17 år - kommune, 07459: Alders- og kjønnsfordeling i kommuner, fylker og hele landets befolkning (K) 1986 - 2020 #10826: Alders- og kjønnsfordeling for befolkningen i de 4 største byene (B) 2001 - 2020 #statbank: https://www.ssb.no/statbank/table/10826/

#1 Antall innbyggere 0-17 år - kommune
#07459: Alders- og kjønnsfordeling i kommuner, fylker og hele landets befolkning (K) 1986 - 2020
#statbank: https://www.ssb.no/statbank/table/07459/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/07459/ 

tabell_1_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/07459",
  Region = TRUE,
  Kjonn = FALSE,
  Alder = c("000", "001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017"),
  ContentsCode = "Personer1",
  Tid = c("2015", "2016", "2017", "2018", "2019", "2020")
)

#apiet returnerer dette som ei liste med to elementer, der 1 er datasett med labels, og 2 er datasett med koder,
#vi henter datasettet med koder

tabell_1_kommune = tabell_1_kommune$dataset

#skriver ut denne

write.csv2(tabell_1_kommune, "datauttrekk_raw/tabell_1_kommune.csv", row.names = FALSE)

#1 Antall innbyggere 0-17 år bydeler
#10826: Alders- og kjønnsfordeling for befolkningen i de 4 største byene (B) 2001 - 2020
#siden bydeler i Oslo og Trondheim ble kraftig endra i 2004, starter vi der.
#statbank: https://www.ssb.no/statbank/table/10826/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/10826/

tabell_1_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/10826",
  Region = TRUE,
  Kjonn = FALSE,
  Alder = c("000", "001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017"),
  ContentsCode = "Personer",
  Tid = c("2015", "2016", "2017", "2018", "2019", "2020")
)

#skriver ut denne
tabell_1_bydel = tabell_1_bydel$dataset
write.csv2(tabell_1_bydel, "datauttrekk_raw/tabell_1_bydel.csv", row.names = FALSE)

# 4A - Barn i befolkningen
#45	Antall innbyggere totalt	https://www.ssb.no/statbank/table/10826/ (B) og https://www.ssb.no/statbank/table/07459/ (K)
#46	Andel Barn i befolkningen	Beregnes ved å dele v45 på v1

#45	Antall innbyggere totalt	https://www.ssb.no/statbank/table/10826/ (B) og https://www.ssb.no/statbank/table/07459/ (K)

#kommune
#07459: Alders- og kjønnsfordeling i kommuner, fylker og hele landets befolkning (K) 1986 - 2020
#statbank: https://www.ssb.no/statbank/table/07459/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/07459/

tabell_45_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/07459",
  Region = TRUE,
  Kjonn = FALSE,
  Alder = FALSE,
  ContentsCode = "Personer1",
  Tid = c("2015", "2016", "2017", "2018", "2019", "2020")
)

tabell_45_kommune = tabell_45_kommune$dataset
write.csv2(tabell_45_kommune, "datauttrekk_raw/tabell_45_kommune.csv", row.names = FALSE)

#bydeler
#10826: Alders- og kjønnsfordeling for befolkningen i de 4 største byene (B) 2001 - 2020
#statbank: https://www.ssb.no/statbank/table/10826/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/10826/

tabell_45_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/10826",
  Region = TRUE,
  Kjonn = FALSE,
  Alder = FALSE,
  ContentsCode = "Personer",
  Tid = c("2015", "2016", "2017", "2018", "2019", "2020")
)

tabell_45_bydel = tabell_45_bydel$dataset
write.csv2(tabell_45_bydel, "datauttrekk_raw/tabell_45_bydel.csv", row.names = FALSE)


#46	Andel Barn i befolkningen	
#Beregnes ved å dele v45 på v1 - ingen kilde til denne.

# 4B - Enslige forsørgere med barn under 18 år
#47	Antall Enslige forsørgere med barn under 18 år https://www.ssb.no/statbank/table/06070/ 
#48	Antall forsørgere med barn under 18 år totalt	https://www.ssb.no/statbank/table/06070/ 
#49	Andel enslige forsørgere med barn under 18 år Beregnes ved å dele nr. 47 på nr. 48

#47	Antall Enslige forsørgere med barn under 18 år https://www.ssb.no/statbank/table/06070/ 

#kommune og bydeler
#06070: Privathusholdninger, etter husholdningstype (K) (B) 2005 - 2020
#statbank: https://www.ssb.no/statbank/table/06070/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/06070/

tabell_47 = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/06070",
  Region = TRUE,
  HushType = c("005", "006"),
  ContentsCode = "Husholdninger",
  Tid = c("2015", "2016", "2017", "2018", "2019", "2020")
)

tabell_47 = tabell_47$dataset
write.csv2(tabell_47, "datauttrekk_raw/tabell_47.csv", row.names = FALSE)

#48	Antall forsørgere med barn under 18 år totalt	https://www.ssb.no/statbank/table/06070/ 

#kommune og bydeler
#06070: Privathusholdninger, etter husholdningstype (K) (B) 2005 - 2020
#statbank: https://www.ssb.no/statbank/table/06070/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/06070/

tabell_48 = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/06070",
  Region = TRUE,
  HushType = c("003", "004", "005", "006", "009", "010"),
  ContentsCode = "Husholdninger",
  Tid = c("2015", "2016", "2017", "2018", "2019", "2020")
)

tabell_48 = tabell_48$dataset
write.csv2(tabell_48, "datauttrekk_raw/tabell_48.csv", row.names = FALSE)

#49	Andel enslige forsørgere med barn under 18 år 
#Beregnes ved å dele nr. 47 på nr. 48


#kommune
#09429: Utdanningsnivå, etter kommune og kjønn (K) 1970 - 2019
#statbank: https://www.ssb.no/statbank/table/09429/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/09429/

tabell_50_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/09429",
  Region = TRUE,
  Nivaa = c("02a", "11", "03a", "04a"),
  Kjonn = "0",
  ContentsCode = "Personer",
  Tid = c("2015", "2016", "2017", "2018", "2019")
)

tabell_50_kommune = tabell_50_kommune$dataset
write.csv2(tabell_50_kommune, "datauttrekk_raw/tabell_50_kommune.csv", row.names = FALSE)

#bydeler
#09434: Utdanningsnivå, etter bydel og kjønn (B) 2010 - 2019
#statbank: https://www.ssb.no/statbank/table/09434/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/09434/

tabell_50_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/09434",
  Region = TRUE,
  Kjonn = "0",
  UtdNivaa = c("02a", "11", "03a", "04a"),
  ContentsCode = "Personer",
  Tid = c("2015", "2016", "2017", "2018", "2019")
)

tabell_50_bydel = tabell_50_bydel$dataset
write.csv2(tabell_50_bydel, "datauttrekk_raw/tabell_50_bydel.csv", row.names = FALSE)

#51	Antall i befolkningen 16 år og over totalt	https://www.ssb.no/statbank/table/09434 (B) og https://www.ssb.no/statbank/table/09429 (K)

#kommune
#09429: Utdanningsnivå, etter kommune og kjønn (K) 1970 - 2019
#statbank: https://www.ssb.no/statbank/table/09429/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/09429/

tabell_51_kommune = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/09429",
  Region = TRUE,
  Nivaa = FALSE,
  Kjonn = "0",
  ContentsCode = "Personer",
  Tid = c("2015", "2016", "2017", "2018", "2019")
)

tabell_51_kommune = tabell_51_kommune$dataset
write.csv2(tabell_51_kommune, "datauttrekk_raw/tabell_51_kommune.csv", row.names = FALSE)

#bydeler
#09434: Utdanningsnivå, etter bydel og kjønn (B) 2010 - 2019
#statbank: https://www.ssb.no/statbank/table/09434/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/09434/

tabell_51_bydel = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/09434",
  Region = TRUE,
  Kjonn = "0",
  UtdNivaa = FALSE,
  ContentsCode = "Personer",
  Tid = c("2015", "2016", "2017", "2018", "2019")
)

tabell_51_bydel = tabell_51_bydel$dataset
write.csv2(tabell_51_bydel, "datauttrekk_raw/tabell_51_bydel.csv", row.names = FALSE)

#52	Andel i befolkningen med utdanning utover grunnskole
#Beregnes

# 4E - Andel barn i lavinntektsfamilier (EU-skala 60 prosent)
#VURDERES OM LAVINNTEKTSTALL FRA BARNEFATTIGDOM.NO SKAL BRUKES HER

#55	Andel barn 0-17 år som bor i lavinntektsfamilier, andel barn etter «EU-skala 60 prosent»	https://www.ssb.no/statbank/table/08764/ 

#55	Andel barn 0-17 år som bor i lavinntektsfamilier, andel barn etter «EU-skala 60 prosent»	https://www.ssb.no/statbank/table/08764/ 

#08764: Personer under 18 år i privathusholdninger med årlig inntekt etter skatt per forbruksenhet, under ulike avstander til medianinntekten, etter region, statistikkvariabel og år
#statbank: https://www.ssb.no/statbank/table/08764/tableViewLayout1/
#metadata: https://data.ssb.no/api/v0/no/console/meta/table/08764/

tabell_55 = ApiData(
  urlToData = "https://data.ssb.no/api/v0/no/table/08764",
  Region = TRUE,
  ContentsCode = "EUskala60",
  Tid = c("2015", "2016", "2017", "2018", "2019") #NB - KORTERE TIDSPERIODE HER.
)

tabell_55 = tabell_55$dataset
write.csv2(tabell_55, "datauttrekk_raw/tabell_55.csv", row.names = FALSE)

#56	Antall Barn 0-17 år som bor i lavinntektsfamilier, antall «personer under 18 år»
#fjernet - ikke behov for denne.
