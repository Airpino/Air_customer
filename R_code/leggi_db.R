library(readxl)
library(tidyverse)
library(lubridate)

#BARI
library(RODBC)    #loads the RODBC package
dta <- odbcConnectAccess2007("AEROPORTI DI PUGLIA/access/BARI/2017 ESTATE BARI.accdb")   #specifies the file path
BARI_EST_17 <- sqlFetch(dta, "Tabella1")   #loads the table called 'bar' in the original Access file
dta <- odbcConnectAccess2007("AEROPORTI DI PUGLIA/access/BARI/2017 INVERNO BARI.accdb")   #specifies the file path
BARI_INV_17 <- sqlFetch(dta, "Tabella1")   #loads the table called 'bar' in the original Access file
dta <- odbcConnectAccess2007("AEROPORTI DI PUGLIA/access/BARI/2016 ESTATE BARI.accdb")   #specifies the file path
BARI_EST_16 <- sqlFetch(dta, "Foglio1")   #loads the table called 'bar' in the original Access file
dta <- odbcConnectAccess2007("AEROPORTI DI PUGLIA/access/BARI/2016 INVERNO BARI.accdb")   #specifies the file path
BARI_INV_16 <- sqlFetch(dta, "Foglio1")   #loads the table called 'bar' in the original Access file
dta <- odbcConnectAccess2007("AEROPORTI DI PUGLIA/access/BARI/2015 ESTATE BARI.accdb")   #specifies the file path
BARI_EST_15 <- sqlFetch(dta, "dati")   #loads the table called 'bar' in the original Access file
dta <- odbcConnectAccess2007("AEROPORTI DI PUGLIA/access/BARI/2015 INVERNO BARI.accdb")   #specifies the file path
BARI_INV_15 <- sqlFetch(dta, "Foglio1")   #loads the table called 'bar' in the original Access file

#BRINDISI
dta <- odbcConnectAccess2007("AEROPORTI DI PUGLIA/access/BRINDISI/2015 ESTATE BRINDISI.accdb")   #specifies the file path
BRI_EST_15 <- sqlFetch(dta, "Foglio1")   #loads the table called 'bar' in the original Access file
dta <- odbcConnectAccess2007("AEROPORTI DI PUGLIA/access/BRINDISI/2015 INVERNO BRINDISI.accdb")   #specifies the file path
BRI_INV_15 <- sqlFetch(dta, "Foglio1")   #loads the table called 'bar' in the original Access file
dta <- odbcConnectAccess2007("AEROPORTI DI PUGLIA/access/BRINDISI/2016 ESTATE BRINDISI.accdb")   #specifies the file path
BRI_EST_16 <- sqlFetch(dta, "Foglio1")   #loads the table called 'bar' in the original Access file
dta <- odbcConnectAccess2007("AEROPORTI DI PUGLIA/access/BRINDISI/2016 INVERNO BRINDISI.accdb")   #specifies the file path
BRI_INV_16 <- sqlFetch(dta, "Foglio1")   #loads the table called 'bar' in the original Access file
dta <- odbcConnectAccess2007("AEROPORTI DI PUGLIA/access/BRINDISI/2017 ESTATE BRINDISI.accdb")   #specifies the file path
BRI_EST_17 <- sqlFetch(dta, "Tabella1")   #loads the table called 'bar' in the original Access file
dta <- odbcConnectAccess2007("AEROPORTI DI PUGLIA/access/BRINDISI/2017  INVERNO BRINDISI.accdb")   #specifies the file path
BRI_INV_17 <- sqlFetch(dta, "Tabella1")   #loads the table called 'bar' in the original Access file
RODBC::odbcCloseAll()




BARI_2017<-rbind.data.frame(BARI_EST_17,BARI_INV_17)

names(BARI_EST_16)[1:7]=names(BARI_INV_16)[1:7]
names(BARI_EST_16)[13:56]=names(BARI_INV_16)[8:51]
BARI_2016<-rbind.data.frame(BARI_EST_16[,c(1:7,13:56)],BARI_INV_16[,c(1:51)])
######
#aggiusta Bari estivo 2015
BARI_EST_15$Periodo="ESTIVO"
BARI_INV_15$Data=as.POSIXct(BARI_INV_15$Data, format =  "%d.%m.%y")
BARI_EST_15$Settimana =as.character(wday(BARI_EST_15$Data, label=TRUE))
BARI_INV_15$Settimana =as.character(wday(BARI_INV_15$Data, label=TRUE))
BARI_EST_15$Compagnia=NA_character_
names(BARI_INV_15)[c(2:52)]=names(BARI_EST_15)[c(1,50,5,49,2:4,51,
                                                 6:48)]
BARI_INV_15<-BARI_INV_15[,-c(1,53)]
BARI_2015<-rbind.data.frame(BARI_EST_15,BARI_INV_15)
BARI_2015<-BARI_2015 %>% mutate(Compagnia=case_when(
  grepl("FR",A5) ~ "Ryanair",
  grepl("AZ",A5) ~ "Alitalia",
  grepl("BA",A5) ~ "British Airways",
  grepl("V7",A5) ~ "Volotea",
  grepl("W6",A5) ~ "Wizz Air",
  grepl("4U",A5) ~ "Germanwings",
  grepl("EZY",A5) ~ "Easyjet",
  grepl("EN",A5) ~ "Air Dolomiti",
  grepl("U2",A5) ~ "Easyjet",
  grepl("AB",A5) ~ "Air Berlin",
  grepl("HV",A5) ~ "Transavia",
  grepl("AV",A5) ~ "Transavia",
  grepl("VK",A5) ~ "Air Vallee",
  grepl("VY",A5) ~ "Vueling Airlines",
  grepl("BT",A5) ~ "Air Baltic",
  grepl("LH",A5) ~ "Lufthansa",
  grepl("OB",A5) ~ "Blue Air",
  grepl("TK",A5) ~ "Turkish Airlines",
  grepl("LX",A5) ~ "Swiss",
  grepl("KL",A5) ~"ROYAL DUTCH AIRLINES",
  grepl("LG",A5) ~"Luxair",
  grepl("V62362",A5) ~"Wizz Air",
  grepl("W3166",A5) ~"Wizz Air",
  grepl("VT",A5) ~"Volotea",
  TRUE ~ NA_character_
))


# BRINDISI UNISCI
BRIN_2017<-rbind.data.frame(BRI_INV_17,BRI_EST_17)

names(BRI_EST_16)[c(1:7,13:55)]=names(BRI_INV_16)[c(1:7,8:50)]
BRI_EST_16<-BRI_EST_16[,-c(8:12,56)]
BRI_INV_16<-BRI_INV_16[,-c(51:56)]
BRIN_2016<-rbind.data.frame(BRI_INV_16,BRI_EST_16)
BRIN_2015<-rbind.data.frame(BRI_INV_15,BRI_EST_15)
BRIN_2015$Data=as.POSIXct(BRIN_2015$Data, format =  "%d.%m.%y")
BRIN_2015$Compagnia2=NA_character_
BRIN_2015<-BRIN_2015 %>% mutate(Compagnia2=case_when(
  grepl("FR",Volo) ~ "Ryanair",
  grepl("AZ",Volo) ~ "Alitalia",
  grepl("BA",Volo) ~ "British Airways",
  grepl("V7",Volo) ~ "Volotea",
  grepl("W6",Volo) ~ "Wizz Air",
  grepl("4U",Volo) ~ "Germanwings",
  grepl("EZY",Volo) ~ "Easyjet",
  grepl("EN",Volo) ~ "Air Dolomiti",
  grepl("U2",Volo) ~ "Easyjet",
  grepl("AB",Volo) ~ "Air Berlin",
  grepl("HV",Volo) ~ "Transavia",
  grepl("AV",Volo) ~ "Transavia",
  grepl("VK",Volo) ~ "Air Vallee",
  grepl("VY",Volo) ~ "Vueling Airlines",
  grepl("BT",Volo) ~ "Air Baltic",
  grepl("LH",Volo) ~ "Lufthansa",
  grepl("OB",Volo) ~ "Blue Air",
  grepl("TK",Volo) ~ "Turkish Airlines",
  grepl("LX",Volo) ~ "Swiss",
  grepl("KL",Volo) ~"ROYAL DUTCH AIRLINES",
  grepl("LG",Volo) ~"Luxair",
  grepl("V62362",Volo) ~"Wizz Air",
  grepl("W3166",Volo) ~"Wizz Air",
  grepl("VT",Volo) ~"Volotea",
  TRUE ~ NA_character_
))
rm(dta)
save.image(file="Oridata.RData")
save(BARI_2015,BARI_2016,BARI_2017,BRIN_2015,BRIN_2016,BRIN_2017,file="AGG_1.RData")

# Aggrega BARI
BARI_2015_rid<-BARI_2015[,c(3,1,49,50,4,51,5,13,6:9,14:48)]
BARI_2015_rid$ANNO<-2015
BARI_2016_rid<-BARI_2016[,-c(6,14,25,27,52)]
BARI_2016_rid$ANNO<-2016
names(BARI_2015_rid)=names(BARI_2016_rid)
BARI_15_16<-rbind.data.frame(BARI_2015_rid,BARI_2016_rid)
BARI_2017$ANNO<-2017
BA_15_16_rid <- BARI_15_16[,c(3,2,4,1,5,8,6,7,9:48)]
BARI_17_rid <- BARI_2017[,-c(1,2,6,9,27,30,53,55,56)]
names(BA_15_16_rid)=names(BARI_17_rid)
BARI<-rbind.data.frame(BA_15_16_rid,BARI_17_rid)
save(BARI,BRIN_2015,BRIN_2016,BRIN_2017,file="AGG_2.RData")

#Aggrega BRINDISI
BRIN_2015$ANNO<-2015
# nel 2015 non ci sono le destinazioni, si possono recuperare da num volo?
BRIN_2015$DESTINAZIONE<-NA_character_
BRIN_2015<-BRIN_2015 %>% mutate(DESTINAZIONE=case_when(
  grepl('AB2261',Volo) ~'Berlin',
  grepl('AB8911',Volo) ~'Munich',
  grepl('AB8915',Volo) ~'Zurich',
  grepl('AZ1620',Volo) ~'ROMA',
  grepl('AZ1622',Volo) ~'ROMA',
  grepl('AZ1624',Volo) ~'ROMA',
  grepl('AZ1626',Volo) ~'MILANO',
  grepl('AZ1634',Volo) ~'ROMA',
  grepl('AZ1642',Volo) ~'MILANO',
  grepl('AZ1644',Volo) ~'MILANO',
  grepl('AZ1650',Volo) ~'MILANO',
  grepl('FR4339',Volo) ~'BOLOGNA',
  grepl('FR4352',Volo) ~'BOLOGNA',
  grepl('FR4977',Volo) ~'London',
  grepl('FR6781',Volo) ~'TORINO',
  grepl('FR7738',Volo) ~'Bruxelles',
  grepl('FR8096',Volo) ~'BERGAMO',
  grepl('FR8496',Volo) ~'BERGAMO',
  grepl('FR8796',Volo) ~'BERGAMO',
  grepl('FR8808',Volo) ~'Paris',
  grepl('FR8826',Volo) ~'TORINO',
  grepl('FR8828',Volo) ~'VENEZIA',
  grepl('FR8832',Volo) ~'Eindhoven',
  grepl('FR8886',Volo) ~'BERGAMO',
  grepl('FR9779',Volo) ~'PISA',
  grepl('LX2505',Volo) ~'Zurich',
  grepl('U21238',Volo) ~'Basel',
  grepl('U21546',Volo) ~'Geneva',
  grepl('U22826',Volo) ~'MILANO',
  grepl('U22828',Volo) ~'MILANO',
  grepl('V71736',Volo) ~'VERONA',
  grepl('VT1247',Volo) ~'VENEZIA',
  grepl('VY6282',Volo) ~'Barcelona',
    TRUE ~ NA_character_
))
BRIN_2016$ANNO<-2016
BRIN_2017$ANNO<-2017

BRIN_15_rid<-BRIN_2015[,c(7,2,5,3,8,9,4,56,10:14,18:51,55
)]
BRIN_16_rid<-BRIN_2016[,-c(6,25,27)]
names(BRIN_15_rid)=names(BRIN_16_rid)
BRI_15_16<-rbind.data.frame(BRIN_15_rid,BRIN_16_rid)

BRI_15_16_rid<-BRI_15_16[,c(3,2,4,1,5,8,6,7,9:12,14:48)]
BRIN_2017_rid<-BRIN_2017[,-c(1,2,6,9,27,30,53:56)]
names(BRI_15_16_rid)=names(BRIN_2017_rid)
BRINDISI<-rbind.data.frame(BRI_15_16_rid,BRIN_2017_rid)
ALL<-rbind.data.frame(BARI[,-47],BRINDISI)



### Ricordati di mettere le destinazioni a brindisi 2015 (fatto!)
### Aggiustare le etichette e costruire i factors

# STAGIONE
ALL<-ALL %>% filter(!is.na(STAGIONE)) %>% 
  mutate(STAGIONE=toupper(STAGIONE),STAGIONE=str_sub(STAGIONE,1,3), 
         STAGIONE=factor(STAGIONE))

#Giorno 7
ALL<-ALL %>%  
  mutate(Giorno7=toupper(Giorno7),Giorno7=str_sub(Giorno7,1,3), 
         Giorno7=factor(Giorno7,levels = c("LUN","MAR","MER","GIO","VEN","SAB","DOM")))
#LUOGO
ALL[which(is.na(ALL$LUOGO)),"LUOGO"]="BRINDISI"
ALL<-ALL %>%  
  mutate(LUOGO=toupper(LUOGO),LUOGO=factor(LUOGO,levels = c("BARI","BRINDISI")))


#DESTINAZIONE-uniformare
ALL$DESTINAZIONE=toupper(ALL$DESTINAZIONE)
#AGGIUNGERE NAZIONALI INTERNAZIONALI
#COMP3 UNIFORMARE
ALL$Comp3=toupper(ALL$Comp3)

#VOLO NUM - tagliare spazi
#SESSO numeri e stringhe
ALL<-ALL %>% mutate(SESSO=case_when(
  grepl('1',SESSO) ~'Male',
  grepl('2',SESSO) ~'Female',
  grepl('FEMMINA',SESSO) ~'Female',
  grepl('MASCHIO',SESSO) ~'Female',
  TRUE ~ SESSO
), SESSO=factor(SESSO,levels=c("Male","Female")))

#ETA numeri e stringhe
ALL$`ETA'`=toupper(ALL$`ETA'`)
ALL<-ALL %>% mutate(`ETA'`=case_when(
  `ETA'`==" " ~ NA_character_,
  `ETA'`=="1" ~ '0-11',
  `ETA'`=="2" ~ '12-18',
  `ETA'`=="3" ~ '18-30',
  `ETA'`=="4" ~ '30-45',
  `ETA'`=="5" ~ '45-60',
  `ETA'`=="6" ~ 'over 60',
  `ETA'`=="18_30" ~ '18-30',
  `ETA'`=="PIÙ DI 60" ~ 'over 60',
  `ETA'`=="43435" ~ 'INVALID',
  TRUE ~ `ETA'`
), `ETA'`=factor(`ETA'`,levels=c('0-11',
                                 '12-18',
                                 '18-30',
                                 '30-45',
                                 '45-60',
                                 'over 60','INVALID')))
#ISTRUZIONE numeri e stringhe
ALL<-ALL %>% mutate(ISTRUZIONE=toupper(ISTRUZIONE))
ALL$ISTRUZIONE[which(ALL$ISTRUZIONE=="")]=NA_character_
ALL$ISTRUZIONE[which(ALL$ISTRUZIONE==" ")]=NA_character_
ALL$ISTRUZIONE[which(ALL$ISTRUZIONE=="36")]="High School"
ALL<-ALL %>% mutate(ISTRUZIONE=case_when(
  ISTRUZIONE=="1" ~'Elementary School',
  ISTRUZIONE=="2" ~'Middle School',
  ISTRUZIONE=="3" ~'High School',
  ISTRUZIONE=="4" ~'Bachelor Degree',
  ISTRUZIONE=="5" ~'Master Degree',
  ISTRUZIONE=="6" ~'PhD',
  ISTRUZIONE=="LICENZA ELEMENTARE" ~'Elementary School',
  ISTRUZIONE=="LICENZA MEDIA" ~'Middle School',
  ISTRUZIONE=="DIPLOMA" ~'High School',
  ISTRUZIONE=="LAUREA TRIENNALE" ~'Bachelor Degree',
  ISTRUZIONE=="LAUREA MAGISTRALE" ~'Master Degree',
  ISTRUZIONE=="DOTTORATO DI RICERCA" ~'PhD',
  TRUE ~ ISTRUZIONE
), ISTRUZIONE=factor(ISTRUZIONE,levels=c('Elementary School',
                                         'Middle School',
                                         'High School',
                                         'Bachelor Degree',
                                         'Master Degree',
                                         'PhD')))
# SCOPO VIAGGIO numeri e stringhe
ALL<-ALL %>% mutate(`SCOPO VIAGGIO`=toupper(`SCOPO VIAGGIO`),
                    `SCOPO VIAGGIO`=case_when(
                      `SCOPO VIAGGIO`==" " ~ NA_character_,
                      `SCOPO VIAGGIO`=="1" ~'Business',
                      `SCOPO VIAGGIO`=="2" ~'Leisure',
                      `SCOPO VIAGGIO`=="3" ~ NA_character_,
                      `SCOPO VIAGGIO`=="6" ~ NA_character_,
                      `SCOPO VIAGGIO`=="7" ~ NA_character_,
                      `SCOPO VIAGGIO`=="LAVORO" ~'Business',
                      `SCOPO VIAGGIO`=="PIACERE" ~'Leisure',
                      TRUE ~ `SCOPO VIAGGIO`
), `SCOPO VIAGGIO`=factor(`SCOPO VIAGGIO`,levels=c('Business',
                                         'Leisure')))
# qui aggiungiamo le info sul tipo di volo recuperate in vario modo
destinations <- read_excel("destinazione_integ.xlsx",
col_types = c("numeric", "text", "numeric",
"text", "text", "text", "numeric",
"text", "text"))
destinations$key=paste(destinations$LUOGO,
                       destinations$ANNO,
                       destinations$Comp3,
                       destinations$`VOLO NUM`,
                       destinations$`toupper(DESTINAZIONE)`,sep="_")
NEW_dest<-destinations %>% group_by(key,VERA_dest_volo,TIPO) %>% summarize(n=n())

ALL$key=paste(ALL$LUOGO,
              ALL$ANNO,
              ALL$Comp3,
              ALL$`VOLO NUM`,
              toupper(ALL$DESTINAZIONE),sep="_")
ALL<-ALL %>% left_join(NEW_dest,by=c("key"="key"))

#Nomi variabili in inglese
 names(ALL)[1:12]=c("SEASON","DATE","WEEK_DAY", 
                    "AIRPORT","INTERW_NAME","DESTINATION", 
                    "COMPANY","FLIGHT_NO", "SEX",
                    "AGE", "EDUCATION", "TRAVEL_PURPOSE")
 names(ALL)[47]="YEAR"
 names(ALL)[49]="FLIGHT_DEST"
 names(ALL)[50]="TYPE_OF_FLIGHT"
 ALL$TYPE_OF_FLIGHT<-factor(ALL$TYPE_OF_FLIGHT,levels=c("NAZ","INT"))
 ALL<-ALL[,-51]
 strange_rows<-ALL %>% mutate(IDR=c(1:nrow(ALL))) %>% filter(if_any(B1:L2,~ (.x>10))) 
save(BARI,BRINDISI,ALL,NEW_dest,strange_rows,file="AGG_3.RData")


# Companies=ALL %>% group_by(Comp3) %>% summarise(n=n())
# Destinations=ALL %>% group_by(toupper(DESTINAZIONE)) %>% summarise(n=n())
# Vol_no_destin_airp=ALL %>% group_by(LUOGO,ANNO,Comp3,`VOLO NUM`,toupper(DESTINAZIONE)) %>% summarise(n=n())
# 
# ALL$key=paste(ALL$LUOGO,
#               ALL$ANNO,
#               ALL$Comp3,
#               ALL$`VOLO NUM`,
#               toupper(ALL$DESTINAZIONE),sep="_")
# ALL2<-ALL %>% left_join(NEW_dest,by=c("key"="key"))

#correct strange cells
ALL[strange_rows$IDR[1],"I21"]=7
ALL[strange_rows$IDR[1],"I1"]=8
ALL[strange_rows$IDR[2],"I21"]=7
ALL[strange_rows$IDR[2],"I1"]=8
ALL[strange_rows$IDR[3],c('F3','F41','F42','F43','F44','F51')]=c(6,6,6,6,6,7)
ALL[strange_rows$IDR[4],'F3']=9
ALL[strange_rows$IDR[5],'F53']=6
ALL[strange_rows$IDR[6],'B1']=8
ALL[strange_rows$IDR[7],c('B1','B2')]=c(9,9)
ALL[strange_rows$IDR[8],'D2']=7
ALL[strange_rows$IDR[9],'G1']=8
ALL[strange_rows$IDR[10],'E1']=9
ALL[strange_rows$IDR[11],'F43']=4
ALL[strange_rows$IDR[12],'D2']=8
ALL[strange_rows$IDR[13],'B1']=8
ALL[strange_rows$IDR[14],'I21']=9
ALL[strange_rows$IDR[15],'F1']=10
ALL[strange_rows$IDR[16],'C1']=8
ALL[strange_rows$IDR[17],'G2']=10
ALL[strange_rows$IDR[18],'F44']=9
ALL[strange_rows$IDR[19],'D1']=NA_real_
ALL[strange_rows$IDR[20],'F54']=8
ALL[strange_rows$IDR[21],'G1']=8
ALL[strange_rows$IDR[22],'G1']=8
ALL[strange_rows$IDR[23],'F3']=8
ALL[strange_rows$IDR[24],c('G1','G2','G3','G4','G5','H1','H2','H3')]=c(NA_real_,4,4,4,4,4,4,4)
ALL[strange_rows$IDR[25],'I21']=8
ALL[strange_rows$IDR[26],'I21']=8
ALL[strange_rows$IDR[27],'F51']=7
ALL[strange_rows$IDR[28],'F51']=9
ALL[strange_rows$IDR[29],'F54']=8

 #prepare groups
`%!in%` <- Negate(`%in%`)
ALL <- ALL %>% mutate(WORK_DAY = if_else(WEEK_DAY%in%c("SAB","DOM"),"Weekend","Work"),
                      WEEK_DAY=factor(WEEK_DAY,levels=c("Work","Weekend")),
                      DEST_agg=case_when(
                        FLIGHT_DEST=="BERGAMO" ~ "MILANO",
                        TYPE_OF_FLIGHT=="INT" ~ "INTERN",
                        (TYPE_OF_FLIGHT=="NAZ") &(FLIGHT_DEST%!in%c("ROMA","MILANO"))~ "OTH_NAZ",
                        TRUE ~ FLIGHT_DEST),
                      DEST_agg=factor(DEST_agg,c("ROMA","MILANO","OTH_NAZ","INTERN")),
                      COMP_agg=case_when(
                        (!is.na(COMPANY))&(COMPANY%!in%c("ALITALIA","RYANAIR"))~ "OTHER",
                        TRUE ~ COMPANY),
                      COMP_agg=factor(COMP_agg,c("ALITALIA","RYANAIR","OTHER")),
                      AGE_agg=case_when(
                        AGE=="0-11" ~ "0-30",
                        AGE=="12-18" ~ "0-30",
                        AGE=="18-30" ~ "0-30",
                        AGE=="30-45" ~ "30-60",
                        AGE=="45-60" ~ "30-60",
                        AGE=="over 60" ~ "over 60",
                        AGE=="INVALID" ~ NA_character_,
                        is.na(AGE) ~ NA_character_
#                        TRUE ~ AGE
                        ),
                      AGE_agg=factor(AGE_agg,c("0-30","30-60","over 60"))
                      )
save(BARI,BRINDISI,ALL,NEW_dest,strange_rows,file="AGG_4.RData")
#a fisrt selection 1.024 classes/objects an 12,887 obs
ALL %>% group_by(AIRPORT,YEAR,SEASON,DEST_agg,WORK_DAY,COMP_agg,TRAVEL_PURPOSE,AGE_agg) %>% summarise(n=n()) %>% na.omit()
#a reasonable selection 164 classes/objects 12,629 obs
SPLI_all<-ALL %>% filter_at(vars(AIRPORT,YEAR,SEASON,DEST_agg,COMP_agg,TRAVEL_PURPOSE), all_vars(!is.na(.))) %>% 
  select(AIRPORT,YEAR,SEASON,DEST_agg,COMP_agg,TRAVEL_PURPOSE,B1:F3,L1,L2)%>% 
  group_by(AIRPORT,YEAR,SEASON,DEST_agg,COMP_agg,TRAVEL_PURPOSE) %>% 
  #summarise(n=n()) %>% na.omit() %>% filter(n>20) %>% 
  group_split()
to_delete <- which(unlist(lapply(SPLI_all, nrow))<20)
SPLIT_all_red<-SPLI_all[-to_delete]
#create a nested tibble
labels<-character()
listone<-list()
c<-0
for(i in 1:length(SPLIT_all_red)){
  labels<-c(labels, paste(SPLIT_all_red[[i]]$AIRPORT[1],
                          SPLIT_all_red[[i]]$YEAR[1],
                          SPLIT_all_red[[i]]$SEASON[1],
                          SPLIT_all_red[[i]]$DEST_agg[1],
                          SPLIT_all_red[[i]]$COMP_agg[1],
                          SPLIT_all_red[[i]]$TRAVEL_PURPOSE[1],sep="."))
  for (j in 7:20){
    c<-c+1
    TMP_TIBB<-SPLIT_all_red[[i]] %>% select_at(vars(j)) %>% na.omit() %>%  
      group_by_at(vars(1)) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n),cdf=ceiling(cumsum(freq)*10^5)/10^5,freq=diff(c(0,cdf)))
    listone[[c]]=TMP_TIBB
  }
  print(i)
}
Fin_tibble<-tibble(LAB=labels)
for (j in 1:14){
  Fin_tibble<-cbind(Fin_tibble,tibble((!!names(listone[[j]])[1]):=listone[seq(j,2310,by=14)]))
}

metad<-data.frame(LAB=Fin_tibble$LAB) %>% separate(LAB, into=c("AIRPORT", "YEAR","SEASON","FLIGHT","COMPANY","TYPE"),sep="[.]",remove=F)
counts_tibb<-COUNTS_NestTibb(Fin_tibble) %>% as.data.frame()
counts_tibb$min<-apply(counts_tibb %>% select(where(is.numeric)), MARGIN =  1, FUN = min, na.rm = T)
counts_tibb$max<-apply(counts_tibb %>% select(where(is.numeric)), MARGIN =  1, FUN = max, na.rm = T)
everything<-Fin_tibble %>% left_join(metad,by=c("LAB")) %>%
  left_join(counts_tibb %>% mutate(LAB=rownames(.)),by=c("LAB"),suffix = c("", "_counts")) 

ALL_to_copy<-ALL[,c(1:2,4,9:24,45:47,50:54)] %>% drop_na(AIRPORT,TYPE_OF_FLIGHT,TRAVEL_PURPOSE,DEST_agg,COMP_agg) %>% 
  relocate(AIRPORT, YEAR, SEASON, DEST_agg,COMP_agg,TRAVEL_PURPOSE) 

save(Fin_tibble,metad, counts_tibb,everything,ALL_to_copy,file="My_table3.RData")






