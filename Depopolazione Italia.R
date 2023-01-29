
#install.packages("devtools")
#devtools::install_github("ramnathv/rCharts")
library(rCharts)

# install.packages("readr")
library(readr)

# install.packages("tidyr")
library(tidyr)

# install.packages("lubridate")
library(lubridate)

# install.packages("stringr")
library(stringr)

# install.packages("scales")
library(scales)

# install.packages("dplyr")
library(dplyr)

#install.packages("stringr")
library(stringr)

#install.packages('tidyverse')
library(tidyverse)

#install.packages('ggplot2')
library(ggplot2)

#install.packages('gridExtra')
library(gridExtra)

#install.packages('ggthemes')
library(ggthemes)

# install.packages('patchwork')
library(patchwork) 

# install.packages('grid')
library(grid)

#install.packages('ggrepel')
library(ggrepel)

# install.packages('forcats')
library(forcats)

#install.packages('RColorBrewer')
library(RColorBrewer)

# install.packages('viridis')
library(viridis)

#install.packages('numbers')
library(numbers)

#install.packages("XML")
library(XML)

#install.packages("reshape2")
library(reshape2)

#install.packages("plyr")
library(plyr)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("hrbrthemes")
library(hrbrthemes)

#install.packages("viridis")
library(viridis)

#Emigrazione
Emigrazione = read.csv("C:/Users/david/Desktop/Compiti/Universita/1 anno/R/Progetto Main/Migrazioni dall'Italia.csv")

#Normalizzazione dei csv "Fasce d'et? 2002-2019" e "Fasce d'et? 2019-2022" in un dataFrame che va dagli anni 2002 agli anni 2022
Fasceeta2002_2019 = read.csv("C:/Users/david/Desktop/Compiti/Universita/1 anno/R/Progetto Main/Fasce d'eta 2002-2019.csv")
Fasceeta2002_2019$Classe.di.eta<-str_replace(Fasceeta2002_2019$Classe.di.eta, ' anni', '')
Fasceeta2002_2019$Classe.di.eta<-str_replace(Fasceeta2002_2019$Classe.di.eta, ' e piu', '')

Fasceeta2020_2022 = read.csv("C:/Users/david/Desktop/Compiti/Universita/1 anno/R/Progetto Main/Fasce d'eta 2019-2022.csv")
Fasceeta2020_2022$Classe.di.eta<-str_replace(Fasceeta2020_2022$Classe.di.eta, ' anni', '')
Fasceeta2020_2022$Classe.di.eta<-str_replace(Fasceeta2020_2022$Classe.di.eta, ' e piu', '')

Fasceeta2002_2019$Classe.di.eta<-strtoi(Fasceeta2002_2019$Classe.di.eta) 
Fasceeta2020_2022$Classe.di.eta<-strtoi(Fasceeta2020_2022$Classe.di.eta) 
Fasceeta2002_2019 = Fasceeta2002_2019 %>%
  select(Territorio,Classe.di.eta,Sesso,Anno,Value)

Fasceeta2020_2022_It=Fasceeta2020_2022 %>% 
  filter(Territorio == "Italia") 
#Modifico la tipologia di dato da String a Integer

#Normalizzazione dei csv "Fasce d'et? 2002-2019" e "Fasce d'et? 2019-2022" in un dataFrame che va dagli anni 2002 agli anni 2022

Fasceeta2002_2019_It = Fasceeta2002_2019%>%
  filter( Territorio=="Italia")

FascEtaIt = full_join(Fasceeta2002_2019_It, Fasceeta2020_2022_It)
FascEtaIt=FascEtaIt%>%
  arrange( Anno, Classe.di.eta)


FasecEta0_4 = FascEtaIt%>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=0) & (Classe.di.eta<5))) %>%
  arrange(Anno)
FasecEta0_4= aggregate(Value ~ (Sesso+Anno), FasecEta0_4,sum)
FasecEta0_4 = FasecEta0_4%>%
  mutate(classeEta= " 00 - 04 Anni")


FasecEta5_9 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=5) & (Classe.di.eta<10)))%>%
  arrange(Anno)
FasecEta5_9= aggregate(Value ~ (Sesso+Anno), FasecEta5_9,sum)
FasecEta5_9 = FasecEta5_9%>%
  mutate(classeEta= " 05 - 09 Anni")


FasecEta10_14 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=10) & (Classe.di.eta<15)))%>%
  arrange(Anno)
FasecEta10_14= aggregate(Value ~ (Sesso+Anno), FasecEta10_14,sum)
FasecEta10_14 = FasecEta10_14%>%
  mutate(classeEta= " 10 - 15 Anni")


FasecEta15_19 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=15) & (Classe.di.eta<20)))%>%
  arrange(Anno)
FasecEta15_19= aggregate(Value ~ (Sesso+Anno), FasecEta15_19,sum)
FasecEta15_19 = FasecEta15_19%>%
  mutate(classeEta= " 15 - 19 Anni")



FasecEta20_24 = FascEtaIt%>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=20) & (Classe.di.eta<25))) %>%
  arrange(Anno, Classe.di.eta)
FasecEta20_24= aggregate(Value ~ (Sesso+Anno), FasecEta0_4,sum)
FasecEta20_24 = FasecEta20_24%>%
  mutate(classeEta= " 20 - 24 Anni")


FasecEta25_29 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=25) & (Classe.di.eta<30)))%>%
  arrange(Anno, Classe.di.eta)
FasecEta25_29= aggregate(Value ~ (Sesso+Anno), FasecEta25_29,sum)
FasecEta25_29 = FasecEta25_29%>%
  mutate(classeEta= " 25 - 29 Anni")


FasecEta30_34 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=30) & (Classe.di.eta<35)))%>%
  arrange(Anno)
FasecEta30_34= aggregate(Value ~ (Sesso+Anno), FasecEta30_34,sum)
FasecEta30_34 = FasecEta30_34%>%
  mutate(classeEta= " 30 - 34 Anni")


FasecEta35_39 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=35) & (Classe.di.eta<40)))%>%
  arrange(Anno)
FasecEta35_39= aggregate(Value ~ (Sesso+Anno), FasecEta35_39,sum)
FasecEta35_39 = FasecEta35_39%>%
  mutate(classeEta= " 35 - 39 Anni")


FasecEta40_44 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=30) & (Classe.di.eta<35)))%>%
  arrange(Anno)
FasecEta40_44= aggregate(Value ~ (Sesso+Anno), FasecEta40_44,sum)
FasecEta40_44 = FasecEta40_44%>%
  mutate(classeEta= " 40 - 44 Anni")


FasecEta45_49 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=45) & (Classe.di.eta<50)))%>%
  arrange(Anno)
FasecEta45_49= aggregate(Value ~ (Sesso+Anno), FasecEta45_49,sum)
FasecEta45_49 = FasecEta45_49%>%
  mutate(classeEta= " 45 - 49 Anni")


FasecEta50_54 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=50) & (Classe.di.eta<55)))%>%
  arrange(Anno)
FasecEta50_54= aggregate(Value ~ (Sesso+Anno), FasecEta50_54,sum)
FasecEta50_54 = FasecEta50_54%>%
  mutate(classeEta= " 50 - 54 Anni")


FasecEta55_59 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=55) & (Classe.di.eta<60)))%>%
  arrange(Anno)
FasecEta55_59= aggregate(Value ~ (Sesso+Anno), FasecEta55_59,sum)
FasecEta55_59 = FasecEta55_59%>%
  mutate(classeEta= " 55 - 59 Anni")


FasecEta60_64 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=60) & (Classe.di.eta<65)))%>%
  arrange(Anno)
FasecEta60_64= aggregate(Value ~ (Sesso+Anno), FasecEta60_64,sum)
FasecEta60_64 = FasecEta60_64%>%
  mutate(classeEta= " 60 - 64 Anni")


FasecEta65_69 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=65) & (Classe.di.eta<70)))%>%
  arrange(Anno)
FasecEta65_69= aggregate(Value ~ (Sesso+Anno), FasecEta65_69,sum)
FasecEta65_69 = FasecEta65_69%>%
  mutate(classeEta= " 65 - 69 Anni")


FasecEta70_74 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=70) & (Classe.di.eta<75)))%>%
  arrange(Anno)
FasecEta70_74= aggregate(Value ~ (Sesso+Anno), FasecEta70_74,sum)
FasecEta70_74 = FasecEta70_74%>%
  mutate(classeEta= " 70 - 74 Anni")


FasecEta75_79 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=75) & (Classe.di.eta<80)))%>%
  arrange(Anno)
FasecEta75_79= aggregate(Value ~ (Sesso+Anno), FasecEta75_79,sum)
FasecEta75_79 = FasecEta75_79%>%
  mutate(classeEta= " 75 - 79 Anni")


FasecEta80_84 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=80) & (Classe.di.eta<85)))%>%
  arrange(Anno)
FasecEta80_84= aggregate(Value ~ (Sesso+Anno), FasecEta80_84,sum)
FasecEta80_84 = FasecEta80_84%>%
  mutate(classeEta= " 80 - 84 Anni")


FasecEta85_89 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=85) & (Classe.di.eta<90)))%>%
  arrange(Anno)
FasecEta85_89= aggregate(Value ~ (Sesso+Anno), FasecEta85_89,sum)
FasecEta85_89 = FasecEta85_89%>%
  mutate(classeEta=  " 85 - 89 Anni")


FasecEta90_94 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=90) & (Classe.di.eta<95)))%>%
  arrange(Anno)
FasecEta90_94= aggregate(Value ~ (Sesso+Anno), FasecEta90_94,sum)
FasecEta90_94 = FasecEta90_94%>%
  mutate(classeEta= " 90 - 94 Anni")


FasecEta95_99 = FascEtaIt %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=95) & (Classe.di.eta<100)))%>%
  arrange(Anno)
FasecEta95_99= aggregate(Value ~ (Sesso+Anno), FasecEta95_99,sum)
FasecEta95_99 = FasecEta95_99%>%
  mutate(classeEta= " 95 - 99 Anni")

FasecEta100pi = FascEtaIt %>%
  filter(mod(Anno,5)==1 & Classe.di.eta==100)%>%
  arrange(Anno)
FasecEta100pi= aggregate(Value ~ (Sesso+Anno), FasecEta100pi,sum)
FasecEta100pi = FasecEta100pi%>%
  mutate(classeEta= "100 e piu")

FascEtaItCorretta = full_join(FasecEta0_4, FasecEta5_9)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta10_14)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta15_19)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta20_24)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta25_29)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta30_34)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta35_39)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta40_44)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta45_49)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta50_54)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta55_59)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta60_64)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta65_69)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta70_74)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta75_79)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta80_84)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta85_89)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta90_94)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta95_99)
FascEtaItCorretta = full_join(FascEtaItCorretta,FasecEta100pi)


#Plotto lq popolazione Italiana del 2006 - 2011 - 2016 - 2021 con un barplot orizzontale
Popolazione_ITA=FascEtaItCorretta %>%
  select(Anno,Value,classeEta,Sesso)%>%
  mutate( 
    population = ifelse(Sesso=="maschi", Value*(-1),
                        Value*1))%>%
  mutate(
    Eta=strtoi(substr(classeEta,1,3)))%>%
  ggplot(aes(x = Eta, y = population, fill=Sesso))+
  geom_bar(stat="identity", width = 2.5)+
  theme_fivethirtyeight(base_size = 11, base_family = "sans") +
  scale_colour_gradient(
    name="",
    low = "#0C36A7",
    high = "#14CBF8",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  theme(
    axis.text.x = element_text(size=7,angle=45),
    strip.text.x = element_text(size = 9),
    legend.position="none") +
  labs(title = "Popolazione Italiana", x = "Age", 
       y = "Population")+
  facet_wrap(~Anno,ncol=2, scales = "free") +
  labs(title="Popolazione italiana suddivisa per fasce d'eta")+
  coord_flip()+
  scale_y_continuous(breaks = c(-2000000,-1000000,0,1000000,2000000),
                     labels = c("2000000","1000000",0,"1000000","2000000"))+
  scale_x_continuous(breaks = c(0,25,50,75,100),
                     labels = c("0-4","25-29","50-54","75-79","100 e più"))

Popolazione_ITA


#Cose a caso
#Fine di cose a caso

#C'è differenza nell'evoluzione demografica tra regioni italiane del Nord e del Sud?
#Average annual rate of population change per Region
#Unisco in un unico dataframe le popolazione delle regioni italiane in questo modo posso
#unire i dati della regione senza ripetere il processo due volte
Fasceeta2002_2019_Reg = Fasceeta2002_2019%>%
  filter(Territorio=="Piemonte" 
         | Territorio=="Liguria"
         | Territorio=="Lombardia"
         | Territorio=="Valle d'Aosta / Vallée d'Aoste"
         | Territorio=="Trentino Alto Adige / Südtirol"
         | Territorio=="Veneto"
         | Territorio=="Friuli-Venezia Giulia"
         | Territorio=="Emilia-Romagna"
         | Territorio=="Toscana"
         | Territorio=="Umbria"
         | Territorio=="Marche"
         | Territorio=="Lazio"
         | Territorio=="Abruzzo"
         | Territorio=="Molise"
         | Territorio=="Campania"
         | Territorio=="Calabria"
         | Territorio=="Puglia"
         | Territorio=="Basilicata"
         | Territorio=="Sicilia"
         | Territorio=="Sardegna"
         )%>%
  filter((Classe.di.eta>=0) & (Classe.di.eta<=100))

#e con questo ho solo i dati del 2002-2019

Fasceeta2020_2022_Reg = Fasceeta2020_2022%>%
  filter(Territorio=="Piemonte" 
         | Territorio=="Liguria"
         | Territorio=="Valle d'Aosta / Vallée d'Aoste"
         | Territorio=="Lombardia"
         | Territorio=="Trentino Alto Adige / Südtirol"
         | Territorio=="Veneto"
         | Territorio=="Friuli-Venezia Giulia"
         | Territorio=="Emilia-Romagna"
         | Territorio=="Toscana"
         | Territorio=="Umbria"
         | Territorio=="Marche"
         | Territorio=="Lazio"
         | Territorio=="Abruzzo"
         | Territorio=="Molise"
         | Territorio=="Campania"
         | Territorio=="Calabria"
         | Territorio=="Puglia"
         | Territorio=="Basilicata"
         | Territorio=="Sicilia"
         | Territorio=="Sardegna"
  )%>%
  filter((Classe.di.eta>=0) & (Classe.di.eta<=100))
  

#dataframe dati regioni con i dati temporali di tutti gli anni e la classe di età 
FascEtaRegione = full_join(Fasceeta2002_2019_Reg, Fasceeta2020_2022_Reg)
FascEtaRegione = FascEtaRegione%>%
  arrange(Anno, Classe.di.eta)

#dataframe dati regioni con i dati temporali di tutti gli anni con tutta la popolazione sommata
Fasceeta2002_2019_Reg_Tot = aggregate(Value ~ (Anno+Territorio), Fasceeta2002_2019_Reg,sum)

Fasceeta2002_2019_Reg_Tot = aggregate(Value ~ (Anno+Territorio), Fasceeta2002_2019_Reg,sum)
Fasceeta2002_2019_Reg_Tot = Fasceeta2002_2019_Reg_Tot%>%
  mutate(classeEta= "Totale")

Fasceeta2020_2022_Reg_Tot = aggregate(Value ~ (Anno+Territorio), Fasceeta2020_2022_Reg,sum)
Fasceeta2020_2022_Reg_Tot = Fasceeta2020_2022_Reg_Tot%>%
  mutate(classeEta= "Totale")
FascEtaItRegioneTotale = full_join(Fasceeta2002_2019_Reg_Tot, Fasceeta2020_2022_Reg_Tot)

#dataframe dati regioni con i dati temporali di tutti gli anni con media
Fasceeta2002_2019_Reg_Medie = aggregate(Value ~ (Territorio), Fasceeta2002_2019_Reg,mean)
Fasceeta2002_2019_Reg_Medie = Fasceeta2002_2019_Reg_Medie %>%
  arrange(desc(Value))

Fasceeta2020_2022_Reg_Medie = aggregate(Value ~ (Territorio), Fasceeta2020_2022_Reg,mean)
Fasceeta2020_2022_Reg_Medie = Fasceeta2020_2022_Reg_Medie %>%
  arrange(desc(Value))


FascEtaItRegioneMedia = full_join(Fasceeta2002_2019_Reg_Medie, Fasceeta2020_2022_Reg_Medie)
FascEtaItRegioneMedia = aggregate(Value ~ (Territorio), FascEtaItRegioneMedia,mean)

#Sezione Lombardia
Lombardia = FascEtaRegione%>%
  filter( Territorio=="Lombardia")%>%
  filter((Classe.di.eta>=0) & (Classe.di.eta<100))%>%
  arrange(Anno)
Lombardia_Classe.di.eta = aggregate(Value ~ (Anno+Territorio+Classe.di.eta), Lombardia,sum)
Lombardia_Reg = aggregate(Value ~ (Anno+Territorio), Lombardia,sum)
Lombardia_Reg = Lombardia_Reg %>%
  mutate(classeEta= "Totale")%>%
  select(Anno, Value, Territorio)

#Sezione Lombardia nati
Nati_Lombardia=Lombardia_Classe.di.eta %>%
  filter(Classe.di.eta==0)%>%
  select(Anno, Value, Territorio)
summary(Nati_Lombardia$Value)

Nat_Lom <- Nati_Lombardia %>%
  ggplot(aes(x=Anno, y=Value)) +
  geom_point(size=.6,color="red")+
  geom_line(color="#69b3a2", size=1, alpha=0.9)+
  geom_smooth(method=lm , color="red",fill="#69b3a2", se=TRUE)+
  scale_y_continuous(labels = label_comma())
Nat_Lom

#Sezione Lombardia nati incremento_percentuale
Nati_Lombardia$incremento_percentuale <- NA
for (i in 2:nrow(Nati_Lombardia)) {
  Nati_Lombardia$incremento_percentuale[i] <- (Nati_Lombardia$Value[i] - Nati_Lombardia$Value[i-1]) / Nati_Lombardia$Value[i-1] * 100
}

#Sezione Campania
Campania = FascEtaRegione%>%
  filter( Territorio=="Campania")%>%
  filter((Classe.di.eta>=0) & (Classe.di.eta<100))%>%
  arrange(Anno)
Campania_Classe.di.eta = aggregate(Value ~ (Anno+Territorio+Classe.di.eta), Campania,sum)
Campania_Reg = aggregate(Value ~ (Anno+Territorio), Campania,sum)
Campania_Reg = Campania_Reg %>%
  mutate(classeEta= "Totale")%>%
  select(Anno, Value, Territorio)

#Sezione Campania nati
Nati_Campania=Campania_Classe.di.eta %>%
  filter(Classe.di.eta==0)%>%
  select(Anno, Value, Territorio)
summary(Nati_Campania$Value)

Nat_Cam <- Nati_Campania %>%
  ggplot(aes(x=Anno, y=Value)) +
  geom_point(size=.6,color="red")+
  geom_line(color="#69b3a2", size=1, alpha=0.9)+
  geom_smooth(method=lm , color="red",fill="#F1948A", se=TRUE, se_color="red")+
  scale_y_continuous(labels = label_comma())
Nat_Cam

#Sezione Campania nati incremento_percentuale
Nati_Campania$incremento_percentuale <- NA
for (i in 2:nrow(Nati_Campania)) {
  Nati_Campania$incremento_percentuale[i] <- (Nati_Campania$Value[i] - Nati_Campania$Value[i-1]) / Nati_Lombardia$Value[i-1] * 100
}

#Sezione Lazio
Lazio = FascEtaRegione%>%
  filter( Territorio=="Lazio")%>%
  filter((Classe.di.eta>=0) & (Classe.di.eta<100))%>%
  arrange(Anno)
Lazio_Classe.di.eta = aggregate(Value ~ (Anno+Territorio+Classe.di.eta), Lazio,sum)
Lazio_Reg = aggregate(Value ~ (Anno+Territorio), Lazio,sum)
Lazio_Reg = Lazio_Reg %>%
  mutate(classeEta= "Totale")%>%
  select(Anno, Value, Territorio)

#Sezione Lazio nati
Nati_Lazio=Lazio_Classe.di.eta %>%
  filter(Classe.di.eta==0)%>%
  select(Anno, Value, Territorio)
summary(Nati_Lazio$Value)

Nat_Laz <- Nati_Lazio %>%
  ggplot(aes(x=Anno, y=Value)) +
  geom_point(size=.6,color="red")+
  geom_line(color="#69b3a2", size=1, alpha=0.9)+
  geom_smooth(method=lm , color="red",fill="#69b3a2", se=TRUE)+
  scale_y_continuous(labels = label_comma())
Nat_Laz

#Sezione Lazio nati incremento_percentuale
Nati_Lazio$incremento_percentuale <- NA
for (i in 2:nrow(Nati_Lazio)) {
  Nati_Lazio$incremento_percentuale[i] <- (Nati_Lazio$Value[i] - Nati_Lazio$Value[i-1]) / Nati_Lazio$Value[i-1] * 100
}

#Sezione Sicilia
Sicilia = FascEtaRegione%>%
  filter( Territorio=="Sicilia")%>%
  filter((Classe.di.eta>=0) & (Classe.di.eta<100))%>%
  arrange(Anno)
Sicilia_Classe.di.eta = aggregate(Value ~ (Anno+Territorio+Classe.di.eta), Sicilia,sum)
Sicilia_Reg = aggregate(Value ~ (Anno+Territorio), Sicilia,sum)
Sicilia_Reg = Sicilia_Reg %>%
  mutate(classeEta= "Totale")%>%
  select(Anno, Value, Territorio)

#Sezione Sicilia nati
Nati_Sicilia=Sicilia_Classe.di.eta %>%
  filter(Classe.di.eta==0)%>%
  select(Anno, Value, Territorio)
summary(Nati_Sicilia$Value)

Nat_Sic <- Nati_Sicilia %>%
  ggplot(aes(x=Anno, y=Value)) +
  geom_point(size=.6,color="red")+
  geom_line(color="#69b3a2", size=1, alpha=0.9)+
  geom_smooth(method=lm , color="red",fill="#69b3a2", se=TRUE)+
  scale_y_continuous(labels = label_comma())
Nat_Sic

#Sezione Sicilia nati incremento_percentuale
Nati_Sicilia$incremento_percentuale <- NA
for (i in 2:nrow(Nati_Sicilia)) {
  Nati_Sicilia$incremento_percentuale[i] <- (Nati_Sicilia$Value[i] - Nati_Sicilia$Value[i-1]) / Nati_Sicilia$Value[i-1] * 100
}

#Sezione Veneto
Veneto = FascEtaRegione%>%
  filter( Territorio=="Veneto")%>%
  filter((Classe.di.eta>=0) & (Classe.di.eta<100))%>%
  arrange(Anno)
Veneto_Classe.di.eta = aggregate(Value ~ (Anno+Territorio+Classe.di.eta), Veneto,sum)
Veneto_Reg = aggregate(Value ~ (Anno+Territorio), Veneto,sum)
Veneto_Reg = Veneto_Reg %>%
  mutate(classeEta= "Totale")%>%
  select(Anno, Value, Territorio)

#Sezione Veneto
Veneto = FascEtaRegione%>%
  filter( Territorio=="Veneto")%>%
  filter((Classe.di.eta>=0) & (Classe.di.eta<100))%>%
  arrange(Anno)
Veneto_Classe.di.eta = aggregate(Value ~ (Anno+Territorio+Classe.di.eta), Veneto,sum)
Veneto_Reg = aggregate(Value ~ (Anno+Territorio), Veneto,sum)
Veneto_Reg = Veneto_Reg %>%
  mutate(classeEta= "Totale")%>%
  select(Anno, Value, Territorio)

#Sezione Veneto nati
Nati_Veneto=Veneto_Classe.di.eta %>%
  filter(Classe.di.eta==0)%>%
  select(Anno, Value, Territorio)
summary(Nati_Veneto$Value)


Nat_Ven <- Nati_Veneto %>%
  ggplot(aes(x=Anno, y=Value)) +
  geom_point(size=.6,color="red")+
  geom_line(color="#69b3a2", size=1, alpha=0.9)+
  geom_smooth(method=lm , color="red",fill="#69b3a2", se=TRUE)+
  scale_y_continuous(labels = label_comma())
Nat_Ven

#Sezione Veneto nati incremento_percentuale
Nati_Veneto$incremento_percentuale <- NA
for (i in 2:nrow(Nati_Veneto)) {
  Nati_Veneto$incremento_percentuale[i] <- (Nati_Veneto$Value[i] - Nati_Veneto$Value[i-1]) / Nati_Veneto$Value[i-1] * 100
}


#Dato di crescità annua delle nascite regionale
Nati_Campione_Regione= full_join(Nati_Lombardia, Nati_Lazio)
Nati_Campione_Regione= full_join(Nati_Campione_Regione, Nati_Campania)
Nati_Campione_Regione= full_join(Nati_Campione_Regione, Nati_Sicilia)
Nati_Campione_Regione= full_join(Nati_Campione_Regione, Nati_Veneto)



new_order <- with(Nati_Campione_Regione, reorder(Nati_Campione_Regione$Territorio , Nati_Campione_Regione$incremento_percentuale, median , na.rm=T))
boxplot(Nati_Campione_Regione$incremento_percentuale ~ new_order , ylab="Incremento percentuale" ,xlab = "Regioni", col="tomato",  main="Incremento percentuale della natalità")
summary(Nati_Campione_Regione$incremento_percentuale)

#Nuovi nati italia 
Nati_Italia = FascEtaIt %>%
  filter(Classe.di.eta==0)
Nati_Italia = aggregate(Value ~ (Anno+Territorio), Nati_Italia,sum)
Nati_Italia$incremento_percentuale <- NA
for (i in 2:nrow(Nati_Italia)) {
  Nati_Italia$incremento_percentuale[i] <- (Nati_Italia$Value[i] - Nati_Italia$Value[i-1]) / Nati_Italia$Value[i-1] * 100
}
Nat_It <- Nati_Italia %>%
  ggplot(aes(x=Anno, y=Value)) +
  geom_point(size=.6,color="red")+
  geom_line(color="#69b3a2", size=1, alpha=0.9)+
  geom_smooth(method=lm , color="red",fill="#69b3a2", se=TRUE)+
  scale_y_continuous(labels = label_comma())
Nat_It

FascEtaIt=drop_na(FascEtaIt)
Popolazione_Totale_Italiana = aggregate(Value ~ (Anno+Territorio), FascEtaIt,sum)


Nati_Campione_Regione= full_join(Nati_Campione_Regione, Nati_Italia)
new_order <- with(Nati_Campione_Regione, reorder(Nati_Campione_Regione$Territorio , Nati_Campione_Regione$incremento_percentuale, median , na.rm=T))
Natalita<-boxplot(Nati_Campione_Regione$incremento_percentuale ~ new_order , ylab="Incremento percentuale" ,xlab = "Regioni", col="light blue",  main="Incremento percentuale della natalita")
Natalita

Pop_It_Tot <- Popolazione_Totale_Italiana %>%
  ggplot(aes(x=Anno, y=Value)) +
  geom_point(size=.6,color="red")+
  geom_line(color="#69b3a2", size=1, alpha=0.9)+
  scale_y_continuous(labels = label_comma())
Pop_It_Tot

etaMediaGenitori<-c(34.3,30.5, 34.4,30.7, 34.6,30.7, 34.6,30.8, 34.7,30.9, 34.8,31.0, 34.8,31.0, 
                    34.8,31.1, 34.9,31.2, 35.0,31.3, 35.0,31.3, 35.1,31.4, 35.2,31.5, 35.3,31.7,
                    35.3,31.8, 35.4,31.9, 35.5,32.0, 35.6,32.1, 35.5,32.2, 35.8,32.4)

SessoIt<-c("Maschi","Femmine", "Maschi","Femmine", "Maschi","Femmine", "Maschi","Femmine", "Maschi","Femmine",
           "Maschi","Femmine", "Maschi","Femmine", "Maschi","Femmine", "Maschi","Femmine", "Maschi","Femmine",
           "Maschi","Femmine", "Maschi","Femmine", "Maschi","Femmine", "Maschi","Femmine", "Maschi","Femmine",
           "Maschi","Femmine", "Maschi","Femmine", "Maschi","Femmine", "Maschi","Femmine", "Maschi","Femmine")

Anni<-c(2002,2002, 2003,2003, 2004,2004, 2005,2005, 2006,2006,
        2007,2007, 2008,2008, 2009,2009, 2010,2010, 2011,2011,
        2012,2012, 2013,2013, 2014,2014, 2015,2015, 2016,2016,
        2017,2017, 2018,2018, 2019,2019, 2020,2020, 2021,2021)

Eta_media_dei_genitori_alla_nascita<-data.frame(etaMediaGenitori,SessoIt,Anni)
Eta_media_del_padre_alla_nascita=Eta_media_dei_genitori_alla_nascita%>%
  filter(SessoIt=="Maschi")%>%
  ggplot(aes(x=Anni, y=etaMediaGenitori))+
  geom_line(color="#69b3a2", size=1, alpha=0.9)


Eta_media_della_madre_alla_nascita = Eta_media_dei_genitori_alla_nascita%>%
  filter(SessoIt=="Femmine")%>%
  ggplot(aes(x=Anni, y=etaMediaGenitori))+
  geom_line(color="#69b3a2", size=1, alpha=0.9)

Eta_media_della_madre_alla_nascita

Emigrazione_Giovani = Emigrazione%>%
  filter(Eta=="18-39 anni")%>%
  ggplot(aes(x=Anno, y=Value))+
  geom_point(size=.6,color="red")+
  geom_line(color="#69b3a2", size=1, alpha=0.9)+
  geom_smooth(method=lm , color="red",fill="#69b3a2", se=TRUE)+
  scale_y_continuous(labels = label_comma())

Emigrazione_Giovani
Emigrazione$incremento_percentuale <- NA
for (i in 2:nrow(Emigrazione)) {
  Emigrazione$incremento_percentuale[i] <- (Emigrazione$Value[i] - Emigrazione$Value[i-1]) / Emigrazione$Value[i-1] * 100
}
