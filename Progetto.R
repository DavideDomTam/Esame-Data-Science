
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



#Normalizzazione dei csv "Fasce d'et? 2002-2019" e "Fasce d'et? 2019-2022" in un dataFrame che va dagli anni 2002 agli anni 2022
Fasceeta2002_2019 = read.csv("C:/Users/david/Desktop/Compiti/Universita/1 anno/R/Progetto 5/Capo.csv")
Fasceeta2002_2019$Classe.di.eta<-str_replace(Fasceeta2002_2019$Classe.di.eta, ' anni', '')


Fasceeta2020_2022=Fasceeta2020_2022 %>% filter(Territorio == "Italia") 
#Modifico la tipologia di dato da String a Integer
Fasceeta2002_2019$Classe.di.eta<-strtoi(Fasceeta2002_2019$Classe.di.eta) 
Fasceeta2020_2022$Classe.di.eta<-strtoi(Fasceeta2020_2022$Classe.di.eta) 

Fasceeta2002_2019 = Fasceeta2002_2019%>%
  filter( Territorio=="Italia")%>%
  select(Territorio,Classe.di.eta,Sesso,Anno,Value)

FascEta = full_join(Fasceeta2002_2019, Fasceeta2020_2022)


FasecEta0_4 = FascEta%>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=0) & (Classe.di.eta<5))) %>%
  arrange(Anno)
FasecEta0_4= aggregate(Value ~ (Sesso+Anno), FasecEta0_4,sum)
FasecEta0_4 = FasecEta0_4%>%
  mutate(classeEta= "00 - 04 Anni")


FasecEta5_9 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=5) & (Classe.di.eta<10)))%>%
  arrange(Anno)
FasecEta5_9= aggregate(Value ~ (Sesso+Anno), FasecEta5_9,sum)
FasecEta5_9 = FasecEta5_9%>%
  mutate(classeEta= "05 - 09 Anni")


FasecEta10_14 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=10) & (Classe.di.eta<15)))%>%
  arrange(Anno)
FasecEta10_14= aggregate(Value ~ (Sesso+Anno), FasecEta10_14,sum)
FasecEta10_14 = FasecEta10_14%>%
  mutate(classeEta= "10 - 15 Anni")


FasecEta15_19 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=15) & (Classe.di.eta<20)))%>%
  arrange(Anno)
FasecEta15_19= aggregate(Value ~ (Sesso+Anno), FasecEta15_19,sum)
FasecEta15_19 = FasecEta15_19%>%
  mutate(classeEta= "15 - 19 Anni")



FasecEta20_24 = FascEta%>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=20) & (Classe.di.eta<25))) %>%
  arrange(Anno, Classe.di.eta)
FasecEta20_24= aggregate(Value ~ (Sesso+Anno), FasecEta0_4,sum)
FasecEta20_24 = FasecEta20_24%>%
  mutate(classeEta= "20 - 24 Anni")


FasecEta25_29 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=25) & (Classe.di.eta<30)))%>%
  arrange(Anno, Classe.di.eta)
FasecEta25_29= aggregate(Value ~ (Sesso+Anno), FasecEta25_29,sum)
FasecEta25_29 = FasecEta25_29%>%
  mutate(classeEta= "25 - 29 Anni")


FasecEta30_34 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=30) & (Classe.di.eta<35)))%>%
  arrange(Anno)
FasecEta30_34= aggregate(Value ~ (Sesso+Anno), FasecEta30_34,sum)
FasecEta30_34 = FasecEta30_34%>%
  mutate(classeEta= "30 - 34 Anni")


FasecEta35_39 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=35) & (Classe.di.eta<40)))%>%
  arrange(Anno)
FasecEta35_39= aggregate(Value ~ (Sesso+Anno), FasecEta35_39,sum)
FasecEta35_39 = FasecEta35_39%>%
  mutate(classeEta= "35 - 39 Anni")


FasecEta40_44 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=30) & (Classe.di.eta<35)))%>%
  arrange(Anno)
FasecEta40_44= aggregate(Value ~ (Sesso+Anno), FasecEta40_44,sum)
FasecEta40_44 = FasecEta40_44%>%
  mutate(classeEta= "40 - 44 Anni")


FasecEta45_49 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=45) & (Classe.di.eta<50)))%>%
  arrange(Anno)
FasecEta45_49= aggregate(Value ~ (Sesso+Anno), FasecEta45_49,sum)
FasecEta45_49 = FasecEta45_49%>%
  mutate(classeEta= "45 - 49 Anni")


FasecEta50_54 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=50) & (Classe.di.eta<55)))%>%
  arrange(Anno)
FasecEta50_54= aggregate(Value ~ (Sesso+Anno), FasecEta50_54,sum)
FasecEta50_54 = FasecEta50_54%>%
  mutate(classeEta= "50 - 54 Anni")


FasecEta55_59 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=55) & (Classe.di.eta<60)))%>%
  arrange(Anno)
FasecEta55_59= aggregate(Value ~ (Sesso+Anno), FasecEta55_59,sum)
FasecEta55_59 = FasecEta55_59%>%
  mutate(classeEta= "55 - 59 Anni")


FasecEta60_64 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=60) & (Classe.di.eta<65)))%>%
  arrange(Anno)
FasecEta60_64= aggregate(Value ~ (Sesso+Anno), FasecEta60_64,sum)
FasecEta60_64 = FasecEta60_64%>%
  mutate(classeEta= "60 - 64 Anni")


FasecEta65_69 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=65) & (Classe.di.eta<70)))%>%
  arrange(Anno)
FasecEta65_69= aggregate(Value ~ (Sesso+Anno), FasecEta65_69,sum)
FasecEta65_69 = FasecEta65_69%>%
  mutate(classeEta= "65 - 69 Anni")


FasecEta70_74 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=70) & (Classe.di.eta<75)))%>%
  arrange(Anno)
FasecEta70_74= aggregate(Value ~ (Sesso+Anno), FasecEta70_74,sum)
FasecEta70_74 = FasecEta70_74%>%
  mutate(classeEta= "70 - 74 Anni")


FasecEta75_79 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=75) & (Classe.di.eta<80)))%>%
  arrange(Anno)
FasecEta75_79= aggregate(Value ~ (Sesso+Anno), FasecEta75_79,sum)
FasecEta75_79 = FasecEta75_79%>%
  mutate(classeEta= "75 - 79 Anni")


FasecEta80_84 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=80) & (Classe.di.eta<85)))%>%
  arrange(Anno)
FasecEta80_84= aggregate(Value ~ (Sesso+Anno), FasecEta80_84,sum)
FasecEta80_84 = FasecEta80_84%>%
  mutate(classeEta= "80 - 84 Anni")


FasecEta85_89 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=85) & (Classe.di.eta<90)))%>%
  arrange(Anno)
FasecEta85_89= aggregate(Value ~ (Sesso+Anno), FasecEta85_89,sum)
FasecEta85_89 = FasecEta85_89%>%
  mutate(classeEta= "85 - 89 Anni")


FasecEta90_94 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=90) & (Classe.di.eta<95)))%>%
  arrange(Anno)
FasecEta90_94= aggregate(Value ~ (Sesso+Anno), FasecEta90_94,sum)
FasecEta90_94 = FasecEta90_94%>%
  mutate(classeEta= "90 - 94 Anni")


FasecEta95_99 = FascEta %>%
  filter(mod(Anno,5)==1 & ((Classe.di.eta>=95) & (Classe.di.eta<100)))%>%
  arrange(Anno)
FasecEta95_99= aggregate(Value ~ (Sesso+Anno), FasecEta95_99,sum)
FasecEta95_99 = FasecEta95_99%>%
  mutate(classeEta= "95 - 99 Anni")

FascEtaCorretta = full_join(FasecEta0_4, FasecEta5_9)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta10_14)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta15_19)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta20_24)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta25_29)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta30_34)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta35_39)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta40_44)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta45_49)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta50_54)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta55_59)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta60_64)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta65_69)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta70_74)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta75_79)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta80_84)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta85_89)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta90_94)
FascEtaCorretta = full_join(FascEtaCorretta,FasecEta95_99)

#Plotto lq popolazione del 2002 - 2007 - 2012 - 2017 - 2022
#Anno2002 = 
DiffernzaPop2017_2012_25_29_30_34 = FascEtaCorretta$Value[FascEtaCorretta$Anno==2022 & FascEtaCorretta$Sesso=="maschi" & FascEtaCorretta$classeEta =="25 - 29 Anni"]-FascEtaCorretta$Value[FascEtaCorretta$Anno==2017 & FascEtaCorretta$Sesso=="maschi" & FascEtaCorretta$classeEta =="20 - 24 Anni"]
DiffernzaPop2017_2012_25_29_30_34
Popolazione_ITA=FascEtaCorretta %>%
  select(Anno,Value,classeEta,Sesso)%>%
  mutate( #
    population = ifelse(Sesso=="maschi", Value*(-1),
                        Value*1))%>%
  mutate(
    Eta=strtoi(substr(classeEta,1,2)))%>%
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
  labs(title="Popolazione italiana suddivisa per classi d'eta")+
  coord_flip()

Popolazione_ITA

Eta=strtoi(substr(FascEtaCorretta$classeEta,1,2))
color = 'green'
firstCharacter = substr(color,1,2)
