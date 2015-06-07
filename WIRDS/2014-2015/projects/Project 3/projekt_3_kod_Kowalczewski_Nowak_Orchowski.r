library(dplyr)
library(tidyr)
library(ggplot2)
library(sjPlot)
library(scales)


#Ustawienie working Directory. 
#Należy podać lokalizację repozytorium Dydaktyka/WIRDS
setwd("E:/Pliki/Studia/Wizualizacje i raportowanie/Dydaktyka/WIRDS/2014-2015")

#wczytujemy workspace z danymi z matur
load("./datasets/matury_kraj.RData")

#Oczyszczanie braków danych dla dysleksi oraz zmiana wartości T i F

matury_mod <- matury %>% filter(dysleksja!= 'NA')
matury_mod <- matury_mod %>% mutate(dysleksja = factor(dysleksja,
                                                       levels = c('FALSE', 'TRUE'),
                                                       labels = c('Bez dysleksji',
                                                                  'Dyslektyktycy')))
matury_mod <- matury_mod %>% mutate(matura_nazwa = as.factor(matura_nazwa))

#nowa zmienna wynik % (matura z geog. na 60 pkt, pozostałe na 50)
matury_mod$wynik_procent <- ifelse(matury_mod$matura_nazwa !='geografia rozszerzona',matury_mod$wyniki_matur/50*100, matury_mod$wyniki_matur/60*100)


#wstępne rozpoznanie danych

sjt.xtab(matury_mod$dysleksja, matury_mod$matura_nazwa,
         encoding = 'cp1250',
         stringTotal = "Ogółem",
         variableLabels = c("Dysleksja","Wynik matura"),
         showLegend = F,
         highlightTotal = T,
         showCellPerc = T,
         showRowPerc = T,
         showColPerc = T)

#Zestawienie średnich wyników dla dyslektyków i piszących bez dysleksji ogółem

sjt.grpmean(matury_mod$wynik_procent, matury_mod$dysleksja,
            encoding = 'cp1250')

#Struktura płci dyslektyków
matury_plec <- matury_mod %>%
  select(plec, rok, dysleksja) %>% 
  filter(plec !='NA') %>%
  group_by(plec, rok) %>%
  summarize(licz = n())

matury_plec_dyslektycy <- matury_mod %>%
  select(plec, rok, dysleksja) %>%
  filter(dysleksja != 'Bez dysleksji') %>%
  group_by(plec, rok) %>% 
  summarize(licz = n())

struktura_dysleksji <- matury_plec_dyslektycy$licz/matury_plec$licz *100

matury_plec_dyslektycy$struktura <- struktura_dysleksji

ggplot(data= matury_plec_dyslektycy,
       aes(x=rok,
           y=struktura,
           fill=plec,
           ymax = 100))+
  geom_bar(stat='identity',
           position='dodge',
           colour='black') + 
  theme_bw()+
  ylab('Odsetek dyslektyków (%)')+
  xlab('Rok')+
  scale_fill_discrete(name='Uczniowie:')+
  ggtitle('Odsetek dyslektyków wg płci wśród piszących matur w latach 2010-2014')+
  scale_y_continuous(limits=c(0,20))+
  geom_text(aes(label = round(struktura, digits=2)), 
            position = position_dodge(width=0.90), vjust=-0.4, size=4.5)

#Średnie dla płci
sjt.grpmean(matury_mod$wynik_procent, as.factor(matury_mod$plec),
            encoding = 'cp1250')

#Wykres wyników dyslektyków w poszczególnych latach

matury_mod %>%
  select(rok, dysleksja, wynik_procent) %>%
  group_by(rok, dysleksja) %>%
  summarize(srednia=mean(wynik_procent, na.rm=T),
            liczebnosc = n()) %>%
  ggplot(data=.,
         aes(x=rok,
             y=srednia,
             fill=dysleksja,
             ymax = 100))+
  geom_bar(stat='identity',
           position='dodge',
           colour='black')+ 
  theme_bw()+
  ylab('Średni wynik (%)')+
  xlab('Rok')+
  scale_fill_discrete(name='Uczniowie:')+
  ggtitle('Średni wynik wybranych matur uczniów z dysleksją i bez dysleksjiw latach 2010-2014')+
  scale_y_continuous(limits=c(0,100))+
  geom_text(aes(label = round(srednia, digits=2)), 
            position = position_dodge(width=0.90), vjust=-0.4, size=4.5) +
  geom_hline(aes(yintercept = 30))


#Średnie w latach
sjt.grpmean(matury_mod$wynik_procent, as.factor(matury_mod$rok),
            encoding = 'cp1250')

#Wykres wyników poszcz. matur dla dyslektyków i piszących bez dysleksji
matury_mod %>%
  select(matura_nazwa, dysleksja, wynik_procent) %>%
  group_by(matura_nazwa, dysleksja) %>%
  summarize(srednia=mean(wynik_procent, na.rm=T),
            liczebnosc = n()) %>%
  ggplot(data=.,
         aes(x=matura_nazwa,
             y=srednia,
             fill=dysleksja,
             ymax = 100))+
  geom_bar(stat='identity',
           position='dodge',
           colour='black')+ 
  theme_bw()+
  ylab('Średni wynik (%)')+
  xlab('Przedmiot')+
  scale_fill_discrete(name='Uczniowie:')+
  ggtitle('Średni wynik matur z poszczególnych przedmiotów uczniów z dysleksją i bez dysleksji')+
  scale_y_continuous(limits=c(0,100))+
  geom_text(aes(label = round(srednia, digits=2)), 
            position = position_dodge(width=0.90), vjust=-0.4, size=4.5) +
  geom_hline(aes(yintercept = 30))

#Średnie z poszczególnych matur
sjt.grpmean(matury_mod$wynik_procent, as.factor(matury_mod$matura_nazwa),
            encoding = 'cp1250')

#histogramy, funkcje gęstości

matury_mod %>%
  count(wyniki_matur, dysleksja) %>%
  group_by(dysleksja)%>%
  mutate(procent = n/sum(n))%>%
  ggplot (data=., aes(x=wyniki_matur, y=procent, fill=dysleksja))+
  geom_bar(stat='identity', position='identity', col='black')+
  facet_wrap(~dysleksja)+
  xlab("Wyniki matur (pkt.)")+
  scale_y_continuous("Procent maturzystów", labels = percent) +
  ggtitle('Histogramy wyników matur uczniów z dysleksją i bez dysleksji')+
  theme_bw()+
  scale_fill_discrete(name="Uczniowie:")

matury_mod %>%
  group_by(dysleksja)%>%
  ggplot (data=., aes(x=wynik_procent,fill=dysleksja)) + geom_density(alpha=0.3)+
  scale_x_continuous(limits=c(0,100))+
  theme_bw()+
  xlab("Wyniki matur (%)")+
  ylab("Gęstość")+
  ggtitle("Funkcja gęstości wyników matur uczniów z dysleksją i bez dysleksji")+
  scale_fill_discrete(name="Uczniowie:")
