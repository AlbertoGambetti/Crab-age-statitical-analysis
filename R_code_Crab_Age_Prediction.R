# R project - Biometria e Statistica/Statistical Analysis and Modelling
# Group: Giacomo Brasini, Alberto Gambetti e Giovanni Teodori 
# Dataset Link: https://www.kaggle.com/datasets/sidhus/crab-age-prediction

#Installazione dei pacchetti ----
#install.packages("tidyverse")
#install.packages("measurements")
##Caricamento dei pacchetti necessari----
library(tidyverse)
library(measurements)     #Il pacchetto measurements serve per fare le conversioni tra le unità di misura
#Importazione dataset ----
#importazione del dataset e assegnazione di questo ad una variabile
dat <- read_csv("data/CrabAgePrediction.csv")

#Commento sul dataset
#Comprende dati relativi a granchi appartenenti alla stessa specie, provenienti da un campionamento svolto nella zona di Boston.
#Il dataset è utile per stimare l'età del granchio basandosi su suoi attributi fisici,
# questo è importante, nell'ambito commerciale dell'allevamento dei granchi, per conoscere l'età in cui è più opportuna la loro raccolta.
#Descrizione delle variabili presenti nel dataset:
#Sex : Genere del granchio (maschio e femmina), (tipo dato : character)
#Length : Lunghezza del granchio, in piedi, (tipo dato : double)
#Diameter : Diametro del granchio, in piedi, (tipo dato : double)
#Heigth : Altezza del granchio, in piedi, (tipo dato : double)
#Weight : Peso del granchio, in once, (tipo dato : double)
#Shucked Weight : peso del granchio senza visceri e guscio, in once, (tipo dato : double)
#Viscera Weight: è il peso dei visceri addominali nelle profondità del corpo, in once, (tipo dato : double)
#Shell weight : Peso del guscio, in once, (tipo dato : double)
#Age : età del granchio, in mesi (tipo dato : double)

# Ispezione del dataset ----
names(dat)     #visualizzazione dei nomi dei caratteri(variabili)
nrow(dat)      #numero di osservazioni = 3893
ncol(dat)      #numero delle variabili fische dei granchi = 9
glimpse(dat)   #visualizzazione riassuntiva dei dati rispetto alle colonne
str(dat)       #controllo della struttura del dataset
dat

# Ispezione delle anomalie ----
sort(unique(dat$Height), decreasing = FALSE)    # elencare in ordine crescente i valori unici della variabile altezza 
unique(dat$Sex)       # elenco dei valori unici, assunti dalla variabile Sex

which(dat$Sex == "I")           # Trovare la posizione dei valori indeterminati ("I") assunti dalla variabile Sex
which(dat$Height == 0)          # Trovare la posizione dei valori = 0 assunti dalla variabile Altezza
which(dat$Height > 1)           # Controllo che non ci siano altezze anomale irrealisticamente grandi

dat[270, ]
dat[3868, ]
dat[749, ]
dat[2257, ]

# Rimozione dati errati ----
#utilizzo della funzione filter per la rimozione dei dati errati
dat <- filter(dat, Sex != "I")
dat <- filter(dat, Height < 1)

dat[, -(6:8)]    #rimozione colonne non ritenute interessanti per la nostra analisi dati
glimpse(dat)

# Modifica della tipologia del dato ---- 
#Trasformazione della variabile Sex da character a  fattore con 2 livelli 
dat <- dat %>% 
  mutate(Sex = as.factor(Sex))

# Controllo dati rimossi e modificati ----
str(dat)     #controllo che la tipoligia dei dati sia modificata
summary(dat) #controllo che i valori indeterminati siano stati rimossi  

# Correzione delle unità di misura e rinominazione delle variabili ----

dat <- dat %>% 
  mutate(Length = conv_unit(Length, "foot", "cm"),
         Diameter = conv_unit(Diameter, "foot", "cm"),
         Height = conv_unit(Height, "foot", "cm"),
         Weight = conv_unit(Weight, "oz", "g"),
         `Shucked Weight` = conv_unit(`Shucked Weight`, "oz", "g"),
         `Viscera Weight` = conv_unit(`Viscera Weight`, "oz", "g"),
         `Shell Weight` = conv_unit(`Shell Weight`, "oz", "g"),
  ) %>% 
  rename(`Length(cm)`= Length,
         `Diameter(cm)` = Diameter,
         `Height(cm)` = Height,
         `Weight(gr)`= Weight,
         `Shucked Weight(gr)` = `Shucked Weight`,
         `Viscera Weight(gr)` = `Viscera Weight`,
         `Shell Weight(gr)` = `Shell Weight`,
         `Age(month)` = Age)

glimpse(dat)   #controllo delle modifiche apportate

## Calcolo delle statistiche di sintesi della variabili fische dei granchi ----
head(dat)
summary(dat)   

# Visualizzazione dei dati ----

## Distribuzioni univariate ----

### Istogramma della variabile Età ----
#Esplorazione grafica della distribuzione univariata dell' Età suddivisa per Sesso 
histogramplot_age <- ggplot(data = dat) +
  geom_histogram(aes(x = `Age(month)`, color = Sex, fill = Sex),
                 binwidth = 1,
                 alpha = 0.3)+
  scale_x_continuous(breaks = seq(1,29, by = 1))+
  labs(title = "Age of the crabs",
       y = "Number of individuals",
       x = "Age in months")+ 
  theme_bw()
#Nel grafico sono visualizzati il numero di individui in corrispondenza della loro età.
#Si può intuire che l'età più rappresentate sono quelle dai 9 agli 11 mesi. 

histogramplot_age  #visualizzazione grafico

#Esportazione del grafico
ggsave("Grafici/histogramplot_age.png",
       plot = histogramplot_age,
       width = 12,
       height = 7)

### Grafico di densità per la variabile Lunghezza ----

densityplot_length <- ggplot(data = dat) +
  geom_density(aes(x = sqrt(`Length(cm)`), fill = Sex, color = Sex),
               alpha = 0.3)+
  scale_x_continuous(breaks = seq(0,10, by = 0.5))+
  labs(title = "Lenght of the crabs",
       y = "Kernel density estimate",
       x = "Lenght in cm")+ 
  theme_bw()
#Distribuzione della Lunghezza dei granchi attraverso un grafico di densità
#Si può dedurre che la lunghezza più rappresentata è tra 6,5 cm e 7 cm, e non si nota una 
#grande variazione della lunghezza nei due sessi.
densityplot_length    #visualizzazione grafico

#Esportazione grafico
ggsave("Grafici/densityplot_length.png",
       plot = densityplot_length,
       width = 12,
       height = 7)

### Grafico di densità per la variabile  Diametro ----

densityplot_diameter <- ggplot(data = dat) +
  geom_density(aes(x = `Diameter(cm)`, fill = Sex, color = Sex),
               alpha = 0.3)+
  scale_x_continuous(breaks = seq(0,50, by = 2))+
  labs(title = "Diameter of the crabs",
       y = "Kernel density estimate",
       x = "Diameter in cm")+ 
  theme_bw()
#Distribuzione del Diametro dei granchi attraverso un grafico di densità.
#Si può intuire che la maggior parte di individui ha un diametro tra  24 cm e 44cm. 
#Non si nota una grande variazione del diametro nei due sessi.
densityplot_diameter  #visualizzazione grafico 

#Esportazione grafico
ggsave("Grafici/densityplot_diameter.png",
       plot = densityplot_diameter,
       width = 12,
       height = 7)

### Grafico di densità per la variabile  Altezza ----

densityplot_height <- ggplot(data = dat) +
  geom_density(aes(x = sqrt(`Height(cm)`), fill = Sex, color = Sex),
               alpha = 0.3)+
  scale_x_continuous(breaks = seq(0,10, by = 0.5))+
  labs(title = "Heigt of the crabs",
       y = "Kernel density estimate",
       x = "Square root of the heigt in cm")+ 
  theme_bw()
#Distribuzione dell'Altezza dei granchi attraverso un grafico di densità
#Si può dedurre che la maggior parte di individui ha un'altezza  tra  9 cm e 16cm. 
#Non si nota una grande variazione dell'altezza nei due sessi.
densityplot_height      #visualizzazione grafico

#Esportazione grafico
ggsave("Grafici/densityplot_height.png",
       plot = densityplot_height,
       width = 12,
       height = 7)

### Grafico di densità per la variabile  Peso ----

densityplot_weight <- ggplot(data = dat) +
  geom_density(aes(x = `Weight(gr)`, fill = Sex, color = Sex),
               alpha = 0.3)+
  scale_x_continuous(breaks = seq(0,3000, by = 100))+
  labs(title = "Weight of the crabs",
       y = "Kernel density estimate",
       x = "Weight in gr")+ 
  theme_bw()
#Distribuzione del Peso dei granchi attraverso un grafico di densità.
#Si può riscontrare che la maggior parte di individui ha un peso  tra 300 gr e 1500 gr,
#le femmine sembrano essere leggermente più pesanti dei maschi.
densityplot_weight    #visualizzzione grafico

#Esportazione grafico
ggsave("Grafici/densityplot_weight.png",
       plot = densityplot_weight,
       width = 12,
       height = 7)

dev.off()

## Distribuzioni bivariate ----
### Boxplot Età - Sesso ----

boxplot_agesex <- ggplot(data = dat) +
  geom_boxplot(aes(x = Sex,
                   y = `Age(month)`,
                   color = Sex,
                   fill = Sex),
               alpha = 0.3)+
  labs(y = "Age in month",
       x = "Sex")+
  theme_bw()
#Distribuzione dell'Età rispetto alla variabile Sesso tramite boxplot.
#Si deduce che l'età sia distribuita in modo uguale tra i maschi e le femmine.
boxplot_agesex   #visualizzazione grafico

#Esportazione grafico
ggsave("Grafici/boxplot_agesex.png",
       plot = boxplot_agesex,
       width = 12,
       height = 7)

### Scatter plot Età - Lunghezza ----

jitterplot_lenght <- ggplot(data = dat) +
  geom_jitter(aes(x = `Length(cm)`,
                  y = `Age(month)`,
                  color = Sex),
              alpha = 0.3)+
  geom_smooth(aes(x = `Length(cm)`,
                  y = `Age(month)`),
              alpha = 0.3)+
  theme_bw()
#Distribuzione dell'Età rispetto alla variabile Lunghezza tramite uno scatter plot.
#Si può intuire che all'aumentare della lunghezza corrisponde un aumento dell'età.

jitterplot_lenght    #visualizzazione grafico

#Esportazione grafico
ggsave("Grafici/jitterplot_lenght.png",
       plot = jitterplot_lenght,
       width = 12,
       height = 7)

###Scatter plot Età-Diametro ----

jitterplot_diameter <- ggplot(data = dat) +
  geom_jitter(aes(x = `Diameter(cm)`,
                  y = `Age(month)`,
                  color = Sex),
              alpha = 0.3)+
  geom_smooth(aes(x = `Diameter(cm)`,
                  y = `Age(month)`),
              alpha = 0.3)+
  theme_bw()
#Distribuzione dell'Età rispetto alla variabile Diametro tramite uno scatter plot.
#Si può dedurre che all'aumentare del diametro corrisponde un aumento dell'età.

jitterplot_diameter     #visualizzazione grafico

#Esportazione grafico
ggsave("Grafici/jitterplot_diameter.png",
       plot = jitterplot_diameter,
       width = 12,
       height = 7)

### Scatter plot Età-Altezza ----

jitterplot_height <- ggplot(data = dat) +
  geom_jitter(aes(x = `Height(cm)`,
                  y = `Age(month)`,
                  color = Sex),
              alpha = 0.3)+
  geom_smooth(aes(x = `Height(cm)`,
                  y = `Age(month)`),
              alpha = 0.3)+
  theme_bw()
#Distribuzione dell'Età rispetto alla variabile Altezza tramite uno scatter plot.
#Si può intuire che all'aumentare dell'altezza corrisponde un incremento dell'età.

jitterplot_height      #visualizzazione grafico

#Esportazione grafico
ggsave("Grafici/jitterplot_height.png",
       plot = jitterplot_height,
       width = 12,
       height = 7)

###Scatter plot Età-Peso ----

jitterplot_weight <- ggplot(data = dat) +
  geom_jitter(aes(x = `Weight(gr)`,
                  y = `Age(month)`,
                  color = Sex),
              alpha = 0.3)+
  geom_smooth(aes(x = `Weight(gr)`,
                  y = `Age(month)`),
              alpha = 0.3)+
  theme_bw()
#Distribuzione dell'Età rispetto alla variabile Peso tramite uno scatter plot.
#Si può dedurre che all'aumentare dell'altezza corrisponde un aumento dell'età.
jitterplot_weight   #visualizzazione grafico

#Esportazione grafico
ggsave("Grafici/jitterplot_weight.png",
       plot = jitterplot_weight,
       width = 12,
       height = 7)

dev.off()
# Formulazione e test dell'ipotesi ----

#Siamo a interessati a vedere se c'è correlazione tra l'età e i carratteri morfologici dei granchi in questione,
#in modo da poter stimare quando un granchio ha l'età giusta per essere raccolto

## Ipotesi di correlazione tra la lunghezza e l'età ----
# H1 -> esiste una correlazione tra l'età e la lunghezza dei granchi
# H0 -> non esiste correlazione tra le due variabili
# Per testare la nostra ipotesi applichiamo il test di correlazione (Pearson)
cor.test(dat$`Age(month)`,dat$`Length(cm)`)

## Ipotesi di correlazione tra l'altezza  e l'età ----
# H1 -> esiste una correlazione tra l'età e l'altezza dei granchi
# H0 -> non esiste correlazione tra le due variabili
# Per testare la nostra ipotesi applichiamo il test di correlazione (Pearson)
cor.test(dat$`Age(month)`,dat$`Height(cm)`)

## Ipotesi di correlazione tra il diametro e l'età ----
# H1 -> esiste una correlazione tra l'età e il diametro dei granchi
# H0 -> non esiste correlazione tra le due variabili
# Per testare la nostra ipotesi applichiamo il test di correlazione (Pearson)
cor.test(dat$`Age(month)`,dat$`Diameter(cm)`)

## Ipotesi di correlazione tra il peso e l'età ----
# H1 -> esiste una correlazione tra l'età e il peso dei granchi
# H0 -> non esiste correlazione tra le due variabili
# Per testare la nostra ipotesi applichiamo il test di correlazione (Pearson)
cor.test(dat$`Age(month)`,dat$`Weight(gr)`)

#Possiamo concludere che le variabili "Lunghezza", "Diametro", "Altezza" e " Peso" 
#sono  ciascuna, correlata significativamente con l'età, con un coefficiente di correlazione (cor)
#positivo e un p-value = 2.2e-16 il quale è più piccolo del livello
#significativo alpha(0.01), quindi scartiamo  l'ipotesi H0.

#Costruzione dei modelli di regressione lineare ----
lm <- lm(`Age(month)`~`Length(cm)`, data = dat)
summary(lm)


lm <- lm(`Age(month)`~`Height(cm)`, data = dat)
summary(lm)

lm <- lm(`Age(month)`~`Diameter(cm)`, data = dat)
summary(lm)

lm <- lm(`Age(month)`~`Weight(gr)`, data = dat)
summary(lm)

#In tutti i modelli il p-value è molto basso (2e-16 ***), questo viene indicato con tre stelline e  
#ciò mi porta a rifiutare fortemente H0, quindi che ci sia
#una forte correlazione tra le due variabili, usate in ciascun modello di regressione lineare, le quali quindi, possono essere rappresentate da esso.



