##########################################################
# Recherche	
##########################################################

#===============================
library(dplyr)
library(foreign) # remove
#library(stringr)

#===============================
# 1. Laden Sie den Datensatz gemeindedaten.csv 
gemeindedaten.raw <- read.table("./material/gemeindedaten.csv", 
                                header=TRUE, 
                                sep = "," , 
                                na.strings = c("NA","*"),
                                fileEncoding = "UTF-8")

#===============================
# 2. Verschaffen Sie sich einen ersten Überblick zu den Daten? 
# Hat mit dem Einlesen alles wie erwartet geklappt?
# Sind die Daten so kodiert, wie Sie es erwarten? 
# Stimmen die Datenformate der Variablen?
# Gibt es fehlende Werte und wie sind diese kodiert?

head(gemeindedaten.raw)
str(gemeindedaten.raw)
nrow(gemeindedaten.raw)
ncol(gemeindedaten.raw)
colnames(gemeindedaten.raw)


null <- 0
na <- 0
for (i in 1:nrow(gemeindedaten.raw)){
  paste(" row : " , i)
  for (j in 1:ncol(gemeindedaten.raw)){
    if (is.na(gemeindedaten.raw[i,j])){
      na <- na + 1
    }
    if (is.null(gemeindedaten.raw[i,j])){
      null <- null + 1
    }
  }
}
print(paste("na: " ,na, " - null:", null))

#===============================
# 3. Falls nötig, definieren Sie im dataframe fehlende Werte so,
# dass R diese tatsächlich als fehlende Werte auffasst.

# gemacht über import

#===============================
# 4. Beantworten Sie folgende Fragen:
# - Wie viele Gemeinden gab es in der Schweiz im Jahr 2014? 
nrow(gemeindedaten.raw)
# - Was ist die mittlere Einwohnerzahl einer Schweizer Gemeinde? 
mean(gemeindedaten.raw$bev_total)
# - Wie viele Einwohner leben in der grössten Gemeinde? 
max(gemeindedaten.raw$bev_total)
# - Wie viele in der kleinsten?
min(gemeindedaten.raw$bev_total)

#===============================
# 5. In welchem Kanton gibt es am meisten Gemeinden? In welchem am wenigsten?
# "select kanton, count(gemeinde) from gemeinden group by kanton"

# notes
gemeindedaten.raw[gemeindedaten.raw$kantone == "BS",]$gmdename
str(gemeindedaten.raw)
tmp [tmp[,2] ==max(tmp[,2]),]

# loesung
gemeindedaten.raw %>%
  group_by(kantone) %>%
  summarise (anz_gemeinden = n()) %>%
  filter(anz_gemeinden == max(anz_gemeinden))

gemeindedaten.raw %>%
  group_by(kantone) %>%
  summarise (anz_gemeinden = n()) %>%
  filter(anz_gemeinden == min(anz_gemeinden))

#===============================
# 6. Betrachten Sie die Einwohnerzahlen der Gemeinden gruppiert nach Sprachregionen. 
# Wie heissen die jeweils grössten Gemeinden?

gemeindedaten.raw %>%
  group_by(sprachregionen) %>%
  filter(rank(desc(bev_total))==1) %>%
  dplyr::select(sprachregionen, gmdename, bev_total)
  

gemeindedaten.raw %>%
  group_by(sprachregionen) %>%
  filter(rank(bev_total)==1) %>%
  dplyr::select(sprachregionen, gmdename, bev_total)

#===============================
# 7. Betrachten Sie die Veränderung der Einwohnerzahl von 2010 bis 2014 nach 
# Sprachregionen. In welcher Sprachregionen sind die Gemeinden am stärksten gewachsen? 
# In welcher am wenigsten oder gibt es Sprachregionen, in welcher die Einwohnerentwicklung
# in der Tendenz sogar eher rückläufig ist? 
# Analysieren sie zusätzlich graphisch, 
# ob die Unterscheidung von städtischen und ländlichen Gemeinden dabei eine Rolle spielt?
gemeindedaten.raw %>%
  group_by(sprachregionen) %>%
  summarise (mean(bev_1014))


# Barplot
library(ggplot2)

gde.growth <- gemeindedaten.raw %>%
  group_by(sprachregionen, stadt_land) %>%
  summarise (growth = mean(bev_1014))

gde.growth

plot.new()

p <- ggplot(gde.growth, 
            aes(gde.growth$stadt_land,
                gde.growth$growth))
p +  geom_bar(stat = "identity", 
              aes(fill=gde.growth$sprachregionen), 
              position="dodge") +
  ggtitle("Bevölkerungswachstum 2010-14 nach Sprachregion und Gemeindetypen") +
  xlab("Gemeindetypen") + 
  ylab("Bevölkerungswachstum 2010-14 (%)") +
  labs(fill = "Sprachregionen")

#===============================
# 8. Untersuchen Sie die Zusammenhangsstruktur folgender Variablen:
# bev_dichte, bev_ausl, alter_0_19, alter_20_64, alter_65, bevbew_geburt, 
# sozsich_sh, strafen_stgb Gibt es Korrelationen? Falls ja, lassen Sie sich erklären 
# oder sind sie eher unerwartet? Suchen Sie sich einen Ihnen interessant erscheinenden 
# Zusammenhang und schauen Sie sich diesen in einem eigenen Scatterplot an

tmp <- gemeindedaten.raw %>%
  dplyr::select(bev_dichte, bev_ausl, alter_0_19, alter_20_64, alter_65., bevbew_geburt,sozsich_sh, strafen_stgb )

library(corrplot)
mcor<-cor(tmp)
corrplot(mcor)

# interessante Korrelation: bev_dichte vs bev_ausl, bev_ausl vs Strafen_stgb, sozsich_sh vs bev_dichte
