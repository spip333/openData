##########################################################
# openData	
##########################################################

library(dplyr)


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

# Import mit Setzen Encoding = UTF-8 is korrekt

# Prüfen, ob <na> oder NULL vorkommen:

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
# <na> - Werte kommen vor

#===============================
# 3. Falls nötig, definieren Sie im dataframe fehlende Werte so,
# dass R diese tatsächlich als fehlende Werte auffasst.

# => gemacht über import

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

# STARKE Vereinfachung über die mittlere Wachstum. Ist mathematisch FALSCH, ermöglicht eine 
# Annäherung.
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
# bev_dichte, bev_ausl, alter_0_19, alter_20_64, alter_65,bevbew_geburt, 
# sozsich_sh, strafen_stgb Gibt es Korrelationen? Falls ja, lassen Sie sich erklären 
# oder sind sie eher unerwartet? Suchen Sie sich einen Ihnen interessant erscheinenden 
# Zusammenhang und schauen Sie sich diesen in einem eigenen Scatterplot an
cordata <- gemeindedaten.raw %>%
  dplyr::select(bev_dichte, bev_ausl, alter_0_19, alter_20_64, alter_65., bevbew_geburt,sozsich_sh, strafen_stgb )

library(corrplot)
mcor<-cor(cordata)
corrplot(mcor)

# interessante Korrelation: bev_dichte vs bev_ausl, bev_ausl vs Strafen_stgb, sozsich_sh vs bev_dichte

#===============================
# 9. Visualisieren Sie eine Kontingenztabelle mit den Variablen Stadt_Land und Sprachregionen. 
# Welcher Gemeindetyp überwiegt bei deutschsprachigen Gemeinden, 
# welcher bei italienischsprachigen Gemeinden. Gibt es in jeder Sprachregion isolierte Städte?
lang.typ <- gemeindedaten.raw %>%
  dplyr::select(sprachregionen, stadt_land) %>% 
  group_by(sprachregionen, stadt_land) %>%
  summarise(n=n()) %>%
  arrange(sprachregionen, desc(n))

xtabs(lang.typ$n ~ lang.typ$sprachregionen + lang.typ$stadt_land, data=lang.typ)

#===============================
# 10. Erstellen Sie ein politisches Profil nach Sprachregionen mit der Hilfe der 
# Variablen zu den Wähleranteilen.
polit <- gemeindedaten.raw %>%
  dplyr::select(sprachregionen, 
                bev_total, 
                polit_svp, 
                polit_sp, 
                polit_fdp, 
                polit_cvp, 
                polit_gps, 
                polit_evp, 
                polit_glp, 
                polit_bdp) %>% 
  group_by(sprachregionen) %>%
  summarise(pop = sum(bev_total), 
            svp = round(sum( polit_svp * bev_total / 100, na.rm=T) / pop,3),
            sp = round(sum(polit_sp * bev_total / 100, na.rm=T) / pop,3),
            fdp = round(sum(polit_fdp * bev_total / 100, na.rm=T) / pop,3),
            evp = round(sum(polit_evp * bev_total / 100, na.rm=T) / pop,3),
            gps = round(sum(polit_gps * bev_total / 100, na.rm=T) / pop,3),
            bdp = round(sum(polit_bdp * bev_total / 100, na.rm=T) / pop,3),
            glp = round(sum(polit_glp * bev_total / 100, na.rm=T) / pop,3),
            cvp = round(sum( polit_cvp * bev_total / 100, na.rm = T) / pop,3))

polit
