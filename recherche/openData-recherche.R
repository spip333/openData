##########################################################
# Recherche	
##########################################################

#===============================
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
  dplyr::select(bev_dichte, bev_ausl, alter_0_19, alter_20_64, alter_65.,sozsich_sh, strafen_stgb )

cor1  <- gemeindedaten.raw %>%
  dplyr::select(bev_dichte, bev_ausl,sozsich_sh, strafen_stgb )

cor2  <- gemeindedaten.raw %>%
  dplyr::select(alter_0_19, alter_20_64, alter_65. )

ggpairs(cor1, 1:4)
ggpairs(cor2, 1:3)


library(corrplot)
mcor<-cor(tmp)
corrplot(mcor)

# interessante Korrelation: bev_dichte vs bev_ausl, bev_ausl vs Strafen_stgb, sozsich_sh vs bev_dichte
head(tmp)

ggpairs(tmp, 1:2)

library(gcookbook) # Für Daten
library(GGally)
help(countries)

# Wir untersuchen, wie Wirtschaftswachstum (GDP), Erwerbsquote(laborrate)
# Gesundheitsausgaben (healthexp) und Kindersterblichkeit (infmortality)
# korrelieren

head(countries)


# Erneut wird zuerst ein Subset für das Jahr 2009 erstellt
c2009<-countries %>%
  filter(Year==2009) %>%
  select(c(-Code,-Year))

# Die Funktion ggpairs() ist ausgezeichnet für Scatterplot-Matrizen
# Erstellen Sie eine Scatterplot/Korrelations-Matrix aller Variablen des reduzierten 
# Datensatzes c2009 (Achtung: schliessen Sie die Variable "Name" aus)
ggpairs(c2009[,2:5])

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
#
#===============================
# 10. Erstellen Sie ein politisches Profil nach Sprachregionen mit der Hilfe der 
# Variablen zu den Wähleranteilen.
gemeindedaten.raw %>%
  dplyr::select(sprachregionen , polit_fdp, polit_cvp, polit_gps) %>% 
  group_by(sprachregionen) %>%
  summarise(mean(polit_gps, na.rm = T))

gemeindedaten.raw %>%
  dplyr::select(sprachregionen, bev_total, polit_fdp) %>% 
  group_by(sprachregionen) %>%
  summarise(sum(bev_total), sum(polit_fdp * bev_total / 100, na.rm=T))



gemeindedaten.raw %>%
  dplyr::select(sprachregionen, bev_total, polit_fdp, polit_cvp, polit_svp) %>% 
  mutate(
    sum_fdp = polit_fdp * bev_total / 100, 
    sum_svp = polit_svp * bev_total / 100, 
    sum_cvp = polit_cvp * bev_total / 100) %>%
  group_by(sprachregionen) %>%
  summarise(bev = sum(bev_total), 
            fdp = sum(sum_fdp), 
            svp = sum(sum_svp), 
            cvp = sum(sum_cvp))

tmp <- gemeindedaten.raw %>%
  dplyr::group_by(sprachregionen) %>%
  dplyr::select(sprachregionen, bev_total,
                polit_fdp, polit_cvp, polit_gps, polit_svp, polit_bdp, polit_sp) %>% 
  mutate(
    sum_fdp = polit_fdp * bev_total / 100, 
    sum_gps = polit_gps * bev_total / 100, 
    sum_bdp = polit_bdp * bev_total / 100, 
    sum_svp = polit_svp * bev_total / 100, 
    sum_sp = polit_sp * bev_total / 100, 
    sum_cvp = polit_cvp * bev_total / 100) %>%
    summarise(x = sum(bev_total))

            sum(tmp$sum_cvp, na.rm = T),
            sum(tmp$sum_gps, na.rm = T),
            sum(tmp$sum_svp, na.rm = T),
            sum(tmp$sum_sp, na.rm = T),
            sum(tmp$sum_bdp, na.rm = T),
            sum(tmp$sum_fdp, na.rm = T))

t <- c(sum(tmp$bev_total),
       sum(tmp$sum_cvp, na.rm = T),
       sum(tmp$sum_gps, na.rm = T),
       sum(tmp$sum_svp, na.rm = T),
       sum(tmp$sum_sp, na.rm = T),
       sum(tmp$sum_bdp, na.rm = T),
       sum(tmp$sum_fdp, na.rm = T))

t

gemeindedaten.raw %>%
  dplyr::select(sprachregionen, bev_total, polit_fdp, polit_gps, polit_bdp, polit_svp, polit_sp, polit_cvp, polit_gps) %>% 
  group_by(sprachregionen) %>%
  summarise(pop = sum(bev_total), 
            fdp = sum(polit_fdp * bev_total / 100, na.rm=T) / pop,
            gps = sum(polit_gps * bev_total / 100, na.rm=T) / pop,
            bdp = sum(polit_bdp * bev_total / 100, na.rm=T) / pop,
            svp = sum( polit_svp * bev_total / 100, na.rm=T) / pop,
            sp = sum(polit_sp * bev_total / 100, na.rm=T) / pop,
            cvp = sum( polit_cvp * bev_total / 100, na.rm = T) / pop)
  
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

d <- t(polit[1,3:9])
f <- t(polit[2,3:9])
i <- t(polit[3,3:9])
r <- t(polit[4,3:9])

parteien <- rownames(d)

pol <- data.frame(d)
pol$f <- f
pol$i <- i
pol$r <- r

p <- ggplot(pol,aes(parteien, pol$d))
p +  geom_bar(stat = "identity", 
              aes(fill=parteien), 
              position="dodge") +
  ggtitle("title") +
  xlab("Partei") + 
  ylab("Wähleranteil")
p <- ggplot(pol,aes(parteien, pol$f))
p +  geom_bar(stat = "identity", 
              aes(fill=parteien), 
              position="dodge") +
  ggtitle("Politische Präferenzen im Deutschsprachigen Raum") +
  xlab("Partei") + 
  ylab("Wähleranteil")

polit

p <- ggplot(polit,aes(parteien, polit[1,]))
p +  geom_bar(stat = "identity", 
              aes(fill=parteien), 
              position="dodge") +
  ggtitle("title") +
  xlab("Partei") + 
  ylab("Wähleranteil")

p <- ggplot(pol,aes(parteien, pol$f))
p +  geom_bar(stat = "identity", 
              aes(fill=parteien), 
              position="dodge") +
  ggtitle("Politische Präferenzen im Deutschsprachigen Regionen") +
  xlab("Partei") + 
  ylab("Wähleranteil")


p +  geom_bar(stat = "identity", 
              aes(fill=parteien), 
              position="dodge") +
  ggtitle("Bevölkerungswachstum 2010-14 nach Sprachregion und Gemeindetypen") +
  xlab("Gemeindetypen") + 
  ylab("Bevölkerungswachstum 2010-14 (%)")





gde.growth

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
```


