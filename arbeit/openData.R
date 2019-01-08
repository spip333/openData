##########################################################
# openData	
##########################################################



#===============================
# 1. Laden Sie den Datensatz gemeindedaten.csv 

getwd()
gemeindedaten.raw <- read.table("./material/gemeindedaten.csv", sep = ",", fileEncoding = "UTF-8")
?read.table
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

