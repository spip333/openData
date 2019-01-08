##########################################################
# Recherche	
##########################################################

#===============================
#library(dplyr)
#library(foreign)
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
# 4. Beantworten Sie folgende Fragen: Wie viele Gemeinden gab es in der Schweiz 
# im Jahr 2014? Was ist die mittlere Einwohnerzahl einer Schweizer Gemeinde? 
# Wie viele Einwohner leben in der grössten Gemeinde? Wie viele in der kleinsten?

