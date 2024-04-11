#Arbetisblatt zur Fixed Effekts Regression von df_all_cases.

#Einladen des plm-Packages für Linear Models mit Panel Data.
#install.packages("plm")
library(plm)

#Vorbereiten der Daten:
#Umwandlung des data.frame in ein pdata.frame, um Indexe für Zeit und die Länder zu spezifizieren:
pdf_all_cases = pdata.frame(df_all_cases, index = c("country_text_id", "year"))

### Formel:
vollmodell_libdem = v2x_libdem ~ EconFree_Index + Gini_Index + e_gdppc + gdppc_growth + Wealth_10by50 + Income_10by50 + e_pop + e_regionpolstr


##### Regressionsmodelle #####

### Fully Pooled Model ###
plm.fpooled = plm(vollmodell_libdem, 
                  data = pdf_all_cases, model = "pooling")
summary(plm.fpooled)



### Fixed Effects Time FE(C) ###
plm.FE.C = plm(update(vollmodell_libdem, .~. + country_name ), data = pdf_all_cases, model = "pooling")
summary(plm.FE.C)


### Fixed Effects Time FE(T) ###
plm.FE.T = plm(update(vollmodell_libdem, .~. + year), data = pdf_all_cases, model = "pooling")
summary(plm.FE.T)


### Fixed Effects Country & Time FE(CT) ###
plm.FE.CT = plm(update(vollmodell_libdem, .~. + year + country_name), data = pdf_all_cases, model = "pooling")
summary(plm.FE.CT)


### First Differences  ###
plm.FD = plm(vollmodell_libdem, 
             data = pdf_all_cases, model = "fd")
summary(plm.FD)
#geht auch mit Zeitdummys





##### Tests #####

pFtest()


### Autokorrelation
acf()




