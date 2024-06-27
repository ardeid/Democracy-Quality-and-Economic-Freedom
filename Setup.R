#Arbeitsblatt zum konstruieren von df_all_cases und als Setup der Relevanten Packages und weiterer Daten.


##### Packages #####
#Nur beim Ersen mal
#install.packages("devtools")
#devtools::install_github("vdeminstitute/vdemdata")
#devtools::install_github("WIDworld/wid-r-tool")


library(vdemdata)
library(tidyverse)
library(foreign)
library(readxl)
library(stargazer)
library(wid)
library(ggeffects)
library(car)
library(GGally)


##### Daten Einladen #####

### Das Welt Bank Gini-Koeffizenten Dataset umformatieren: ###
#Datensatz einladen
Gini_Koeffizent <- read_excel("Data/World Bank Indicators/API_SI.POV.GINI_DS2_en_excel_v2_5994963.xlsx")

#Umformartieren: 1. Irrelevante Zeilen entfernen 2. Alle Werte des Gini Koeffizienten in eine Zeile bringen 3. NA´s entfernen.
#summary(WorldBankGini) # Die Jahre 1960, 1961, 1962 haben keine Werte.
Gini_Koeffizent = Gini_Koeffizent %>% 
  select(!c(`Indicator Name`, `Indicator Code`, "1960", "1961", "1963")) %>% 
  pivot_longer(cols = -c(`Country Name`, `Country Code`), names_to = "Year", values_to = "Gini_Index") %>% 
  filter(Gini_Index !=is.na(TRUE))

#Jahreszahlen in nummeric umwandeln. Wichtig fürs zusammenführen der Daten.
Gini_Koeffizent$Year = as.numeric(Gini_Koeffizent$Year)


### Weiteres Weltbank Dataset Einladen. ###
#Datensatz einladen
Weltbank_Indicators <- read_excel("Data/World Bank Indicators/P_Popular Indicators.xlsx", na = "..")

#Umformatieren: Werte Verschiedener Jahre in eine zeile bringen, Jahresszahlen definieren, Variablen Trennen und in Reihen ordnen.
Weltbank_Indicators = Weltbank_Indicators %>% 
  pivot_longer(cols = !c(`Series Name`, `Series Code`, `Country Name`, `Country Code`)) %>% 
  separate_wider_delim(cols = name, delim = " ", names = c("Year", "1")) %>% 
  select(`Country Name`, `Country Code`, Year, `Series Name`, value) %>% 
  mutate(value = as.numeric(value), Year = as.numeric(Year)) %>% 
  filter(!is.na(`Country Code`)) %>% 
  pivot_wider(names_from = `Series Name`, values_from = value) %>% 
  rename("gdppc_growth" = `GDP per capita growth (annual %)`, "gdp_growth" = `GDP growth (annual %)`)



### Weltbank GPD per Capita zur Kontrolle mit den V-Dem GDP per Capita ###
#Einladen
gdppc_Wordbank <- read_excel("Data/World Bank Indicators/gdppc_Wordbank.xlsx")

#Als Tidy Data Formatieren
gdppc_Wordbank = gdppc_Wordbank %>% pivot_longer(cols = !c(`Series Name`, `Series Code`, `Country Name`, `Country Code`)) %>% 
  separate_wider_delim(cols = name, delim = " ", names = c("Year", "1")) %>% 
  select(`Country Name`, `Country Code`, Year, `Series Name`, value) %>% 
  mutate(value = as.numeric(value), Year = as.numeric(Year)) %>% 
  filter(!is.na(`Country Code`)) %>% 
  pivot_wider(names_from = `Series Name`, values_from = value) %>%
  rename("GDPpc_WB" = `GDP per capita (constant 2015 US$)`) %>% 
  select(!`Country Name`)

### Weltbank Population weil V-Dem e_pop keine ordentlichen Werte liefert.
pop_WB <- read_delim("Data/World Bank Indicators/API_SP.POP.TOTL_DS2_en_csv_v2_633687.csv", 
                                                   delim = ";", escape_double = FALSE, na = "NA", 
                                                   trim_ws = TRUE)
#Als Tidy Data Formatieren
pop_WB = pop_WB %>% pivot_longer(cols = !c(`Indicator Name`, `Indicator Code`, `Country Name`, `Country Code`)) %>% 
  mutate(value = as.numeric(value), name = as.numeric(name)) %>% 
  filter(!is.na(`Country Code`)) %>% 
  rename("year" = name, "pop_WB" = value) %>% 
  select(!c(`Country Name`,`Indicator Name`, `Indicator Code`))



### Das Economic Freedom of the World Dataset umformatieren ###
#Daten für 1970 bis 2021 einladen
efotw_1970_2021 <- read_excel("Data/EFOTW/efotw-2023-master-index-data-for-researchers-iso.xlsx")

#Umbenennen des Index
efotw_1970_2021 = efotw_1970_2021 %>% 
  rename("EconFree_Index" = `Economic Freedom Summary Index`)
#Auswahl aus efotw_1970_2021 treffen
EconFreeOTWorld = efotw_1970_2021 %>% 
  select(Year, `ISO Code 2`, `ISO Code 3`, Countries, EconFree_Index)

#Daten für 1950 bis 1965 einladen
efotw_1950_1965 <- read_excel("Data/EFOTW/efotw-2023-master-index-data-for-researchers-iso.xlsx", 
                                                               sheet = "EFW Ratings 1950-1965")
#Variablennamen der Datensätze angleichen
efotw_1950_1965 = efotw_1950_1965 %>% 
  rename("Countries" = Country, 
         "EconFree_Index" = EFW)
#Zusammenfügen der beiden Datensätzen
EconFreeOTWorld = bind_rows(EconFreeOTWorld, efotw_1950_1965)
#Entfernen von Fehlenden Werten des ECFOTW Index
EconFreeOTWorld = EconFreeOTWorld %>% 
  filter(EconFree_Index != is.na(TRUE))

#efotw_1950-1965 hat keine ISO Code 3 Spalte. Diese zeile füllt die NA´s mit den passenden Ländercodes auf.
EconFreeOTWorld = EconFreeOTWorld %>% 
  group_by(Countries) %>% 
  fill(`ISO Code 3`,
       .direction = "down")

#Storage Managment
rm(efotw_1950_1965, efotw_1970_2021)


### Das V-Dem v13 Dataset Umfomatieren: ###
V_Dem_v13 <- vdem
#str(V_Dem_v13)
V_Dem_subset = V_Dem_v13 %>% 
  select(country_name, country_text_id, year, v2x_libdem, v2x_liberal, v2x_polyarchy, v2x_partip, v2xdl_delib, v2x_egal, 
                                    e_gdppc, e_pop, e_regionpol, e_chga_demo, v2x_regime)

#Politische Regionen als Namen in Variable e_regionpolstr zur Übersicht später.
V_Dem_subset$e_regionpolstr[V_Dem_subset$e_regionpol == 1] = "Eastern Europe and post Soviet Union"
V_Dem_subset$e_regionpolstr[V_Dem_subset$e_regionpol == 2] = "Latin America"
V_Dem_subset$e_regionpolstr[V_Dem_subset$e_regionpol == 3] = "North Africa and the Middle East"
V_Dem_subset$e_regionpolstr[V_Dem_subset$e_regionpol == 4] = "Sub–Saharan Africa"
V_Dem_subset$e_regionpolstr[V_Dem_subset$e_regionpol == 5] = "Western Europe and North America"
V_Dem_subset$e_regionpolstr[V_Dem_subset$e_regionpol == 6] = "Eastern Asia"
V_Dem_subset$e_regionpolstr[V_Dem_subset$e_regionpol == 7] = "South–Eastern Asia"
V_Dem_subset$e_regionpolstr[V_Dem_subset$e_regionpol == 8] = "Southern Asia"
V_Dem_subset$e_regionpolstr[V_Dem_subset$e_regionpol == 9] = "The Pacific"
V_Dem_subset$e_regionpolstr[V_Dem_subset$e_regionpol == 10] = "The Caribbean"

#Storage Managment
rm(V_Dem_v13)



### Das WID Dataset Umformartieren: ###
#Daten einladen mit dem WID R Package. (Aus der WID API)

#Einkommensdaten
inc = download_wid(indicators = "sptinc",
                   areas = "all",
                   years = 1950:2022,
                   perc = c("p0p50", "p90p100"),
                   ages = 992,
                   pop = "j")

#Vermögensdaten
wel = download_wid(indicators = "shweal",
                   areas = "all",
                   years = 1950:2022,
                   perc = c("p0p50", "p90p100"),
                   ages = 992,
                   pop = "j")

#Variablen umformen
inc = inc %>%
  pivot_wider(names_from = percentile, values_from = value) %>%
  rename("Income_Top10%_%" = p90p100, "Income_Bottom50%_%" = p0p50) %>%
  select(!variable) %>% mutate(Income_10by50 = `Income_Top10%_%`/`Income_Bottom50%_%`)

wel = wel %>%
  pivot_wider(names_from = percentile, values_from = value) %>%
  rename("Wealth_Top10%_%" = p90p100, "Wealth_Bottom50%_%" = p0p50) %>% 
  select(!variable) %>% 
  mutate(Wealth_10by50 = `Wealth_Top10%_%`/`Wealth_Bottom50%_%`)

#Variablen zusammenfügen
WID_Data = full_join(wel, inc,
                     join_by(x$year == y$year,
                             x$country == y$country))

#Storage Managment
rm(inc, wel)


##### df_all_cases Konstruieren #####
#Zusammenführen der Datensätzen zu einem. 
df_all_cases= full_join(V_Dem_subset, EconFreeOTWorld, 
                        join_by(x$year == y$Year, x$country_text_id == y$`ISO Code 3`))

df_all_cases = full_join(df_all_cases, Gini_Koeffizent, 
                         join_by(x$year == y$Year, x$country_text_id == y$`Country Code`))

df_all_cases = full_join(df_all_cases, WID_Data, 
                         join_by(x$year == y$year, x$`ISO Code 2` == y$country))

df_all_cases = full_join(df_all_cases, Weltbank_Indicators, 
                         join_by(x$year == y$Year, x$country_text_id == y$`Country Code`))

df_all_cases = full_join(df_all_cases, gdppc_Wordbank, 
                         join_by(x$year == y$Year, x$country_text_id == y$`Country Code`))

df_all_cases = full_join(df_all_cases, pop_WB, 
                         join_by(x$year == y$year, x$country_text_id == y$`Country Code`))

#Filtern des df nach Fehlenden Werten, Entfernen von Irrelevanten Variablen.
df_all_cases = df_all_cases %>% 
  select(!c(`ISO Code 2`, Countries, `Country Name.x`, `Country Name.y`)) %>% 
  filter(!is.na(EconFree_Index),
         !is.na(v2x_liberal))


##### Sonstige Daten ######
#ISO 3 Codes der OECD Länder.
OECD_ISO3 = c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NDL", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")



##### Environment Management #####
rm(EconFreeOTWorld, V_Dem_subset, WID_Data, Gini_Koeffizent, Weltbank_Indicators, gdppc_Wordbank, pop_WB)

##### Datensatz Speichern ####
saveRDS(df_all_cases, file = file.path("Data", "rds", "df_all_cases.rds"))
saveRDS(OECD_ISO3, file = file.path("Data", "rds", "OECD_ISO3.rds"))
