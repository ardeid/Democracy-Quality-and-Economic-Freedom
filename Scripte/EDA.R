#Arbeitsblatt zur Exploritory Data Analysis (EDA) von df_all_cases und Subsets.


df = readRDS(file.path("Data", "rds", "df_all_cases.rds"))
OECD_ISO3 = readRDS(file.path("Data", "rds", "OECD_ISO3.rds"))

##### Tabellen #####

### Tabellen zur Übersicht: ###
#Fälle pro Region:
table(df_all_cases$e_regionpolstr)
#Fälle pro Land:
table(df_all_cases2$country_name)
#Alle Jahre, für die Daten vorhanden sind:
table(df_all_cases$year)
bewerben(unique(df_all_cases$year))

##### Graphen #####

### Univariate Daten zur übersicht: ###
#Falle x Zeit
df_all_cases %>% ggplot(aes(year)) + geom_bar(width = 1) + labs(title = "Verteilung der Fälle über Zeit") + scale_x_continuous(n.breaks = 16)

#Fälle X Zeit für OECD DD1 Fälle
df_all_cases %>% filter(df_all_cases$country_text_id %in% OECD_ISO3 & df_all_cases$e_chga_demo != 0) %>%
  ggplot(aes(year)) + geom_bar(width = 1) + labs(title = "Verteilung der Fälle über Zeit") + 
  scale_x_continuous(n.breaks = 16)
### GGF e_chga_demo für aktuellste fälle selbst nachcodieren?? #######

### Bivariate Graphen zur Übersicht: ###
df_all_cases2 %>% ggplot(aes(v2x_libdem, Economic_Freedom_Summary_Index)) + geom_density_2d_filled()
df_all_cases2 %>% ggplot(aes(v2x_libdem, Gini_Index)) + geom_density2d_filled()
df_all_cases2 %>% ggplot(aes(Economic_Freedom_Summary_Index, Gini_Index)) + geom_density2d_filled()
df_all_cases2 %>% group_by(e_regionpol) %>% ggplot(aes(v2x_libdem, Economic_Freedom_Summary_Index, color = e_regionpol)) + geom_point(alpha = 0.3) + facet_wrap(.~ e_regionpol, ncol = 5,scales = "free_x") + theme(legend.position = "none") + labs(x = "Liberal Democracy Index", y = "Economic Freedom of the World Index", caption = "Ein Punkt ist ein Land zu einem bestimmten Jahr")
df_all_cases2 %>% group_by(e_regionpol) %>% ggplot(aes(v2x_polyarchy , Economic_Freedom_Summary_Index, color = e_regionpol)) + geom_point(alpha = 0.3) + facet_wrap(.~ e_regionpol, ncol = 5, scales = "free_x") + theme(legend.position = "none") + labs(x = "Electoral Democracy Index", y = "Economic Freedom of the World Index", caption = "Ein Punkt ist ein Land zu einem bestimmten Jahr") 

#Testen, ob die V-Dem GDP per Capita und die Weltbank GDP per Capita gleich sind.
df_all_cases %>% ggplot(aes(e_gdppc, GDPpc_WB)) + geom_smooth() + geom_point(alpha = 0.2)


### Korrelationstabellen für Unterscheidliche Subsets: ###

### Korrelationstabellen mit Graphen

#Für alle Fälle
df_all_cases %>% ggpairs(columns = c(3:10, 14,15, 18, 21, 29), aes(alpha = 0.1), title = "All Cases")

#Für Demokratien, nach dem DD-Index:
df_all_cases %>% filter(e_chga_demo == 1) %>% ggpairs(columns = c(3:10, 14,15, 18, 21, 29), aes(alpha = 0.1), title = "DD-Index = 1")

#Für nicht Demokratien, nach dem DD-Index:
df_all_cases %>% filter(e_chga_demo == 0) %>% ggpairs(columns = c(3:10, 14,15, 18, 21, 29), aes(alpha = 0.1), title = "DD-Index = 0")

#Für Länder der OECD
df_all_cases %>% filter(country_text_id %in% OECD_ISO3) %>% ggpairs(columns = c(3:10, 14,15, 18, 21, 29), aes(alpha = 0.1), title = "OECD")

#Für Länder der OECD und DD-1
df_all_cases %>% filter(country_text_id %in% OECD_ISO3 & e_chga_demo == 1) %>% ggpairs(columns = c(3:10, 14,15, 18, 21, 29), aes(alpha = 0.1), title = "OECD & DD-1")

#Nach Regionen; 
df_all_cases %>% filter(e_regionpolstr %in% c("Western Europe and North America", "Eastern Europe and post Soviet Union")) %>% ggpairs(columns = c(4:7, 11, 12, 15, 18, 25,26), aes(color = e_regionpolstr, alpha = 0.5))

### Korrelationstabellen ohne Graphen, dafür Bunt.
#Alle Fälle
df_all_cases %>% select(c(3:10, 14,15, 18, 21, 29)) %>% ggcorr(name = "All Cases")

#Für Demokratien, nach dem DD-Index:
df_all_cases %>% filter(e_chga_demo == 1) %>% select(c(3:10, 14,15, 18, 21, 29)) %>% ggcorr(name = "DD-Index = 1")

#Für nicht Demokratien, nach dem DD-Index:
df_all_cases %>% filter(e_chga_demo == 0) %>% select(c(3:10, 14,15, 18, 21, 29)) %>% ggcorr(name = "DD-Index = 0")

#Für Länder der OECD
df_all_cases %>% filter(country_text_id %in% OECD_ISO3) %>% select(c(3:10, 14,15, 18, 21, 29)) %>% ggcorr(name = "OECD")

#Für Länder der OECD und DD-1
df_all_cases %>% filter(country_text_id %in% OECD_ISO3 & e_chga_demo == 1) %>% select(c(3:10, 14,15, 18, 21, 29)) %>% ggcorr(name = "OECD & DD-Index = 1")


##### Regressionsmodelle OLS #####
modell_1 = lm(v2x_libdem ~ v2x_polyarchy, data = df_all_cases2)
summary(modell_1)


modell_libdemxEFOTW = lm(data = df_all_cases, v2x_libdem ~ EconFree_Index)
summary(modell_libdemxEFOTW)
plot(modell_libdemxEFOTW, 4)
df_all_cases2[c(110, 730, 827),]
plot(modell_libdemxEFOTW, 5)

#Linear Modell für alle Cases & alle Variablen
lm2 = lm(v2x_libdem ~ EconFree_Index + Gini_Index + e_gdppc + `GDP per capita growth (annual %)`+ Wealth_10by50 + Income_10by50 + e_pop, na.action = na.exclude, data = df_all_cases[-c(3089, 3491, 4233),])

vif(lm1)
summary(lm2)
summary((lm1))

df_all_cases[c(3089, 3491, 4233),]

plot(lm2, 4)
plot(lm1, 3)
plot(ggpredict(lm1, terms = c("EconFree_Index")))
plot(ggpredict(lm1, terms = c("Wealth_10by50")))
plot(ggpredict(lm1, terms = c("Income_10by50")))
plot(ggpredict(lm1, terms = c("Gini_Index")))


### DD-1 Kodierte Länder: ###
df_DD_1 = df_all_cases %>% filter(e_chga_demo == 1)
#Nullmodell
lmDD_1_b = lm(v2x_libdem ~ EconFree_Index, na.action = na.omit, data = df_DD_1)
#Vollmodell
lmDD_1 = lm(v2x_libdem ~ EconFree_Index + Gini_Index + e_gdppc + `GDP per capita growth (annual %)`+ Wealth_10by50 + Income_10by50 + e_pop, na.action = na.omit, data = df_DD_1)

### OECD Länder: ###
df_OECD = df_all_cases %>% filter(country_text_id %in% OECD_ISO3)

#Nullmodell
lmOECD_b = lm(v2x_libdem ~ EconFree_Index, na.action = na.omit, data = df_OECD)
#Vollmodell:
lmOECD = lm(v2x_libdem ~ EconFree_Index + Gini_Index + e_gdppc + `GDP per capita growth (annual %)`+ Wealth_10by50 + Income_10by50 + e_pop, na.action = na.omit, data = df_OECD)

#Übersichts-Regressionstabelle für DD-1 und OECD als Null- und Vollmodell
stargazer(lmOECD_b, lmOECD, lmDD_1_b,lmDD_1, type = "text", 
          dep.var.labels = "Liberal Democracy Index (LDI)", 
          column.labels = c("OEDC Länder", " ", "DD = 1 Länder", " "),
          covariate.labels = c("Constant", "Economical Freedom of the World Index", "GINI Index", "GDP per Capita", "GDP per Capita growth (annual %)", "Wealth Inequality 10%/50%", "Income Inequality 10%/50%", "Population"), intercept.top = TRUE, intercept.bottom = FALSE)





LDI_plot <- df_LD %>% ggplot(aes(v2x_libdem, EconFree_Index)) + geom_point()
LIB_plot <- df_LD %>% ggplot(aes(v2x_liberal, EconFree_Index)) + geom_point()
p3 <- df_LD %>% ggplot(aes(v2x_egal, EconFree_Index)) + geom_point()
p4 <- df_LD %>% ggplot(aes(v2x_partip, EconFree_Index)) + geom_point()
p5 <- df_LD %>% ggplot(aes(v2xdl_delib, EconFree_Index)) + geom_point()
p6 <- df_LD %>% ggplot(aes(v2x_polyarchy, EconFree_Index)) + geom_point()
library(ggpubr)
ggarrange(LDI_plot, LIB_plot)