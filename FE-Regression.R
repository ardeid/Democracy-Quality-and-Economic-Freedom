#Arbetisblatt zur Fixed Effekts Regression von df_all_cases.

#Einladen des plm-Packages für Linear Models mit Panel Data.
#install.packages("plm")
library(plm)
library(tidyverse)
library(stargazer)

df = readRDS(file.path("Data", "rds", "df_all_cases.rds"))
OECD_ISO3 = readRDS(file.path("Data", "rds", "OECD_ISO3.rds"))

#### Vorbereiten der Daten: ####

#Welche Fälle von OECD Ländern die Chebub et al nicht als Demokratien klassifizieren gibt es?
df$year <-  as.character(df$year)

#Ausgewählt: Liberale und Elektorale Demokratien die OECD Länder sind. 
df_LD <- df %>% filter(v2x_regime >= 2 & country_text_id %in% OECD_ISO3)



#### Formeln: ####
#Liberal-Democracy-Index
LDI_voll = v2x_libdem ~ EconFree_Index + Gini_Index + GDPpc_WB + gdppc_growth + Wealth_10by50 + Income_10by50 + pop_WB

## Component Indices
#Liberal Component
LIB_voll = v2x_liberal ~ EconFree_Index + Gini_Index + GDPpc_WB + gdppc_growth + Wealth_10by50 + Income_10by50 + pop_WB

#Egalitarian Component
EGAL_voll = v2x_egal ~ EconFree_Index + Gini_Index + GDPpc_WB + gdppc_growth + Wealth_10by50 + Income_10by50 + pop_WB

#Participative Component
PART_voll = v2x_partip ~ EconFree_Index + Gini_Index + GDPpc_WB + gdppc_growth + Wealth_10by50 + Income_10by50 + pop_WB

#Deliberative Component
DELIB_voll = v2xdl_delib ~ EconFree_Index + Gini_Index + GDPpc_WB + gdppc_growth + Wealth_10by50 + Income_10by50 + pop_WB

#Institutional Component
POLY_voll = v2x_polyarchy ~ EconFree_Index + Gini_Index + GDPpc_WB + gdppc_growth + Wealth_10by50 + Income_10by50 + pop_WB


######### Regressionsmodelle ##########

######## Fixed Effekts ########

#### Fully Pooled Model ####

#Liberal-Democracy Index
LDI_pooled <- lm(LDI_voll, data = df_LD)

#Liberal Component
LIB_pooled <- lm(LIB_voll, data = df_LD)

#Egalitarian Component
EGAL_pooled <- lm(EGAL_voll, data = df_LD)

#Participative Component
PART_pooled <- lm(PART_voll, data = df_LD)

#Deliberative Component
DELIB_pooled <- lm(DELIB_voll, data = df_LD)

#Institutional Component
POLY_pooled <- lm(POLY_voll, data = df_LD)

## Tabellen
# Alle Modelle außer LDI
stargazer(LIB_pooled, EGAL_pooled, PART_pooled, DELIB_pooled, POLY_pooled, 
          type = "text",
          dep.var.caption = "V-Dem Component Indices:",
          star.char = c(".", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          decimal.mark = ",",
          digit.separator = ".",
          notes.label = "Legende:",
          notes.append = FALSE,
          notes = ".p<0,1; *p<0,05; **p<0,01; ***p<0,001",
          add.lines = list(c("Fixed Effects: Land", "nein", "nein", "nein", "nein", "nein"), 
                           c("Fixed Effects: Jahr", "nein", "nein", "nein", "nein", "nein")))

# Modell zum LDI -> Zusammengesetzt aus LIB und POLY
stargazer(LDI_pooled, type = "text")


#### FE Year ####

#Liberal-Democracy Index
LDI_fe_y <- lm(update(LDI_voll, ~. + year), data = df_LD)

#Liberal Component
LIB_fe_y <- lm(update(LIB_voll, ~. + year), data = df_LD)

#Egalitarian Component
EGAL_fe_y <- lm(update(EGAL_voll, ~. + year), data = df_LD)

#Participative Component
PART_fe_y <- lm(update(PART_voll, ~. + year), data = df_LD)

#Deliberative Component
DELIB_fe_y <- lm(update(DELIB_voll, ~. + year), data = df_LD)

#Institutional Component
POLY_fe_y <- lm(update(POLY_voll, ~. + year), data = df_LD)


## Tabellen
# Alle Modelle außer LDI
stargazer(LIB_fe_y, EGAL_fe_y, PART_fe_y, DELIB_fe_y, POLY_fe_y, 
          type = "text", 
          omit = names(table(df_LD$year)),
          dep.var.caption = "V-Dem Component Indices:",
          star.char = c(".", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          decimal.mark = ",",
          digit.separator = ".",
          notes.label = "Legende:",
          notes.append = FALSE,
          notes = ".p<0,1; *p<0,05; **p<0,01; ***p<0,001",
          add.lines = list(c("Fixed Effects: Land", "nein", "nein", "nein", "nein", "nein"), 
                           c("Fixed Effects: Jahr", "ja", "ja", "ja", "ja", "ja"))
          )
#### FE Country ####

#Liberal-Democracy Index
LDI_fe_c <- lm(update(LDI_voll, ~. + country_name), data = df_LD)

#Liberal Component
LIB_fe_c <- lm(update(LIB_voll, ~. + country_name), data = df_LD)

#Egalitarian Component
EGAL_fe_c <- lm(update(EGAL_voll, ~. + country_name), data = df_LD)

#Participative Component
PART_fe_c <- lm(update(PART_voll, ~. + country_name), data = df_LD)

#Deliberative Component
DELIB_fe_c <- lm(update(DELIB_voll, ~. + country_name), data = df_LD)

#Institutional Component
POLY_fe_c <- lm(update(POLY_voll, ~. + country_name), data = df_LD)

#Tabelle für alle außer LDI
stargazer(LIB_fe_c, EGAL_fe_c, PART_fe_c, DELIB_fe_c, POLY_fe_c, 
          type = "text", 
          omit = names(table(df_LD$country_name)),
          dep.var.caption = "V-Dem Component Indices:",
          star.char = c(".", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          decimal.mark = ",",
          digit.separator = ".",
          notes.label = "Legende:",
          notes.append = FALSE,
          notes = ".p<0,1; *p<0,05; **p<0,01; ***p<0,001",
          add.lines = list(c("Fixed Effects: Land", "ja", "ja", "ja", "ja", "ja"), 
                           c("Fixed Effects: Jahr", "nein", "nein", "nein", "nein", "nein"))
          )


#### FE Year-Country ####

#Liberal-Democracy Index
LDI_fe_c_y <- lm(update(LDI_voll, ~. + country_name + year), data = df_LD)

#Liberal Component
LIB_fe_c_y <- lm(update(LIB_voll, ~. + country_name + year), data = df_LD)

#Egalitarian Component
EGAL_fe_c_y <- lm(update(EGAL_voll, ~. + country_name + year), data = df_LD)

#Participative Component
PART_fe_c_y <- lm(update(PART_voll, ~. + country_name + year), data = df_LD)

#Deliberative Component
DELIB_fe_c_y <- lm(update(DELIB_voll, ~. + country_name + year), data = df_LD)

#Institutional Component
POLY_fe_c_y <- lm(update(POLY_voll, ~. + country_name + year), data = df_LD)


#Tabelle für alle außer LDI
stargazer(LIB_fe_c_y, EGAL_fe_c_y, PART_fe_c_y, DELIB_fe_c_y, POLY_fe_c_y, 
          type = "text", 
          omit = c(names(table(df_LD$country_name)), names(table(df_LD$year))),
          dep.var.caption = "V-Dem Component Indices:",
          star.char = c(".", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          decimal.mark = ",",
          digit.separator = ".",
          notes.label = "Legende:",
          notes.append = FALSE,
          notes = ".p<0,1; *p<0,05; **p<0,01; ***p<0,001",
          add.lines = list(c("Fixed Effects: Land", "ja", "ja", "ja", "ja", "ja"), 
                           c("Fixed Effects: Jahr", "ja", "ja", "ja", "ja", "ja"))
)



######## Random Effekts ########

#Nötig?
