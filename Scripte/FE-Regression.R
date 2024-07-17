

################################################################################
#Arbetisblatt zur Fixed Effekts Regression von df_all_cases mit dem Vollmodell.#
################################################################################


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

###Transformationen 
#Democracy Indices
df_LD$v2x_libdem <- df_LD$v2x_libdem * 10
df_LD$v2x_liberal <- df_LD$v2x_liberal * 10
df_LD$v2x_egal <- df_LD$v2x_egal * 10
df_LD$v2x_partip <- df_LD$v2x_partip * 10
df_LD$v2xdl_delib <- df_LD$v2xdl_delib * 10
df_LD$v2x_polyarchy <- df_LD$v2x_polyarchy * 10

df_LD$GDPpc_WB <- df_LD$GDPpc_WB/1000




#### Formeln: ####
#Liberal-Democracy-Index
LDI_voll = v2x_libdem ~ EconFree_Index + Gini_Index + GDPpc_WB + gdppc_growth + Wealth_10by50 + Income_10by50

## Component Indices
#Liberal Component
LIB_voll = v2x_liberal ~ EconFree_Index + Gini_Index + GDPpc_WB + gdppc_growth + Wealth_10by50 + Income_10by50

#Egalitarian Component
EGAL_voll = v2x_egal ~ EconFree_Index + Gini_Index + GDPpc_WB + gdppc_growth + Wealth_10by50 + Income_10by50

#Participative Component
PART_voll = v2x_partip ~ EconFree_Index + Gini_Index + GDPpc_WB + gdppc_growth + Wealth_10by50 + Income_10by50

#Deliberative Component
DELIB_voll = v2xdl_delib ~ EconFree_Index + Gini_Index + GDPpc_WB + gdppc_growth + Wealth_10by50 + Income_10by50

#Institutional Component
POLY_voll = v2x_polyarchy ~ EconFree_Index + Gini_Index + GDPpc_WB + gdppc_growth + Wealth_10by50 + Income_10by50


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


### Plot
LDI_plot <- df_LD %>% 
  ggplot(aes(v2x_libdem, EconFree_Index)) + 
  geom_point() + scale_x_continuous(limits = c(0.4,1)) + labs(title = "RoW = 3")
LIB_plot <- df_LD %>% ggplot(aes(v2x_liberal, EconFree_Index)) + geom_point() + scale_x_continuous(limits = c(0.4,1))
EGAL_plot <- df_LD %>% ggplot(aes(v2x_egal, EconFree_Index)) + geom_point() + scale_x_continuous(limits = c(0.4,1))
PART_plot <- df_LD %>% ggplot(aes(v2x_partip, EconFree_Index)) + geom_point() + scale_x_continuous(limits = c(0.4,1))
DELIB_plot <- df_LD %>% ggplot(aes(v2xdl_delib, EconFree_Index)) + geom_point() + scale_x_continuous(limits = c(0.4,1))
POLY_plot <- df_LD %>% ggplot(aes(v2x_polyarchy, EconFree_Index)) + geom_point() + scale_x_continuous(limits = c(0.4,1))
library(ggpubr)
ggarrange(LDI_plot, LIB_plot, EGAL_plot, PART_plot, DELIB_plot, POLY_plot, nrow = 2, ncol = 3, legend = "top")



######## Random Effekts ########

#Nötig?


##### Grafik #####
#Pulling the Regeression Parameters for each Variable into one Vector.
LIB_coef <- c(LIB_fe_c$coefficients[2],
              LIB_fe_y$coefficients[2],
              LIB_fe_c_y$coefficients[2]
              )

EGAL_coef <- c(EGAL_fe_c$coefficients[2],
               EGAL_fe_y$coefficients[2],
               EGAL_fe_c_y$coefficients[2]
                )

PART_coef <- c(PART_fe_c$coefficients[2],
               PART_fe_y$coefficients[2],
               PART_fe_c_y$coefficients[2]
               )

DELIB_coef <- c(DELIB_fe_c$coefficients[2],
                DELIB_fe_y$coefficients[2],
                DELIB_fe_c_y$coefficients[2]
                )

POLY_coef <- c(POLY_fe_c$coefficients[2],
               POLY_fe_y$coefficients[2],
               POLY_fe_c_y$coefficients[2]
               )
## Standard Error of the Coefficents
sd <- c(coef(summary(LIB_fe_c))[2, 2],
            coef(summary(LIB_fe_y))[2, 2],
            coef(summary(LIB_fe_c_y))[2, 2],
            
        coef(summary(EGAL_fe_c))[2, 2],
            coef(summary(EGAL_fe_y))[2, 2],
            coef(summary(EGAL_fe_c_y))[2, 2],
            
        coef(summary(PART_fe_c))[2, 2],
            coef(summary(PART_fe_y))[2, 2],
            coef(summary(PART_fe_c_y))[2, 2],
            
        coef(summary(DELIB_fe_c))[2, 2],
            coef(summary(DELIB_fe_y))[2, 2],
            coef(summary(DELIB_fe_c_y))[2, 2],
        
        coef(summary(POLY_fe_c))[2, 2],
            coef(summary(POLY_fe_y))[2, 2],
            coef(summary(POLY_fe_c_y))[2, 2]
            )

#Kombining the Vectors into a DF
df_coef <- as.data.frame(rbind(LIB_coef, EGAL_coef, PART_coef, DELIB_coef, POLY_coef))

#Giving the Collums names
colnames(df_coef) <- c("Country", "Year", "Country-Year")
df_coef$coef <- rownames(df_coef)
#Making the DF ggplot ready
df_coef <- df_coef %>% pivot_longer(cols = c(Country, Year, `Country-Year`), names_to = "reg_type", values_to = "value")
df_coef$sd <- sd

rm(LIB_coef, EGAL_coef, PART_coef, DELIB_coef, POLY_coef, sd)
#Plot1 Barplot
df_coef %>% group_by(coef) %>% ggplot(aes(coef, value, fill = reg_type)) + 
  geom_col(position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Regressionskoeffizienten des Effekts des Economic Freedom of the World Index \nauf die V-Dem Demokratidimensionen",
       x = "",
       y = "") +
  geom_hline(aes(yintercept = 0), size = 1) + 
  scale_x_discrete(labels = c("Liberal", "Egalitarian", "Participative", "Deliberative", "Polyarchy"), name = "Component Indices") +
  theme(legend.title=element_blank())

#plot2 Dotplot (mit SD)
df_coef %>% group_by(coef) %>% 
  ggplot(aes(coef, value, color = reg_type)) + 
  geom_hline(aes(yintercept = 0), size = 1.2, color = "darkgray") + 
  geom_errorbar(ymin = df_coef$value - 1.645 * df_coef$sd,
                ymax = df_coef$value + 1.645 * df_coef$sd,
                position = position_dodge(width = 0.9),
                linewidth = 1.3, width = 0.85) +
  geom_errorbar(ymin = df_coef$value - 1.96 * df_coef$sd,
                ymax = df_coef$value + 1.96 * df_coef$sd,
                position = position_dodge(width = 0.9),
                linewidth = 0.8, width = 0.5)+
  geom_point(position = position_dodge(width = 0.9), size = 4, shape = 21, fill = "white") +
  scale_y_continuous(limits = c(-0.4, 0.8), n.breaks = 7) +
  labs(title = "Regressionskoeffizienten des Effekts des Economic Freedom of the World Index \nauf die V-Dem Demokratidimensionen",
       subtitle = "90% und 95% Konfidenzintervalle",
       x = "",
       y = "") +
  scale_x_discrete(labels = c("Liberal", "Egalitarian", "Participative", "Deliberative", "Polyarchy"), name = "Component Indices") +
  theme(legend.title=element_blank()) 
  



