##### Einlesen der Daten #####

df_all_cases = readRDS("df_all_cases.rds")
OECD_ISO3 = readRDS("OECD_ISO3.rds")

##### Filtern auf Länder von Interesse #####

countries_OI = c("Estonia", "Poland", "Bulgaria", "Latvia", "Hungary", "Lithuania", "Czechia", "Slovakia", "Romania")

df_easternEurope = df_all_cases %>% filter(country_name %in% countries_OI) %>% filter(year >= 1980)

table(df_easternEurope$country_name)
df_easternEurope %>% group_by(country_name) %>% pull(max(year))

##### Plots #####
df_easternEurope %>% group_by(country_name) %>% ggplot(aes(year, v2x_polyarchy, colour = country_name)) + geom_line()

df_easternEurope %>% group_by(country_name) %>% ggplot(aes(year, Wealth_10by50, colour = country_name)) + geom_line()

df_easternEurope %>% group_by(country_name) %>% ggplot(aes(year, Gini_Index, colour = country_name)) + geom_line()

df_easternEurope %>% group_by(country_name) %>% ggplot(aes(year, Income_10by50, colour = country_name)) + geom_line()


df_easternEurope %>% pivot_longer(cols = !c(year, country_name, country_text_id, e_regionpol, e_regionpolstr, e_chga_demo, `Income_Bottom50%_%`, `Income_Top10%_%`, `Income share held by second 20%`, `Income share held by lowest 20%`, `Income share held by lowest 10%`, `Income share held by highest 20%`, `Income share held by highest 10%`, `Income share held by third 20%`), names_to = "vars", values_to = "values") %>% group_by(vars) %>% ggplot(aes(year, fill = vars, colour = "black")) + geom_bar(position = "stack")




