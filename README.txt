This is a Uni Project about Democracy Quality and Economic freedom. The data is sourced from V-Dem, Worldbank, World Inequality Database and the Economic freedom of the World Index.
The analysis will be used to write a paper for a polysci class.

Setup.R builds one big dataframe from multiple excel sheets to use for the analysis. It uses the various source tables in the folder Data and the World Inequality Database package. The two dataframes are stored under 
~/data/rds. df_all_cases.rds is a big data frame with all the available cases. OECD-ISO3.rds stores the ISO 3 Codes of all OEDC countries. This is to filter those cases later. 

EDA.R is a somewhat disconnected exploritory data analysis. It includes some usefull overview graphs.

FE-Regression.R is were the analysis lies. Here i estimate various fixed effects models.

Goal is to estimate a Fixed Effects Model with democracy Quality (as measured by various V-Dem indices) as dependent variable. The Economic Freedom of the World index is used as indipendend variable. The Worldbank and World Inequaliy Database variales are used as controll. 