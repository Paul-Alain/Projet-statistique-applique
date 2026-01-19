### Lecture de la BD
df <- readRDS("C:/Users/Paulkengne/Desktop/Projet stat app/Data/BASE_SCR_ENSAE_ANONYME.rds")
View(df)

### Transforme numeric variable into log-variable to reduce  the scale

num_vars <- names(df)[sapply(df, is.numeric)]
num_vars
num_vars <- setdiff(num_vars, c("SOCIETE","Annee")) # to exclude years and societes
  # a copy to keep original

df_log[num_vars] <- lapply(df[num_vars], function(x) log(x))

View(df_log)

# Split years from quarters
unique(df_log$Remise)
df_annual <- df[df$Remise == "A", ]
df_quarterly <- df[df$Remise %in% c("T1","T2","T3","T4"), ]

# Transform numeric variables to log in df_annual
num_vars <- setdiff(names(df_annual)[sapply(df_annual, is.numeric)], c("SOCIETE","Annee"))

df_annual[num_vars] <- lapply(df_annual[num_vars], log)
df_quarterly[num_vars] <- lapply(df_quarterly[num_vars], log)

View((df_annual))

##structure of df

########### Missing value

colSums(is.na(df_annual))
colSums(is.na(df_quarterly))  # no missisng value here
  
  ####### unique value
unique(df$Nature)
unique(df)

  ### Basique statistics
summary(df_annual)

table(df_annual$Nature)

table(df_annual$Famille)




### Tranform into team serie
s
# Keep only numeric variables
num_vars <- names(df_annual)[sapply(df_annual, is.numeric)]

# Exclude 'Annee'
num_vars <- setdiff(num_vars, c("SOCIETE","Annee"))
num_vars

df_annual <- df_annual[order(df_annual$Annee), ]

# Create a named list of ts objects
library(zoo)

ts_annual <- lapply(num_vars, function(v) {
  zoo(df_annual[[v]], order.by = df_annual$Annee)
})
names(ts_annual) <- num_vars
windows()
# Plot


# Convert SCR to a proper time series indexed by Annee
ts_scr <- zoo(df_annual$SCR, order.by = df_annual$Annee)

# Plot
plot(ts_scr, type="o", main="SCR over Years", xlab="Year", ylab="SCR")



