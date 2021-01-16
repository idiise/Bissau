
# import packages ---------------------------------------------------------

library(readxl)
library(stringi)
library(tidyverse)
library(tidyquant)
library(writexl)
library(haven)
library(labelled)
library(gmodels)
library(janitor)
library(rlist)
library(openxlsx)
# Import Dataset ----------------------------------------------------------

GNB1 <-  read.csv("GNB_MFI_Dataset_September_2020.csv")
GNB2 <- read_xlsx("GNB_MFI_Dataset_September_2020_v2.xlsx")
marche <- read_xlsx("Marche_Bissau.xlsx")
nomfichier2 <- read_xlsx("nomfichier2.xlsx")
mfi_bissau <- read_xlsx("mfi_bissau.xlsx")
spss <- read_sav("data_mfi_beta.sav")
spss <- to_factor(spss)
d <- var_label(spss)
d <- as.data.frame(do.call(rbind,d))
d <- d %>% rownames_to_column()

# Assortiment -------------------------------------------------------------
# UOASoldGroup_FCer  Selectionnez tous les types de produits alimentaires de céréales habituellement vendus
# UOASoldGroup_FOth-fo Selectionnez tous les types de Produits Alimentaires Autres habituellement vendus
# UOASoldGroup_NF-nf  Selectionnez tous les types de produits non alimentaires Habituelement vendu

mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("UOASoldGroup_FCer-fc","UOASoldGroup_FOth-fo","UOASoldGroup_NF-nf"))), 
                        funs(factor))
mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("UOASoldGroup_FCer-fc","UOASoldGroup_FOth-fo","UOASoldGroup_NF-nf"))),
                        ~recode_factor(.,"0"="Non", "1"="Oui"))

# toutes les variables
colonne <- colnames(mfi_bissau)

# selectionnez les types de produits qui sont habituellement vendus UOASoldGroup_FCer-fc
Assortiment1 <- list()
Assortiment2 <- list()
Assortiment3 <- list()

for (i in 1:length(colonne)) {
  if(str_detect(colonne[i], "UOASoldGroup_FCer-fc") ){
    Assortiment1 <- append(Assortiment1, colonne[i])
  }
  else if (str_detect(colonne[i], "UOASoldGroup_FOth-fo")) {
    Assortiment2 <- append(Assortiment2, colonne[i])
  }
  else if(str_detect(colonne[i], "UOASoldGroup_NF-nf")) {
    Assortiment3 <- append(Assortiment3, colonne[i])
  }
}


df_list <- map(Assortiment, ~ tableau(mfi_bissau, !!sym(.x)))
names(df_list) <- Assortiment
list2env(df_list, envir = .GlobalEnv)

nom <- map(Assortiment1,~str_replace(.x, "-","."))
d[which(d$rowname%in%nom),]

sheet_names <- excel_sheets("Disponibilité/Assortiment.xlsx")
nb_sheets <-  length(sheet_names)

base_vide <- NULL
for (i in 1:nb_sheets){
  name <- paste0("Produits_habituellement_vendus__",sheet_names[i]) #r?cup?re le nom de la feuille
  data <- read_excel("Disponibilité/Assortiment.xlsx", sheet = i)# stock les donn?es
  data$modalite=name # ajout d'une variable region
  base_vide<- bind_rows(base_vide,data)     
  rm(name,data) 
} 

# Disponibilté ------------------------------------------------------------


# Prix --------------------------------------------------------------------


# Résilience --------------------------------------------------------------


# Service -----------------------------------------------------------------


# Infrastructure ----------------------------------------------------------


# Competition -------------------------------------------------------------


# Qualité -----------------------------------------------------------------


# Acces et Protection -----------------------------------------------------




# Fonction de tabulation --------------------------------------------------

tableau <- function(d,colonne){
  d <- mfi_bissau %>% tabyl(MktNametext, {{colonne}},show_na = TRUE) %>% 
    adorn_totals(where = c("col")) %>%
    adorn_percentages(denominator = "row") %>% 
    adorn_pct_formatting(digits = 0)
  d <- d %>% mutate(
    Admin2name = VLOOKUP(.lookup_values = MktNametext,.data = marche,.lookup_column = MarketName,.return_column = Adm2Name)
  )
  d <- d %>% mutate(
    Admin1name = VLOOKUP(.lookup_values = MktNametext,.data = marche,.lookup_column = MarketName,.return_column = Adm1Name)
  )
  d <- d %>% relocate(Admin2name,.before = MktNametext)
  d <- d %>% relocate(Admin1name,.before = Admin2name)
  
}


# Fonction de sauvegarde --------------------------------------------------



