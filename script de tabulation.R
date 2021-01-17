
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

# GNB1 <-  read.csv("GNB_MFI_Dataset_September_2020.csv")
# GNB2 <- read_xlsx("GNB_MFI_Dataset_September_2020_v2.xlsx")
 # marche <- read_xlsx("Marche_Bissau.xlsx")
# nomfichier2 <- read_xlsx("nomfichier2.xlsx")
mfi_bissau <- read_xlsx("mfi_bissau.xlsx")
spss <- read_sav("data_mfi_beta.sav")
spss <- to_factor(spss)
codebook <- var_label(spss)
codebook <- as.data.frame(do.call(rbind,codebook))
codebook <- codebook %>% rownames_to_column()



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

codebook$rowname <- str_replace(codebook$rowname,"UOASoldGroup_FCer.fc","UOASoldGroup_FCer-fc")
codebook$rowname <- str_replace(codebook$rowname,"UOASoldGroup_FOth.fo","UOASoldGroup_FOth-fo")
codebook$rowname <- str_replace(codebook$rowname,"UOASoldGroup_NF.nf","UOASoldGroup_NF-nf")

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
# Remplacer le separateur pour avoir une correspondance avec les libellés de la base spsss
# Assortiment1 <- map(Assortiment1,~str_replace(.x, "-","."))
# Assortiment2 <- map(Assortiment2,~str_replace(.x, "-","."))
# Assortiment3 <- map(Assortiment3,~str_replace(.x, "-","."))


# Assortiment au niveau admin1 --------------------------------------------

# base assortiment1
df_listAssotiment1 <- map(Assortiment1, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listAssotiment1) <- Assortiment1
BaseAssortiment1 <- map_df(df_listAssotiment1, ~as.data.frame(.x), .id = "id") 
# list2env(df_list, envir = .GlobalEnv)
# Base assortiment2
df_listAssotiment2 <- map(Assortiment2, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listAssotiment2) <- Assortiment2
BaseAssortiment2 <- map_df(df_listAssotiment2, ~as.data.frame(.x), .id = "id") 
# base3 assortiment
df_listAssotiment3 <- map(Assortiment3, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listAssotiment3) <- Assortiment3
BaseAssortiment3 <- map_df(df_listAssotiment3, ~as.data.frame(.x), .id = "id") 

BaseAssortiment <- BaseAssortiment1 %>%
                   bind_rows(BaseAssortiment2, BaseAssortiment3)

#  Ajout des libellés
BaseAssortimentAdmin1 <- BaseAssortiment %>% mutate(
  modalite = VLOOKUP(.lookup_values = id, .data = codebook, .lookup_column = rowname,.return_column = V1)
)

BaseAssortimentAdmin1 <- BaseAssortimentAdmin1 %>% select(id,TrdNodDensLocNameAdm1,modalite,everything()) %>% 
  rename(ADMIN1NAME = TrdNodDensLocNameAdm1)

# Assortiment au niveau Admin2 --------------------------------------------
# base assortiment1
df_listAssotiment1 <- map(Assortiment1, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listAssotiment1) <- Assortiment1
BaseAssortiment1 <- map_df(df_listAssotiment1, ~as.data.frame(.x), .id = "id") 
# Base assortiment2
df_listAssotiment2 <- map(Assortiment2, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listAssotiment2) <- Assortiment2
BaseAssortiment2 <- map_df(df_listAssotiment2, ~as.data.frame(.x), .id = "id") 
# base3 assortiment
df_listAssotiment3 <- map(Assortiment3, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listAssotiment3) <- Assortiment3
BaseAssortiment3 <- map_df(df_listAssotiment3, ~as.data.frame(.x), .id = "id") 

BaseAssortiment <- BaseAssortiment1 %>%
  bind_rows(BaseAssortiment2, BaseAssortiment3)
# ajout admin1name
BaseAssortiment <- BaseAssortiment %>% left_join(TableDeRechercheAdmin2,by = "TrdNodDensLocNameAdm2")

#  Ajout des libellés
BaseAssortimentAdmin2 <- BaseAssortiment %>% mutate(
  modalite = VLOOKUP(.lookup_values = id, .data = codebook, .lookup_column = rowname,.return_column = V1)
)

BaseAssortimentAdmin2 <- BaseAssortimentAdmin2 %>% select(id,TrdNodDensLocNameAdm1,
                                                          TrdNodDensLocNameAdm2,modalite,everything()) %>% 
                                      rename(ADMIN1NAME =TrdNodDensLocNameAdm1, ADMIN2NAME = TrdNodDensLocNameAdm2)
# Assortiment au niveau marché --------------------------------------------
# base assortiment1
df_listAssotiment1 <- map(Assortiment1, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listAssotiment1) <- Assortiment1
BaseAssortiment1 <- map_df(df_listAssotiment1, ~as.data.frame(.x), .id = "id") 
# list2env(df_list, envir = .GlobalEnv)
# Base assortiment2
df_listAssotiment2 <- map(Assortiment2, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listAssotiment2) <- Assortiment2
BaseAssortiment2 <- map_df(df_listAssotiment2, ~as.data.frame(.x), .id = "id") 
# base3 assortiment
df_listAssotiment3 <- map(Assortiment3, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listAssotiment3) <- Assortiment3
BaseAssortiment3 <- map_df(df_listAssotiment3, ~as.data.frame(.x), .id = "id") 

BaseAssortiment <- BaseAssortiment1 %>%
  bind_rows(BaseAssortiment2, BaseAssortiment3)

#  Ajout des libellés
BaseAssortimentAdmin2 <- BaseAssortiment %>% mutate(
  modalite = VLOOKUP(.lookup_values = id, .data = codebook, .lookup_column = rowname,.return_column = V1)
)




# Disponibilté ------------------------------------------------------------


# Prix --------------------------------------------------------------------
a <- mfi_bissau %>% tabyl(TrdNodDensLocNameAdm2, `UOASoldGroup_FCer-fc1`,show_na = TRUE) %>% 
  adorn_totals(where = c("col")) %>%
  adorn_percentages(denominator = "row") %>% 
  adorn_pct_formatting(digits = 0)

a <- a %>% mutate(
  Admin1name = VLOOKUP(.lookup_values = MktNametext,.data = mfi_bissau,.lookup_column = MktNametext,.return_column = TrdNodDensLocNameAdm1)
)
a <- a %>% relocate(Admin1name,.before = Admin2name)

# Résilience --------------------------------------------------------------


# Service -----------------------------------------------------------------


# Infrastructure ----------------------------------------------------------


# Competition -------------------------------------------------------------


# Qualité -----------------------------------------------------------------


# Acces et Protection -----------------------------------------------------




# Fonction de tabulation --------------------------------------------------
# TrdNodDensLocNameAdm0 Niveau Admin0
# TrdNodDensLocNameAdm1 Niveau Admin1
# TrdNodDensLocNameAdm2 Niveau Admin2

tableauMarche <- function(d,colonne){
  d <- mfi_bissau %>% tabyl(MktNametext, {{colonne}},show_na = TRUE) %>% 
    adorn_totals(where = c("col")) %>%
    adorn_percentages(denominator = "row") %>% 
    adorn_pct_formatting(digits = 0)
  # d <- d %>% mutate(
  #   Admin2name = VLOOKUP(.lookup_values = MktNametext,.data = marche,.lookup_column = MarketName,.return_column = Adm2Name)
  # )
  # d <- d %>% mutate(
  #   Admin1name = VLOOKUP(.lookup_values = MktNametext,.data = marche,.lookup_column = MarketName,.return_column = Adm1Name)
  # )
  # d <- d %>% relocate(Admin2name,.before = MktNametext)
  # d <- d %>% relocate(Admin1name,.before = Admin2name)
}

tableauAdmin1 <- function(d,colonne){
  d <- mfi_bissau %>% tabyl(TrdNodDensLocNameAdm1, {{colonne}},show_na = TRUE) %>% 
    adorn_totals(where = c("col")) %>%
    adorn_percentages(denominator = "row") %>% 
    adorn_pct_formatting(digits = 0)
}

tableauAdmin2 <- function(d, colonne){
  d <- mfi_bissau %>% tabyl(TrdNodDensLocNameAdm2, {{colonne}},show_na = TRUE) %>% 
    adorn_totals(where = c("col")) %>%
    adorn_percentages(denominator = "row") %>% 
    adorn_pct_formatting(digits = 0)
  # d <- d %>% mutate(
  #   Admin1name = VLOOKUP(.lookup_values = TrdNodDensLocNameAdm2,.data = marche,.lookup_column = Adm2Name,.return_column = Adm1Name)
  # )
  # d <- d %>% relocate(Admin1name,.before = TrdNodDensLocNameAdm2)
}

TableDeRecherche <- mfi_bissau %>% 
  select(TrdNodDensLocNameAdm0, TrdNodDensLocNameAdm1,
         TrdNodDensLocNameAdm2, TrdNodDensLocNameAdm3,
          MktNametext)
TableDeRecherche <- unique(TableDeRecherche)
TableDeRechercheAdmin2 <- TableDeRecherche %>% select(TrdNodDensLocNameAdm1,TrdNodDensLocNameAdm2)
TableDeRechercheMarche <- TableDeRecherche %>% select(TrdNodDensLocNameAdm1,TrdNodDensLocNameAdm2,MktNametext)



# Fonction de sauvegarde --------------------------------------------------
tableauAdmin1(mfi_bissau,"UOASoldGroup_FCer-fc1")


