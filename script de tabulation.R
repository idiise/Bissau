
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


# Fonction nécessaire -----------------------------------------------------
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

# TableDeRecherche <- mfi_bissau %>% 
#   select(TrdNodDensLocNameAdm0, TrdNodDensLocNameAdm1,
#          TrdNodDensLocNameAdm2, TrdNodDensLocNameAdm3,
#           MktNametext)

TableDeRechercheAdmin2 <- mfi_bissau %>% select(TrdNodDensLocNameAdm1,TrdNodDensLocNameAdm2)
TableDeRechercheAdmin2 <- unique(TableDeRechercheAdmin2)
TableDeRechercheMarche <- mfi_bissau %>% select(TrdNodDensLocNameAdm1,TrdNodDensLocNameAdm2,MktNametext)
TableDeRechercheMarche <- unique(TableDeRechercheMarche)

# Fonction de Sauvegarde multiple
xlsx.writeMultipleData <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
}

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
BaseAssortiment1 <- map_df(df_listAssotiment1, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable ="Produits alimentaire de céréales vendus") 
# list2env(df_list, envir = .GlobalEnv)
# Base assortiment2
df_listAssotiment2 <- map(Assortiment2, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listAssotiment2) <- Assortiment2
BaseAssortiment2 <- map_df(df_listAssotiment2, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Autres produits alimentaires vendus")
# base3 assortiment
df_listAssotiment3 <- map(Assortiment3, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listAssotiment3) <- Assortiment3
BaseAssortiment3 <- map_df(df_listAssotiment3, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Produits non alimentaire vendus")

BaseAssortiment <- BaseAssortiment1 %>%
                   bind_rows(BaseAssortiment2, BaseAssortiment3)

#  Ajout des libellés
BaseAssortimentAdmin1 <- BaseAssortiment %>% mutate(
  modalite = VLOOKUP(.lookup_values = id, .data = codebook, .lookup_column = rowname,.return_column = V1)
)

BaseAssortimentAdmin1 <- BaseAssortimentAdmin1 %>% select(id,TrdNodDensLocNameAdm1,variable,modalite,everything()) %>% 
  rename(ADMIN1NAME = TrdNodDensLocNameAdm1)

BaseAssortimentAdmin1 <- BaseAssortimentAdmin1 %>% mutate(Dimension = "Assortiment") %>% 
  select(Dimension,everything()) %>% select(-id)

# Assortiment au niveau Admin2 --------------------------------------------
# base assortiment1
df_listAssotiment1 <- map(Assortiment1, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listAssotiment1) <- Assortiment1
BaseAssortiment1 <- map_df(df_listAssotiment1, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable ="Produits alimentaire de céréales vendus") 
# Base assortiment2
df_listAssotiment2 <- map(Assortiment2, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listAssotiment2) <- Assortiment2
BaseAssortiment2 <- map_df(df_listAssotiment2, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Autres produits alimentaires vendus") 
# base3 assortiment
df_listAssotiment3 <- map(Assortiment3, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listAssotiment3) <- Assortiment3
BaseAssortiment3 <- map_df(df_listAssotiment3, ~as.data.frame(.x), .id = "id")%>% 
  mutate(variable = "Produits non alimentaire vendus") 

BaseAssortiment <- BaseAssortiment1 %>%
  bind_rows(BaseAssortiment2, BaseAssortiment3)
# ajout admin1name
BaseAssortiment <- BaseAssortiment %>% left_join(TableDeRechercheAdmin2,by = "TrdNodDensLocNameAdm2")

#  Ajout des libellés
BaseAssortimentAdmin2 <- BaseAssortiment %>% mutate(
  modalite = VLOOKUP(.lookup_values = id, .data = codebook, .lookup_column = rowname,.return_column = V1)
)

BaseAssortimentAdmin2 <- BaseAssortimentAdmin2 %>% select(id,TrdNodDensLocNameAdm1,
                                                          TrdNodDensLocNameAdm2,variable,modalite,everything()) %>% 
                          rename(ADMIN1NAME =TrdNodDensLocNameAdm1, ADMIN2NAME = TrdNodDensLocNameAdm2)

BaseAssortimentAdmin2 <- BaseAssortimentAdmin2 %>% mutate(Dimension = "Assortiment") %>% 
  select(Dimension,everything()) %>% select(-id)

# Assortiment au niveau marché --------------------------------------------
# base assortiment1
df_listAssotiment1 <- map(Assortiment1, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listAssotiment1) <- Assortiment1
BaseAssortiment1 <- map_df(df_listAssotiment1, ~as.data.frame(.x), .id = "id")%>% 
  mutate(variable ="Produits alimentaire de céréales vendus")  
# list2env(df_list, envir = .GlobalEnv)
# Base assortiment2
df_listAssotiment2 <- map(Assortiment2, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listAssotiment2) <- Assortiment2
BaseAssortiment2 <- map_df(df_listAssotiment2, ~as.data.frame(.x), .id = "id")%>% 
  mutate(variable = "Autres produits alimentaires vendus")  
# base3 assortiment
df_listAssotiment3 <- map(Assortiment3, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listAssotiment3) <- Assortiment3
BaseAssortiment3 <- map_df(df_listAssotiment3, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Produits non alimentaire vendus")

BaseAssortiment <- BaseAssortiment1 %>%
  bind_rows(BaseAssortiment2, BaseAssortiment3)

BaseAssortiment <- BaseAssortiment %>% left_join(TableDeRechercheMarche,by = "MktNametext")

#  Ajout des libellés
BaseAssortimentMarche <- BaseAssortiment %>% mutate(
  modalite = VLOOKUP(.lookup_values = id, .data = codebook, .lookup_column = rowname,.return_column = V1)
)

BaseAssortimentMarche <- BaseAssortimentMarche %>% select(id,TrdNodDensLocNameAdm1,TrdNodDensLocNameAdm2,
                                                          MktNametext,variable,modalite,everything()) %>% 
                                        rename(ADMIN1NAME =TrdNodDensLocNameAdm1, ADMIN2NAME = TrdNodDensLocNameAdm2)

BaseAssortimentMarche <- BaseAssortimentMarche %>% mutate(Dimension = "Assortiment") %>% 
  select(Dimension,everything()) %>% select(-id)

# Disponibilté ------------------------------------------------------------
# B1
# UOAAvailScarce_Gr- -- Ya t-il des produits rares dans le marché
# UOAAvailScarce_FCer-fc -- Produits alimentaire de céréales rares
# UOAAvailScarce_FOth-fo --Produits alimentaires Autres rares
# UOAAvailScarce_NF-nf --Produits Non alimentaires rares
# B2
# TrdAvailRunout_Gr- --Avez-vous peur de manquer de stocks d'ici une semaine en ce qui concerne
# B2
# MktAvailRunout_Gr- --Les commerçants de ce marché ont-ils peur de manquer de stocks d'ici 1e semaine en ce qui concerne
# UOAAvailRunout_FCer-fc -- Manque de stocks de produits alimentaires de céréales d'ici une semaine
# UOAAvailRunout_FOth-fo --  Manque de Stock d'autres prosuits alimentaires d'ici une semaine
# UOAAvailRunout_NF-nf   --  Manque de Stock de produits Nom alimentaires d'ici une semaine
mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("UOAAvailScarce_Gr-","UOAAvailScarce_FCer-fc",
                        "UOAAvailScarce_FOth-fo","UOAAvailScarce_NF-nf","TrdAvailRunout_Gr-",
                        "MktAvailRunout_Gr-","UOAAvailRunout_FCer-fc",
                        "UOAAvailRunout_FOth-fo","UOAAvailRunout_NF-nf"))), 
                        funs(factor))

mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("UOAAvailScarce_Gr-","UOAAvailScarce_FCer-fc",
                                        "UOAAvailScarce_FOth-fo","UOAAvailScarce_NF-nf","TrdAvailRunout_Gr-",
                                        "MktAvailRunout_Gr-","UOAAvailRunout_FCer-fc",
                                        "UOAAvailRunout_FOth-fo","UOAAvailRunout_NF-nf"))),
                        ~recode_factor(.,"0"="Non", "1"="Oui"))

codebook$rowname <- str_replace(codebook$rowname,"UOAAvailScarce_Gr.", "UOAAvailScarce_Gr-")
codebook$rowname <- str_replace(codebook$rowname,"UOAAvailScarce_FCer.fc","UOAAvailScarce_FCer-fc")
codebook$rowname <- str_replace(codebook$rowname,"UOAAvailScarce_FOth.fo","UOAAvailScarce_FOth-fo")
codebook$rowname <- str_replace(codebook$rowname,"UOAAvailScarce_NF.nf","UOAAvailScarce_NF-nf")
codebook$rowname <- str_replace(codebook$rowname,"TrdAvailRunout_Gr.","TrdAvailRunout_Gr-")
codebook$rowname <- str_replace(codebook$rowname,"MktAvailRunout_Gr.","MktAvailRunout_Gr-")
codebook$rowname <- str_replace(codebook$rowname,"UOAAvailRunout_FCer.fc","UOAAvailRunout_FCer-fc")
codebook$rowname <- str_replace(codebook$rowname,"UOAAvailRunout_FOth.fo","UOAAvailRunout_FOth-fo")
codebook$rowname <- str_replace(codebook$rowname,"UOAAvailRunout_NF.nf","UOAAvailRunout_NF-nf")


# toutes les variables
colonne <- colnames(mfi_bissau)

# selectionnez les types de produits qui sont habituellement vendus UOASoldGroup_FCer-fc
Disponibilite1 <- list()
Disponibilite2 <- list()
Disponibilite3 <- list()
Disponibilite4 <- list()
Disponibilite5 <- list()
Disponibilite6 <- list()
Disponibilite7 <- list()
Disponibilite8 <- list()
Disponibilite9 <- list()


for (i in 1:length(colonne)) {
  if(str_detect(colonne[i], "UOAAvailScarce_Gr-") ){
    Disponibilite1 <- append(Disponibilite1, colonne[i])
  }
  else if (str_detect(colonne[i], "UOAAvailScarce_FCer-fc")) {
    Disponibilite2 <- append(Disponibilite2, colonne[i])
  }
  else if(str_detect(colonne[i], "UOAAvailScarce_FOth-fo")) {
    Disponibilite3 <- append(Disponibilite3, colonne[i])
  }else if(str_detect(colonne[i], "UOAAvailScarce_NF-nf")){
    Disponibilite4 <- append(Disponibilite4, colonne[i])
  }else if(str_detect(colonne[i], "TrdAvailRunout_Gr-")){
    Disponibilite5 <- append(Disponibilite5, colonne[i])
  }else if(str_detect(colonne[i], "MktAvailRunout_Gr-")){
    Disponibilite6 <- append(Disponibilite6, colonne[i])
  }else if(str_detect(colonne[i], "UOAAvailRunout_FCer-fc")){
    Disponibilite7 <- append(Disponibilite7, colonne[i])
  }else if(str_detect(colonne[i], "UOAAvailRunout_FOth-fo")){
    Disponibilite8 <- append(Disponibilite8, colonne[i])
  }else if(str_detect(colonne[i], "UOAAvailRunout_NF-nf")){
    Disponibilite9 <- append(Disponibilite9, colonne[i])
  }
}



# Disponibilte niveau admin1 ----------------------------------------------
# Base disponibilité1
df_listDisponibilite1 <- map(Disponibilite1, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite1) <- Disponibilite1
BaseDisponibilite1 <- map_df(df_listDisponibilite1, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable ="Ya t-il des produits rares dans le marché") 
# list2env(df_list, envir = .GlobalEnv)
#Base disponibilité2
df_listDisponibilite2 <- map(Disponibilite2, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite2) <- Disponibilite2
BaseDisponibilite2 <- map_df(df_listDisponibilite2, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Produits alimentaire de céréales rares")
# Base disponibilité3
df_listDisponibilite3 <- map(Disponibilite3, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite3) <- Disponibilite3
BaseDisponibilite3 <- map_df(df_listDisponibilite3, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Produits alimentaires Autres rares")
# Base disponibilité4
df_listDisponibilite4 <- map(Disponibilite4, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite4) <- Disponibilite4
BaseDisponibilite4 <- map_df(df_listDisponibilite4, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Produits Non alimentaires  rares")
# Base disponibilité5
df_listDisponibilite5 <- map(Disponibilite5, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite5) <- Disponibilite5
BaseDisponibilite5 <- map_df(df_listDisponibilite5, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Avez-vous peur de manquer de stocks d'ici une semaine en ce qui concerne")
# Base disponibilité6
df_listDisponibilite6 <- map(Disponibilite6, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite6) <- Disponibilite6
BaseDisponibilite6 <- map_df(df_listDisponibilite6, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Les commerçants de ce marché ont-ils peur de manquer de stocks d'ici 1e semaine en ce qui concerne")
# Base disponibilité7
df_listDisponibilite7 <- map(Disponibilite7, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite7) <- Disponibilite7
BaseDisponibilite7 <- map_df(df_listDisponibilite7, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Manque de stocks de produits alimentaires de céréales d'ici une semaine")
# Base disponibilité8
df_listDisponibilite8 <- map(Disponibilite8, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite8) <- Disponibilite8
BaseDisponibilite8 <- map_df(df_listDisponibilite8, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Manque de Stock d'autres produits alimentaires d'ici une semaine")
# Base disponibilité9
df_listDisponibilite9 <- map(Disponibilite9, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite9) <- Disponibilite9
BaseDisponibilite9 <- map_df(df_listDisponibilite9, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Manque de Stock de produits Nom alimentaires d'ici une semaine")

BaseDisponibilite <- BaseDisponibilite1 %>%
  bind_rows(BaseDisponibilite2, BaseDisponibilite3,BaseDisponibilite4,BaseDisponibilite5,
            BaseDisponibilite6,BaseDisponibilite7,BaseDisponibilite8,BaseDisponibilite9)

#  Ajout des libellés
BaseDisponibiliteAdmin1 <- BaseDisponibilite %>% mutate(
  modalite = VLOOKUP(.lookup_values = id, .data = codebook, .lookup_column = rowname,.return_column = V1)
)

BaseDisponibiliteAdmin1 <- BaseDisponibiliteAdmin1 %>% select(id,TrdNodDensLocNameAdm1,variable,modalite,everything()) %>% 
  rename(ADMIN1NAME = TrdNodDensLocNameAdm1)

BaseDisponibiliteAdmin1 <- BaseDisponibiliteAdmin1 %>% mutate(Dimension = "Disponibilité") %>% 
  select(Dimension,everything()) %>% select(-id)

# Disponibilté au niveau admin2 -------------------------------------------
# Base disponibilité1
df_listDisponibilite1 <- map(Disponibilite1, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite1) <- Disponibilite1
BaseDisponibilite1 <- map_df(df_listDisponibilite1, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable ="Ya t-il des produits rares dans le marché") 
# list2env(df_list, envir = .GlobalEnv)
#Base disponibilité2
df_listDisponibilite2 <- map(Disponibilite2, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite2) <- Disponibilite2
BaseDisponibilite2 <- map_df(df_listDisponibilite2, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Produits alimentaire de céréales rares")
# Base disponibilité3
df_listDisponibilite3 <- map(Disponibilite3, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite3) <- Disponibilite3
BaseDisponibilite3 <- map_df(df_listDisponibilite3, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Produits alimentaires Autres rares")
# Base disponibilité4
df_listDisponibilite4 <- map(Disponibilite4, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite4) <- Disponibilite4
BaseDisponibilite4 <- map_df(df_listDisponibilite4, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Produits Non alimentaires  rares")
# Base disponibilité5
df_listDisponibilite5 <- map(Disponibilite5, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite5) <- Disponibilite5
BaseDisponibilite5 <- map_df(df_listDisponibilite5, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Avez-vous peur de manquer de stocks d'ici une semaine en ce qui concerne")
# Base disponibilité6
df_listDisponibilite6 <- map(Disponibilite6, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite6) <- Disponibilite6
BaseDisponibilite6 <- map_df(df_listDisponibilite6, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Les commerçants de ce marché ont-ils peur de manquer de stocks d'ici 1e semaine en ce qui concerne")
# Base disponibilité7
df_listDisponibilite7 <- map(Disponibilite7, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite7) <- Disponibilite7
BaseDisponibilite7 <- map_df(df_listDisponibilite7, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Manque de stocks de produits alimentaires de céréales d'ici une semaine")
# Base disponibilité8
df_listDisponibilite8 <- map(Disponibilite8, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite8) <- Disponibilite8
BaseDisponibilite8 <- map_df(df_listDisponibilite8, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Manque de Stock d'autres produits alimentaires d'ici une semaine")
# Base disponibilité9
df_listDisponibilite9 <- map(Disponibilite9, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite9) <- Disponibilite9
BaseDisponibilite9 <- map_df(df_listDisponibilite9, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Manque de Stock de produits Nom alimentaires d'ici une semaine")


BaseDisponibilite <- BaseDisponibilite1 %>%
  bind_rows(BaseDisponibilite2, BaseDisponibilite3,BaseDisponibilite4,BaseDisponibilite5,
            BaseDisponibilite6,BaseDisponibilite7,BaseDisponibilite8,BaseDisponibilite9)

BaseDisponibilite <- BaseDisponibilite %>% left_join(TableDeRechercheAdmin2,by = "TrdNodDensLocNameAdm2")


#  Ajout des libellés
BaseDisponibiliteAdmin2 <- BaseDisponibilite %>% mutate(
  modalite = VLOOKUP(.lookup_values = id, .data = codebook, .lookup_column = rowname,.return_column = V1)
)

BaseDisponibiliteAdmin2 <- BaseDisponibiliteAdmin2 %>% select(id,TrdNodDensLocNameAdm1,
                                                          TrdNodDensLocNameAdm2,variable,modalite,everything()) %>% 
  rename(ADMIN1NAME =TrdNodDensLocNameAdm1, ADMIN2NAME = TrdNodDensLocNameAdm2)

BaseDisponibiliteAdmin2 <- BaseDisponibiliteAdmin2 %>% mutate(Dimension = "Disponibilité") %>% 
  select(Dimension,everything()) %>% select(-id)

# Disponibilité au niveau marché ------------------------------------------
# Base disponibilité1
df_listDisponibilite1 <- map(Disponibilite1, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite1) <- Disponibilite1
BaseDisponibilite1 <- map_df(df_listDisponibilite1, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable ="Ya t-il des produits rares dans le marché") 
# list2env(df_list, envir = .GlobalEnv)
#Base disponibilité2
df_listDisponibilite2 <- map(Disponibilite2, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite2) <- Disponibilite2
BaseDisponibilite2 <- map_df(df_listDisponibilite2, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Produits alimentaire de céréales rares")
# Base disponibilité3
df_listDisponibilite3 <- map(Disponibilite3, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite3) <- Disponibilite3
BaseDisponibilite3 <- map_df(df_listDisponibilite3, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Produits alimentaires Autres rares")
# Base disponibilité4
df_listDisponibilite4 <- map(Disponibilite4, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite4) <- Disponibilite4
BaseDisponibilite4 <- map_df(df_listDisponibilite4, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Produits Non alimentaires  rares")
# Base disponibilité5
df_listDisponibilite5 <- map(Disponibilite5, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite5) <- Disponibilite5
BaseDisponibilite5 <- map_df(df_listDisponibilite5, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Avez-vous peur de manquer de stocks d'ici une semaine en ce qui concerne")
# Base disponibilité6
df_listDisponibilite6 <- map(Disponibilite6, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite6) <- Disponibilite6
BaseDisponibilite6 <- map_df(df_listDisponibilite6, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Les commerçants de ce marché ont-ils peur de manquer de stocks d'ici 1e semaine en ce qui concerne")
# Base disponibilité7
df_listDisponibilite7 <- map(Disponibilite7, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite7) <- Disponibilite7
BaseDisponibilite7 <- map_df(df_listDisponibilite7, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Manque de stocks de produits alimentaires de céréales d'ici une semaine")
# Base disponibilité8
df_listDisponibilite8 <- map(Disponibilite8, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite8) <- Disponibilite8
BaseDisponibilite8 <- map_df(df_listDisponibilite8, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Manque de Stock d'autres produits alimentaires d'ici une semaine")
# Base disponibilité9
df_listDisponibilite9 <- map(Disponibilite9, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite9) <- Disponibilite9
BaseDisponibilite9 <- map_df(df_listDisponibilite9, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Manque de Stock de produits Nom alimentaires d'ici une semaine")


BaseDisponibilite <- BaseDisponibilite1 %>%
  bind_rows(BaseDisponibilite2, BaseDisponibilite3,BaseDisponibilite4,BaseDisponibilite5,
            BaseDisponibilite6,BaseDisponibilite7,BaseDisponibilite8,BaseDisponibilite9)

BaseDisponibilite <- BaseDisponibilite %>% left_join(TableDeRechercheMarche,by = "MktNametext")


#  Ajout des libellés
BaseDisponibiliteMarche <- BaseDisponibilite %>% mutate(
  modalite = VLOOKUP(.lookup_values = id, .data = codebook, .lookup_column = rowname,.return_column = V1)
)

BaseDisponibiliteMarche <- BaseDisponibiliteMarche %>% select(id,TrdNodDensLocNameAdm1,TrdNodDensLocNameAdm2,
                                                          MktNametext,variable,modalite,everything()) %>% 
  rename(ADMIN1NAME =TrdNodDensLocNameAdm1, ADMIN2NAME = TrdNodDensLocNameAdm2)

BaseDisponibiliteMarche <- BaseDisponibiliteMarche %>% mutate(Dimension = "Disponibilité") %>% 
  select(Dimension,everything()) %>% select(-id)

# Prix --------------------------------------------------------------------
# UOAPriceIncr_Gr-  -- Y-a-t-il des produits dont le prix a fortement augmenté dans le dernier mois
# UOAPriceIncr_FCer-fc -- Prix produits Alimentaires de Céréales fortement augmenté
# UOAPriceIncr_FOth-fo -- Prix produits Alimentaires Autres fortement augmenté
# UOAPriceIncr_NF-nf -- Prix produits Non Alimentaires fortement augmenté
# TrdPriceStab_Gr-  -- Êtes-vous en mesure d'estimer le prix qu'un produit aura la semaine Prochaine
# MktPriceStab_Gr-  -- Les commerçants de ce marché peuvent-ils estimer le prix de la semaine prochaine pour
# UOAPriceUnstab_FCer-fc -- Quels produits alimentaires de céréales n'estimeriez-vous pas  correctement
# UOAPriceUnstab_FOth-fo -- Quels autres produits alimentaires n'estimeriez-vous pas correctement
# UOAPriceUnstab_NF-nf -- Quels autres produits non alimentaires n'estimeriez-vous pas correctement

mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("UOAPriceIncr_Gr-","UOAPriceIncr_FCer-fc",
                                        "UOAPriceIncr_FOth-fo","UOAPriceIncr_NF-nf","TrdPriceStab_Gr-",
                                        "MktPriceStab_Gr-","UOAPriceUnstab_FCer-fc",
                                        "UOAPriceUnstab_FOth-fo","UOAPriceUnstab_NF-nf"))), 
                        funs(factor))

mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("UOAPriceIncr_Gr-","UOAPriceIncr_FCer-fc",
                                        "UOAPriceIncr_FOth-fo","UOAPriceIncr_NF-nf","TrdPriceStab_Gr-",
                                        "MktPriceStab_Gr-","UOAPriceUnstab_FCer-fc",
                                        "UOAPriceUnstab_FOth-fo","UOAPriceUnstab_NF-nf"))),
                        ~recode_factor(.,"0"="Non", "1"="Oui"))

codebook$rowname <- str_replace(codebook$rowname,"UOAPriceIncr_Gr.", "UOAPriceIncr_Gr-")
codebook$rowname <- str_replace(codebook$rowname,"UOAPriceIncr_FCer.fc","UOAPriceIncr_FCer-fc")
codebook$rowname <- str_replace(codebook$rowname,"UOAPriceIncr_FOth.fo","UOAPriceIncr_FOth-fo")
codebook$rowname <- str_replace(codebook$rowname,"UOAPriceIncr_NF.nf","UOAPriceIncr_NF-nf")
codebook$rowname <- str_replace(codebook$rowname,"TrdPriceStab_Gr.","TrdPriceStab_Gr-")
codebook$rowname <- str_replace(codebook$rowname,"MktPriceStab_Gr.","MktPriceStab_Gr-")
codebook$rowname <- str_replace(codebook$rowname,"UOAPriceUnstab_FCer.fc","UOAPriceUnstab_FCer-fc")
codebook$rowname <- str_replace(codebook$rowname,"UOAPriceUnstab_FOth.fo","UOAPriceUnstab_FOth-fo")
codebook$rowname <- str_replace(codebook$rowname,"UOAPriceUnstab_NF-nf","UOAPriceUnstab_NF-nf")

# toutes les variables
colonne <- colnames(mfi_bissau)

# selectionnez les types de produits qui sont habituellement vendus UOASoldGroup_FCer-fc
Prix1 <- list()
Prix2 <- list()
Prix3 <- list()
Prix4 <- list()
Prix5 <- list()
Prix6 <- list()
Prix7 <- list()
Prix8 <- list()
Prix9 <- list()

for (i in 1:length(colonne)) {
  if(str_detect(colonne[i], "UOAPriceIncr_Gr-") ){
    Prix1 <- append(Prix1, colonne[i])
  }
  else if (str_detect(colonne[i], "UOAPriceIncr_FCer-fc")) {
    Prix2 <- append(Prix2, colonne[i])
  }
  else if(str_detect(colonne[i], "UOAPriceIncr_FOth-fo")) {
    Prix3 <- append(Prix3, colonne[i])
  }else if(str_detect(colonne[i], "UOAPriceIncr_NF-nf")){
    Prix4 <- append(Prix4, colonne[i])
  }else if(str_detect(colonne[i], "TrdPriceStab_Gr-")){
    Prix5 <- append(Prix5, colonne[i])
  }else if(str_detect(colonne[i], "MktPriceStab_Gr-")){
    Prix6 <- append(Prix6, colonne[i])
  }else if(str_detect(colonne[i], "UOAPriceUnstab_FCer-fc")){
    Prix7 <- append(Prix7, colonne[i])
  }else if(str_detect(colonne[i], "UOAPriceUnstab_FOth-fo")){
    Prix8 <- append(Prix8, colonne[i])
  }else if(str_detect(colonne[i], "UOAPriceUnstab_NF-nf")){
    Prix9 <- append(Prix9, colonne[i])
  }
}

# Prix au niveau Admin1 ---------------------------------------------------

# Base Prix1
df_listPrix1 <- map(Prix1, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listPrix1) <- Prix1
BasePrix1 <- map_df(df_listPrix1, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable ="Y-a-t-il des produits dont le prix a fortement augmenté dans le dernier mois") 
# list2env(df_list, envir = .GlobalEnv)
#Base prix2
df_listPrix2 <- map(Prix2, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listPrix2) <- Prix2
BasePrix2 <- map_df(df_listPrix2, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Prix produits Alimentaires de Céréales fortement augmenté")
# Base prix3
df_listPrix3 <- map(Prix3, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listPrix3) <- Prix3
BasePrix3 <- map_df(df_listPrix3, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Prix produits Alimentaires Autres fortement augmenté")
# Base prix4
df_listPrix4 <- map(Prix4, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listPrix4) <- Prix4
BasePrix4 <- map_df(df_listPrix4, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Prix produits Non Alimentaires fortement augmenté")
# Base prix5
df_listPrix5 <- map(Prix5, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listPrix5) <- Prix5
BasePrix5 <- map_df(df_listPrix5, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Êtes-vous en mesure d'estimer le prix qu'un produit aura la semaine Prochaine")
# Base prix6
df_listPrix6 <- map(Prix6, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listPrix6) <- Prix6
BasePrix6 <- map_df(df_listPrix6, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Les commerçants de ce marché peuvent-ils estimer le prix de la semaine prochaine pour")
# Base prix7
df_listPrix7 <- map(Prix7, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listPrix7) <- Prix7
BasePrix7 <- map_df(df_listPrix7, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Quels produits alimentaires de céréales n'estimeriez-vous pas  correctement")
# Base prix8
df_listPrix8 <- map(Prix8, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listPrix8) <- Prix8
BasePrix8 <- map_df(df_listPrix8, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Quels autres produits alimentaires n'estimeriez-vous pas correctement")
# Base prix9
df_listPrix9 <- map(Prix9, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
names(df_listPrix9) <- Prix9
BasePrix9 <- map_df(df_listPrix9, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Quels autres produits non alimentaires n'estimeriez-vous pas correctement")

BasePrix <- BasePrix1 %>%
  bind_rows(BasePrix2, BasePrix3,BasePrix4,BasePrix5,
            BasePrix6,BasePrix7,BasePrix8,BasePrix9)

#  Ajout des libellés
BasePrixAdmin1 <- BasePrix %>% mutate(
  modalite = VLOOKUP(.lookup_values = id, .data = codebook, .lookup_column = rowname,.return_column = V1)
)

BasePrixAdmin1 <- BasePrixAdmin1 %>% select(id,TrdNodDensLocNameAdm1,variable,modalite,everything()) %>% 
  rename(ADMIN1NAME = TrdNodDensLocNameAdm1)

BasePrixAdmin1 <- BasePrixAdmin1 %>% mutate(Dimension = "Prix") %>% 
  select(Dimension,everything()) %>% select(-id)


# Prix niveau Admin2 ------------------------------------------------------
# Base Prix1
df_listPrix1 <- map(Prix1, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix1) <- Prix1
BasePrix1 <- map_df(df_listPrix1, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable ="Y-a-t-il des produits dont le prix a fortement augmenté dans le dernier mois") 
# list2env(df_list, envir = .GlobalEnv)
#Base prix2
df_listPrix2 <- map(Prix2, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix2) <- Prix2
BasePrix2 <- map_df(df_listPrix2, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Prix produits Alimentaires de Céréales fortement augmenté")
# Base prix3
df_listPrix3 <- map(Prix3, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix3) <- Prix3
BasePrix3 <- map_df(df_listPrix3, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Prix produits Alimentaires Autres fortement augmenté")
# Base prix4
df_listPrix4 <- map(Prix4, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix4) <- Prix4
BasePrix4 <- map_df(df_listPrix4, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Prix produits Non Alimentaires fortement augmenté")
# Base prix5
df_listPrix5 <- map(Prix5, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix5) <- Prix5
BasePrix5 <- map_df(df_listPrix5, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Êtes-vous en mesure d'estimer le prix qu'un produit aura la semaine Prochaine")
# Base prix6
df_listPrix6 <- map(Prix6, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix6) <- Prix6
BasePrix6 <- map_df(df_listPrix6, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Les commerçants de ce marché peuvent-ils estimer le prix de la semaine prochaine pour")
# Base prix7
df_listPrix7 <- map(Prix7, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix7) <- Prix7
BasePrix7 <- map_df(df_listPrix7, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Quels produits alimentaires de céréales n'estimeriez-vous pas  correctement")
# Base prix8
df_listPrix8 <- map(Prix8, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix8) <- Prix8
BasePrix8 <- map_df(df_listPrix8, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Quels autres produits alimentaires n'estimeriez-vous pas correctement")
# Base prix9
df_listPrix9 <- map(Prix9, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix9) <- Prix9
BasePrix9 <- map_df(df_listPrix9, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Quels autres produits non alimentaires n'estimeriez-vous pas correctement")

BasePrix <- BasePrix1 %>%
  bind_rows(BasePrix2, BasePrix3,BasePrix4,BasePrix5,
            BasePrix6,BasePrix7,BasePrix8,BasePrix9)

BasePrix <- BasePrix %>% left_join(TableDeRechercheAdmin2,by = "TrdNodDensLocNameAdm2")

BasePrixAdmin2 <- BasePrix %>% mutate(
  modalite = VLOOKUP(.lookup_values = id, .data = codebook, .lookup_column = rowname,.return_column = V1)
)

BasePrixAdmin2 <- BasePrixAdmin2 %>% select(id,TrdNodDensLocNameAdm1,
                                                              TrdNodDensLocNameAdm2,variable,modalite,everything()) %>% 
  rename(ADMIN1NAME =TrdNodDensLocNameAdm1, ADMIN2NAME = TrdNodDensLocNameAdm2)

BasePrixAdmin2 <- BasePrixAdmin2 %>% mutate(Dimension = "Prix") %>% 
  select(Dimension,everything()) %>% select(-id)

# Prix au niveau marché ---------------------------------------------------
# Base Prix1
df_listPrix1 <- map(Prix1, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix1) <- Prix1
BasePrix1 <- map_df(df_listPrix1, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable ="Y-a-t-il des produits dont le prix a fortement augmenté dans le dernier mois") 
# list2env(df_list, envir = .GlobalEnv)
#Base prix2
df_listPrix2 <- map(Prix2, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix2) <- Prix2
BasePrix2 <- map_df(df_listPrix2, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Prix produits Alimentaires de Céréales fortement augmenté")
# Base prix3
df_listPrix3 <- map(Prix3, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix3) <- Prix3
BasePrix3 <- map_df(df_listPrix3, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Prix produits Alimentaires Autres fortement augmenté")
# Base prix4
df_listPrix4 <- map(Prix4, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix4) <- Prix4
BasePrix4 <- map_df(df_listPrix4, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Prix produits Non Alimentaires fortement augmenté")
# Base prix5
df_listPrix5 <- map(Prix5, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix5) <- Prix5
BasePrix5 <- map_df(df_listPrix5, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Êtes-vous en mesure d'estimer le prix qu'un produit aura la semaine Prochaine")
# Base prix6
df_listPrix6 <- map(Prix6, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix6) <- Prix6
BasePrix6 <- map_df(df_listPrix6, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Les commerçants de ce marché peuvent-ils estimer le prix de la semaine prochaine pour")
# Base prix7
df_listPrix7 <- map(Prix7, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix7) <- Prix7
BasePrix7 <- map_df(df_listPrix7, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Quels produits alimentaires de céréales n'estimeriez-vous pas  correctement")
# Base prix8
df_listPrix8 <- map(Prix8, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix8) <- Prix8
BasePrix8 <- map_df(df_listPrix8, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Quels autres produits alimentaires n'estimeriez-vous pas correctement")
# Base prix9
df_listPrix9 <- map(Prix9, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix9) <- Prix9
BasePrix9 <- map_df(df_listPrix9, ~as.data.frame(.x), .id = "id") %>% 
  mutate(variable = "Quels autres produits non alimentaires n'estimeriez-vous pas correctement")

BasePrix <- BasePrix1 %>%
  bind_rows(BasePrix2, BasePrix3,BasePrix4,BasePrix5,
            BasePrix6,BasePrix7,BasePrix8,BasePrix9)

BasePrix <- BasePrix %>% left_join(TableDeRechercheMarche,by = "MktNametext")


#  Ajout des libellés
BasePrixMarche <- BasePrix %>% mutate(
  modalite = VLOOKUP(.lookup_values = id, .data = codebook, .lookup_column = rowname,.return_column = V1)
)

BasePrixMarche <- BasePrixMarche %>% select(id,TrdNodDensLocNameAdm1,TrdNodDensLocNameAdm2,
                                                              MktNametext,variable,modalite,everything()) %>% 
  rename(ADMIN1NAME =TrdNodDensLocNameAdm1, ADMIN2NAME = TrdNodDensLocNameAdm2)

BasePrixMarche <- BasePrixMarche %>% mutate(Dimension = "Prix") %>% 
  select(Dimension,everything()) %>% select(-id)

# Résilience --------------------------------------------------------------


# Service -----------------------------------------------------------------


# Infrastructure ----------------------------------------------------------


# Competition -------------------------------------------------------------


# Qualité -----------------------------------------------------------------


# Acces et Protection -----------------------------------------------------









# regroupement des Base ----------------------------------------------------

BaseAdmin1 <- BaseAssortimentAdmin1 %>% 
  bind_rows(BaseDisponibiliteAdmin1, BasePrixAdmin1)

BaseAdmin2 <- BaseAssortimentAdmin2 %>% 
  bind_rows(BaseDisponibiliteAdmin2,BasePrixAdmin2)

BaseMarche <- BaseAssortimentMarche %>% 
  bind_rows(BaseDisponibiliteMarche,BasePrixMarche)

xlsx.writeMultipleData("Resultat_MFI_Bissau.xlsx",BaseAdmin1,BaseAdmin2,BaseMarche)
