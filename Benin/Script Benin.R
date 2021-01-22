
# import de pacakge -------------------------------------------------------

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

mfi_Benin <- read.csv("Benin/mfi_beta_1_2021_01_21_11_42_03_960985-group-name-removed.csv")%>% rename(
  Admin1NAME = TrdNodDensLocNameAdm1, Admin2NAME = TrdNodDensLocNameAdm2)
marche_benin <- read_xlsx("Benin/marche.xlsx")

spss <- read_sav("data_mfi_beta.sav")
spss <- to_factor(spss)
codebook <- var_label(spss)
codebook <- as.data.frame(do.call(rbind,codebook))
codebook <- codebook %>% rownames_to_column()
mfi_bissau <- mfi_Benin

# Fonctions nécessaires ---------------------------------------------------
# TrdNodDensLocNameAdm0 Niveau Admin0
# TrdNodDensLocNameAdm1 Niveau Admin1
# TrdNodDensLocNameAdm2 Niveau Admin2

tableauMarche <- function(d,colonne){
  d <- mfi_bissau %>% tabyl(MktNametext, {{colonne}},show_na = TRUE) %>% 
    # adorn_totals(where = c("col")) %>%
    adorn_percentages(denominator = "row") %>% 
    adorn_pct_formatting(digits = 0,affix_sign = FALSE)
  d <- d %>% mutate(
    Admin2name = VLOOKUP(.lookup_values = MktNametext,.data = TableDeRechercheMarche,.lookup_column = MktNametext,.return_column = Admin2NAME)
  )
  d <- d %>% mutate(
    Admin1name = VLOOKUP(.lookup_values = MktNametext,.data = TableDeRechercheMarche,.lookup_column = MktNametext,.return_column = Admin1NAME)
  )
  d <- d %>% relocate(Admin2name,.before = MktNametext)
  d <- d %>% relocate(Admin1name,.before = Admin2name)
}

tableauAdmin1 <- function(d,colonne){
  d <- mfi_bissau %>% tabyl(Admin1NAME, {{colonne}},show_na = TRUE) %>% 
    # adorn_totals(where = c("col")) %>%
    adorn_percentages(denominator = "row") %>% 
    adorn_pct_formatting(digits = 0,affix_sign = FALSE)
}

tableauAdmin2 <- function(d, colonne){
  d <- mfi_bissau %>% tabyl(Admin2NAME, {{colonne}},show_na = TRUE) %>% 
    # adorn_totals(where = c("col")) %>%
    adorn_percentages(denominator = "row") %>% 
    adorn_pct_formatting(digits = 0,affix_sign = FALSE)
  # d <- d %>% left_join(TableDeRechercheAdmin2,by = Admin2NAME)
  d <- d %>% mutate(
    Admin1name = VLOOKUP(.lookup_values = Admin2NAME,.data = TableDeRechercheAdmin2,.lookup_column = Admin2NAME,.return_column = Admin1NAME)
  )
  d <- d %>% relocate(Admin1name,.before = Admin2NAME)
}

# TableDeRecherche <- mfi_bissau %>% 
#   select(TrdNodDensLocNameAdm0, TrdNodDensLocNameAdm1,
#          TrdNodDensLocNameAdm2, TrdNodDensLocNameAdm3,
#           MktNametext)

TableDeRechercheAdmin2 <- mfi_bissau %>% select(Admin1NAME,Admin2NAME)
TableDeRechercheAdmin2 <- unique(TableDeRechercheAdmin2)
TableDeRechercheMarche <- mfi_bissau %>% select(Admin1NAME,Admin2NAME,MktNametext)
TableDeRechercheMarche <- unique(TableDeRechercheMarche)


# Assortiment -------------------------------------------------------------

mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("UOASoldGroup_FCer.fc","UOASoldGroup_FOth.fo",
                                        "UOASoldGroup_NF.nf","UOASoldGroup_Gr."))), 
                        funs(factor))
mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("UOASoldGroup_FCer.fc","UOASoldGroup_FOth.fo",
                                        "UOASoldGroup_NF.nf","UOASoldGroup_Gr."))),
                        ~recode_factor(.,"0"="Non", "1"="Oui"))

# toutes les variables
colonne <- colnames(mfi_bissau)

# selectionnez les types de produits qui sont habituellement vendus UOASoldGroup_FCer-fc
Assortiment1 <- list()
Assortiment2 <- list()
Assortiment3 <- list()
Assortiment4 <- list()

for (i in 1:length(colonne)) {
  if(str_detect(colonne[i], "UOASoldGroup_FCer.fc") ){
    Assortiment2 <- append(Assortiment2, colonne[i])
  }
  else if (str_detect(colonne[i], "UOASoldGroup_FOth.fo")) {
    Assortiment3 <- append(Assortiment3, colonne[i])
  }
  else if(str_detect(colonne[i], "UOASoldGroup_NF.nf")) {
    Assortiment4 <- append(Assortiment4, colonne[i])
  }
  else if(str_detect(colonne[i], "UOASoldGroup_Gr.")) {
    Assortiment1 <- append(Assortiment1, colonne[i])
  }
}

# Assortiment niveau Admin1 -----------------------------------------------

# base assortiment1
df_listAssotiment1 <- map(Assortiment1, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nom1 <- codebook %>% filter(rowname %in% Assortiment1) %>% select(V1) %>% pull()
nom1 <- paste("ASSORTIMENT : Quels sont les prouits Habituellement vendus",nom1,sep = "/")
names(df_listAssotiment1) <- nom1
# list2env(df_listAssotiment1, envir = .GlobalEnv)
# Base assortiment2
df_listAssotiment2 <- map(Assortiment2, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nom2 <- codebook %>% filter(rowname %in% Assortiment2) %>% select(V1) %>% pull()
nom2 <- paste("ASSORTIMENT : Types de céréales vendus",nom2,sep = "/")
names(df_listAssotiment2) <- nom2
# Base assortiment3
df_listAssotiment3 <- map(Assortiment3, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nom3 <- codebook %>% filter(rowname %in% Assortiment3) %>% select(V1) %>% pull()
nom3 <- paste("ASSORTIMENT : Quel Autres types de produits alimentaires Vendus",nom3,sep = "/")
names(df_listAssotiment3) <- nom3

# base4 assortiment4
df_listAssotiment4 <- map(Assortiment4, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nom4 <- codebook %>% filter(rowname %in% Assortiment4) %>% select(V1) %>% pull()
nom4 <- paste("ASSORTIMENT : Quel autres types de produits non alimentaire sont vendus",nom4,sep = "/")
names(df_listAssotiment4) <- nom4

AssortimentAdmin1 <- c(df_listAssotiment1,df_listAssotiment2,
                       df_listAssotiment3,df_listAssotiment4)

# Assortiment au niveau Admin2 --------------------------------------------

# base assortiment1
df_listAssotiment1 <- map(Assortiment1, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listAssotiment1) <- nom1
# Base assortiment2
df_listAssotiment2 <- map(Assortiment2, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listAssotiment2) <- nom2
# base3 assortiment3
df_listAssotiment3 <- map(Assortiment3, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listAssotiment3) <- nom3
# Base Assortiment4
df_listAssotiment4 <- map(Assortiment4, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listAssotiment4) <- nom4

AssortimentAdmin2 <- c(df_listAssotiment1,df_listAssotiment2,
                       df_listAssotiment3,df_listAssotiment4)

# Assortiment au niveau marché --------------------------------------------

# base assortiment1
df_listAssotiment1 <- map(Assortiment1, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listAssotiment1) <- nom1
# Base assortiment2
df_listAssotiment2 <- map(Assortiment2, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listAssotiment2) <- nom2
# base3 assortiment
df_listAssotiment3 <- map(Assortiment3, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listAssotiment3) <- nom3
# Base assortiment4
df_listAssotiment4 <- map(Assortiment4, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listAssotiment4) <- nom4

AssortimentMarche <- c(df_listAssotiment1,df_listAssotiment2,
                       df_listAssotiment3,df_listAssotiment4)

# Disponibilité -----------------------------------------------------------

mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("UOAAvailScarce_Gr.","UOAAvailScarce_FCer.fc",
                                        "UOAAvailScarce_FOth.fo","UOAAvailScarce_NF.nf","TrdAvailRunout_Gr.",
                                        "MktAvailRunout_Gr.","UOAAvailRunout_FCer.fc",
                                        "UOAAvailRunout_FOth.fo","UOAAvailRunout_NF.nf"))), 
                        funs(factor))

mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("UOAAvailScarce_Gr.","UOAAvailScarce_FCer.fc",
                                        "UOAAvailScarce_FOth.fo","UOAAvailScarce_NF.nf","TrdAvailRunout_Gr.",
                                        "MktAvailRunout_Gr.","UOAAvailRunout_FCer.fc",
                                        "UOAAvailRunout_FOth.fo","UOAAvailRunout_NF.nf"))),
                        ~recode_factor(.,"0"="Non", "1"="Oui"))

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
  if(str_detect(colonne[i], "UOAAvailScarce_Gr.") ){
    Disponibilite1 <- append(Disponibilite1, colonne[i])
  }
  else if (str_detect(colonne[i], "UOAAvailScarce_FCer.fc")) {
    Disponibilite2 <- append(Disponibilite2, colonne[i])
  }
  else if(str_detect(colonne[i], "UOAAvailScarce_FOth.fo")) {
    Disponibilite3 <- append(Disponibilite3, colonne[i])
  }else if(str_detect(colonne[i], "UOAAvailScarce_NF.nf")){
    Disponibilite4 <- append(Disponibilite4, colonne[i])
  }else if(str_detect(colonne[i], "TrdAvailRunout_Gr.")){
    Disponibilite5 <- append(Disponibilite5, colonne[i])
  }else if(str_detect(colonne[i], "MktAvailRunout_Gr.")){
    Disponibilite6 <- append(Disponibilite6, colonne[i])
  }else if(str_detect(colonne[i], "UOAAvailRunout_FCer.fc")){
    Disponibilite7 <- append(Disponibilite7, colonne[i])
  }else if(str_detect(colonne[i], "UOAAvailRunout_FOth.fo")){
    Disponibilite8 <- append(Disponibilite8, colonne[i])
  }else if(str_detect(colonne[i], "UOAAvailRunout_NF.nf")){
    Disponibilite9 <- append(Disponibilite9, colonne[i])
  }
}

# Disponibilte niveau admin1 ----------------------------------------------
# Base disponibilité1
df_listDisponibilite1 <- map(Disponibilite1, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd1 <- codebook %>% filter(rowname %in% Disponibilite1) %>% select(V1) %>% pull()
nomd1 <- paste("DISPONIBILITE : Ya t-il des produits rares dans le marché",nomd1,sep = "/")
names(df_listDisponibilite1) <- nomd1
# list2env(df_list, envir = .GlobalEnv)
#Base disponibilité2
df_listDisponibilite2 <- map(Disponibilite2, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd2 <- codebook %>% filter(rowname %in% Disponibilite2) %>% select(V1) %>% pull()
nomd2 <- paste("DISPONIBILITE : Produits alimentaire de céréales rares",nomd2,sep = "/")
names(df_listDisponibilite2) <- nomd2
# Base disponibilité3
df_listDisponibilite3 <- map(Disponibilite3, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd3 <- codebook %>% filter(rowname %in% Disponibilite3) %>% select(V1) %>% pull()
nomd3 <- paste("DISPONIBILITE : Produits alimentaires Autres rares",nomd3,sep = "/")
names(df_listDisponibilite3) <- nomd3
# Base disponibilité4
df_listDisponibilite4 <- map(Disponibilite4, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd4 <- codebook %>% filter(rowname %in% Disponibilite4) %>% select(V1) %>% pull()
nomd4 <- paste("DISPONIBILITE : Produits Non alimentaires  rares",nomd4,sep = "/")
names(df_listDisponibilite4) <- nomd4
# Base disponibilité5
df_listDisponibilite5 <- map(Disponibilite5, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd5 <- codebook %>% filter(rowname %in% Disponibilite5) %>% select(V1) %>% pull()
nomd5 <- paste("DISPONIBILITE : Avez-vous peur de manquer de stocks d'ici une semaine en ce qui concerne",nomd5,sep = "/")
names(df_listDisponibilite5) <- nomd5
# Base disponibilité6
df_listDisponibilite6 <- map(Disponibilite6, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd6 <- codebook %>% filter(rowname %in% Disponibilite6) %>% select(V1) %>% pull()
nomd6 <- paste("DISPONIBILITE : Les commerçants de ce marché ont-ils peur de manquer de stocks d'ici 1e semaine en ce qui concerne",nomd6,sep = "/")
names(df_listDisponibilite6) <- nomd6
# Base disponibilité7
df_listDisponibilite7 <- map(Disponibilite7, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd7 <- codebook %>% filter(rowname %in% Disponibilite7) %>% select(V1) %>% pull()
nomd7 <- paste("DISPONIBILITE : Manque de stocks de produits alimentaires de céréales d'ici une semaine",nomd7,sep = "/")
names(df_listDisponibilite7) <- nomd7
##   Base disponibilité8
df_listDisponibilite8 <- map(Disponibilite8, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd8 <- codebook %>% filter(rowname %in% Disponibilite8) %>% select(V1) %>% pull()
nomd8 <- paste("DISPONIBILITE : Manque de Stock d'autres produits alimentaires d'ici une semaine",nomd8,sep = "/")
names(df_listDisponibilite8) <- nomd8
# Base disponibilité9
df_listDisponibilite9 <- map(Disponibilite9, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd9 <- codebook %>% filter(rowname %in% Disponibilite9) %>% select(V1) %>% pull()
nomd9 <- paste("DISPONIBILITE : Manque de Stock de produits Nom alimentaires d'ici une semaine",nomd9,sep = "/")
names(df_listDisponibilite9) <- nomd9

DisponibiliteAdmin1 <- c(df_listDisponibilite1,df_listDisponibilite2,
                         df_listDisponibilite3,df_listDisponibilite4,
                         df_listDisponibilite5,df_listDisponibilite6,
                         df_listDisponibilite7,df_listDisponibilite8,
                         df_listDisponibilite9)


# Disponibilté au niveau admin2 -------------------------------------------
# Base disponibilité1
df_listDisponibilite1 <- map(Disponibilite1, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite1) <- nomd1
# list2env(df_list, envir = .GlobalEnv)
#Base disponibilité2
df_listDisponibilite2 <- map(Disponibilite2, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite2) <- nomd2
# Base disponibilité3
df_listDisponibilite3 <- map(Disponibilite3, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite3) <- nomd3
# Base disponibilité4
df_listDisponibilite4 <- map(Disponibilite4, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite4) <- nomd4
# Base disponibilité5
df_listDisponibilite5 <- map(Disponibilite5, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite5) <- nomd5
# Base disponibilité6
df_listDisponibilite6 <- map(Disponibilite6, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite6) <- nomd6
# Base disponibilité7
df_listDisponibilite7 <- map(Disponibilite7, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite7) <- nomd7
# Base disponibilité8
df_listDisponibilite8 <- map(Disponibilite8, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite8) <- nomd8
# Base disponibilité9
df_listDisponibilite9 <- map(Disponibilite9, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite9) <- nomd9

DisponibiliteAdmin2 <- c(df_listDisponibilite1,df_listDisponibilite2,
                         df_listDisponibilite3,df_listDisponibilite4,
                         df_listDisponibilite5,df_listDisponibilite6,
                         df_listDisponibilite7,df_listDisponibilite8,
                         df_listDisponibilite9)


# Disponibilité au niveau marché ------------------------------------------
# Base disponibilité1
df_listDisponibilite1 <- map(Disponibilite1, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite1) <- nomd1
# list2env(df_list, envir = .GlobalEnv)
#Base disponibilité2
df_listDisponibilite2 <- map(Disponibilite2, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite2) <- nomd2
# Base disponibilité3
df_listDisponibilite3 <- map(Disponibilite3, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite3) <- nomd3
# Base disponibilité4
df_listDisponibilite4 <- map(Disponibilite4, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite4) <- nomd4
# Base disponibilité5
df_listDisponibilite5 <- map(Disponibilite5, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite5) <- nomd5
# Base disponibilité6
df_listDisponibilite6 <- map(Disponibilite6, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite6) <- nomd6
# Base disponibilité7
df_listDisponibilite7 <- map(Disponibilite7, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite7) <- nomd7
# Base disponibilité8
df_listDisponibilite8 <- map(Disponibilite8, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite8) <- nomd8
# Base disponibilité9
df_listDisponibilite9 <- map(Disponibilite9, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listDisponibilite9) <- nomd9

DisponibiliteMarche <- c(df_listDisponibilite1,df_listDisponibilite2,
                         df_listDisponibilite3,df_listDisponibilite4,
                         df_listDisponibilite5,df_listDisponibilite6,
                         df_listDisponibilite7,df_listDisponibilite8,
                         df_listDisponibilite9)

# Prix --------------------------------------------------------------------

mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("UOAPriceIncr_Gr.","UOAPriceIncr_FCer.fc",
                                        "UOAPriceIncr_FOth.fo","UOAPriceIncr_NF.nf","TrdPriceStab_Gr.",
                                        "MktPriceStab_Gr.","UOAPriceUnstab_FCer.fc",
                                        "UOAPriceUnstab_FOth.fo","UOAPriceUnstab_NF.nf"))), 
                        funs(factor))

mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("UOAPriceIncr_Gr.","UOAPriceIncr_FCer.fc",
                                        "UOAPriceIncr_FOth.fo","UOAPriceIncr_NF.nf","TrdPriceStab_Gr.",
                                        "MktPriceStab_Gr.","UOAPriceUnstab_FCer.fc",
                                        "UOAPriceUnstab_FOth.fo","UOAPriceUnstab_NF.nf"))),
                        ~recode_factor(.,"0"="Non", "1"="Oui"))
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
  if(str_detect(colonne[i], "UOAPriceIncr_Gr.") ){
    Prix1 <- append(Prix1, colonne[i])
  }
  else if (str_detect(colonne[i], "UOAPriceIncr_FCer.fc")) {
    Prix2 <- append(Prix2, colonne[i])
  }
  else if(str_detect(colonne[i], "UOAPriceIncr_FOth.fo")) {
    Prix3 <- append(Prix3, colonne[i])
  }else if(str_detect(colonne[i], "UOAPriceIncr_NF.nf")){
    Prix4 <- append(Prix4, colonne[i])
  }else if(str_detect(colonne[i], "TrdPriceStab_Gr.")){
    Prix5 <- append(Prix5, colonne[i])
  }else if(str_detect(colonne[i], "MktPriceStab_Gr.")){
    Prix6 <- append(Prix6, colonne[i])
  }else if(str_detect(colonne[i], "UOAPriceUnstab_FCer.fc")){
    Prix7 <- append(Prix7, colonne[i])
  }else if(str_detect(colonne[i], "UOAPriceUnstab_FOth.fo")){
    Prix8 <- append(Prix8, colonne[i])
  }else if(str_detect(colonne[i], "UOAPriceUnstab_NF.nf")){
    Prix9 <- append(Prix9, colonne[i])
  }
}

# Prix au niveau Admin1 ---------------------------------------------------

# Base Prix1
df_listPrix1 <- map(Prix1, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP1 <- codebook %>% filter(rowname %in% Prix1) %>% select(V1) %>% pull()
nomP1 <- paste("PRIX  : Y-a-t-il des produits dont le prix a fortement augmenté dans le dernier mois",nomP1,sep = "/")
names(df_listPrix1) <- nomP1
#Base prix2
df_listPrix2 <- map(Prix2, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP2 <- codebook %>% filter(rowname %in% Prix2) %>% select(V1) %>% pull()
nomP2 <- paste("PRIX  : Prix produits Alimentaires de Céréales fortement augmenté",nomP2,sep = "/")
names(df_listPrix2) <- nomP2
# Base prix3
df_listPrix3 <- map(Prix3, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP3 <- codebook %>% filter(rowname %in% Prix3) %>% select(V1) %>% pull()
nomP3 <- paste("PRIX  : Prix produits Alimentaires Autres fortement augmenté",nomP3,sep = "/")
names(df_listPrix3) <- nomP3
# Base prix4
df_listPrix4 <- map(Prix4, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP4 <- codebook %>% filter(rowname %in% Prix4) %>% select(V1) %>% pull()
nomP4 <- paste("PRIX  : Prix produits Non Alimentaires fortement augmenté",nomP4,sep = "/")
names(df_listPrix4) <- nomP4
# Base prix5
df_listPrix5 <- map(Prix5, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP5 <- codebook %>% filter(rowname %in% Prix5) %>% select(V1) %>% pull()
nomP5 <- paste("PRIX  : Êtes-vous en mesure d'estimer le prix qu'un produit aura la semaine Prochaine",nomP5,sep = "/")
names(df_listPrix5) <- nomP5
# Base prix6
df_listPrix6 <- map(Prix6, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP6 <- codebook %>% filter(rowname %in% Prix6) %>% select(V1) %>% pull()
nomP6 <- paste("PRIX  : Les commerçants de ce marché peuvent-ils estimer le prix de la semaine prochaine pour",nomP6,sep = "/")
names(df_listPrix6) <- nomP6
# Base prix7
df_listPrix7 <- map(Prix7, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP7 <- codebook %>% filter(rowname %in% Prix7) %>% select(V1) %>% pull()
nomP7 <- paste("PRIX  : Quels produits alimentaires de céréales n'estimeriez-vous pas  correctement",nomP7,sep = "/")
names(df_listPrix7) <- nomP7
# Base prix8
df_listPrix8 <- map(Prix8, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP8 <- codebook %>% filter(rowname %in% Prix8) %>% select(V1) %>% pull()
nomP8 <- paste("PRIX  : Quels autres produits alimentaires n'estimeriez-vous pas correctement",nomP8,sep = "/")
names(df_listPrix8) <- nomP8
# Base prix9
df_listPrix9 <- map(Prix9, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP9 <- codebook %>% filter(rowname %in% Prix9) %>% select(V1) %>% pull()
nomP9 <- paste("PRIX  : Quels autres produits non alimentaires n'estimeriez-vous pas correctement",nomP9,sep = "/")
names(df_listPrix9) <- nomP9

PrixAdmin1 <- c(df_listPrix1,df_listPrix2,df_listPrix3,
                df_listPrix4,df_listPrix5,df_listPrix6,
                df_listPrix7,df_listPrix8,df_listPrix9)



# Prix niveau Admin2 ------------------------------------------------------
# Base Prix1
df_listPrix1 <- map(Prix1, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix1) <- nomP1
# list2env(df_list, envir = .GlobalEnv)
#Base prix2
df_listPrix2 <- map(Prix2, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix2) <- nomP2
# Base prix3
df_listPrix3 <- map(Prix3, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix3) <- nomP3
# Base prix4
df_listPrix4 <- map(Prix4, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix4) <- nomP4
# Base prix5
df_listPrix5 <- map(Prix5, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix5) <- nomP5
# Base prix6
df_listPrix6 <- map(Prix6, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix6) <- nomP6
# Base prix7
df_listPrix7 <- map(Prix7, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix7) <- nomP7
# Base prix8
df_listPrix8 <- map(Prix8, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix8) <- nomP8
# Base prix9
df_listPrix9 <- map(Prix9, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listPrix9) <- nomP9

PrixAdmin2 <- c(df_listPrix1,df_listPrix2,df_listPrix3,
                df_listPrix4,df_listPrix5,df_listPrix6,
                df_listPrix7,df_listPrix8,df_listPrix9)


# Prix au niveau marché ---------------------------------------------------
# Base Prix1
df_listPrix1 <- map(Prix1, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix1) <- nomP1
# list2env(df_list, envir = .GlobalEnv)
#Base prix2
df_listPrix2 <- map(Prix2, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix2) <- nomP2
# Base prix3
df_listPrix3 <- map(Prix3, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix3) <- nomP3
# Base prix4
df_listPrix4 <- map(Prix4, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix4) <- nomP4
# Base prix5
df_listPrix5 <- map(Prix5, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix5) <- nomP5
# Base prix6
df_listPrix6 <- map(Prix6, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix6) <- nomP6
# Base prix7
df_listPrix7 <- map(Prix7, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix7) <- nomP7
# Base prix8
df_listPrix8 <- map(Prix8, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix8) <- nomP8
# Base prix9
df_listPrix9 <- map(Prix9, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listPrix9) <- nomP9

PrixMarche <- c(df_listPrix1,df_listPrix2,df_listPrix3,
                df_listPrix4,df_listPrix5,df_listPrix6,
                df_listPrix7,df_listPrix8,df_listPrix9)
# Résilience --------------------------------------------------------------


# Service -----------------------------------------------------------------


# Infrastructure ----------------------------------------------------------


# Competition -------------------------------------------------------------


# Qualité -----------------------------------------------------------------


# Acces et Protection -----------------------------------------------------



# regroupement des Base ----------------------------------------------------

BaseAdmin1 <- c(AssortimentAdmin1,DisponibiliteAdmin1,PrixAdmin1)

BaseAdmin2 <- c(AssortimentAdmin2, DisponibiliteAdmin2,PrixAdmin2)

BaseMarche <- c(AssortimentMarche, DisponibiliteMarche, PrixMarche)


# Admin1
wb <- createWorkbook()
addWorksheet(wb, "Foo")

curr_row <- 1
for(i in seq_along(BaseAdmin1)) {
  writeData(wb, "Foo", names(BaseAdmin1)[i], startCol = 1, startRow = curr_row)
  writeData(wb, "Foo", BaseAdmin1[[i]], startCol = 1, startRow = curr_row+1)
  curr_row <- curr_row + 1 + nrow(BaseAdmin1[[i]]) + 2
}

saveWorkbook(wb, "Benin/Admin1.xlsx")

# ADmin2
wb <- createWorkbook()
addWorksheet(wb, "Foo")

curr_row <- 1
for(i in seq_along(BaseAdmin2)) {
  writeData(wb, "Foo", names(BaseAdmin2)[i], startCol = 1, startRow = curr_row)
  writeData(wb, "Foo", BaseAdmin2[[i]], startCol = 1, startRow = curr_row+1)
  curr_row <- curr_row + 1 + nrow(BaseAdmin2[[i]]) + 2
}

saveWorkbook(wb, "Benin/Admin2.xlsx")

# Marché
wb <- createWorkbook()
addWorksheet(wb, "Foo")

curr_row <- 1
for(i in seq_along(BaseMarche)) {
  writeData(wb, "Foo", names(BaseMarche)[i], startCol = 1, startRow = curr_row)
  writeData(wb, "Foo", BaseMarche[[i]], startCol = 1, startRow = curr_row+1)
  curr_row <- curr_row + 1 + nrow(BaseMarche[[i]]) + 2
}

saveWorkbook(wb, "Benin/Marche.xlsx")
