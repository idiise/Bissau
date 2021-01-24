
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
# library(flextable)
# library(officer)
# library(rlist)
# Import Dataset ----------------------------------------------------------

# GNB1 <-  read.csv("GNB_MFI_Dataset_September_2020.csv")
# GNB2 <- read_xlsx("GNB_MFI_Dataset_September_2020_v2.xlsx")
# marche <- readxl::read_xlsx("Marche_Bissau.xlsx")
# nomfichier2 <- read_xlsx("nomfichier2.xlsx")
mfi_bissau <- readxl::read_xlsx("mfi_bissau.xlsx")%>% rename(
  Admin1NAME = TrdNodDensLocNameAdm1, Admin2NAME = TrdNodDensLocNameAdm2
)

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
                        vars(contains(c("UOASoldGroup_FCer-fc","UOASoldGroup_FOth-fo",
                                        "UOASoldGroup_NF-nf","UOASoldGroup_Gr-"))), 
                        funs(factor))
mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("UOASoldGroup_FCer-fc","UOASoldGroup_FOth-fo",
                                        "UOASoldGroup_NF-nf","UOASoldGroup_Gr-"))),
                        ~recode_factor(.,"0"="Non", "1"="Oui"))

codebook$rowname <- str_replace(codebook$rowname,"UOASoldGroup_FCer.fc","UOASoldGroup_FCer-fc")
codebook$rowname <- str_replace(codebook$rowname,"UOASoldGroup_FOth.fo","UOASoldGroup_FOth-fo")
codebook$rowname <- str_replace(codebook$rowname,"UOASoldGroup_NF.nf","UOASoldGroup_NF-nf")
codebook$rowname <- str_replace(codebook$rowname,"UOASoldGroup_Gr.","UOASoldGroup_Gr-")

# toutes les variables
colonne <- colnames(mfi_bissau)

# selectionnez les types de produits qui sont habituellement vendus UOASoldGroup_FCer-fc
Assortiment1 <- list()
Assortiment2 <- list()
Assortiment3 <- list()
Assortiment4 <- list()

for (i in 1:length(colonne)) {
  if(str_detect(colonne[i], "UOASoldGroup_FCer-fc") ){
    Assortiment2 <- append(Assortiment2, colonne[i])
  }
  else if (str_detect(colonne[i], "UOASoldGroup_FOth-fo")) {
    Assortiment3 <- append(Assortiment3, colonne[i])
  }
  else if(str_detect(colonne[i], "UOASoldGroup_NF-nf")) {
    Assortiment4 <- append(Assortiment4, colonne[i])
  }
  else if(str_detect(colonne[i], "UOASoldGroup_Gr-")) {
    Assortiment1 <- append(Assortiment1, colonne[i])
  }
}
# Remplacer le separateur pour avoir une correspondance avec les libellés de la base spsss
# Assortiment1 <- map(Assortiment1,~str_replace(.x, "-","."))
# Assortiment2 <- map(Assortiment2,~str_replace(.x, "-","."))
# Assortiment3 <- map(Assortiment3,~str_replace(.x, "-","."))


# Assortiment au niveau admin1 --------------------------------------------
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

AssortimentAdmin1 <- append(df_listAssotiment1,df_listAssotiment2)
AssortimentAdmin1 <- append(AssortimentAdmin1,df_listAssotiment3)
AssortimentAdmin1 <- append(AssortimentAdmin1,df_listAssotiment4)


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

AssortimentAdmin2 <- append(df_listAssotiment1,df_listAssotiment2)
AssortimentAdmin2 <- append(AssortimentAdmin2,df_listAssotiment3)
AssortimentAdmin2 <- append(AssortimentAdmin2,df_listAssotiment4)


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


AssortimentMarche <- append(df_listAssotiment1,df_listAssotiment2)
AssortimentMarche <- append(AssortimentMarche,df_listAssotiment3)
AssortimentMarche <- append(AssortimentMarche,df_listAssotiment4)
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
nomd1 <- codebook %>% filter(rowname %in% Disponibilite1) %>% select(V1) %>% pull()
nomd1 <- paste("DISPONIBILITE : Ya t-il des produits rares dans le marché",nomd1,sep = "/")
names(df_listDisponibilite1) <- nomd1
# BaseDisponibilite1 <- map_df(df_listDisponibilite1, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable ="Ya t-il des produits rares dans le marché") 
# list2env(df_list, envir = .GlobalEnv)
#Base disponibilité2
Disponibilite2 <- Disponibilite2[-12]
df_listDisponibilite2 <- map(Disponibilite2, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd2 <- codebook %>% filter(rowname %in% Disponibilite2) %>% select(V1) %>% pull()
nomd2 <- paste("DISPONIBILITE : Produits alimentaire de céréales rares",nomd2,sep = "/")
names(df_listDisponibilite2) <- nomd2
# BaseDisponibilite2 <- map_df(df_listDisponibilite2, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Produits alimentaire de céréales rares")
# Base disponibilité3
Disponibilite3 <- Disponibilite3[-9]
df_listDisponibilite3 <- map(Disponibilite3, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd3 <- codebook %>% filter(rowname %in% Disponibilite3) %>% select(V1) %>% pull()
nomd3 <- paste("DISPONIBILITE : Produits alimentaires Autres rares",nomd3,sep = "/")
names(df_listDisponibilite3) <- nomd3
# BaseDisponibilite3 <- map_df(df_listDisponibilite3, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Produits alimentaires Autres rares")
# Base disponibilité4
Disponibilite4 <- Disponibilite4[-14]
df_listDisponibilite4 <- map(Disponibilite4, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd4 <- codebook %>% filter(rowname %in% Disponibilite4) %>% select(V1) %>% pull()
nomd4 <- paste("DISPONIBILITE : Produits Non alimentaires  rares",nomd4,sep = "/")
names(df_listDisponibilite4) <- nomd4
# BaseDisponibilite4 <- map_df(df_listDisponibilite4, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Produits Non alimentaires  rares")
# Base disponibilité5
df_listDisponibilite5 <- map(Disponibilite5, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd5 <- codebook %>% filter(rowname %in% Disponibilite5) %>% select(V1) %>% pull()
nomd5 <- paste("DISPONIBILITE : Avez-vous peur de manquer de stocks d'ici une semaine en ce qui concerne",nomd5,sep = "/")
names(df_listDisponibilite5) <- nomd5
# BaseDisponibilite5 <- map_df(df_listDisponibilite5, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Avez-vous peur de manquer de stocks d'ici une semaine en ce qui concerne")
# Base disponibilité6
df_listDisponibilite6 <- map(Disponibilite6, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd6 <- codebook %>% filter(rowname %in% Disponibilite6) %>% select(V1) %>% pull()
nomd6 <- paste("DISPONIBILITE : Les commerçants de ce marché ont-ils peur de manquer de stocks d'ici 1e semaine en ce qui concerne",nomd6,sep = "/")
names(df_listDisponibilite6) <- nomd6
# BaseDisponibilite6 <- map_df(df_listDisponibilite6, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Les commerçants de ce marché ont-ils peur de manquer de stocks d'ici 1e semaine en ce qui concerne")
# Base disponibilité7
Disponibilite7 <- Disponibilite7[-12]
df_listDisponibilite7 <- map(Disponibilite7, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd7 <- codebook %>% filter(rowname %in% Disponibilite7) %>% select(V1) %>% pull()
nomd7 <- paste("DISPONIBILITE : Manque de stocks de produits alimentaires de céréales d'ici une semaine",nomd7,sep = "/")
names(df_listDisponibilite7) <- nomd7
# BaseDisponibilite7 <- map_df(df_listDisponibilite7, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Manque de stocks de produits alimentaires de céréales d'ici une semaine")
##   Base disponibilité8
Disponibilite8 <- Disponibilite8[-9]
df_listDisponibilite8 <- map(Disponibilite8, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd8 <- codebook %>% filter(rowname %in% Disponibilite8) %>% select(V1) %>% pull()
nomd8 <- paste("DISPONIBILITE : Manque de Stock d'autres produits alimentaires d'ici une semaine",nomd8,sep = "/")
names(df_listDisponibilite8) <- nomd8
# BaseDisponibilite8 <- map_df(df_listDisponibilite8, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Manque de Stock d'autres produits alimentaires d'ici une semaine")
# Base disponibilité9
Disponibilite9 <- Disponibilite9[-14]
df_listDisponibilite9 <- map(Disponibilite9, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomd9 <- codebook %>% filter(rowname %in% Disponibilite9) %>% select(V1) %>% pull()
nomd9 <- paste("DISPONIBILITE : Manque de Stock de produits Non alimentaires d'ici une semaine",nomd9,sep = "/")
names(df_listDisponibilite9) <- nomd9
# BaseDisponibilite9 <- map_df(df_listDisponibilite9, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Manque de Stock de produits Nom alimentaires d'ici une semaine")

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
codebook$rowname <- str_replace(codebook$rowname,"UOAPriceUnstab_NF.nf","UOAPriceUnstab_NF-nf")

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
nomP1 <- codebook %>% filter(rowname %in% Prix1) %>% select(V1) %>% pull()
nomP1 <- paste("PRIX  : Y-a-t-il des produits dont le prix a fortement augmenté dans le dernier mois",nomP1,sep = "/")
names(df_listPrix1) <- nomP1
# BasePrix1 <- map_df(df_listPrix1, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable ="Y-a-t-il des produits dont le prix a fortement augmenté dans le dernier mois") 
# list2env(df_list, envir = .GlobalEnv)
#Base prix2
Prix2 <- Prix2[-12]
df_listPrix2 <- map(Prix2, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP2 <- codebook %>% filter(rowname %in% Prix2) %>% select(V1) %>% pull()
nomP2 <- paste("PRIX  : Prix produits Alimentaires de Céréales fortement augmenté",nomP2,sep = "/")
names(df_listPrix2) <- nomP2
# BasePrix2 <- map_df(df_listPrix2, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Prix produits Alimentaires de Céréales fortement augmenté")
# Base prix3
Prix3 <- Prix3[-9]
df_listPrix3 <- map(Prix3, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP3 <- codebook %>% filter(rowname %in% Prix3) %>% select(V1) %>% pull()
nomP3 <- paste("PRIX  : Prix produits Alimentaires Autres fortement augmenté",nomP3,sep = "/")
names(df_listPrix3) <- nomP3
# BasePrix3 <- map_df(df_listPrix3, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Prix produits Alimentaires Autres fortement augmenté")
# Base prix4
Prix4 <- Prix4[-14]
df_listPrix4 <- map(Prix4, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP4 <- codebook %>% filter(rowname %in% Prix4) %>% select(V1) %>% pull()
nomP4 <- paste("PRIX  : Prix produits Non Alimentaires fortement augmenté",nomP4,sep = "/")
names(df_listPrix4) <- nomP4
# BasePrix4 <- map_df(df_listPrix4, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Prix produits Non Alimentaires fortement augmenté")
# Base prix5
df_listPrix5 <- map(Prix5, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP5 <- codebook %>% filter(rowname %in% Prix5) %>% select(V1) %>% pull()
nomP5 <- paste("PRIX  : Êtes-vous en mesure d'estimer le prix qu'un produit aura la semaine Prochaine",nomP5,sep = "/")
names(df_listPrix5) <- nomP5
# BasePrix5 <- map_df(df_listPrix5, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Êtes-vous en mesure d'estimer le prix qu'un produit aura la semaine Prochaine")
# Base prix6
df_listPrix6 <- map(Prix6, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP6 <- codebook %>% filter(rowname %in% Prix6) %>% select(V1) %>% pull()
nomP6 <- paste("PRIX  : Les commerçants de ce marché peuvent-ils estimer le prix de la semaine prochaine pour",nomP6,sep = "/")
names(df_listPrix6) <- nomP6
# BasePrix6 <- map_df(df_listPrix6, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Les commerçants de ce marché peuvent-ils estimer le prix de la semaine prochaine pour")
# Base prix7
Prix7 <- Prix7[-12]
df_listPrix7 <- map(Prix7, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP7 <- codebook %>% filter(rowname %in% Prix7) %>% select(V1) %>% pull()
nomP7 <- paste("PRIX  : Quels produits alimentaires de céréales n'estimeriez-vous pas  correctement",nomP7,sep = "/")
names(df_listPrix7) <- nomP7
# BasePrix7 <- map_df(df_listPrix7, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Quels produits alimentaires de céréales n'estimeriez-vous pas  correctement")
# Base prix8
Prix8 <- Prix8[-9]
df_listPrix8 <- map(Prix8, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP8 <- codebook %>% filter(rowname %in% Prix8) %>% select(V1) %>% pull()
nomP8 <- paste("PRIX  : Quels autres produits alimentaires n'estimeriez-vous pas correctement",nomP8,sep = "/")
names(df_listPrix8) <- nomP8
# BasePrix8 <- map_df(df_listPrix8, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Quels autres produits alimentaires n'estimeriez-vous pas correctement")
# Base prix9
Prix9 <- Prix9[-14]
df_listPrix9 <- map(Prix9, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomP9 <- codebook %>% filter(rowname %in% Prix9) %>% select(V1) %>% pull()
nomP9 <- paste("PRIX  : Quels autres produits non alimentaires n'estimeriez-vous pas correctement",nomP9,sep = "/")
names(df_listPrix9) <- nomP9
# BasePrix9 <- map_df(df_listPrix9, ~as.data.frame(.x), .id = "id") %>% 
#   mutate(variable = "Quels autres produits non alimentaires n'estimeriez-vous pas correctement")

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
#1 TrdResilStockout Considérant la demande de vos clients, votre stock actuel vous permet-il d'y répondre
#                         pendant 1 semaine?
#2 TrdResilLeadtime Si vous commandez chez votre fournisseur aujourd'hui, pensez-vous recevoir vos
#                    produits en semaine?
#3 TrdResilNodDens_Gr- La majorité de vos fournisseurs se trouvent-ils dans la même zone géographique pour
#4 TrdResilNodDens_FCer-fc Plus précisement, pour quels produits Alimentaires de céréales la plupart de vos fournisseurs sont-ils
#                         géographiquement situés au même endroit?
#5 TrdResilNodDens_FOth-fo Plus précisement, pour quels autres produits Alimentaires la plupart de vos fournisseurs sont-ils
#                         géographiquement situés au même endroit?
#6 TrdResilNodDens_NF-nf  Plus précisement, pour quels produits non Alimentaires la plupart de vos fournisseurs sont-ils
#                         géographiquement situés au même endroit?
#7 TrdResilNodComplex_Gr- Avez-vous actuellement plus d'un fournisseur pour:
#8 TrdResilNodComplex_FCer-fc  Plus précisement, pour quels produits alimentaires de céréals avez-vous actuellement UN SEUL (1)
#                            fournisseur?:
#9 TrdResilNodComplex_FOth-fo Plus précisement, pour quels autres produits produits avez-vous actuellement UN SEUL (1)
#                            fournisseur?:
#10 TrdResilNodComplex_NF-nf  Plus précisement, pour quels produits non alimentaires avez-vous actuellement UN SEUL (1)
#                            fournisseur?
#11 TrdResilNodCrit_Gr-  Votre entreprise compte-t-elle principalement sur un seul fournisseur pour:
#12 TrdResilNodCrit_FCer-fc Plus précisément, pour quels produits alimentaires de céréales existe-t-il un seul fournisseur de qui dépend la
#                          plupart de votre entreprise?:
# 13	TrdResilNodCrit_FOth-fo  Plus précisément, pour quels autres produits alimentaires existe-t-il un seul fournisseur de qui dépend la
#                          plupart de votre entreprise?:
#14 TrdResilNodCrit_NF-nf   Plus précisément, pour quels produits non alimentaires existe-t-il un seul fournisseur de qui dépend la
#                          plupart de votre entreprise?:

mfi_bissau <- mutate_at(mfi_bissau, 
            vars(contains(c("TrdResilStockout","TrdResilLeadtime","TrdResilNodDens_Gr-",
              "TrdResilNodDens_FCer-fc","TrdResilNodDens_FOth-fo","TrdResilNodDens_NF-nf",      
            "TrdResilNodComplex_Gr-","TrdResilNodComplex_FCer-fc","TrdResilNodComplex_FOth-fo",
            "TrdResilNodComplex_NF-nf","TrdResilNodCrit_Gr-","TrdResilNodCrit_FCer-fc",
            "TrdResilNodCrit_FOth-fo","TrdResilNodCrit_NF-nf"
                        ))), 
                        funs(factor))

mfi_bissau <- mutate_at(mfi_bissau, 
  vars(contains(c("TrdResilStockout","TrdResilLeadtime","TrdResilNodDens_Gr-",
                  "TrdResilNodDens_FCer-fc","TrdResilNodDens_FOth-fo","TrdResilNodDens_NF-nf",      
                  "TrdResilNodComplex_Gr-","TrdResilNodComplex_FCer-fc","TrdResilNodComplex_FOth-fo",
                  "TrdResilNodComplex_NF-nf","TrdResilNodCrit_Gr-","TrdResilNodCrit_FCer-fc",
                  "TrdResilNodCrit_FOth-fo","TrdResilNodCrit_NF-nf"))),
                        ~recode_factor(.,"0"="Non", "1"="Oui"))

codebook$rowname <- str_replace(codebook$rowname,"TrdResilStockout", "TrdResilStockout")
codebook$rowname <- str_replace(codebook$rowname,"TrdResilLeadtime", "TrdResilLeadtime")
codebook$rowname <- str_replace(codebook$rowname,"TrdResilNodDens_Gr.", "TrdResilNodDens_Gr-")
codebook$rowname <- str_replace(codebook$rowname,"TrdResilNodDens_FCer.fc", "TrdResilNodDens_FCer-fc")
codebook$rowname <- str_replace(codebook$rowname,"TrdResilNodDens_FOth.fo", "TrdResilNodDens_FOth-fo")
codebook$rowname <- str_replace(codebook$rowname,"TrdResilNodDens_NF.nf", "TrdResilNodDens_NF-nf")
codebook$rowname <- str_replace(codebook$rowname,"TrdResilNodComplex_Gr.", "TrdResilNodComplex_Gr-")
codebook$rowname <- str_replace(codebook$rowname,"TrdResilNodComplex_FCer.fc", "TrdResilNodComplex_FCer-fc")
codebook$rowname <- str_replace(codebook$rowname,"TrdResilNodComplex_FOth.fo", "TrdResilNodComplex_FOth-fo")
codebook$rowname <- str_replace(codebook$rowname,"TrdResilNodComplex_NF.nf", "TrdResilNodComplex_NF-nf")
codebook$rowname <- str_replace(codebook$rowname,"TrdResilNodCrit_Gr.", "TrdResilNodCrit_Gr-")
codebook$rowname <- str_replace(codebook$rowname,"TrdResilNodCrit_FCer.fc", "TrdResilNodCrit_FCer-fc")
codebook$rowname <- str_replace(codebook$rowname,"TrdResilNodCrit_FOth.fo", "TrdResilNodCrit_FOth-fo")
codebook$rowname <- str_replace(codebook$rowname,"TrdResilNodCrit_NF.nf", "TrdResilNodCrit_NF-nf")

# toutes les variables
colonne <- colnames(mfi_bissau)

# Création des différentes listes
Resilience1 <- list()
Resilience2 <- list()
Resilience3 <- list()
Resilience4 <- list()
Resilience5 <- list()
Resilience6 <- list()
Resilience7 <- list()
Resilience8 <- list()
Resilience9 <- list()
Resilience10 <- list()
Resilience11 <- list()
Resilience12 <- list()
Resilience13 <- list()
Resilience14 <- list()

for (i in 1:length(colonne)) {
  if(str_detect(colonne[i], "TrdResilStockout") ){
    Resilience1 <- append(Resilience1, colonne[i])
  }
  else if (str_detect(colonne[i], "TrdResilLeadtime")) {
    Resilience2 <- append(Resilience2, colonne[i])
  }
  else if (str_detect(colonne[i], "TrdResilNodDens_Gr-")) {
    Resilience3 <- append(Resilience3, colonne[i])
  }
  else if (str_detect(colonne[i], "TrdResilNodDens_FCer-fc")) {
    Resilience4 <- append(Resilience4, colonne[i])
  }
  else if (str_detect(colonne[i], "TrdResilNodDens_FOth-fo")) {
    Resilience5 <- append(Resilience5, colonne[i])
  }
  else if (str_detect(colonne[i], "TrdResilNodDens_NF-nf")) {
    Resilience6 <- append(Resilience6, colonne[i])
  }
  else if (str_detect(colonne[i], "TrdResilNodComplex_Gr-")) {
    Resilience7 <- append(Resilience7, colonne[i])
  }
  else if (str_detect(colonne[i], "TrdResilNodComplex_FCer-fc")) {
    Resilience8 <- append(Resilience8, colonne[i])
  }
  else if (str_detect(colonne[i], "TrdResilNodComplex_FOth-fo")) {
    Resilience9 <- append(Resilience9, colonne[i])
  }
  else if (str_detect(colonne[i], "TrdResilNodComplex_NF-nf")) {
    Resilience10 <- append(Resilience10, colonne[i])
  }
  else if (str_detect(colonne[i], "TrdResilNodCrit_Gr-")) {
    Resilience11 <- append(Resilience11, colonne[i])
  }
  else if (str_detect(colonne[i], "TrdResilNodCrit_FCer-fc")) {
    Resilience12 <- append(Resilience12, colonne[i])
  }
  else if (str_detect(colonne[i], "TrdResilNodCrit_FOth-fo")) {
    Resilience13 <- append(Resilience13, colonne[i])
  }
  else if (str_detect(colonne[i], "TrdResilNodCrit_NF-nf")) {
    Resilience14 <- append(Resilience14, colonne[i])
  }
}


# Résilience au niveau Admin1 ---------------------------------------------

# Base Résilience1
df_listResilience1 <- map(Resilience1, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR1 <- codebook %>% filter(rowname %in% Resilience1) %>% select(V1) %>% pull()
nomR1 <- paste("RESILIENCE  : ",nomR1,sep = "/")
names(df_listResilience1) <- nomR1
# Base Résilience2
df_listResilience2 <- map(Resilience2, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR2 <- codebook %>% filter(rowname %in% Resilience2) %>% select(V1) %>% pull()
nomR2 <- paste("RESILIENCE  : ",nomR2,sep = "/")
names(df_listResilience2) <- nomR2
# Base Résilience3
df_listResilience3 <- map(Resilience3, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR3 <- codebook %>% filter(rowname %in% Resilience3) %>% select(V1) %>% pull()
nomR3 <- paste("RESILIENCE  : La majorité de vos fournisseurs se trouvent-ils dans la même zone géographique pour",nomR3,sep = "/")
names(df_listResilience3) <- nomR3
# Base Résilience4
Resilience4 <- Resilience4[-12]
df_listResilience4 <- map(Resilience4, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR4 <- codebook %>% filter(rowname %in% Resilience4) %>% select(V1) %>% pull()
nomR4 <- paste("RESILIENCE  : Plus précisement, pour quels produits Alimentaires de céréales la plupart de vos fournisseurs sont-ils
 géographiquement situés au même endroit?",nomR4,sep = "/")
names(df_listResilience4) <- nomR4
# Base Résilience5
Resilience5 <- Resilience5[-9]
df_listResilience5 <- map(Resilience5, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR5 <- codebook %>% filter(rowname %in% Resilience5) %>% select(V1) %>% pull()
nomR5 <- paste("RESILIENCE  : Plus précisement, pour quels autres produits Alimentaires la plupart de vos fournisseurs sont-ils
 géographiquement situés au même endroit?",nomR5,sep = "/")
names(df_listResilience5) <- nomR5
# Base Résilience6
Resilience6 <- Resilience6[-14]
df_listResilience6 <- map(Resilience6, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR6 <- codebook %>% filter(rowname %in% Resilience6) %>% select(V1) %>% pull()
nomR6 <- paste("RESILIENCE  : Plus précisement, pour quels produits non Alimentaires la plupart de vos fournisseurs sont-ils
 géographiquement situés au même endroit?",nomR6,sep = "/")
names(df_listResilience6) <- nomR6
# Base Résilience7
df_listResilience7 <- map(Resilience7, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR7 <- codebook %>% filter(rowname %in% Resilience7) %>% select(V1) %>% pull()
nomR7 <- paste("RESILIENCE  : Avez-vous actuellement plus d'un fournisseur pour:",nomR7,sep = "/")
names(df_listResilience7) <- nomR7
# Base Résilience8
Resilience8 <- Resilience8[-12]
df_listResilience8 <- map(Resilience8, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR8 <- codebook %>% filter(rowname %in% Resilience8) %>% select(V1) %>% pull()
nomR8 <- paste("RESILIENCE  : Plus précisement, pour quels produits alimentaires de céréals avez-vous actuellement UN SEUL (1)
 fournisseur?:",nomR8,sep = "/")
names(df_listResilience8) <- nomR8
# Base Résilience9
Resilience9 <- Resilience9[-9]
df_listResilience9 <- map(Resilience9, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR9 <- codebook %>% filter(rowname %in% Resilience9) %>% select(V1) %>% pull()
nomR9 <- paste("RESILIENCE  : Plus précisement, pour quels autres produits produits avez-vous actuellement UN SEUL (1)
 fournisseur?:",nomR9,sep = "/")
names(df_listResilience9) <- nomR9
# Base Résilience10
Resilience10 <- Resilience10[-14]
df_listResilience10 <- map(Resilience10, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR10 <- codebook %>% filter(rowname %in% Resilience10) %>% select(V1) %>% pull()
nomR10 <- paste("RESILIENCE  : Plus précisement, pour quels produits non alimentaires avez-vous actuellement UN SEUL (1)
 fournisseur?",nomR10,sep = "/")
names(df_listResilience10) <- nomR10
# Base Résilience11
df_listResilience11 <- map(Resilience11, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR11 <- codebook %>% filter(rowname %in% Resilience11) %>% select(V1) %>% pull()
nomR11 <- paste("RESILIENCE  : Votre entreprise compte-t-elle principalement sur un seul fournisseur pour:",nomR11,sep = "/")
names(df_listResilience11) <- nomR11
# Base Résilience12
Resilience12 <- Resilience12[-12]
df_listResilience12 <- map(Resilience12, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR12 <- codebook %>% filter(rowname %in% Resilience12) %>% select(V1) %>% pull()
nomR12 <- paste("RESILIENCE  : Plus précisément, pour quels produits alimentaires de céréales existe-t-il un seul fournisseur de qui dépend la
 plupart de votre entreprise?:",nomR12,sep = "/")
names(df_listResilience12) <- nomR12
# Base Résilience13
Resilience13 <- Resilience13[-9]
df_listResilience13 <- map(Resilience13, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR13 <- codebook %>% filter(rowname %in% Resilience13) %>% select(V1) %>% pull()
nomR13 <- paste("RESILIENCE  : Plus précisément, pour quels autres produits alimentaires existe-t-il un seul fournisseur de qui dépend la
 plupart de votre entreprise?:",nomR13,sep = "/")
names(df_listResilience13) <- nomR13
# Base Résilience14
Resilience14 <- Resilience14[-14]
df_listResilience14 <- map(Resilience14, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomR14 <- codebook %>% filter(rowname %in% Resilience14) %>% select(V1) %>% pull()
nomR14 <- paste("RESILIENCE  : Plus précisément, pour quels produits non alimentaires existe-t-il un seul fournisseur de qui dépend la
 plupart de votre entreprise?:",nomR14,sep = "/")
names(df_listResilience14) <- nomR14

ResilienceAdmin1 <- c(df_listResilience1,df_listResilience2,df_listResilience3,
                      df_listResilience4,df_listResilience5,df_listResilience6,
                      df_listResilience7,df_listResilience8,df_listResilience9,
                      df_listResilience10,df_listResilience11,df_listResilience12,
                      df_listResilience13,df_listResilience14)

# Résilience niveau Admin2 ------------------------------------------------
# Base Résilience1
df_listResilience1 <- map(Resilience1, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience1) <- nomR1
# Base Résilience2
df_listResilience2 <- map(Resilience2, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience2) <- nomR2
# Base Résilience3
df_listResilience3 <- map(Resilience3, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience3) <- nomR3
# Base Résilience4
df_listResilience4 <- map(Resilience4, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience4) <- nomR4
# Base Résilience5
df_listResilience5 <- map(Resilience5, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience5) <- nomR5
# Base Résilience6
df_listResilience6 <- map(Resilience6, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience6) <- nomR6
# Base Résilience7
df_listResilience7 <- map(Resilience7, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience7) <- nomR7
# Base Résilience8
df_listResilience8 <- map(Resilience8, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience8) <- nomR8
# Base Résilience9
df_listResilience9 <- map(Resilience9, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience9) <- nomR9
# Base Résilience10
df_listResilience10 <- map(Resilience10, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience10) <- nomR10
# Base Résilience11
df_listResilience11 <- map(Resilience11, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience11) <- nomR11
# Base Résilience12
df_listResilience12 <- map(Resilience12, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience12) <- nomR12
# Base Résilience13
df_listResilience13 <- map(Resilience13, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience13) <- nomR13
# Base Résilience14
df_listResilience14 <- map(Resilience14, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listResilience14) <- nomR14

ResilienceAdmin2 <- c(df_listResilience1,df_listResilience2,df_listResilience3,
                      df_listResilience4,df_listResilience5,df_listResilience6,
                      df_listResilience7,df_listResilience8,df_listResilience9,
                      df_listResilience10,df_listResilience11,df_listResilience12,
                      df_listResilience13,df_listResilience14)


# Resilience au niveau Marché ---------------------------------------------
# Base Résilience1
df_listResilience1 <- map(Resilience1, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience1) <- nomR1
# Base Résilience2
df_listResilience2 <- map(Resilience2, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience2) <- nomR2
# Base Résilience3
df_listResilience3 <- map(Resilience3, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience3) <- nomR3
# Base Résilience4
df_listResilience4 <- map(Resilience4, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience4) <- nomR4
# Base Résilience5
df_listResilience5 <- map(Resilience5, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience5) <- nomR5
# Base Résilience6
df_listResilience6 <- map(Resilience6, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience6) <- nomR6
# Base Résilience7
df_listResilience7 <- map(Resilience7, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience7) <- nomR7
# Base Résilience8
df_listResilience8 <- map(Resilience8, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience8) <- nomR8
# Base Résilience9
df_listResilience9 <- map(Resilience9, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience9) <- nomR9
# Base Résilience10
df_listResilience10 <- map(Resilience10, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience10) <- nomR10
# Base Résilience11
df_listResilience11 <- map(Resilience11, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience11) <- nomR11
# Base Résilience12
df_listResilience12 <- map(Resilience12, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience12) <- nomR12
# Base Résilience13
df_listResilience13 <- map(Resilience13, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience13) <- nomR13
# Base Résilience14
df_listResilience14 <- map(Resilience14, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listResilience14) <- nomR14

ResilienceMarche <- c(df_listResilience1,df_listResilience2,df_listResilience3,
                      df_listResilience4,df_listResilience5,df_listResilience6,
                      df_listResilience7,df_listResilience8,df_listResilience9,
                      df_listResilience10,df_listResilience11,df_listResilience12,
                      df_listResilience13,df_listResilience14)


# Service -----------------------------------------------------------------
#1 	TrdServiceLoyalty Offrez-vous un programme de fidélité à vos clients réguliers? Réductions ou système de
#        points à utiliser plus tard dans votre boutique?
#2 TrdServicePos Cette boutique possède-t-elle un terminal de vente (POS)?
#3 TrdServicePosAnalysis Analysez-vous les informations collectées par votre terminal de vente. Informations du
#                  type, inventaire, catalogue de données? 

mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("TrdServiceLoyalty","TrdServicePos",
                                        "TrdServicePosAnalysis"))), 
                        funs(factor))

mfi_bissau <- mutate_at(mfi_bissau, 
                        vars(contains(c("TrdServiceLoyalty","TrdServicePos",
                                        "TrdServicePosAnalysis"))),
                        ~recode_factor(.,"0"="Non", "1"="Oui"))

Service1 <- list("TrdServiceLoyalty")
Service2 <- list("TrdServicePos")
Service3 <- list("TrdServicePosAnalysis")


# Service au Niveau Admin1 ------------------------------------------------

df_listService1 <- map(Service1, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomS1 <- "SERVICE : TrdServiceLoyalty Offrez-vous un programme de fidélité à vos clients réguliers? Réductions ou système de
 points à utiliser plus tard dans votre boutique?"
names(df_listService1) <- nomS1

df_listService2 <- map(Service2, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomS2 <- "SERVICE : Cette boutique possède-t-elle un terminal de vente (POS)?"
names(df_listService2) <- nomS2

df_listService3 <- map(Service3, ~ tableauAdmin1(mfi_bissau, !!sym(.x)))
nomS3 <- "SERVICE : Analysez-vous les informations collectées par votre terminal de vente. Informations du
  type, inventaire, catalogue de données?"
names(df_listService3) <- nomS3

ServiceAdmin1 <- c(df_listService1,df_listService2,df_listService3)

# Service au niveau Admin2 ------------------------------------------------
df_listService1 <- map(Service1, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listService1) <- nomS1

df_listService2 <- map(Service2, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listService2) <- nomS2

df_listService3 <- map(Service3, ~ tableauAdmin2(mfi_bissau, !!sym(.x)))
names(df_listService3) <- nomS3

ServiceAdmin2 <- c(df_listService1,df_listService2,df_listService3)

# Service au niveau maché -------------------------------------------------
df_listService1 <- map(Service1, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listService1) <- nomS1

df_listService2 <- map(Service2, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listService2) <- nomS2

df_listService3 <- map(Service3, ~ tableauMarche(mfi_bissau, !!sym(.x)))
names(df_listService3) <- nomS3

ServiceMarche <- c(df_listService1,df_listService2,df_listService3)




# Infrastructure ----------------------------------------------------------


# Competition -------------------------------------------------------------


# Qualité -----------------------------------------------------------------


# Acces et Protection -----------------------------------------------------



# regroupement des Base ----------------------------------------------------

BaseAdmin1 <- c(AssortimentAdmin1,DisponibiliteAdmin1,PrixAdmin1, ResilienceAdmin1,ServiceAdmin1)

BaseAdmin2 <- c(AssortimentAdmin2, DisponibiliteAdmin2,PrixAdmin2,ResilienceAdmin2,ServiceAdmin2)

BaseMarche <- c(AssortimentMarche, DisponibiliteMarche, PrixMarche,ResilienceMarche,ServiceMarche)


# Admin1
wb <- createWorkbook()
addWorksheet(wb, "Foo")

curr_row <- 1
for(i in seq_along(BaseAdmin1)) {
  writeData(wb, "Foo", names(BaseAdmin1)[i], startCol = 1, startRow = curr_row)
  writeData(wb, "Foo", BaseAdmin1[[i]], startCol = 1, startRow = curr_row+1)
  curr_row <- curr_row + 1 + nrow(BaseAdmin1[[i]]) + 2
}

saveWorkbook(wb, "Admin1.xlsx")

# ADmin2
wb <- createWorkbook()
addWorksheet(wb, "Foo")

curr_row <- 1
for(i in seq_along(BaseAdmin2)) {
  writeData(wb, "Foo", names(BaseAdmin2)[i], startCol = 1, startRow = curr_row)
  writeData(wb, "Foo", BaseAdmin2[[i]], startCol = 1, startRow = curr_row+1)
  curr_row <- curr_row + 1 + nrow(BaseAdmin2[[i]]) + 2
}

saveWorkbook(wb, "Admin2.xlsx")

# Marché
wb <- createWorkbook()
addWorksheet(wb, "Foo")

curr_row <- 1
for(i in seq_along(BaseMarche)) {
  writeData(wb, "Foo", names(BaseMarche)[i], startCol = 1, startRow = curr_row)
  writeData(wb, "Foo", BaseMarche[[i]], startCol = 1, startRow = curr_row+1)
  curr_row <- curr_row + 1 + nrow(BaseMarche[[i]]) + 2
}

saveWorkbook(wb, "Marche.xlsx")


