
# import packages ---------------------------------------------------------
library(readxl)
library(stringi)
library(tidyverse)
library(tidyquant)
library(writexl)
# import dataset ----------------------------------------------------------

GNB1 <-  read.csv("GNB_MFI_Dataset_September_2020.csv")
GNB2 <- read_xlsx("GNB_MFI_Dataset_September_2020_v2.xlsx")
marche <- read_xlsx("Marche_Bissau.xlsx")
nomfichier2 <- read_xlsx("nomfichier2.xlsx")
mfi_bissau2 <- read_xlsx("mfi_bissau.xlsx")

# comparaison -------------------------------------------------------------
stri_enc_mark(GNB1$MktNametext)
iconv(names(GNB1), from = "ASCII", to = "UTF-8")
iconv(GNB1$MktNametext, from = "ASCII", to = "UTF-8")

setdiff(names(GNB1),names(GNB2))
# base 1
setdiff(GNB1$MktNametext, marche$MarketName)
setdiff(marche$MarketName, GNB1$MktNametext)
# base2
setdiff(GNB2$MktNametext, marche$MarketName)
setdiff(marche$MarketName, GNB2$MktNametext)

# nom fichier 2
setdiff(nomfichier2$MktNametext, marche$MarketName)
setdiff(marche$MarketName, nomfichier2$MktNametext)

# différence base1 et base2
setdiff(GNB1$MktNametext, GNB2$MktNametext)
setdiff(GNB2$MktNametext, GNB1$MktNametext)

# différence base1 et mfi_bissau
setdiff(mfi_bissau$MktNametext, GNB1$MktNametext)
setdiff(GNB1$MktNametext, mfi_bissau$MktNametext)

# différence admin2code entre mfi_bissau et 

# quelques ajustement supplémentaire --------------------------------------

which(is.na(mfi_bissau2$MktNametext))

vide <- mfi_bissau[which(is.na(mfi_bissau$MktNametext)),]
# Partie Recherche V ------------------------------------------------------

GNB2 <- GNB2 %>% mutate(
  idmarche = VLOOKUP(.lookup_values = MktNametext,.data = marche,.lookup_column = MarketName,.return_column = MarketID)
)
GNB2 <- GNB2 %>% relocate(idmarche,.after= MktName)

setdiff(GNB2$idmarche, marche$MarketID)

mfi_bissau <- GNB2 %>% select(-MktName)
mfi_bissau <- mfi_bissau %>% rename(MktName = idmarche)


# enregistrement des fichiers ---------------------------------------------
write_xlsx(mfi_bissau, "mfi_bissau.xlsx")
write_xlsx(vide, "vide.xlsx")
