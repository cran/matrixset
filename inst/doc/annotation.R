## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE, message=FALSE-----------------------------------------
library(matrixset)

## ----message=FALSE------------------------------------------------------------
library(tidyverse)
animals <- as.matrix(MASS::Animals)
log_animals <- log(animals)
animal_info <- MASS::Animals %>% 
  rownames_to_column("Animal") %>% 
  mutate(is_extinct = case_when(Animal %in% c("Dipliodocus", "Triceratops", "Brachiosaurus") ~ TRUE,
                                TRUE ~ FALSE),
         class = case_when(Animal %in% c("Mountain beaver", "Guinea pig", "Golden hamster", "Mouse", "Rabbit", "Rat") ~ "Rodent",
                           Animal %in% c("Potar monkey", "Gorilla", "Human", "Rhesus monkey", "Chimpanzee") ~ "Primate",
                           Animal %in% c("Cow", "Goat", "Giraffe", "Sheep") ~ "Ruminant",
                           Animal %in% c("Asian elephant", "African elephant") ~ "Elephantidae",
                           Animal %in% c("Grey wolf") ~ "Canine",
                           Animal %in% c("Cat", "Jaguar") ~ "Feline",
                           Animal %in% c("Donkey", "Horse") ~ "Equidae",
                           Animal == "Pig" ~ "Sus",
                           Animal == "Mole" ~ "Talpidae",
                           Animal == "Kangaroo" ~ "Macropodidae",
                           TRUE ~ "Dinosaurs")) %>% 
  select(-body, -brain)

## -----------------------------------------------------------------------------
ms <- matrixset(msr = animals, log_msr = log_animals, row_info = animal_info,
                row_key = "Animal")
ms

## -----------------------------------------------------------------------------
row_info(ms) <- animal_info %>% rename(.rowname = Animal)

## ----results='hide'-----------------------------------------------------------
matrixset(msr = animals, log_msr = log_animals) %>% 
    join_row_info(animal_info, by = c(".rowname" = "Animal"))

## -----------------------------------------------------------------------------
ms <- matrixset(msr = animals, log_msr = log_animals) %>% 
    join_row_info(animal_info, by = c(".rowname" = "Animal")) %>% 
    annotate_column(unit = case_when(.colname == "body" ~ "kg",
                                     TRUE ~ "g")) %>% 
    annotate_column(us_unit = case_when(unit == "kg" ~ "lb",
                                        TRUE ~ "oz"))
column_info(ms)

## -----------------------------------------------------------------------------
ms <- ms %>% annotate_column(us_unit = NULL)
column_info(ms)

## -----------------------------------------------------------------------------
ms %>% 
    annotate_row_from_apply(msr, ratio_brain_body = ~ .i[2]/(10*.i[1])) %>% 
    row_info()

## -----------------------------------------------------------------------------
ms %>% 
    row_group_by(class) %>% 
    annotate_column_from_apply(msr, mean) %>% 
    column_info()

