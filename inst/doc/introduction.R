## ----include = FALSE, echo = FALSE, message = FALSE---------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(matrixset)

## -----------------------------------------------------------------------------
animals <- as.matrix(MASS::Animals)
head(animals)

## ----error=TRUE---------------------------------------------------------------
try({
animals_ms <- matrixset(animals)
})

## -----------------------------------------------------------------------------
animals_ms <- matrixset(msr = animals)
animals_ms

## -----------------------------------------------------------------------------
as_matrixset(animals)

## -----------------------------------------------------------------------------
log_animals <- log(animals)
ms <- matrixset(msr = animals, log_msr = log_animals)
ms

## -----------------------------------------------------------------------------
ms2 <- matrixset(list(msr = animals, log_msr = log_animals))
identical(ms, ms2)

## -----------------------------------------------------------------------------
animals_ms <- add_matrix(animals_ms, log_msr = log_animals)
identical(ms, animals_ms)

## ----message = FALSE----------------------------------------------------------
library(tidyverse)
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
animal_info

## -----------------------------------------------------------------------------
ms <- matrixset(msr = animals, log_msr = log_animals, row_info = animal_info,
                row_key = "Animal")
ms

## -----------------------------------------------------------------------------
row_info(ms2) <- animal_info %>% rename(.rowname = Animal)
identical(ms, ms2)

## -----------------------------------------------------------------------------
animals_ms <- animals_ms %>% 
  join_row_info(animal_info, by = c(".rowname" = "Animal"))
identical(ms, animals_ms)
animals_ms <- animals_ms %>% 
  annotate_column(unit = case_when(.colname == "body" ~ "kg",
                                   TRUE ~ "g")) 
animals_ms

## -----------------------------------------------------------------------------
animals_ms %>% 
  annotate_column(us_unit = case_when(unit == "kg" ~ "lb",
                                   TRUE ~ "oz")) %>% 
  column_info()

## ----error = TRUE-------------------------------------------------------------
try({
animals_ms %>% annotate_column(.colname = NULL)
})

