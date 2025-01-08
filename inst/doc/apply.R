## ----include = FALSE, echo = FALSE, message = FALSE---------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(matrixset)

## ----echo=FALSE, message=FALSE------------------------------------------------
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
animals_ms <- matrixset(msr = animals, log_msr = log_animals, row_info = animal_info,
                row_key = "Animal")
animals_ms <- animals_ms %>% 
  annotate_column(unit = case_when(.colname == "body" ~ "kg",
                                   TRUE ~ "g")) 

## -----------------------------------------------------------------------------
animals_ms

## -----------------------------------------------------------------------------
show_matrix <- function(x) {
  if (nrow(x) > 4) {
    newx <- head(x, 4)
    storage.mode(newx) <- "character"
    newx <- rbind(newx, rep("...", ncol(x)))
  }  else newx <- x
  newx
}
show_vector <- function(x) {
  newx <- if (length(x) > 4) {
    c(as.character(x[1:4]), "...")
  } else x
  newx
}
show_lst <- function(x) {
  lapply(x, function(u) {
    if (is.matrix(u)) show_matrix(u) else if (is.vector(u)) show_vector(u) else u
  })
}

## ----message=FALSE------------------------------------------------------------
library(magrittr)
library(purrr)
out <- animals_ms %>% 
   apply_matrix(exp,
                ~ mean(.m, trim=.1),
                foo=asinh,
                pow = ~ 2^.m,
                reg = ~ {
                  is_alive <- !is_extinct
                  lm(.m ~ is_alive + class)
                  })
# out[[1]] %>% map(~ if (is.matrix(.x)) {head(.x, 5)} else .x)
show_lst(out[[1]])

## -----------------------------------------------------------------------------
out <- animals_ms %>% 
   apply_column(exp,
                ~ mean(.j, trim=.1),
                foo=asinh,
                pow = ~ 2^.j,
                reg = ~ {
                  is_alive <- !is_extinct
                  lm(.j ~ is_alive + class)
                  })
out[[1]] %>% map(show_lst)

## -----------------------------------------------------------------------------
animals_ms %>% 
  row_group_by(class) %>% 
  apply_matrix(exp,
               ~ mean(.m, trim=.1),
               foo=asinh,
               pow = ~ 2^.m,
               reg = ~ {
                 is_alive <- !is_extinct
                 lm(.m ~ is_alive)
                 })

## -----------------------------------------------------------------------------
animals_ms %>% 
    apply_matrix_dfl(~ mean(.m, trim=.1),
                     MAD=mad,
                     reg = ~ {
                         is_alive <- !is_extinct
                         list(lm(.m ~ is_alive + class))
                     })

## -----------------------------------------------------------------------------
animals_ms %>% 
    apply_column_dfl(~ mean(.j, trim=.1),
                     MAD=mad,
                     reg = ~ {
                         is_alive <- !is_extinct
                         list(lm(.j ~ is_alive + class))
                     })

## -----------------------------------------------------------------------------
animals_ms %>% 
    apply_row_dfl(rg = ~ range(.i),
                  qt = ~ quantile(.i, probs = c(.25, .75)))   

## -----------------------------------------------------------------------------
animals_ms %>% 
    apply_row_dfw(rg = ~ range(.i),
                  qt = ~ quantile(.i, probs = c(.25, .75)))   

## -----------------------------------------------------------------------------
animals_ms %>% 
    row_group_by(class) %>% 
    apply_matrix_dfl(n = ~ current_n_row()) %>% 
    .$msr

## -----------------------------------------------------------------------------
# ms_object %>% 
#     apply_matrix( ~ {
#       ctrt <- current_column_info()$common_trait
#       rtrt <- current_row_info()$common_trait
#       
#       do something with ctrt and rtrt
#     })

## -----------------------------------------------------------------------------
reg_expr <- expr({
    is_alive <- !is_extinct
    list(lm(.j ~ is_alive + class))
})

animals_ms %>% 
    apply_column_dfl(~ mean(.j, trim=.1),
                     MAD=mad,
                     reg = ~ !!reg_expr)

