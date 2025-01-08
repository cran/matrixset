## ----include = FALSE, echo = FALSE, message = FALSE---------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(matrixset)

## -----------------------------------------------------------------------------
mrm_plus2015

## ----message=FALSE------------------------------------------------------------
library(tidyverse)
mrm_plus2015_ratio <- mrm_plus2015 %>% 
  mutate_matrix(area_log_ratio = log(light_area+1) - log(heavy_area+1)) %>% 
  annotate_row(dilution = case_when(CalibrationPoint == "Point_1" ~ 57.6,
                                    CalibrationPoint == "Point_2" ~ 288,
                                    CalibrationPoint == "Point_3" ~ 1440,
                                    CalibrationPoint == "Point_4" ~ 7200,
                                    CalibrationPoint == "Point_5" ~ 36000,
                                    CalibrationPoint == "Point_6" ~ 180000,
                                    CalibrationPoint == "Point_7" ~ 900000,
                                    TRUE ~ NA_real_)) %>% 
  annotate_row(point = case_when(is.na(CalibrationPoint) ~ str_match(.rowname, "(.*)_\\d$")[,2],
                                 TRUE ~ CalibrationPoint),
               replicate = case_when(is.na(CalibrationPoint) ~ paste("Replicate", str_match(.rowname, ".*_(\\d)$")[,2], sep = "_"),
                                     TRUE ~ Replicate))

## ----fig.width=7, fig.height=6------------------------------------------------
mrm_plus2015_ratio[,1,] %>%
    filter_row(!is.na(CalibrationPoint)) %>% # discards the blanks
    apply_matrix(~ {
        d <- current_row_info()
        d$`AGPNGTLFVADAYK|y10` <- .m[,1]
        ggplot(d, aes(log(dilution), `AGPNGTLFVADAYK|y10`)) +
            geom_point()
        },
        .matrix = "area_log_ratio")

## ----fig.width=7, fig.height=6------------------------------------------------
mrm_plus2015_ratio %>%
    # to average the triplicates
    row_group_by(CalibrationPoint) %>%
    # send the analyte averaged values in the annotation frame
    annotate_column_from_apply(foo = ~ mean(.j), .matrix = "area_log_ratio") %>% 
    column_info() %>%
    select(-`NA`) %>% # discards the blanks
    pivot_longer(Point_1:Point_7, names_to = "CalibrationPoint", values_to = "Rep_avr") %>%
    left_join(mrm_plus2015_ratio %>%
                  row_info() %>%
                  select(-.rowname, -Replicate, -RunOrder) %>%
                  distinct()) %>%
    ggplot(aes(log(dilution), Rep_avr)) +
    geom_point() +
    geom_smooth()

## ----fig.width=7.2, fig.height=14, message = FALSE----------------------------
library(patchwork)
library(magrittr)
library(ggfortify)

pcas <- mrm_plus2015_ratio %>% 
    mutate_matrix(light_area = log(light_area+1),
                  heavy_area = log(heavy_area+1)) %>% 
    apply_matrix(pca = ~ {
        list(meta=current_row_info(), pca=prcomp(.m, scale. = TRUE))
    }, .matrix = c("light_area", "heavy_area", "area_log_ratio")) 


bps <- imap(pcas, ~ autoplot(.x$pca$pca, data = .x$pca$meta, 
                             colour = "point") +
                ggtitle(.y))

(bps[[1]] / bps[[2]] / bps[[3]]) + plot_layout(guides = "collect")

## ----fig.width=7, fig.height=5, message = FALSE-------------------------------
mrm_plus2015 %>% 
    apply_row_dfl(poor = ~ sum(.i < 50000),
                  .matrix = c("light_area", "heavy_area")) %>% 
    bind_rows(.id = "area") %>% 
    ggplot(aes(.rowname, poor, fill = area)) + 
    geom_col(position = "dodge") +
    labs(y = "% < 50000") +
    theme(axis.text.x = element_text(angle = 90))

## ----fig.width=7, fig.height=7, message = FALSE-------------------------------
box_expr <- list(ymin = expr(~ min(.i)), 
                 lower = expr(~ quantile(.i, probs = .25, names = FALSE)),
                 middle = expr(~ median(.i)),
                 upper = expr(~ quantile(.i, probs = .75, names = FALSE)),
                 ymax = expr(~ max(.i)))


mrm_plus2015_ratio %>% 
    annotate_row_from_apply(.matrix = area_log_ratio,
                            !!!box_expr) %>% 
    row_info() %>% 
    ggplot(aes(.rowname, 
               ymin = ymin, 
               lower = lower, 
               middle = middle, 
               upper = upper, 
               ymax = ymax)) + 
    geom_boxplot(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90))

## ----message=FALSE------------------------------------------------------------
# library(lme4)
diff <- mrm_plus2015_ratio %>% 
    apply_column_dfl(p_cov = ~ predict(lm(.j ~ 1 + SampleType + point),
                                     type = "response"),
                     #p_cov = predict(lmer(.j ~ 1 + SampleType + (1|point)),
                     #                type = "response"),
                     p_null = ~ predict(lm(.j ~ 1)),
                     .matrix = "area_log_ratio") %>% 
    .[[1]] %>% 
    mutate(d = p_cov - p_null) %>% 
    select(.colname, .rowname = p_cov.name, d) %>% 
    pivot_wider(names_from = ".colname", values_from = "d") %>% 
    column_to_rownames(".rowname") %>% 
    data.matrix()


mrm_plus2015_ratio %<>% 
    add_matrix(diff = diff) %>% 
    mutate_matrix(norm_log_ratio = area_log_ratio - diff) 

## ----fig.width=7, fig.height=6, message = FALSE-------------------------------
mrm_plus2015_ratio %>% 
    annotate_row_from_apply(.matrix = norm_log_ratio,
                            !!!box_expr) %>% 
    row_info() %>% 
    ggplot(aes(.rowname, 
               ymin = ymin, 
               lower = lower, 
               middle = middle, 
               upper = upper, 
               ymax = ymax)) + 
    geom_boxplot(stat = "identity")

## ----fig.width=7, fig.height=6, message = FALSE-------------------------------
pca <- mrm_plus2015_ratio %>% 
    apply_matrix(pca = ~ {
        list(meta=current_row_info(), pca=prcomp(.m, scale. = TRUE))}, 
        .matrix = "norm_log_ratio") %>% 
    .[[1]] %>% 
    .$pca
autoplot(pca$pca, data = pca$meta, colour = "point")

