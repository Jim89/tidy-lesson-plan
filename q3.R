library(tidyverse)


q3 <- read_csv(here::here("data", "infant_hiv.csv"))

# Describe the columns:
* iso3
* year
* estimate
* hi
* lo


make_tidy <- function(.path) {
    .path %>%
        read_csv() %>%
        pivot_longer(-ISO3, names_to = c("year", "metric"), names_sep = " ") %>%
        mutate(
            value = str_remove_all(value, "-|( -)|(>95%)|%"),
            value = na_if(value, "")
        ) %>%
        janitor::clean_names() %>%
        arrange(iso3, year, metric)
}

make_tidy(here::here("data", "infant_hiv.csv"))


