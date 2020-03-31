library(tidyverse)

q1 <- readr::read_csv("https://education.rstudio.com/blog/2020/02/instructor-certification-exams/at_health_facilities.csv")


# How many countries reported data?
q1 %>%
    count(iso3) %>%
    nrow()

n_distinct(q1$iso3) # 100 different countries (assuming not typos or duplication if iso3 codes have changed)



# What is the difference between the minimum and maximum year with valid data for each country?

mean(is.na(q1)) # 0 missing values to begin with, but I can see there are some potentially invalid records with -

# Assuming all data are valid (would need to check)
q1 %>%
    group_by(iso3) %>%
    summarise(diff = max(year) - min(year))

# Some countries have just one year, so maybe remove them first?
q1 %>%
    add_count(iso3) %>%
    filter(n > 1) %>%
    group_by(iso3) %>%
    summarise(diff = max(year) - min(year))

# Filter out -
q1 %>%
    janitor::clean_names() %>%
    filter(age_15_17 != '-', age_20_34 != '-') %>%
    add_count(iso3) %>%
    filter(n > 1) %>%
    group_by(iso3) %>%
    summarise(diff = max(year) - min(year))



# How many countries reported data in 3 or more years? -------------------------
q1 %>%
    count(iso3) %>%
    filter(n >= 3) %>%
    nrow() # 34 - assuming no filtering criteria before (e.g. removal of records)


# Which countries reported 100% incidence for at least one year in either age group? ------
q1 %>%
    janitor::clean_names() %>%
    filter(age_15_17 == 100 | age_20_34 == 100) %>%
    distinct(iso3)

