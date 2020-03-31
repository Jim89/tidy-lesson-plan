q4 <- read_csv(here::here("data", "ranking.csv"))


q4 %>%
    count(item, rank) %>%
    complete(crossing(item, rank), fill = list(n = 0)) %>%
    group_by(item) %>%
    mutate(total = sum(n), per = n / total) %>%
    select(-n) %>%
    pivot_wider(names_from = rank, values_from = per) %>%
    ggplot(aes(negative, positive)) +
    geom_point(aes(size = total), alpha = .25) +
    geom_smooth(method = "lm") +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    theme_grey()



