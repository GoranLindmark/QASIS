library(tidyverse)
library(lubridate)


data <- qasis


  data %>%
  mutate(year = year(`Audit Date`)) %>%
  mutate(ASP = str_sub(ASP, start = 5, end = length(ASP))) %>%
  # select('Numeric Rating', ASP, Equipment) %>%
  group_by(ASP, Equipment, Country, Audit) %>%
  summarize( NumRate = sum(`Numeric Rating`)) %>%
  view()

data$`Numeric Rating`
summary(data)

ClassDistribution(data$`Numeric Rating`)

# aggregate per site, count number of errors per category


counts <- table(data[,17])
sites <-
    data %>%
        mutate(flag = 1) %>%
        pivot_wider(names_from = Rating, values_from = flag, values_fn = sum, values_fill = 0) %>%
        group_by(`Site Name`, year, ASP, Country) %>%
        summarize(observations = sum(Observation),
                  minor = sum(Minor),
                  major = sum(Major),
                  critical = sum(Critical),
                  .groups = "drop")
  countries <-
    sites %>%
        group_by(Country,observations, ASP, critical) %>%
        summarize(siter = n(),
                  .groups = "drop") %>%
        group_by(Country) %>%
        summarize(antal = n(),
                  critical = sum(critical),
                  criticalPsite = critical / antal,
                  .groups = "drop") %>% View
  ASP <-
    sites %>%
    group_by(ASP) %>%
    summarize(antal = n(),
              critical = sum(critical),
              chriticalPct = critical / antal *100,
              .groups = "drop")

  # ASP <-
      sites %>%
      group_by(ASP,observations, critical) %>%
      summarize(siter = n(),
                .groups = "drop") %>%
      group_by(ASP) %>%
      summarize(antal = n(),
                critical = sum(critical),
                criticalPsite = critical / antal,
                .groups = "drop") %>% View


      qasis %>%
          select(Country,  Rating) %>%
          filter(Country == "Tunisia") %>%
          mutate(major = ifelse(Rating == "Major", 1, 0)) %>%
          mutate(minor = ifelse(Rating == "Minor", 1, 0)) %>%
          mutate(critical = ifelse(Rating == "Critical", 1, 0)) %>%
          mutate(observation = ifelse(Rating == "Observation", 1, 0)) %>%
          select(-Rating) %>%
          pivot_longer(cols = c(minor, major, critical, observation), names_to = "faultreport", values_to = "number") %>% view
          group_by( Country ) %>%
          summarize( major = sum(major),
                     minor = sum(minor),
                     critical = sum(critical),
                     observation = sum(observation),
                     .groups = "drop") %>%
              ggplot( aes(x = `Project Name`, y = antal, groups = 1) ) +
              geom_col() +
          facet_wrap(~Rating)

      qasis %>%
          group_by(Rating) %>%
          summarize(antal = n())
