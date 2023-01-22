library(fpp3)
d <-
  qasis %>%
  select(date, num_rating) %>%
  group_by(date) %>%
  summarize(num_rating = sum(num_rating),
            antal = n(),
            rating = num_rating/antal)

series <- tsibble(d, index = date)

autoplot(series, antal)
autoplot(series, num_rating)
autoplot(series, rating)

dcmp <-
  series %>%
  model(stl = STL(rating))
components(dcmp) %>% autoplot()


series %>%
  gg_season(rating, period = "year")
