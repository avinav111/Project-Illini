library(tidyverse)

url = "https://raw.githubusercontent.com/wadefagen/datasets/master/illini-football/illini-football-scores.csv"

football = read_csv(url)

football = football |>
  mutate(PointDiff = IlliniScore - OpponentScore)
#  group_by(Opponent) |>
#  summarize(n = n(), .groups = "drop") |>
#  arrange(desc(n))

write_csv(x = football, file = "data/football.csv")

  football |>
  filter(Opponent == "Northwestern") |>
  filter(Season == 2010)|>
  group_by(Opponent) |>
  select(PointDiff, Season, Result) |>
  ggplot() + aes(x = Season, y = PointDiff, color = Result) |>
  geom_point(size = 3) 
  
  
  
#view(football)
