library(nycflights13)
library(tidyverse)
library(psych)
data("flights")

glimpse(flights)

flights |> 
  filter(dest == "ATL") |> 
  group_by(year, month, day) |> 
  summarize(arr_delay = mean(arr_delay, na.rm = TRUE))

flights |> 
  filter(month  %in% c(1,2)) |> 
  group()

flights |> 
  select(tailnum) |> 
  arrange(arr_delay)
#> Error in `arrange()`:
#> ℹ In argument: `..1 = arr_delay`.
#> Caused by error:
#> ! object 'arr_delay' not found
#> 
flights |> select(contains("TIME"))

flights |> 
  plot(dep_time ~ arr_time, data = _)


flights |> 
  describe()


flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1) |>
  relocate(arr_delay) |> 
  relocate(dest) |> 
  arrange(desc(arr_delay))


# Styling -----------------------------------------------------------------

short_flights <- flights |> filter(air_time < 60)

flights |> 
  group_by(dest) |> 
  summarize(
    distance = mean(distance),
    speed = mean(distance / air_time, na.rm = TRUE)
  ) |> 
  ggplot(aes(x = distance, y = speed)) +
  geom_smooth(
    method = "loess",
    span = 0.5,
    se = FALSE, 
    color = "white", 
    linewidth = 4
  ) +
  geom_point()

flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    n = n(),
    delay = mean(arr_delay, na.rm = TRUE)
  ) |>
  filter(n > 10)

flights |>
  filter(dest =="IAH") |>
  group_by(year, month, day) |> 
  summarize(n = n(),
            delay = mean(arr_delay, 
                         na.rm=TRUE)) |> 
  filter(n>10)

flights |> filter(carrier=="UA", 
                  dest %in% c("IAH","HOU"), 
                  sched_dep_time > 0900, 
                  sched_arr_time < 2000) |> 
  group_by(flight) |>
  summarize(
    delay=mean(
    arr_delay, na.rm = TRUE),
    cancelled = sum(is.na(arr_delay)),
    n=n()
  ) |> 
  filter(n > 10)


# Tidy data ---------------------------------------------------------------

library(tidyverse)

table1

table2

table3

table1 |> 
  summarize(
    total_cases = sum(cases), 
    .by = year)

data(billboard)

billboard_longer <- billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week)
  )

billboard_longer |> 
  ggplot(aes(x = week, y = rank, group = track)) + 
  geom_line(alpha = 0.25) + 
  scale_y_reverse()

df <- tribble(
  ~id,  ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  120,  125
)

df_longer <- df |> 
  pivot_longer(
    cols = starts_with("bp"),
    names_to = "measurement",
    values_to = "value"
  )



# Data import -------------------------------------------------------------

students <- read_csv("https://pos.it/r4ds-students-csv")
spec(students)
