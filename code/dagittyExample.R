library(dagitty)
library(ggdag)

model <- dagitty("dag{x->y; z->x; z->y}")
coordinates(model) <- list(
  x = c(x=1, y=3, z=2),
  y = c(x=1, y=1, z=2)
)

ggdag(model) + theme_dag()



library(ggdag)
library(ggplot2)

#  example from the dagitty package
dag <- dagitty::dagitty("dag {
    y <- x <- z1 <- v -> z2 -> y
    z1 <- w1 <-> w2 -> z2
    x <- w1 -> y
    x <- w2 -> y
    x [exposure]
    y [outcome]
  }")

tidy_dag <- tidy_dagitty(dag)

tidy_dag

#  using more R-like syntax to create the same DAG
tidy_ggdag <- dagify(
  y ~ x + z2 + w2 + w1,
  x ~ z1 + w1 + w2,
  z1 ~ w1 + v,
  z2 ~ w2 + v,
  w1 ~ ~w2, # bidirected path
  exposure = "x",
  outcome = "y"
) %>%
  tidy_dagitty()

tidy_ggdag

ggdag(tidy_ggdag) +
  theme_dag()


table2dag1 <- dagify(
  Stroke ~ Smoke + HIV + Age,
  HIV ~ Smoke + Age,
  Smoke ~ Age,
  exposure = "HIV",
  outcome = "Smoke",
  coords = list(
    x = c(Stroke = 4, HIV = 2, Smoke = 1, Age = 1),
    y = c(Stroke = 2, HIV = 2, Smoke = 3, Age = 1)
))


table2dag1 <-   tidy_dagitty(table2dag1) 

table2dag1

ggdag(table2dag1) 




table2dag1 <- dagify(
  Stroke ~ Smoke + HIV + Age,
  HIV ~ Smoke + Age,
  Smoke ~ Age,
  exposure = "HIV",
  outcome = "Smoke"
) |> 
  coordinates() <- list(
  x = c(Stroke = 4, HIV = 2, Smoke = 1, Age = 1),
  y = c(Stroke = 2, HIV = 2, Smoke = 3, Age = 1)
)

table2dag1 <-   tidy_dagitty(table2dag1) 

table2dag1