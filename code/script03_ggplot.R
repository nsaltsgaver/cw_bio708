## call every single time

library(tidyverse)

# point figure ------------------------------------------------------------

iris %>%
  ggplot(
    mapping = aes(x = Sepal.Length,
        y = Sepal.Width,
        color = Species)
  ) +
  geom_point()

## error, no aes()
# iris %>%
#   ggplot(
#     x = Sepal.Length,
#     y = Sepal.Width
#   ) +
#   geom_point()

## color by species

ggplot(data = iris,
       mapping = aes(x=Sepal.Length,
                     y=Sepal.Width,
                     color = Species)
       ) +
  geom_point()

## color all points the same color
ggplot(data = iris,
       mapping = aes(x=Sepal.Length,
                     y=Sepal.Width)
) +
  geom_point(color = 'steelblue')

## error color argument not in aes()
# ggplot(data = iris,
#        mapping = aes(x=Sepal.Length,
#                      y=Sepal.Width),
#        color = Species
# ) +
#   geom_point()


# line figure -------------------------------------------------------------

df0 <- tibble(
  x=rep(1:50,3),
  y=x*2
)

df0 %>%
  ggplot(
    mapping = aes(x=x,
        y=y)
  ) +
  geom_line()


# histogram ---------------------------------------------------------------

iris %>%
  ggplot(
    mapping = aes(x=Sepal.Length)
  ) +
  geom_histogram()

## change bin width
iris %>%
  ggplot(mapping = aes(x=Sepal.Length)
         ) +
  geom_histogram(binwidth=0.5)

## change bin number
iris %>%
  ggplot(mapping = aes(x=Sepal.Length)
         ) +
  geom_histogram(bins=50)


# boxplot -----------------------------------------------------------------

## basic boxplot
iris %>%
  ggplot(mapping = aes(x=Species,
                       y=Sepal.Length)
         ) +
  geom_boxplot()

## fill by species
iris %>%
  ggplot(mapping = aes(x=Species,
                       y=Sepal.Length,
                       fill=Species)
  ) +
  geom_boxplot()

## consistent color
iris %>%
  ggplot(mapping = aes(x=Species,
                       y=Sepal.Length,
                       fill=Species)
  ) +
  geom_boxplot(color = '#A9A9A9')


# fun plot ----------------------------------------------------------------

iris %>%
  ggplot(mapping = aes(x=Sepal.Length,
                       fill=Species)
         ) + geom_dotplot()
