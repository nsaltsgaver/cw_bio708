library("tidyverse")

set.seed(123)

iris_sub <- as_tibble(iris) %>%
  group_by(Species) %>%
  sample_n(3) %>%
  ungroup

print(iris_sub)

filter(iris_sub, Species == "virginica")
filter(iris_sub, Species != "virginica")

filter(iris_sub, Species %in% c("virginica","versicolor"))
filter(iris_sub, Sepal.Width > 3, 
       Species == "versicolor")
arrange(iris_sub, desc(Sepal.Length))

select(iris_sub, Sepal.Length)
select(iris_sub, c(Sepal.Length, Sepal.Width))
select(iris_sub, -Sepal.Length)
select(iris_sub, -c(Sepal.Width,Sepal.Length))
select(iris_sub, starts_with("Sepal"))
select(iris_sub, -starts_with("Sepal"))
select(iris_sub, ends_with("Width"))
select(iris_sub, -ends_with("Length"))

(x_max <- nrow(iris_sub))
x <- 1:x_max
mutate(iris_sub, row_id = x)
mutate(iris_sub, sl_two_times = 2*Sepal.Length)


iris_pw <- select(iris_sub, c(Petal.Width, Species))
iris_petal <- select(iris_pw, starts_with("Petal"))
iris_pw_two <- mutate(iris_petal, sl_two_times = 2*Petal.Width)

df_vir <- filter(iris_sub, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)

df_vir_sl <- iris_sub %>%
  filter(Species == "virginica") %>%
  select(Sepal.Length)
