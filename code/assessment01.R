# To complete the following exercises, you must use either base R or tidyverse functions.  
# Manual searching or hard-coding answers will not be graded.

# base R ------------------------------------------------------------------

# 1: Create a vector with three elements (either numeric or character, up to you).
# Assign it to `v_three`.

v_three <- c(1,2,3)

# 2: Create a vector object containing 
# - 20 character elements of "a",
# - 50 character elements of "b",
# - 30 character elements of "c",
# (thus, the vector has a length of 100 elements)
# Assign it to `v_abc100`.

v_abc100 <- c(rep("a", times=20),rep("b",times=50),rep("c",times=30))

# 3: The script below creates a vector `v_x` with 100 random numbers from a normal distribution.  
# Select only the positive numbers (> 0) from `v_x`, calculate their mean, and assign it to `mu_x_plus`.
set.seed(100)
v_x <- rnorm(100)
mu_x_plus <- mean(v_x[v_x > 0])

# 4: Create a numeric matrix with the numbers 1 through 9 arranged in 3 rows × 3 columns.  
# Assign it to `m_num`.

m_num <- matrix(1:9, nrow=3, ncol=3)

# 5: Create a base R data frame (`data.frame()` function) using `v_x` and `v_abc100`.  
# Name the columns `"x"` for `v_x` and `"group"` for `v_abc100`, and assign it to `df_sample`.

df_sample <- data.frame(x = v_x, group = v_abc100)

# tidyverse ---------------------------------------------------------------

# 6: Load the `tidyverse` package.

library(tidyverse)

# 7: The `mtcars` dataset is a built-in base R data frame.  
# Convert it to a tibble using `as_tibble()` and assign it to `df_mtcars`.  
# Use `?as_tibble()` to read the documentation before doing so.

df_mtcars <- as_tibble(mtcars)

# 8: `mtcars` has the following columns:
#
# mpg   - Miles per gallon  
# cyl   - Number of cylinders  
# disp  - Engine displacement (cu. in.)  
# hp    - Gross horsepower  
# drat  - Rear axle ratio  
# wt    - Weight (1,000 lbs)  
# qsec  - 1/4 mile time (seconds)  
# vs    - Engine shape (0 = V-shaped, 1 = straight)  
# am    - Transmission (0 = automatic, 1 = manual)  
# gear  - Number of forward gears  
# carb  - Number of carburetors  
#
# Display the column names of `df_mtcars` using `colnames()`.  
# Do NOT assign the result to a new object.

colnames(df_mtcars)

# 9: Extract the row names of the `mtcars` dataset using `rownames()`.  
# Assign the result to `v_make`.

v_make <- rownames(mtcars)

# 10: Add `v_make` as a new column to `df_mtcars` and name the column `"make"`.

df_mtcars <- mutate(df_mtcars, make = v_make)

# 11: Filter `df_mtcars` to include only rows where:  
# - `mpg` is less than 20 AND  
# - `disp` is greater than 200  
# Assign the result to `df_subset`.

df_subset <- df_mtcars %>%
  filter(mpg <20, disp >200)

# 12: Count how many car makes meet the above conditions (Q11). Fifteen makes.
# Apply `nrow()` to `df_subset`. Use ?nrow() for the usage of nrow() function.

nrow(df_subset)

# 13: Repeat Q11 and Q12 in a single pipeline (with %>%), and assign the result to `n_make`.

n_make <- df_mtcars %>%
  filter(mpg <20, disp >200) %>%
  nrow()

# 14: Convert the `cyl` column from numeric to factor using `factor()`.  
# Add it to `df_mtcars` as a new column named `f_cyl` using `mutate()` function.
# Use ?factor() for the usage of factor() function.

df_mtcars <- df_mtcars %>%
  mutate(f_cyl = factor(cyl))

# 15: Draw a box plot showing car weight (`wt`) for each number of cylinders (`f_cyl`).

df_mtcars %>%
  ggplot(mapping = aes(x=wt,
                       y=f_cyl)
         ) +
  geom_boxplot()

# 16: Calculate the average car weight (`wt`) separately for each number of cylinders (`cyl`).
# Hint - `group_by()` and `summarize()` functions.

df_mtcars %>%
  group_by(f_cyl) %>%
  summarise(mean_wt = mean(wt))


# 17: Identify the heaviest car make (`wt`) among cars with 6 cylinders (`cyl`).
# Hint - `filter()` and `arrange()` functions.

h_make <- df_mtcars %>%
  filter(cyl == 6) %>%
  arrange(desc(wt))

## created a new object, maybe unnecessarily but the heaviest car will be at the top
## of h_make

# 18: Create a histogram showing the distribution of 1/4 mile time (`qsec`).

df_mtcars %>%
  ggplot(
    mapping = aes(x=qsec)
  ) +
  geom_histogram()

# 19: The following script reads two tibbles:
# one containing fish length data and the other containing fish weight data.
# Combine the two data frames by matching the `species` and `individual` columns
# so that the length and weight measurements for each fish are aligned.
# Assign the resulting data frame to `df_fish`.

df_l <- read_csv("data_src/data_length_assess01.csv")
df_w <- read_csv("data_src/data_weight_assess01.csv")

df_fish <- left_join(x = df_l,
          y = df_w,
          by = c("species","individual"))

# 20: Draw a scatter plot (point plot) of `length` vs. `weight` from `df_fish`,  
# coloring the points by `species`

df_fish %>%
  ggplot(mapping = aes(x=length,
                       y=weight,
                       color=species)
         ) +
  geom_point()
