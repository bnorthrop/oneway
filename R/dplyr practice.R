# write a short program using dplyr
# to get n, mean, and sd for mpg
# by cyl in mtcars data frame

library(dplyr)
mtcars %>%
  group_by(cyl) %>%
  summarise(n = n(),
            mean=mean(mpg),
            sd = sd(mpg))

# Put this inside a function
# USE .data[[x]] for dplyr/ggplot inside function! (when passed as strings)
mystats <- function(data, group, y) {
  require(dplyr)
  data %>%
    group_by(.data[[group]]) %>%
    summarise(n = n(),
              mean=mean(.data[[y]]),
              sd = sd(.data[[y]])) %>%
    as.data.frame()
}
mystats(mtcars, g="cyl", y="mpg")

# Now arguments are not strings
# Use enquo() to keep argument from being evaluated (need !! as well)
mystats2 <- function(data, group, y) {
  require(dplyr)
  group <- enquo(group)
  y <- enquo(y)
  data %>%
    group_by(!!group) %>%
    summarise(n = n(),
              mean=mean(!!y),
              sd = sd(!!y)) %>%
    as.data.frame()
}
mystats2(mtcars, g=cyl, y=mpg)

# Now argument is a formula and data set
mystats3 <- function(formula, data) {
  require(dplyr)
  group <- as.character(formula[[3]])
  y <- as.character(formula[[2]])
  data %>%
    group_by(.data[[group]]) %>%
    summarise(n = n(),
              mean=mean(.data[[y]]),
              sd = sd(.data[[y]])) %>%
    as.data.frame()
}
mystats3(mpg ~ cyl, mtcars)

