data(mtcars)

# is there a relationship between
# number of cylinders and fuel efficiency

table(mtcars$cyl)
stats <- function(x) {
  c(n = length(x), mean = mean(x), sd = sd(x))
}
stats(mtcars$mpg)

# Saves as class = "by" which is good for visualizing, but not working with after
result <- by(mtcars$mpg, mtcars$cyl, stats)
by(mtcars$mpg, mtcars$cyl, stats)

# Saves as data frame, much easier to work with
result2 <- aggregate(mpg~cyl, mtcars, stats)
aggregate(mpg~cyl, mtcars, stats)


######################
#
# tablefnct <- function(x) {
#   c(value = length(x), count = mean(x), proportion = sd(x))
# }
#
# for (i in names(mtcars)) {
#   tablefnct(i)
# }
#
# for (i in names(mtcars)) {
#   aggregate(mpg~i, mtcars, tablefnct)
# }
#
# for (i in names(mtcars)) {
#   result3 <- by(mtcars$i, mtcars$i, tablefnct)
#   return()
# }

