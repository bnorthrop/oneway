#' One Way Analysis of Variance
#'
#' oneway computes a one-way analysis of variance
#' and includes group level summary statistics.
#'
#' @param formula an object of class formula, relating the
#' dependent variable to the grouping variable.
#' @param data a data frame containing the variables in the model.
#'
#' @import dplyr
#'
#' @export
#' @return a list with 2 elements.
#'
#' @examples
#' mileage <- oneway(mpg ~ cyl, mtcars)
oneway <- function(formula, data) {
  # delete missing data
  data <- na.omit(data)

  #anova
  fit <- lm(formula, data)

  # summary statistics for each level of grouping variable
  # stats <- aggregate(formula, data,
  #                    function(x) c(n=length(x),
  #                                  mean = mean(x),
  #                                  sd=sd(x)))

  # summary statistics rewritten using dplyr instead of aggregate
  group <- as.character(formula[[3]])
  y <- as.character(formula[[2]])
    # Don't use library() or require() for dplyr. Add to import documentation instead
  stats <- data %>%
    group_by(.data[[group]]) %>%
    summarise(n = n(),
              mean=mean(.data[[y]]),
              sd = sd(.data[[y]])) %>%
    as.data.frame()


  # return results
  result <- list(anova=fit, summarystats=stats)
  class(result) <- "oneway"
  return(result)
}

# x <- oneway(mpg ~ cyl, mtcars)
# x # when you haven't definied print.oneway (for whatever class you
# # assign), R executes print.default(x)
#
#
#
#
# # Observe outside oneway function
# aggregate(mpg ~ cyl, mtcars,
#           function(x) c(n=length(x), mean = mean(x), sd=sd(x)))






