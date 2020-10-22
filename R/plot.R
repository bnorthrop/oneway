#' Plot One-Way ANOVA Results
#'
#' plot.oneway creates group comparisons for a one-way ANOVA.
#'
#' @param x an object of class 'oneway'.
#' @param ... additional arguments passed to geom_boxplot function.
#'
#' @import ggplot2
#'
#' @export
#' @return ggplot2 graph
#' @examples
#' mileage <- oneway(mpg ~ cyl, mtcars)
#' plot(mileage)

## Bozplot using GGPLOT2
plot.oneway <- function(x, ...) {
  if(!inherits(x, "oneway")){
        stop("Must be class 'oneway'")
      }

  g <- as.character(x$anova$terms[[3]])
  y <- as.character(x$anova$terms[[2]])
  ggplot(x$anova$model, aes(x = factor(.data[[g]]), y = .data[[y]],
                            fill = factor(.data[[g]]))) +
    geom_boxplot() +
    labs(x=g) +
    theme(legend.position = "none")
}


## Boxplot using BASE R
# plot.oneway <- function(x, ...) {
#   # Make sure object is of correct class
#   if(!inherits(x, "oneway")){
#     stop("Must be class 'oneway'")
#   }
#   boxplot(x$anova$terms, x$anova$model, ...)
# }




###########################3
# # From looking through mileage$anova list
# mileage$anova$terms  # formula
# mileage$anova$model  # actual data frame used in the anova
#
# # boxplot(formula, data)
# boxplot(mileage$anova$terms, mileage$anova$model)
