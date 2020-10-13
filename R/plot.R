#' Plot One-Way ANOVA Results
#'
#' plot.oneway creates group comparisons for a one-way ANOVA.
#'
#' @param x an object of class 'oneway'.
#' @param ... additional arguments passed to boxplot function.
#' @export
#' @return NULL
#' @examples
#' # mileage <- oneway(mpg ~ cyl, mtcars)
#' plot(mileage)
plot.oneway <- function(x, ...) {
  # Make sure object is of correct class
  if(!inherits(x, "oneway")){
    stop("Must be class 'oneway'")
  }
  boxplot(x$anova$terms, x$anova$model, ...)
}




###########################3
# # From looking through mileage$anova list
# mileage$anova$terms  # formula
# mileage$anova$model  # actual data frame used in the anova
#
# # boxplot(formula, data)
# boxplot(mileage$anova$terms, mileage$anova$model)
