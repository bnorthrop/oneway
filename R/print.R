#' Print One-Way ANOVA Results
#'
#' print.oneway prints one-way ANOVA results
#'
#' @param x an object of class oneway.
#' @param ... additional arguments passed to the function.
#'
#' @export
#'
#' @return the input object is returned silently
#' @examples
#' mileage <- oneway(mpg ~ cyl, mtcars)
#' print(mileage)
print.oneway <- function(x, ...) {
  # Make sure object is of correct class
  if(!inherits(x, "oneway")){
    stop("Must be class 'oneway'")
  }
  cat("\n\033[31m\033[1m\033[4mSummary Statistics\033[24m\033[22m\033[39m\n") # Code from crayon pkg
  print(x$summarystats, ...)
  cat("\n\033[31m\033[1m\033[4mANOVA\033[24m\033[22m\033[39m\n")
  print(summary.lm(x$anova))
}

# print.oneway(mileage)
# print.oneway(mtcars)
