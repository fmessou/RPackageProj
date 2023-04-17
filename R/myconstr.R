#' myconstr: A constructor function for two-sample t-tests
#'
#' This function performs a two-sample t-test on two given vectors (x and y) with a specified alpha level.
#' It returns an object of class "Rttest" containing the data, alpha, confidence interval, and p-value.
#'
#' @param x A numeric vector of data for the first sample.
#' @param y A numeric vector of data for the second sample.
#' @param alpha A numeric value representing the error level (e.g., 0.05 for a 95% confidence interval).
#'
#' @return An object of class "Rttest" containing the following elements:
#' \itemize{
#'   \item{data}{A list containing the x and y data vectors.}
#'   \item{alpha}{The specified alpha level.}
#'   \item{conf_interval}{The confidence interval of the difference in means.}
#'   \item{pvalue}{The p-value of the t-test.}
#' }
#' @export
#'
#' @examples
#' set.seed(21)
#' x <- rnorm(30, 5, 2)
#' set.seed(23)
#' y <- rnorm(30, 3, 2)
#' alpha <- 0.05
#'
#' obj <- myconstr(x = x, y = y, alpha = 0.05)
#' print(obj)

myconstr <- function(x, y, alpha) {
  t_test <- t.test(x, y, var.equal = TRUE, conf.level = 1 - alpha)

  data_list <- list(x = x, y = y)

  result <- list(
    data = data_list,
    alpha = alpha,
    conf_interval = t_test$conf.int,
    pvalue = t_test$p.value
  )

  class(result) <- "Rttest"

  return(result)
}

print.Rttest <- function(x, ...) {
  cat("Data:\n")
  cat("x:\n")
  print(x$data$x)
  cat("y:\n")
  print(x$data$y)
  cat("\n")
  cat("Alpha: ", x$alpha, "\n")
  cat("Confidence Interval: ", x$conf_interval, "\n")
  cat("P-value: ", x$pvalue, "\n")
}
