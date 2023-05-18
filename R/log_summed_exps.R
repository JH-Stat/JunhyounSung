#'
#' The log-transformed summation of exponential values
#'
#' @param x A vector of value x increasing by 1 unit
#'
#' @return The log-transformed summation of the input vector
#'
#' @examples
#' log_summed_exps(c(1,2,3,4,5))
#' log_summed_exps(seq(1:5))
#' log_summed_exps(1:2000)
#'
#' @export
log_summed_exps <- function(x) {
  n = length(x)
  x_max = max(x)
  if (n == 1) {
    return(x_max)
  } else {
    x_sorted = sort(x, decreasing = TRUE)
    log_sum = x_max + log1p(sum(exp(x_sorted[-1] - x_max)))
    return(log_sum)
  }
}
