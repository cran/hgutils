#'
#' #' Separate values
#' #' @description Separates real numbers from one another that are to close to each other. In the resulting set,
#' #' the values are separated by a minimum distance, bounded by lower and upper limits and are constraint to be as
#' #' close as possible to their original values.
#' #'
#' #' @param X numerical vector of real numbers.
#' #' @param distance minimum distance between subsequent numbers. Must be a scalar or vector of size \code{|X|}.
#' #' @param min,max lower and upper limits.
#' #' @details This function can be used for example to separate labels that are too close to one another.
#' #' The resulting vector will create enough space, such that the labels do not overlap any more, yet are still close to their original values.
#' #'
#' #' The output vector has the following properties. For all elements \code{e_i}, \code{min <= e_i <= max}.
#' #' For the distance \code{D} between \code{e_i} and \code{e_(i+1)}, \code{D >= max(d_i, d_(i+1))}. And finally, the distance
#' #' between \code{e_i} and \code{X_i} is minimized for all \code{e_i}.
#' #'
#' #' @return A numerical vector with the same length as \code{X}, with numbers bounded by min and max, close to their original values and
#' #'         with the minimum allowed distance between subsequent values.
#' #' @export
#' #'
#' #' @examples separate_values(c(0.3,0.4,0.41), distance = 0.05, min = 0, max = 1)
#' #' @importFrom limSolve lsei
#' separate_values = function(X, distance = 0.05, min = 0, max = 1) {
#'   if (!is.vector(X) || !is.numeric(X))
#'     stop(sprintf("Argument 'X' must be a numerical vector of real numbers, but is %s.",frmt(X)))
#'   if (!is.numeric(distance))
#'     stop(sprintf("Argument 'distance' must be numeric, but is %s.", frmt(distance)))
#'   if (max <= min)
#'     stop(sprintf("Argument 'max' must be strictly larger than 'min', but 'min'=%s and 'max'=%s.", frmt(min), frmt(max)))
#'   if (!length(distance) %in% c(1,length(X)))
#'     stop(sprintf("Argument 'distance' must be of length 1 or |X|, but is of length %s.", frmt(length(distance))))
#'
#'   N = length(X)
#'   if (length(distance) == 1) distance = rep(distance, N)
#'
#'   ord = order(X)
#'   distance = distance[ord]
#'   X = X[ord]
#'   distance = if(N>=2) pmax(distance[1:(N-1)], distance[2:N]) else numeric(0)
#'
#'   if ((max - min) < sum(distance))
#'     stop(sprintf(paste0("The total distance constraint is %s, but the space between 'min' and 'max' is only %s.",
#'                         "\nExtend either the bounds or limit the distance constraint."),
#'                  frmt(sum(distance)), frmt(max-min)))
#'
#'   #constraint for limits [min-max]
#'   upper = matrix(nrow = 2 * N, ncol = N, 0)
#'   for (i in 1:N) {
#'     upper[(i*2 - 1):(i*2), i] = c(1, -1)
#'   }
#'
#'   if(N > 1) {
#'     #constraint for distances between elements
#'     lower = matrix(nrow = N - 1, ncol = N, 0)
#'     for (i in 1:(N - 1)) {
#'       lower[i, i:(i+1)] = c(-1, 1)
#'     }
#'   } else {lower = NULL}
#'   H = c(rep(c(min, -max), N), distance)  #solution vectors
#'
#'   # constraint on limits, spacing and distance to original value
#'   lsei(A = diag(N), B = X, G = rbind(upper, lower), H = H, type = 2)$X[ord]
#' }
