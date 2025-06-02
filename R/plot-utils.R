#' Create nice axis breaks for plots
#' @description Set the breaks for a graph in nice positions.
#' @details \code{get_breaks} is the base function and creates a vector of breaks \code{ggplot_breaks} is a wrapper and
#' makes usage easier in \pkg{ggplot2}. The limits of the axis may not be known beforehand,
#' but \code{ggplot_breaks} receives it from \code{ggplot} and then creates nice breaks.
#'
#' @param limits axis limits. May be either a vector of 2 elements with lower and upper bounds, or a
#'               single number (which is the upper bound, the lower bound is then assumed to be 0).
#' @param N step size. The eventual intervals will be multiples of the divisors of \code{N} or
#' multiples of \code{N} when \code{multiples_only} is \code{TRUE}. Defaults to 10.
#' @param max_breaks maximum amount of breaks, defaults to 10.
#' @param int_only whether only integer divisors of \code{N} may be used as breaks, defaults to \code{TRUE}.
#' @param multiples_only whether only multiples of \code{N} can be used as breaks, defaults to \code{FALSE}.
#' @param include_bounds whether the resulting breaks should encompass \code{min} and \code{max}. Defaults to \code{TRUE}.
#'
#' @return A sorted numerical vector with breaks of length \code{|max_breaks|+2} when \code{include_bounds} is \code{TRUE}
#' and of size \code{|max_breaks|} otherwise.
#'
#' @examples
#' get_breaks(24, N=12, max_breaks=15)
#'
#' \dontrun{
#' ggplot() + scale_x_continuous(breaks = ggplot_breaks(N=12, max_breaks=15))}
#'
#' @export
#' @family break functions
get_breaks = function(limits, N=10, max_breaks=10, int_only=TRUE, multiples_only=FALSE, include_bounds=TRUE) {
  if (!is.vector(limits) || length(limits) > 2 || !is.numeric(limits))
    stop("Argument 'limits' must be a scalar or numeric vector.")
  if (length(limits) == 1) {xmin = 0; xmax = limits} else {xmin = limits[1]; xmax = limits[2]}
  if (xmax < xmin)
    stop("In argument 'limits', 'xmax' must be at least as large as 'xmin'.")
  stopifnot(N >= 1)
  xmax = xmax - xmin

  lp = function(d) (xmax/(max_breaks*d)) %>% log10 %>% ceiling %>% ifelse(int_only, max(., 0), .)
  up = function(d) (xmax/d)              %>% log10 %>% floor   %>% ifelse(int_only, max(., 0), .)
  intervals = {if(multiples_only) N else (1:N)[(N%%1:N) == 0]} %>% sapply(. %>% {. * 10^(lp(.):up(.))}) %>% unlist %>% unique %>% sort
  selected = intervals[xmax/intervals <= max_breaks][1]

  #sq = seq(0, floor(xmax/selected + 1)*selected, selected) + ceil(xmin/selected - 1)*selected
  compl = seq(xmin-selected,xmax+xmin+selected, selected) - xmin%%selected
  lb = max(compl[xmin >= compl])
  ub = min(compl[(xmax+xmin) <= compl])
  sq = compl[compl >= lb & compl <= ub]

  if (!include_bounds) sq = sq[sq>=xmin & sq<=(xmin+xmax)]
  sq
}


#' Specifies a square grid which fits N objects.
#'
#' @description The resulting grid will be of size \code{a*a} or \code{a*(a+1)} where \code{a} is an integer.
#' It will therefore always be a square or or have one row/column more than columns/rows.
#'
#' @param N number of objects.
#' @param moreRows whether there should be more rows than columns if the resulting grid is not square. Defaults to more rows (\code{TRUE}).
#'
#' @return A named list with elements rows and columns specifying the size of the optimal grid.
#'
#' @examples get_square_grid(5)
#' @export
get_square_grid = function(N, moreRows = TRUE) {
  N %>% sqrt %>% ceiling %>% {
    list(rows = ifelse(moreRows, ., (N/.) %>% ceiling), columns = ifelse(moreRows, (N/.) %>% ceiling, .))
  }
}
