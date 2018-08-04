#' @title extract discount sets
#' @description Based on \code{analyseShoppingCart()} and
#' \code{enumerateCombinations()} this function provides filtered but possible
#' discount sets.
#' @param alternatives \code{matrix} as it comes from
#' \code{enumerateCombinations()}
#' @param intermediateSteps logical; default is \code{FALSE}
#' @return If \code{intermediateSteps} is set to \code{TRUE}, a list will be
#' returned; otherwise a matrix
#' @examples
#' ```
#' # These books are available in the shop
#' books <- dplyr::tibble(
#'   itemID = 1:5,
#'   name = c(
#'     "Stein der Weisen",
#'     "Kammer des Schreckens",
#'     "Gefangene von Askaban",
#'     "Feuerkelch",
#'     "Orden des Phönix"
#'   )
#' )
#'
#' set.seed(1)
#' shoppingCart <- dplyr::sample_n(books, 15, replace = TRUE) %>%
#'   dplyr::arrange(itemID)
#'
#' ls <- analyseShoppingCart(shoppingCart, itemID, name)
#' alternatives <- enumerateCombinations(ls)
#' extractDiscountSets(alternatives)
#' ```
#' @export

extractDiscountSets <- function(alternatives, intermediateSteps = FALSE) {



}
