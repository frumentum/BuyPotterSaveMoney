#' @title calculate the best discount
#' @description This function calculates the best discount based on
#' \code{analyseShoppingCart()}, \code{enumerateCombinations()} and
#' \code{extractDiscountSets()}. It needs the output of the last mentioned
#' function. Additionally information are required about the price of one item
#' and the discount prices.
#' @param discountSets \code{matrix} as it comes from
#' \code{extractDiscountSets()}
#' @param discountTibble \code{tibble} or \code{data.frame} which contains
#' information about the sets and their discount; read details
#' @param pricePerItem numerical price for one item; Currently only one price is
#' allowed
#' @param intermediateSteps logical; if it's set to \code{TRUE}, intermediate
#' steps are shown; default is \code{FALSE}
#' @details
#' Currently the argument \code{discountTibble} needs one special format: It
#' needs two columns:
#' 1. The first one is called \code{set} and contains numerical information
#'    about the discount sets' possibilities (read \code{examples})
#' 2. The second one is called \code{discount} and contains numerical
#'    information about the discount.
#' @return Depending on \code{intermediateSteps}, only a numerical vector is
#' returned or a list which additionally contains the intermediate steps.
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
#' discountSets <- extractDiscountSets(alternatives)
#'
#' discountTibble <- dplyr::tibble(
#'   set = 1:5,
#'   discount = c(0, 0.05, 0.1, 0.2, 0.25)
#' )
#' calculateBestDiscount(
#'    correctDiscountSets, discountTibble, pricePerItem = 8
#' )
#' ```
#' @importFrom magrittr '%>%'
#' @export

calculateBestDiscount <- function(
  discountSets, discountTibble, pricePerItem, intermediateSteps = FALSE
) {



}