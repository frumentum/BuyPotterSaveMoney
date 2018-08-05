#' @title calculate price
#' @description \code{calculatePrice()} calculates the best price of the
#' existing shopping cart. That means at first every meaningful combination of
#' any discount offers is calculated. Than the cheapest way is filtered and
#' returned. This functions acts as a convenient wrapper for the functions
#' \code{analyseShoppingCart()}, \code{enumerateCombinations()},
#' \code{extractDiscountSets()} and \code{calculateBestDiscount()}.
#' @param shoppingCart shopping cart as an data.frame with one row for one item
#' @param discountInfos \code{tibble} or \code{data.frame} which contains
#' information about the sets and their discount; read details
#' @param pricePerItem numerical price for one item; Currently only one price is
#' allowed
#' @param intermediateSteps if it's set to \code{TRUE}, intermediate
#' steps are shown; default is \code{FALSE}
#' @details Currently the argument \code{discountInfos} needs one special format: It
#' needs two columns:
#' 1. The first one is called \code{set} and contains numerical information
#'    about the discount sets' possibilities (read \code{examples})
#' 2. The second one is called \code{discount} and contains numerical
#'    information about the discount.
#' Further the argument \code{shoppingCart} requires some specific formatting,
#' since no generalisation has been implemented yet (\code{analyseShoppingCart}
#' already has this generalisation). These requirements are:
#' 1. \code{shoppingCart} need to be a \code{data.frame} with...
#' 2. ...two columns. The first need to be called \code{itemID} and contains the
#'    ID of every item. The second one is named \code{name} and contains the
#'    name of the item.
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
#' discountTibble <- dplyr::tibble(
#'   set = 1:5,
#'   discount = c(0, 5, 10, 20, 25)
#' )
#'
#' calculatePrice(shoppingCart, discountTibble, pricePerItem = 8)
#' ```
#' @importFrom magrittr '%>%'
#' @export

calculatePrice <- function(
  shoppingCart, discountInfos, pricePerItem, intermediateSteps = FALSE
) {

  if (! isTRUE(intermediateSteps) ) {
    # analyse shopping cart
    ls <- analyseShoppingCart(shoppingCart)
    # enumerate every possible combination (and filter some useless ones)
    alternatives <- enumerateCombinations(ls)
    # filter only the meaningful combinations using two for loops
    discountSets <- extractDiscountSets(alternatives)
    # calculate the best discount
    bestDiscount <- calculateBestDiscount(
      discountSets, discountInfos, pricePerItem
    )
    return(bestDiscount)
  }

  if (isTRUE(intermediateSteps) ) {
    # analyse shopping cart
    ls <- analyseShoppingCart(shoppingCart)
    # enumerate every possible combination (and filter some useless ones)
    alternatives <- enumerateCombinations(ls, intermediateSteps = TRUE)
    intermediateSteps <- alternatives$intermediateSteps
    alternatives <- alternatives$alternatives
    # filter only the meaningful combinations using two for loops
    discountSets <- extractDiscountSets(alternatives, intermediateSteps = TRUE)
    intermediateSteps <- c(intermediateSteps, discountSets$intermediateSteps)
    discountSets <- discountSets$correctDiscountSets
    # calculate the best discount
    bestDiscount <- calculateBestDiscount(
      discountSets, discountInfos, pricePerItem, intermediateSteps = TRUE
    )

    intermediateSteps <- c(intermediateSteps, bestDiscount$intermediateSteps)
    # intermediateSteps <- c(list("bestDiscount" = bestDiscount$bestDiscount),
    #                        intermediateSteps)

    return(list(
      "bestDiscount" = bestDiscount$bestDiscount,
      "intermediateSteps" = intermediateSteps
    ))
  }

}
