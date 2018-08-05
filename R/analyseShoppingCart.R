#' @title analyse the shopping cart
#' @description This function analyses the content of a online shopping cart. It
#' looks for the following information: How many items are sold in total, how
#' many different items do we have, what's the maximum number of one item and
#' at least how often do we have this 'maximum number of one item' within the
#' shopping cart.
#' @param shoppingCart shopping cart as an data.frame with one row for one item
#' @param ... Variable(s) which shall be used for grouping. Can be given as a
#' name.
#' @return It returns a list with four numeric and named values.
#' @examples
#' ```
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
#' shoppingCart <- dplyr::sample_n(books, 8, replace = TRUE)
#' shoppingCart <- dplyr::arrange(shoppingCart, itemID)
#' analyseShoppingCart(shoppingCart, itemID, name)
#' ```
#' @importFrom magrittr '%>%'
#' @importFrom rlang ':='
#' @export

analyseShoppingCart <- function(shoppingCart, ...) {

  # use defined variable ... so our function can accept any number of arguments
  group_by <- dplyr::quos(...)

  # count items of the shopping cart
  # note: Three "!!!" are necessary to accept any number of grouping variables
  # as names
  groupedShoppingCart <- dplyr::count(shoppingCart, !!!group_by)

  # The new column 'n' contains the number of one item, so if we summarize it
  # we'll get the total number of items
  itemsInTotal <- sum(groupedShoppingCart$n)
  # number of rows represents the number of different items we have
  numberOfDifferentItems <- nrow(groupedShoppingCart)
  # And how often was the item bought which was bought most?
  maxNumberPerItem <- max(groupedShoppingCart$n)
  # probably there are more than one item with the same 'maximum number per
  # item'?
  numberOfMaxima <- groupedShoppingCart %>%
    dplyr::filter(n == maxNumberPerItem) %>%
    nrow()
  # And how often was the item bought which was bought least?
  minNumberPerItem <- min(groupedShoppingCart$n)
  numberOfMinima <- groupedShoppingCart %>%
    dplyr::filter(n == minNumberPerItem) %>%
    nrow()

  # add number of items as a vector
  numbersOfEveryItem <- groupedShoppingCart %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::pull(n)

  return(list(
    "itemsInTotal" = itemsInTotal,
    "numberOfDifferentItems" = numberOfDifferentItems,
    "maxNumberPerItem" = maxNumberPerItem,
    "numberOfMaxima" = numberOfMaxima,
    "minNumberPerItem" = minNumberPerItem,
    "numberOfMinima" = numberOfMinima,
    "numbersOfEveryItem" = numbersOfEveryItem
  ))

}
