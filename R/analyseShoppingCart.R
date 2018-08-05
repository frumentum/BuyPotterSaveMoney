#' @title analyse the shopping cart
#' @description This function analyses the content of a online shopping cart. It
#' looks for the following information: How many items are sold in total, how
#' many different items do we have, what's the maximum number of one item and
#' at least how often do we have this 'maximum number of one item' within the
#' shopping cart.
#' @param shoppingCart shopping cart as an data.frame with one row for one item
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
#' randomNumbers <- sample(1:10, 5, replace = T)
#' shoppingCart <- dplyr::bind_cols(books, number = randomNumbers)
#' analyseShoppingCart(shoppingCart)
#' ```
#' @importFrom magrittr '%>%'
#' @export

analyseShoppingCart <- function(shoppingCart) {

  # at first: remove items with number 0
  shoppingCart <- shoppingCart %>%
    dplyr::filter(number != 0)

  # The new column 'n' contains the number of one item, so if we summarize it
  # we'll get the total number of items
  itemsInTotal <- sum(shoppingCart$number)
  # number of rows represents the number of different items we have
  numberOfDifferentItems <- nrow(shoppingCart)
  # And how often was the item bought which was bought most?
  maxNumberPerItem <- max(shoppingCart$number)
  # probably there are more than one item with the same 'maximum number per
  # item'?
  numberOfMaxima <- shoppingCart %>%
    dplyr::filter(number == maxNumberPerItem) %>%
    nrow()
  # And how often was the item bought which was bought least?
  minNumberPerItem <- min(shoppingCart$number)
  numberOfMinima <- shoppingCart %>%
    dplyr::filter(number == minNumberPerItem) %>%
    nrow()

  # add number of items as a vector
  numbersOfEveryItem <- shoppingCart %>%
    dplyr::arrange(dplyr::desc(number)) %>%
    dplyr::pull(number)

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
