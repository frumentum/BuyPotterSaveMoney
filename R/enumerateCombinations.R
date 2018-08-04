#' @title enumerate combination
#' @description This function is based on the output of
#' \code{analyseShoppingCart()}. Using the mathematical partition function from
#' the package \code{partitions} every combination is enumerated. Afterwards
#' the combinations are filtered so only combinations are returned which make
#' sense for the practical utilization.
#' @param listFromASC list as it comes from \code{analyseShoppingCart()}
#' @param intermediateSteps logical; if TRUE, additionally a list is returned
#' which contains the intermediate steps. More in details; default is FALSE
#' @details
#' If \code{intermediateSteps} is set to \code{TRUE}, the output of this
#' function will change to a list of length 2. The first element will be the
#' same matrix as it is produced with \code{intermediateSteps == FALSE}. The
#' second element is a list containing the intermediate steps which are done
#' within this function. These steps include:
#' 1. Enumerate every possible partition using
#'    \code{partitions::restrictedparts()}.
#' 2. filter1: Since we have only up to five different items in the shopping
#'    cart and discount is given for the items' dissimilarity, the greatest
#'    'discount set' can only be the number of different items in the shopping
#'    cart.
#' 3. filter2: Accordingly the smallest summand need to be greater or equal to
#'    the 'number of maxima' in our shopping cart. This filter minimizes the
#'    combinations of partitions which can't lead to a discount set later on.
#' Because there are still some combinations of partitions that can't be a
#' discount set, another function is necessary to point only the possible
#' discount sets out.
#' @return
#' Depending on \code{intermediateSteps} a matrix or a list is returned. Matrix
#' represents the combinations which shall be used for calculating the discount.
#' @examples
#' ```
#' ### setup
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

#' set.seed(1) # for reproducibility
#' shoppingCart <- dplyr::sample_n(books, 8, replace = TRUE)
#' shoppingCart <- dplyr::arrange(shoppingCart, itemID)
#' ls <- analyseShoppingCart(shoppingCart, itemID, name)
#' ### end of setup
#' enumerateCombinations(ls)
#' ```
#' @export

enumerateCombinations <- function(listFromASC, intermediateSteps = FALSE) {

  # at first an important security check if the list is really the one which is
  # expected
  stopifnot(
    c(
      "itemsInTotal", "numberOfDifferentItems", "maxNumberPerItem",
      "numberOfMaxima", "minNumberPerItem"
    ) %in% names(listFromASC)
  )

  ### enumerate all combinations
  everyCombination <- as.matrix(partitions::restrictedparts(
    listFromASC$itemsInTotal,
    listFromASC$maxNumberPerItem
  ))

  ### let's filter
  # No bigger set of combinations is possible than we have different items in
  # the shopping cart. That means that the first summand need to be lower or
  # equal to the number of different items.
  biggestSummandEqualsDifferentItems <- which(
    everyCombination[1,] <= listFromASC$numberOfDifferentItems
  )

  # Filter impossible combinations, eg the combination 3* 1st book, 3*2nd
  # book, 3*3rd book and 1*4th book: 4 discount + 2*3 discount, but not possible
  # is 2*4 discount + 1*2 discount; in other words, the smallest summand need
  # to be greater or equal to the number of maxima we have in our shopping cart.
  ls <- listFromASC # to make name shorter
  smallestSummandEqualsNumberMaxima <- which(
    everyCombination[ls$maxNumberPerItem,] >= ls$numberOfMaxima
  )
  # for us the intersection of those filters is interesting
  intersection <- intersect(
    biggestSummandEqualsDifferentItems,
    smallestSummandEqualsNumberMaxima
  )
  alternatives <- as.matrix(everyCombination[, intersection])

  if (isTRUE(intermediateSteps)) {
    intermediateSteps <- list(
      everyCombination = everyCombination,
      biggestSummandEqualsDifferentItems = biggestSummandEqualsDifferentItems,
      smallestSummandEqualsNumberMaxima = smallestSummandEqualsNumberMaxima,
      intersection = intersection
    )

    return(list(
      alternatives = alternatives,
      intermediateSteps = intermediateSteps
    ))
  } else {
    return(alternatives)
  }

}
