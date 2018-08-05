#' @title extract discount sets
#' @description Based on \code{analyseShoppingCart()} and
#' \code{enumerateCombinations()} this function provides filtered but possible
#' discount sets.
#' @param alternatives \code{matrix} as it comes from
#' \code{enumerateCombinations()}
#' @param allNumbers numbers of all items in the shopping cart as it comes from
#' \code{analyseShoppingCart()}
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
#' extractDiscountSets(alternatives, ls$numbersOfEveryItem)
#' ```
#' @export
#' @importFrom magrittr '%>%'

extractDiscountSets <- function(
  alternatives, allNumbers, intermediateSteps = FALSE
) {

  colnames(alternatives) <- paste0("V", 1:ncol(alternatives))

  nMat <- matrix(
    allNumbers, nrow = length(allNumbers), ncol = ncol(alternatives)
  )
  colnames(nMat) <- colnames(alternatives)

  for (i in seq_len(ncol(alternatives)) ) {
    # for debugging
    # if (i >= 10) break

    for (j in seq_len(nrow(alternatives)) ) {
      # print(nMat[, i])
      # print(alternatives[j, i])
      numberToSubstract <- alternatives[j, i]
      nMat[1:numberToSubstract, i] <- nMat[1:numberToSubstract, i] - 1
      # print(nMat[, i])
      if (-1 %in% nMat[, i]) break

      # sorting is important. Otherwise substraction doesn't work correctly
      nMat[, i] <- sort(nMat[, i], decreasing = T)
      # print(nMat[, i])
    }
  }

  incorrectDiscount <- nMat %>%
    as.data.frame() %>%
    # dplyr::bind_cols(summands = paste0("summand", 1:nrow(checkCorrectness))) %>%
    tidyr::gather(key = "combination", value = "value") %>%
    dplyr::filter(value < 0) %>%
    dplyr::distinct(combination) %>%
    dplyr::pull(combination)
  # if (length(incorrectDiscount) == 0) incorrectDiscount <- "OK"

  correctDiscountSets <- which(! colnames(alternatives) %in% incorrectDiscount)
  correctDiscountSets <- alternatives[, correctDiscountSets]

  if (isTRUE(intermediateSteps)) {
    intermediateSteps <- list(
      "checkCorrectness" = checkCorrectness,
      "incorrectDiscountSets" = incorrectDiscountSets
    )
    return(list(
      "correctDiscountSets" = correctDiscountSets,
      "intermediateSteps" = intermediateSteps
    ))
  } else return(correctDiscountSets)

}
