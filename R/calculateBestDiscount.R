#' @title calculate the best discount
#' @description This function calculates the best discount based on
#' \code{analyseShoppingCart()}, \code{enumerateCombinations()} and
#' \code{extractDiscountSets()}. It needs the output of the last mentioned
#' function. Additionally information are required about the price of one item
#' and the discount prices.
#' @param discountSets \code{matrix} as it comes from
#' \code{extractDiscountSets()}
#' @param discountInfos \code{tibble} or \code{data.frame} which contains
#' information about the sets and their discount; read details
#' @param pricePerItem numerical price for one item; Currently only one price is
#' allowed
#' @param intermediateSteps logical; if it's set to \code{TRUE}, intermediate
#' steps are shown; default is \code{FALSE}
#' @details
#' Currently the argument \code{discountInfos} needs one special format: It
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
#' randomNumbers <- sample(1:10, 5, replace = T)
#' shoppingCart <- dplyr::bind_cols(books, number = randomNumbers)
#'
#' ls <- analyseShoppingCart(shoppingCart)
#' alternatives <- enumerateCombinations(ls)
#' discountSets <- extractDiscountSets(alternatives, ls$numbersOfEveryItem)
#'
#' discountInfos <- dplyr::tibble(
#'   set = 1:5,
#'   discount = c(0, 5, 10, 20, 25)
#' )
#' calculateBestDiscount(
#'    correctDiscountSets, discountInfos, pricePerItem = 8
#' )
#' ```
#' @importFrom magrittr '%>%'
#' @export

calculateBestDiscount <- function(
  discountSets, discountInfos, pricePerItem, intermediateSteps = FALSE
) {

  # some security checks at the beginning
  stopifnot(is.matrix(discountSets))
  stopifnot(is.data.frame(discountInfos))
  stopifnot(is.numeric(pricePerItem))
  stopifnot(ncol(discountInfos) == 2)
  stopifnot(length(pricePerItem) == 1)

  # join the different discount sets with the information about the discount
  # and calculate the price per set
  discountSetTibble <- discountSets %>%
    as.data.frame() %>%
    tidyr::gather(key = "combination", value = "set") %>%
    dplyr::left_join(discountInfos, by = "set") %>%
    dplyr::mutate(price = set*pricePerItem*(1 - discount/100) )

  # build the sum over every possible discount combination and choose the
  # cheapest one
  bestDiscountSet <- discountSetTibble %>%
    dplyr::group_by(combination) %>%
    dplyr::summarise(itemsInTotal = sum(set),
                     price = sum(price)) %>%
    dplyr::filter(price == min(price)) %>%
    dplyr::mutate(priceWithoutDiscount = itemsInTotal*pricePerItem,
                  discountAbs = priceWithoutDiscount - price,
                  discountPercent = (1 - (price / priceWithoutDiscount)) * 100)

  # probably sometimes it can happen that more combinations lead to the same
  # price in total. In these cases take the first combination
  if (nrow(bestDiscountSet) > 1) bestDiscountSet <- bestDiscountSet[1, ]

  # return(discountSetTibble)
  # add information about the composition of the best discount set
  bestDiscountSetDetailed <- discountSetTibble %>%
    dplyr::filter(combination == bestDiscountSet$combination) %>%
    dplyr::select(set) %>%
    dplyr::mutate(setN = paste0("set", 1:dplyr::n() ) ) %>%
    tidyr::spread(key = setN, value = set) %>%
    dplyr::bind_cols(bestDiscountSet)

  necessaryInformation <- bestDiscountSetDetailed %>%
    dplyr::rename(priceInTotal = price) %>%
    dplyr::select(
      priceInTotal, discountAbs, discountPercent, priceWithoutDiscount
    ) %>%
    unlist()

  if (isTRUE(intermediateSteps)) {
    ls <- list(
      "discountSetTibble" = discountSetTibble,
      "bestDiscountSet" = bestDiscountSet,
      "bestDiscountSetDetailed" = bestDiscountSetDetailed
    )
    return(
      list(
        "bestDiscount" = necessaryInformation,
        "intermediateSteps" = ls
      )
    )
  } else return(necessaryInformation)

}
