context("text_that_calculateBestDiscount")

# These books are available in the shop
books <- dplyr::tibble(
  itemID = 1:5,
  name = c(
    "Stein der Weisen",
    "Kammer des Schreckens",
    "Gefangene von Askaban",
    "Feuerkelch",
    "Orden des Phönix"
  )
)

set.seed(1)
shoppingCart <- dplyr::bind_cols(
  books,
  number = sample(0:8, 5, replace = T)
)

ls <- analyseShoppingCart(shoppingCart)
alternatives <- enumerateCombinations(ls)
correctDiscountSets <- extractDiscountSets(alternatives)

discountTibble <- dplyr::tibble(
  set = 1:5,
  discount = c(0, 5, 10, 20, 25)
)

test_that(
  "calculating the discount works",
  {
    bestDiscount <- calculateBestDiscount(
      correctDiscountSets, discountTibble, pricePerItem = 8
    )
    # the output shall be a numeric vector containing discount, price in total,
    # and so on
    expect_is(bestDiscount, "numeric")
    partsOfOutput <- c(
      "priceInTotal", "discountAbs", "discountPercent", "priceWithoutDiscount"
    )
    expect_equal(partsOfOutput, names(bestDiscount))

    # similiar to the other functions, calculateBestDiscount has an argument
    # intermediateSteps which can be set to TRUE to show all intermediate steps
    bestDiscountSteps <- calculateBestDiscount(
      correctDiscountSets, discountTibble, pricePerItem = 8,
      intermediateSteps = TRUE
    )
    expect_is(bestDiscountSteps, "list")
    expect_length(bestDiscountSteps, 2)
    expect_equal(bestDiscountSteps$bestDiscount, bestDiscount)
    iS <- bestDiscountSteps$intermediateSteps
    expect_is(iS, "list")
    expect_length(iS, 3)
    namesOfIntermediateStepsContent <- c(
      "discountSetTibble", "bestDiscountSet", "bestDiscountSetDetailed"
    )
    expect_equal(names(iS), namesOfIntermediateStepsContent)
    expect_is(iS$discountSetTibble, "data.frame")
    expect_is(iS$bestDiscountSet, "data.frame")
    expect_is(iS$bestDiscountSetDetailed, "data.frame")
  }
)
