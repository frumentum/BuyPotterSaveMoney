context("test_that_extractDiscountSets")

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
randomNumbers <- sample(1:10, 5, replace = T)
shoppingCart <- dplyr::bind_cols(books, number = randomNumbers)
ls <- analyseShoppingCart(shoppingCart)

alternatives <- enumerateCombinations(ls)

test_that(
  "extract only the possible discount sets",
  {
    correctDiscountSets <- extractDiscountSets(
      alternatives, ls$numbersOfEveryItem
    )

    # the output should be a matrix again
    expect_is(correctDiscountSets, "matrix")

    # and the number of columns should be lower or equal to 'alternatives'
    expect_lte(ncol(correctDiscountSets), ncol(alternatives))

    # if we let us show the intermediate steps, we see that every possible
    # discount set solution works
    discountSteps <- extractDiscountSets(
      alternatives, ls$numbersOfEveryItem, intermediateSteps = T
    )
    expect_is(discountSteps, "list")
    expect_length(discountSteps, 2)
    expect_is(discountSteps$correctDiscountSets, "matrix")
    expect_equal(discountSteps$correctDiscountSets, correctDiscountSets)
    expect_is(discountSteps$intermediateSteps, "list")
    iS <- discountSteps$intermediateSteps
    expect_is(iS$checkCorrectness, "matrix")
    expect_is(iS$incorrectDiscountSets, "character")
    # incorrect discount sets is the length difference between the number of
    # columns of 'alternatives' and the correct number of discount sets
    expect_length(
      iS$incorrectDiscountSets, ncol(alternatives) - ncol(correctDiscountSets)
    )
    expect_equal(ncol(iS$checkCorrectness), ncol(alternatives))
  }
)

test_that(
  "Is a shopping cart of size 80 too big?",
  {
    set.seed(1) # for reproducibility
    shoppingCart <- dplyr::bind_cols(
      books,
      number = c(15, 15, 10, 20, 20)
    )

    timeMeasurement <- system.time({
      ls <- analyseShoppingCart(shoppingCart)

      alternatives <- enumerateCombinations(ls)

      discountSets <- extractDiscountSets(alternatives, ls$numbersOfEveryItem)
    })

    expect_lte(timeMeasurement["elapsed"], 5) # less than 5 seconds
  }
)

test_that(
  "shopping cart combination 4,4,1,1,1 is special because the number of
  different items is bigger than the maximum number of one item",
  {
    shoppingCart <- dplyr::bind_cols(
      books,
      number = c(1, 4, 1, 1, 4)
    )
    ls <- analyseShoppingCart(shoppingCart)
    alternatives <- enumerateCombinations(ls)

    discountSets <- extractDiscountSets(alternatives, ls$numbersOfEveryItem)

    expect_is(discountSets, "matrix")
    expect_equal(nrow(discountSets), 4) # because maximum number is 4
    expect_equal(ncol(discountSets), 3) # because the only possibilities are:

    expect_equal(discountSets[, 1], c(5, 2, 2, 2))
    expect_equal(discountSets[, 2], c(4, 3, 2, 2))
    expect_equal(discountSets[, 3], c(3, 3, 3, 2))

  }
)

test_that(
  "shopping cart combination 10,6,4,3,3 is special because it revealed a huge
  bug in the filter algorithm",
  {
    shoppingCart <- dplyr::bind_cols(
      books,
      number = c(10, 6, 4, 3, 3)
    )
    ls <- analyseShoppingCart(shoppingCart)
    alternatives <- enumerateCombinations(ls)

    discountSets <- extractDiscountSets(alternatives, ls$numbersOfEveryItem)

    expect_is(discountSets, "matrix")

    # before the bug was solved, the matrix was empty
    expect_gte(ncol(discountSets), 1)
  }
)
