context("test_that_enumerateCombinations")

test_that(
  "enumerate possible combinations based on restricted partitions",
  {
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
    # create first test shopping cart with the combination 3,1,1,3
    shoppingCart <- dplyr::bind_rows(
      books[2, ],
      books[2, ],
      books[2, ],
      books[3, ],
      books[4, ],
      books[5, ],
      books[5, ],
      books[5, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)

    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    # The output format should be different if intermediate steps are returned
    expect_is(tmpResult, "matrix")
    expect_is(tmpResultT, "list")
    expect_equal(names(tmpResultT), c("alternatives", "intermediateSteps"))
    expect_is(tmpResultT$intermediateSteps, "list")
    expect_equal(tmpResult, tmpResultT$alternatives)
    # only our list type is accepted
    expect_error(enumerateCombinations(list(a = 1)) )

    ### interesting expecations
    # To understand the following procedure: Most of the possible discount
    # combinations don't make sense or are always more expensive (eg 2* 1st
    # book, 2* 2nd book, 2* 3rd book, 1* 4th book, 1*5th book: It makes sense to
    # calculate 4*8€*0.8 + 4*8€*0.8 and 5*8€*0.75 + 3*8€*0.9, but it doesn't
    # make sense to calculate 4*(2*8€*0.95). There are only some combinations,
    # between 1 and 3 for the most common use cases, which make sense to get
    # enumerated. The following expectations shall test if it works to recognize
    # those cases which are interesting for enumeration.

    # only the following test is general; the others are specific for one
    # combination and are tested separately
    expect_equal(nrow(tmpResult), ls$maxNumberPerItem)

    # test output of the intermediate steps
    iS <- tmpResultT$intermediateSteps
    stepNames <- c("everyCombination",
                   "filterForDifferentItems",
                   "filterForPossibleCombinations",
                   "filterMoreCombinations",
                   "intersection")
    expect_equal(stepNames, names(iS))
    # at first data type testing again
    expect_is(iS$everyCombination, "matrix")
    expect_is(iS$filterForDifferentItems, "integer")
    expect_is(iS$filterForPossibleCombinations, "integer")
    expect_is(iS$filterMoreCombinations, "integer")
    expect_is(iS$intersection, "integer")
    # intersection should be lower or equal to the bigger of those filters
    expect_lte(
      length(iS$intersection),
      max(
        c(length(iS$filterForDifferentItems),
          length(iS$filterForPossibleCombinations),
          length(iS$filterMoreCombinations))
      )
    )
  }
)

test_that(
  "enumerateCombinations() specially for combination 3,1,1,3",
  {
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
    # create first test shopping cart with the combination 3,1,1,3
    shoppingCart <- dplyr::bind_rows(
      books[2, ],
      books[2, ],
      books[2, ],
      books[3, ],
      books[4, ],
      books[5, ],
      books[5, ],
      books[5, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 2) # 2 columns in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[,1], c(4,2,2)) # first column
    expect_equal(tmpResult[,2], c(3,3,2)) # second column

    # test intermediate steps
    iS <- tmpResultT$intermediateSteps
    expect_equal(ncol(iS$everyCombination), 10) # 10 columns
    expect_equal(nrow(iS$everyCombination), 3) # 3 rows
    expect_equal(iS$filterForDifferentItems, c(5,8,9,10)) # possible columns
    expect_equal(iS$filterForPossibleCombinations, c(9, 10)) # possible columns
    # expect_equal(iS$filterMoreCombinations, c())
    expect_equal(iS$intersection, c(9, 10))
  }
)

test_that(
  "enumerateCombinations() specially for combination 5,1",
  {
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
    # create first test shopping cart with the combination 5,1
    shoppingCart <- dplyr::bind_rows(
      books[2, ],
      books[2, ],
      books[2, ],
      books[2, ],
      books[2, ],
      books[1, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 1) # 1 column in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[,1], c(2,1,1,1,1)) # only one column

    # test intermediate steps
    iS <- tmpResultT$intermediateSteps
    expect_equal(ncol(iS$everyCombination), 10) # 10 columns
    expect_equal(nrow(iS$everyCombination), 5) # 5 rows
    expect_equal(iS$filterForDifferentItems, c(7,9,10)) # possible columns
    expect_equal(iS$filterForPossibleCombinations, 10) # possible columns
    expect_equal(iS$intersection, 10)
  }
)

test_that(
  "enumerateCombinations() specially for combination 5,4,1",
  {
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
    # create first test shopping cart with the combination 5,1
    shoppingCart <- dplyr::bind_rows(
      books[2, ],
      books[2, ],
      books[2, ],
      books[2, ],
      books[2, ],
      books[1, ],
      books[1, ],
      books[1, ],
      books[1, ],
      books[3, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    # expect_equal(ncol(tmpResult), 2) # 1 column in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[,1], c(3,2,2,2,1)) # first column
    expect_equal(tmpResult[,2], c(2,2,2,2,2)) # second column

    # test intermediate steps
    # iS <- tmpResultT$intermediateSteps
    # expect_equal(ncol(iS$everyCombination), 10) # 10 columns
    # expect_equal(nrow(iS$everyCombination), 5) # 5 rows
    # expect_equal(iS$filterForDifferentItems, c(7,9,10)) # possible columns
    # expect_equal(iS$filterForPossibleCombinations, 10) # possible columns
    # expect_equal(iS$intersection, 10)
  }
)
