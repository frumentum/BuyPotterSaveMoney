context("test_that_enumerateCombinations")

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

test_that(
  "enumerate possible combinations based on restricted partitions",
  {
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
                   "biggestSummandEqualsDifferentItems",
                   "smallestSummandEqualsNumberMaxima",
                   "intersection")
    expect_equal(stepNames, names(iS))
    # at first data type testing again
    expect_is(iS$everyCombination, "matrix")
    expect_is(iS$biggestSummandEqualsDifferentItems, "integer")
    expect_is(iS$smallestSummandEqualsNumberMaxima, "integer")
    expect_is(iS$intersection, "integer")
    # intersection should be lower or equal to the bigger of those filters
    expect_lte(
      length(iS$intersection),
      max(
        c(length(iS$biggestSummandEqualsDifferentItems),
          length(iS$smallestSummandEqualsNumberMaxima))
      )
    )
  }
)

test_that(
  "enumerateCombinations() specially for combination 3,1,1,3",
  {
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
    # possible columns
    expect_equal(iS$biggestSummandEqualsDifferentItems, c(5,8,9,10))
    expect_equal(iS$smallestSummandEqualsNumberMaxima, c(9, 10))
    expect_equal(iS$intersection, c(9, 10))
  }
)

test_that(
  "enumerateCombinations() specially for combination 5,1",
  {
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
    # possible columns
    expect_equal(iS$biggestSummandEqualsDifferentItems, c(7,9,10))
    expect_equal(iS$smallestSummandEqualsNumberMaxima, 10)
    expect_equal(iS$intersection, 10)
  }
)

test_that(
  "enumerateCombinations() specially for combination 5,4,1",
  {
    # create first test shopping cart with the combination 5,4,1
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
    expect_equal(ncol(tmpResult), 3) # 3 possibilites in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 1], c(3,3,2,1,1)) # first possibilty
    expect_equal(tmpResult[, 2], c(3,2,2,2,1)) # second possibilty
    expect_equal(tmpResult[, 3], c(2,2,2,2,2)) # third possibilty

    # test intermediate steps
    iS <- tmpResultT$intermediateSteps
    expect_equal(ncol(iS$everyCombination), 30) # 30 columns
    expect_equal(nrow(iS$everyCombination), 5) # 5 rows
    expect_equal(iS$biggestSummandEqualsDifferentItems, c(21, 23, 28, 29, 30))
    expect_equal(iS$smallestSummandEqualsNumberMaxima, c(24:30))
    expect_equal(iS$intersection, c(28, 29, 30))
  }
)

test_that(
  "enumerateCombinations() specially for combination 5,4,1,1",
  {
    # create first test shopping cart with the combination 5,4,1,1
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
      books[3, ],
      books[4, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 6) # 6 possibilties in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 1], c(4,4,1,1,1)) # 1st possibilty
    expect_equal(tmpResult[, 2], c(4,3,2,1,1)) # 2nd possibilty
    expect_equal(tmpResult[, 3], c(3,3,3,1,1)) # 3rd possibilty
    expect_equal(tmpResult[, 4], c(4,2,2,2,1)) # 4th possibilty
    expect_equal(tmpResult[, 5], c(3,3,2,2,1)) # 5th possibilty
    expect_equal(tmpResult[, 6], c(3,2,2,2,2)) # 6th possibilty
  }
)

test_that(
  "enumerateCombinations() specially for combination 5,4,1,1,1",
  {
    # create first test shopping cart with the combination 5,4,1,1,1
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
      books[3, ],
      books[4, ],
      books[5, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 9) # 9 possibilties in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 5], c(5,2,2,2,1)) # 1st possibilty
    expect_equal(tmpResult[, 6], c(4,3,2,2,1)) # 2nd possibilty
    expect_equal(tmpResult[, 7], c(3,3,3,2,1)) # 3rd possibilty
    expect_equal(tmpResult[, 8], c(4,2,2,2,2)) # 4th possibilty
    expect_equal(tmpResult[, 9], c(3,3,2,2,2)) # 5th possibilty
  }
)

test_that(
  "enumerateCombinations() specially for combination 5,4,2",
  {
    # create first test shopping cart with the combination 5,4,2
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
      books[3, ],
      books[3, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 3) # 3 possibilities in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 2], c(3,3,2,2,1)) # 1st possibilty
    expect_equal(tmpResult[, 3], c(3,2,2,2,2)) # 2nd possibilty
  }
)

test_that(
  "enumerateCombinations() specially for combination 5,4,2,2",
  {
    # create first test shopping cart with the combination 5,4,2,2
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
      books[3, ],
      books[3, ],
      books[4, ],
      books[4, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 6) # 6 possibilities in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 2], c(4,4,2,2,1)) # 1st possibilty
    expect_equal(tmpResult[, 3], c(4,3,3,2,1)) # 2nd possibilty
    expect_equal(tmpResult[, 4], c(3,3,3,3,1)) # 3rd possibilty
    expect_equal(tmpResult[, 5], c(4,3,2,2,2)) # 4th possibilty
    expect_equal(tmpResult[, 6], c(3,3,3,2,2)) # 5th possibilty
  }
)

test_that(
  "enumerateCombinations() specially for combination 5,4,2,2,2",
  {
    # create first test shopping cart with the combination 5,4,2,2,2
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
      books[3, ],
      books[3, ],
      books[4, ],
      books[4, ],
      books[5, ],
      books[5, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 12) # 12 possibilties in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 3], c(5,5,2,2,1)) # 1st possibilty
    expect_equal(tmpResult[, 4], c(5,4,3,2,1)) # 2nd possibilty
    expect_equal(tmpResult[, 5], c(4,4,4,2,1)) # 3rd possibilty
    expect_equal(tmpResult[, 6], c(5,3,3,3,1)) # 4th possibilty
    expect_equal(tmpResult[, 7], c(4,4,3,3,1)) # 5th possibilty
    expect_equal(tmpResult[, 8], c(5,4,2,2,2)) # 6th possibilty
    expect_equal(tmpResult[, 9], c(5,3,3,2,2)) # 7th possibilty
    expect_equal(tmpResult[, 10], c(4,4,3,2,2)) # 8th possibilty
    expect_equal(tmpResult[, 11], c(4,3,3,3,2)) # 9th possibilty
    expect_equal(tmpResult[, 12], c(3,3,3,3,3)) # 10th possibilty
  }
)

test_that(
  "enumerateCombinations() specially for combination 5,4,3,3,3",
  {
    # create first test shopping cart with the combination 5,4,3,3,3
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
      books[3, ],
      books[3, ],
      books[3, ],
      books[4, ],
      books[4, ],
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
    expect_equal(ncol(tmpResult), 9) # 9 possibility in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 1], c(5,5,5,2,1)) # 1st possibilty
    expect_equal(tmpResult[, 2], c(5,5,4,3,1)) # 2nd possibilty
    expect_equal(tmpResult[, 3], c(5,4,4,4,1)) # 3rd possibilty
    expect_equal(tmpResult[, 4], c(5,5,4,2,2)) # 4th possibilty
    expect_equal(tmpResult[, 5], c(5,5,3,3,2)) # 5th possibilty
    expect_equal(tmpResult[, 6], c(5,4,4,3,2)) # 6th possibilty
    expect_equal(tmpResult[, 7], c(4,4,4,4,2)) # 7th possibilty
    expect_equal(tmpResult[, 8], c(5,4,3,3,3)) # 8th possibilty
    expect_equal(tmpResult[, 9], c(4,4,4,3,3)) # 9th possibilty
  }
)


test_that(
  "enumerateCombinations() specially for combination 5,4,3,2,1",
  {
    # create first test shopping cart with the combination 5,4,3,2,1
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
      books[3, ],
      books[3, ],
      books[3, ],
      books[4, ],
      books[4, ],
      books[5, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 12) # 12 possibilities in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 4], c(5,4,3,2,1)) # 1st possibilty
    expect_equal(tmpResult[, 5], c(4,4,4,2,1)) # 2nd possibilty
    expect_equal(tmpResult[, 6], c(5,3,3,3,1)) # 3rd possibilty
    expect_equal(tmpResult[, 7], c(4,4,3,3,1)) # 4th possibilty
    expect_equal(tmpResult[, 8], c(5,4,2,2,2)) # 5th possibilty
    expect_equal(tmpResult[, 9], c(5,3,3,2,2)) # 6th possibilty
    expect_equal(tmpResult[, 10], c(4,4,3,2,2)) # 7th possibilty
    expect_equal(tmpResult[, 11], c(4,3,3,3,2)) # 8th possibilty
    expect_equal(tmpResult[, 12], c(3,3,3,3,3)) # 9th possibilty
  }
)

test_that(
  "enumerateCombinations() specially for combination 3,3,3,1",
  {
    # create first test shopping cart with the combination 3,3,3,1
    shoppingCart <- dplyr::bind_rows(
      books[2, ],
      books[2, ],
      books[2, ],
      books[1, ],
      books[1, ],
      books[1, ],
      books[3, ],
      books[3, ],
      books[3, ],
      books[4, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 1) # only 1 column in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 1], c(4,3,3))
  }
)

test_that(
  "enumerateCombinations() specially for combination 4,3,3,1",
  {
    # create first test shopping cart with the combination 4,3,3,1
    shoppingCart <- dplyr::bind_rows(
      books[2, ],
      books[2, ],
      books[2, ],
      books[2, ],
      books[1, ],
      books[1, ],
      books[1, ],
      books[3, ],
      books[3, ],
      books[3, ],
      books[5, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 4) # 4 possibilities in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 2], c(4,3,3,1)) # 1st possibilty
    expect_equal(tmpResult[, 3], c(4,3,2,2)) # 2nd possibilty
    expect_equal(tmpResult[, 4], c(3,3,3,2)) # 3rd possibilty
  }
)

test_that(
  "enumerateCombinations() specially for combination 4,4,1,1",
  {
    # create first test shopping cart with the combination 4,4,1,1
    shoppingCart <- dplyr::bind_rows(
      books[2, ],
      books[2, ],
      books[2, ],
      books[2, ],
      books[1, ],
      books[1, ],
      books[1, ],
      books[1, ],
      books[3, ],
      books[5, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 2) # 3 columns in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 1], c(4,2,2,2)) # first column
    expect_equal(tmpResult[, 2], c(3,3,2,2)) # second column
  }
)

test_that(
  "enumerateCombinations() specially for combination 4,4,3,2,1",
  {
    # create first test shopping cart with the combination 4,4,3,2,1
    shoppingCart <- dplyr::bind_rows(
      books[2, ],
      books[2, ],
      books[2, ],
      books[2, ],
      books[1, ],
      books[1, ],
      books[1, ],
      books[1, ],
      books[3, ],
      books[3, ],
      books[3, ],
      books[5, ],
      books[5, ],
      books[4, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 5) # 5 possibility in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 2], c(5,4,3,2)) # 1st possibilty
    expect_equal(tmpResult[, 3], c(4,4,4,2)) # 2nd possibilty
  }
)

test_that(
  "enumerateCombinations() specially for combination 2,2,2,1,1",
  {
    # create first test shopping cart with the combination 2,2,2,1,1
    shoppingCart <- dplyr::bind_rows(
      books[2, ],
      books[2, ],
      books[4, ],
      books[4, ],
      books[1, ],
      books[1, ],
      books[3, ],
      books[5, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 2) # 2 possibilities in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 1], c(5,3)) # 1st possibilty
    expect_equal(tmpResult[, 2], c(4,4)) # 2nd possibilty
  }
)

test_that(
  "enumerateCombinations() specially for combination 5,3,2,1",
  {
    # create first test shopping cart with the combination 5,3,2,1
    shoppingCart <- dplyr::bind_rows(
      books[2, ],
      books[2, ],
      books[2, ],
      books[2, ],
      books[2, ],
      books[4, ],
      books[4, ],
      books[4, ],
      books[1, ],
      books[1, ],
      books[5, ]
    )
    # analyse shopping cart at first
    ls <- analyseShoppingCart(shoppingCart, itemID, name)
    # enumerate combinations and test the output
    tmpResult <- enumerateCombinations(ls, intermediateSteps = FALSE)
    tmpResultT <- enumerateCombinations(ls, intermediateSteps = TRUE)

    ### start testing
    expect_equal(ncol(tmpResult), 6) # 6 possibilities in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[, 2], c(4,3,2,1,1)) # 1st possibilty
    expect_equal(tmpResult[, 3], c(3,3,3,1,1)) # 2nd possibilty
    expect_equal(tmpResult[, 4], c(4,2,2,2,1)) # 3rd possibilty
    expect_equal(tmpResult[, 5], c(3,3,2,2,1)) # 4th possibilty
    expect_equal(tmpResult[, 6], c(3,2,2,2,2)) # 5th possibilty
  }
)
