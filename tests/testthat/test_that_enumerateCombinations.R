test_that(
  "enumerate possible combinations based on restricted partitions;
  comb: 3,1,1,3",
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

    expect_equal(nrow(tmpResult), ls$maxNumberPerItem)
    expect_equal(ncol(tmpResult), 2) # 2 columns in this example
    # output numbers shall be as follows
    expect_equal(tmpResult[,1], c(4,2,2)) # first column
    expect_equal(tmpResult[,2], c(3,3,2)) # second column
  }
)
