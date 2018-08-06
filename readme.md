# shiny-application for harry potter books
This package contains some functions for analysing a shopping cast with five
items.
```
< Expelliarmus >
 ----------------------
    \
     \  /\/\
       \   /
       |  0 >>
       |___|
 __((_<|   |
(          |
(__________)
   |      |
   |      |
   /\     /\

```
## USAGE (status quo: 6th of August 2018)
You can test the app on my [shinyapps.io](frumentum.shinyapps.io/buypottersavemoney) account.

The structure of the functionality is quite simple: There is one function called
```
calculatePrice()
```
which is a wrapper function for the other four functions. At first
analyseShoppingCart() analyses the structure and the number of items in the
shopping cart. enumerateCombination() follows and enumerates every possible
combination of this shopping cart. This is done by calling restrictedparts()
from the partitions package. Further extractDiscountSets() filters every
discount combination which is possible. At least calculateBestDiscount()
calculates the discounts for these possibilities and chooses the cheapest.

For functions 2, 3 and 4 it exists an attribute called intermediateSteps which
is logical. If it's set to TRUE, a list is returned with all the
intermediate steps within the function.
