library(shiny)
library(dplyr)
library(tidyr)
library(partitions)

# These books are available in the shop
books <<- dplyr::tibble(
  itemID = 1:5,
  name = c(
    "Der Stein der Weisen",
    "Die Kammer des Schreckens",
    "Der Gefangene von Askaban",
    "Der Feuerkelch",
    "Der Orden des Phönix"
  )
)

discountInfos <<- dplyr::tibble(
  set = 1:5,
  discount = c(0, 5, 10, 20, 25)
)

pricePerItem <<- 8

ui <- fluidPage(
  br(),
  fluidRow(
    column(
      2, offset = 1, img(src = "derSteinDerWeisen_skaliert.jpg")
    ),
    column(
      2, img(src = "dieKammerDesSchreckens.jpeg")
    ),
    column(
      2, img(src = "derGefangeneVonAskaban_skaliert.jpg")
    ),
    column(
      2, img(src = "derFeuerkelch.jpeg")
    ),
    column(
      2, img(src = "derOrdenDesPhönix.jpeg")
    )
  ),
  fluidRow(
    column(
      2, offset = 1,
      numericInput(
        "item1", label = paste("Harry Potter und", books$name[1]),
        min = 0, max = 20, value = 0, step = 1
      )
    ),
    column(
      2,
      numericInput(
        "item2", label = paste("Harry Potter und", books$name[2]),
        min = 0, max = 20, value = 0, step = 1
      )
    ),
    column(
      2,
      sliderInput(
        "item3", label = paste("Harry Potter und", books$name[3]),
        min = 0, max = 20, value = 0, step = 1
      )
    ),
    column(
      2,
      sliderInput(
        "item4", label = paste("Harry Potter und", books$name[4]),
        min = 0, max = 20, value = 0, step = 1
      )
    ),
    column(
      2,
      sliderInput(
        "item5", label = paste("Harry Potter und", books$name[5]),
        min = 0, max = 20, value = 0, step = 1
      )
    )
  ),
  fluidRow(
    column(
      3, offset = 2,
      uiOutput("goShoppingCart")
    )
  ),
  br(), # insert empty row
  fluidRow(
    column(
      6, offset = 3,
      verbatimTextOutput("price")
    )
  )
)

server <- function(input, output, server) {

  # create action button "moveToShoppingCart"
  output$goShoppingCart <- renderUI({
    if (input$item1 != 0 ||
        input$item2 != 0 ||
        input$item3 != 0 ||
        input$item4 != 0 ||
        input$item5 != 0) {
      actionButton(
        "moveToShoppingCart", "in den Warenkorb legen", icon = icon("send")
      )
    }
  })

  # make 'moveToShoppingCart' reactive and identify the shopping cart
  identifyShoppingCart <- eventReactive(input$moveToShoppingCart, {
    shoppingCart <- books %>%
      dplyr::bind_cols(
        number = c(input$item1, input$item2, input$item3, input$item4,
                   input$item5)
      )

    bestDiscountDetailed <- calculatePrice(
      shoppingCart, discountInfos, pricePerItem, intermediateSteps = TRUE
    )

    bestDiscount <- bestDiscountDetailed$bestDiscount

    return(bestDiscount)
  })

  # send price to display
  output$price <- renderPrint({
    identifyShoppingCart()
  })
}

shinyApp(ui, server)
