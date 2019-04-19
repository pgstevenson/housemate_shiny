#' Modal to add new expense
#' 
expenseModalUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("expenses_add"), "", icon = icon("plus-circle", "fa-5x", lib = "font-awesome"))
}

expenseModal <- function(input, output, session, user, group, members, cats, api) {
  ns <- session$ns
  
  myModal <- function() {
    modalDialog(
      fluidRow(
        column(12,
               dateInput(ns("item_date"), "Date", format = "dd MM yyyy")
        )),
      fluidRow(
        column(12,
               numericInput(ns("item_amount"), "Amount", value = "")
        )),
      fluidRow(
        column(12,
               selectInput(ns("item_category"), label = "Expense type", choices = cats)
        )),
      fluidRow(
        column(12,
               tags$div(id = ns("add_stores"),
                       storesUI(ns("add_stores"))),
               shinyjs::hidden(tags$div(id = ns("add_person"),
                       selectInput(ns("add_person"), label = "To",
                                   choices = members()$li[members()$li != user()$uid]))) # exclude current user from list
        )),
      fluidRow(
        column(12,
                textInput(ns("item_notes"), "Notes", value = ""))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("add_ok"), "OK")
      )
    )
  }
  
  # Stores selectize input
  store <- callModule(stores,
             "add_stores",
             api = api)
  
  # open modal on button click
  observeEvent(input$expenses_add,
               showModal(myModal())
  )
  
  # write output to db
  observeEvent(input$add_ok, {
    if (isTruthy(input$item_date))
      shinyjs::runjs("document.getElementById('item_date').style.border = ''") else
        shinyjs::runjs("document.getElementById('item_date').style.border = '1px solid red'")
    if (input$item_amount != "" && !is.na(suppressWarnings(as.numeric(input$item_amount))))
      shinyjs::runjs("document.getElementById('item_amount').style.border = ''") else
        shinyjs::runjs("document.getElementById('item_amount').style.border = '1px solid red'")

    req(input$item_date, input$item_amount, !is.na(suppressWarnings(as.numeric(input$item_amount))))

    # write expense to database
    ## prepare query
    d <- paste0(api,
                "/v1/expenses/new?",
                "uid=", user()$uid,
                "&gid=", group(),
                "&date=", gsub("-", "", input$item_date),
                "&amount=", round(input$item_amount, digits = 2),
                ifelse(input$item_category == "" || input$item_category == "NA", "", paste0("&cid=", input$item_category)),
                ifelse(input$item_category == "12",
                       paste0("&store=", input$add_person), # repayment - add user id in store field
                       ifelse(store() == "", "", paste0("&store=", store()))),
                ifelse(input$item_notes == "", "", paste0("&notes=", URLencode(input$item_notes, reserved = T) )),
                "&mod_user=", user()$uid)
    
    ## run query
    GET(d) %>% content(type = "text", encoding = "UTF-8") %>% fromJSON() %>% tbl_df()

    removeModal()
    
  })
  
  # if category is repayment remove stores slectize and add group members to modal
  observeEvent(input$item_category, {
    shiny::validate(
      need(input$item_category, F)
    )
    if (input$item_category == "12") { # repayment category
      shinyjs::hide("add_stores")
      shinyjs::show("add_person")
    } else {
      shinyjs::show("add_stores")
      shinyjs::hide("add_person")
    }
  })
  
  # Clear cateogry drop down when clicked
  shinyjs::onclick("item_category", {
    updateSelectInput(session, "item_category", selected = "")
  })
  
}