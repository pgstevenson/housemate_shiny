#' Data table output and server logic of the selected group's expenses.
#' 
#' Get's data from the /v1/epenses api
#' 
#' Provides output for the main expenses table and for an items detail which reacts
#' to the selected data table row (and is hidden when nothing is selected).
#' 
#' The expenses modlue returns data frame with group's data.
#' 
#' Created by Paul Stevenson 15-Mar-2019
#' 

# main data table listing expenses
expensesOutput <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("expenses"))
}

# Introduction text/instructions spash screen
introOutput <- function(id) {
  ns <- NS(id)
  tags$div(
    id = ns("intro_screen"),
    tags$h1("Housemate Household Expenses App"),
    tags$p("Keep track of shared expenses by splitting evenly for each person in the group"),
    tags$p("1. To add an expense the the list press the + button."),
    tags$p("2. To make a repayment choose 'Repayments' from the 'Expense Type' dropdown box."),
    tags$p("3. Start typing where the purchase was made in the 'Store' box, if it is not there press the '+' button next to the box to make a new entry."),
    tags$p("4. Click on the expense to see more detail.")
  )
}

# div containing an expense item's detail
# also has a button to edit the expense (currently not functional PGS 15-Mar-19)
itemOutput <- function(id) {
  ns <- NS(id)
  shinyjs::hidden(
    tags$div(
      id = ns("item_div"),
      tags$h3(textOutput(ns("item_category_store"))),
      tags$hr(),
      tags$h1(textOutput(ns("item_amount"))),
      tags$p(textOutput(ns("item_who_date"))),
      tags$p(textOutput(ns("item_notes"))),
      tags$div(class = "rightAlign pad", actionLink(ns("expenses_edit"), "", icon = icon("edit", "fa-3x", lib = "font-awesome")))
    )
  )
}

# edit the selected item
itemEditUI <- function(id) {
  ns <- NS(id)
  shinyjs::hidden(
    tags$div(
      id = ns("edit_div"),
      tags$h2("Edit Expense Entry"),
      uiOutput(ns("edit_uid")),
      uiOutput(ns("edit_date")),
      uiOutput(ns("edit_amount")),
      tags$div(
        id = ns("edit_category"),
        uiOutput(ns("edit_category"))),
      tags$div(
        id = ns("edit_stores"),
        storesUI(ns("edit_stores")),
        htmlOutput(ns("display_store"))),
      shinyjs::hidden(tags$div(
        id = ns("edit_person"),
        uiOutput(ns("edit_person")))),
      uiOutput(ns("edit_notes")),
      actionButton(ns("edit_delete"), "Delete", class="btn btn-danger"),
      actionButton(ns("edit_cancel"), "Cancel", class="btn"),
      actionButton(ns("edit_ok"), "OK", class="btn btn-primary")
  ))
}

# Main logic for expenses
expenses <- function(input, output, session, user, group, members, cats, api) { #### new user variable, add to call.
  ns <- session$ns
  
  dat <- reactive({
    # Validate - needs a group number
    shiny::validate(
      need(group(), F),
      need(!(group() %in% c("a")), F)
    )
    
    # Get data from api and wrangle
    dat <- GET(paste0(api, "/v1/expenses?gid=", group())) %>%
      content(type = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      tbl_df() %>%
      mutate(date = date %>% ymd()) %>%
      arrange(desc(date))

    # replace store names with person paid when categroy = 12: Repayment
    stores_refactored <- apply(dat, 1, function(x) {
      if (is.na(x["cid"]) | x["cid"] != "12") return(x["store"])
      strsplit(names(members()$li)[members()$li == as.numeric(x["sid"])], " ")[[1]][1]
    }) %>%
      unlist() %>%
      unname()
    
    # refactor person name, take from group members list
    who_refactored <- sapply(dat$uid, function(x) strsplit(names(members()$li)[members()$li == as.numeric(x)], " ")[[1]][1])

    who <- sapply(dat$uid, function(x) names(members()$li[members()$li == x]))
    initials <- sapply(who, function(x) {
      sapply(strsplit(x, " "), function(y) paste0(substr(y, 1, 1), collapse = ""))
    })

    dat %>%
      mutate(who = who,
             initials = initials,
             store = stores_refactored,
             description = ifelse(cid == "12", # T: repayment, # F: expense
                                  paste("Repayment:", who, "to", store),
                                  paste(category, ifelse(is.na(store), "", paste("at", store))))
      )
    
  })

  # Stores selectize input
  store <- callModule(stores,
             "edit_stores",
             api = api)
  
  # Render edit who select input
  #### FUTURE FEATURE only group admin users can see/change this field. ----
  output$edit_uid <- renderUI({
    selectInput(ns("edit_uid"),
                label = "Paid by",
                choices = members()$li,
                selected = dat_selected()$uid)
  })
  
  # Render edit amount numeric input
  output$edit_amount <- renderUI({
    numericInput(ns("edit_amount"), "Amount", value = dat_selected()$amount)
  })
  
  # Render edit date input
  output$edit_date <- renderUI({
    dateInput(ns("edit_date"), "Date", value = dat_selected()$date, format = "dd MM yyyy")
  })
  
  # Render edit cateogory select input
  output$edit_category <- renderUI({
    selectInput(ns("edit_category"),
                label = "Expense type",
                choices = cats[cats != "12"], # remove "Repayment" option from dropdown
                selected = dat_selected()$cid
                )
  })
  
  # Render display current store (would rather populate selectize, but don't think I can PGS 17-Mar-2019)
  output$display_store <- renderText({
    shiny::validate(
      need(dat_selected()$sid, F),
      need(dat_selected()$cid != "12", F) # don't pre-populate if original category is repayment
    )
    dat_store <- GET(paste0(api, "/v1/stores?id=", dat_selected()$sid)) %>%
      content(type = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      tbl_df()
    paste0("<strong>", dat_store$name, "</strong> - <em>", dat_store$city, "</em> [", dat_store$entity, "]")
  })
  
  # Render group members for repayment
  output$edit_person <- renderUI({
    selectInput(ns("edit_person"),
                label = "To",
                choices = members()$li,
                selected = dat_selected()$sid)
  })
  
  # Render edit notes
  output$edit_notes <- renderUI({
    textInput(ns("edit_notes"), "Notes", value = dat_selected()$notes)
  })
  
  # Output expenses to data table
  output$expenses <- DT::renderDataTable({
    
    DT::datatable(select(dat(), date, description, initials, amount) %>%
                    rename(Date = date, Store = description, Who = initials, Amount = amount),
                  options = list(lengthMenu = c(5, 10),
                                 pageLength = 10),
                  rownames = F,
                  selection = "single",
                  width = "100%") %>%
      formatCurrency("Amount", currency = "", interval = 3, mark = ",") %>%
      formatDate(
        1, method = 'toLocaleDateString',
        params = list('en-US',  list(year = 'numeric', month = 'long', day = 'numeric'))
      ) },
    server = F)
  
  # Extract data from selected row
  dat_selected <- eventReactive(input$expenses_rows_selected, {
    dat()[input$expenses_rows_selected,]
  })
  
  # Show item detail when a row is selected, hide when deselected
  observe({
    shinyjs::hide("edit_div")
    if (is.null(input$expenses_rows_selected)) {
      shinyjs::hide("item_div")
      shinyjs::show("intro_screen")
    } else {
      d <- dat_selected()
      output$item_category_store <- renderText({ ifelse(d$cid == "12",
                                                        paste(d$category, "to", names(members()$li)[members()$li == d$sid]), # repayment category
                                                        paste(d$category, ifelse(is.na(d$store), "", paste("at", d$store)))) })
      output$item_amount <- renderText({ paste0("$", format(round(d$amount, 2), nsmall = 2, big.mark=",")) })
      output$item_who_date <- renderText({ paste("by", names(members()$li)[members()$li == d$uid], "on", format(as.Date(d$date), "%d %B %Y")) }) # DD Month YYYY format
      output$item_notes <- renderText({ if (!is.na(d$notes)) d$notes else "" })
      shinyjs::hide("intro_screen")
      shinyjs::show("item_div")
    }
  })
  
  # Show edit expense item div
  observeEvent(input$expenses_edit, {
    repayment_flag <- ifelse(!is.na(dat_selected()$cid),
                             ifelse(dat_selected()$cid == "12", T, F),
                             F)
    shinyjs::show("edit_div")
    shinyjs::hide("item_div")
    if (repayment_flag) { # repayment category
      shinyjs::hide("edit_stores")
      shinyjs::show("edit_person")
      shinyjs::hide("edit_category")
    } else {
      shinyjs::show("edit_stores")
      shinyjs::hide("edit_person")
      shinyjs::show("edit_category")
    }
  })
  
  # write item edits to db (1. remove entry, 2. write new entry)
  observeEvent(input$edit_ok, {
    if (isTruthy(input$edit_date))
      shinyjs::runjs("document.getElementById('edit_date').style.border = ''") else
        shinyjs::runjs("document.getElementById('edit_date').style.border = '1px solid red'")
    if (input$edit_amount != "" && !is.na(suppressWarnings(as.numeric(input$edit_amount))))
      shinyjs::runjs("document.getElementById('edit_amount').style.border = ''") else
        shinyjs::runjs("document.getElementById('edit_amount').style.border = '1px solid red'")

    req(input$edit_uid, input$edit_date, input$edit_amount, !is.na(suppressWarnings(as.numeric(input$edit_amount))))
    
    repayment_flag <- ifelse(!is.na(dat_selected()$cid),
                             ifelse(dat_selected()$cid == "12", T, F),
                             F)
    
    # remove initial entry
    GET(paste0(api, "/v1/expenses/remove?rid=", dat_selected()$id)) %>%
      content(type = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      tbl_df()
    
    # write  to database
    ## prepare querys
    d <- paste0(api,
                "/v1/expenses/new?",
                "uid=", input$edit_uid,
                "&gid=", group(),
                "&date=", gsub("-", "", input$edit_date),
                "&amount=", round(input$edit_amount, digits = 2),
                ifelse(repayment_flag,
                       "&cid=12",
                       ifelse(input$edit_category == "" || input$edit_category == "NA", "", paste0("&cid=", input$edit_category))),
                ifelse(repayment_flag,
                       paste0("&store=", input$edit_person), # edit repayment - store field contains uid
                       ifelse(store() == "", 
                              ifelse(is.na(dat_selected()$sid), "", paste0("&store=", dat_selected()$sid)),
                              paste0("&store=", store())
                       )
                ),
                ifelse(input$edit_notes == "", "", paste0("&notes=", URLencode(input$item_notes, reserved = T) )),
                       paste0("&root_id=", ifelse(is.na(dat_selected()$root_id), dat_selected()$id, dat_selected()$root_id)),
                       "&mod_user=", user()$uid)
    
    ## run query
    GET(d) %>% content(type = "text", encoding = "UTF-8") %>% fromJSON() %>% tbl_df()
    
    shinyjs::hide("edit_div")
    shinyjs::show("intro_screen")
    
  })
  
  # Code edit cancel button
  observeEvent(input$edit_cancel, {
    shinyjs::hide("edit_div")
    shinyjs::show("intro_screen")
  })
  
  # Code edit remove button
  observeEvent(input$edit_delete, {
    GET(paste0(api, "/v1/expenses/remove?rid=", dat_selected()$id)) %>%
      content(type = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      tbl_df()
    
    shinyjs::hide("edit_div")
    shinyjs::show("intro_screen")
    
  })
  
  return(dat)
  
}
