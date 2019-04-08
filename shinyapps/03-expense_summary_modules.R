#' I think these would be better using the dat_expenses data already pulled from
#' api rather than re-requesting (PGS 15-Mar-2019)

#' Summary of monthly expenditures by person - table
#'
summary_month_personOutput <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("summary_month_person"))
}

summary_month_person <- function(input, output, session, group, api) {
  dat <- reactive({
    # validate
    shiny::validate(
      need(group(), F),
      need(!(group() %in% c("a")), F)
    )
    
    # get data from api
    dat <- GET(paste0(api, "/v1/months/person?gid=", group())) %>%
      content(type = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      tbl_df()

    # fill in missing values with 0 (i.e. if a person did not make any purchases in a given month)
    dat <- full_join(dat,
                     expand.grid(month = unique(dat$month), person = unique(dat$person), stringsAsFactors = F) %>%
                       as_tibble(),
                     by = c("month", "person"))
    
    # if NAs, then replace with 0
    if (sum(is.na(dat$sum)) > 0)
      dat[is.na(dat$sum),]$sum <- 0

    # wrangle data
    left_join(dat, dat %>% group_by(month) %>% summarise(tot = sum(sum, na.rm = T)),
              by = "month") %>%
      mutate(month = paste0(month, "01") %>% ymd(),
             contribution = sum - tot / length(unique(dat$person))) %>%
      select(-tot, -sum) %>%
      spread(key = person, value = contribution) %>%
      arrange(desc(month)) %>%
      mutate(month = format(month, "%B %Y")) %>% # Date Month YYYY format
      rename("Month" = "month")
    
  })
  
  output$summary_month_person <- DT::renderDataTable({
    # validate 
    shiny::validate(
      need(dat(), F)
    )
    
    # render table
    DT::datatable(dat(),
                  options = list(lengthMenu = c(5, 15, 30),
                                 pageLength = 15),
                  rownames = F,
                  selection = "none",
                  width = "100%") %>%
      formatCurrency(names(dat())[!(names(dat()) %in% "Month")], currency = "", interval = 3, mark = ",")},
    server  = F)
  
  return(dat)
  
}

#' Plots of monthly expenses by category
#' 
#' Calls from /v1/months api
#' 
#' Created by Paul Stevenson 15-Mar-2019
#' 
summary_month_categoryUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h2("Monthly Expenditure by Expense Type"),
    plotOutput(ns("summary_month_category")),
    uiOutput(ns("categories")),
    uiOutput(ns("range_slider"))
  )
  
}

summary_month_category <- function(input, output, session, group, api) {
  dat <- reactive({
    # Validate - need group to be numeric
    shiny::validate(
      need(group(), F),
      need(!(group() %in% c("a")), F)
    )
    
    # Get data from api
    dat2 <- GET(paste0(api, "/v1/months/category?gid=", group())) %>%
      content(type = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      tbl_df()

    # Wrangle
    dat2 %>%
      arrange(category, desc(month)) %>%
      mutate(date = paste0(month, "01") %>% ymd(),
             category = factor(category))

  })
  
  # Render select with valid group categories
  output$categories <- renderUI({
    selectInput(session$ns("categories"),
                label = "Expense Category",
                choices = dat()$category %>% levels(),
                selected = NULL)
  })
  
  # render slider output for plot ranges
  output$range_slider <- renderUI({
    sliderInput(session$ns("range_slider"),
                label = h3("Date Range"),
                min = min(dat()$date),
                max = max(dat()$date),
                value = c(min(dat()$date), max(dat()$date)),
                timeFormat = "%b %Y")
  })

  # make plot
  output$summary_month_category <- renderPlot({
    # validate, needs data and inputs for category select and range slider
    shiny::validate(
      need(dat(), F),
      need(input$categories, F),
      need(input$range_slider, F)
    )
    
    # Create ggplot2 object
    dat() %>%
      filter(category %in% input$categories,
             date >= ymd(input$range_slider[1]) & date <= ymd(input$range_slider[2])) %>% # uses slider to filter data by date
      ggplot(aes(x = date, y = sum)) +
      geom_line(size = 1) +
      ylim(0, max(dat()[dat()$category == input$categories,]$sum)) + # plots from $0 to max in selection
      labs(x = "Month", y = "Month Total") +
      theme_classic() +
      theme(text = element_text(size = 20), axis.text.x = element_text(angle = 60, hjust = 1))
  })

  return(dat)
  
}

