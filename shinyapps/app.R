## app.R ##

#### app libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(lubridate)
library(shinydashboard)
library(shinyjs)
library(jsonlite) # need to see if I can use googleAuthR to do my own APIs
library(httr)
library(tools)

#### helper functions ----

source("02-expense_add.R")
source("03-expense_summary.R")
source("04-expense_edit.R")
source("05-expense_table.R")
source("06-dat_initiation.R")
source("99-helper.R")

#### env variables for dev ----

Sys.setenv(SHINYPROXY_USERNAME = "pstevenson6@gmail.com")
# Sys.setenv(SHINYPROXY_USERNAME = "jessica.pandohee@gmail.com")
# Sys.setenv(SHINYPROXY_USERNAME = "abc10@def.com")

#### APP UI ----

ui <- dashboardPage(
  dashboardHeader(
    title = "Housemate",
    tags$li(class = "dropdown",
            tags$li(class = "dropdown", actionLink("logout", "Sign Out", onclick = "location.href='https://housemate.pgstevenson.com/logout';")))
  ),
  dashboardSidebar(
    useShinyjs(),
    # tags$script(selectize_js),
    sidebarMenu(
      
      menuItem("Expenses", tabName = "expenses", icon = icon("th")),
      menuItem("Analytics", tabName = "analytics", icon = icon("dashboard")),
      
      div(style = "margin: 0 0 0 10px;", htmlOutput("user_name")),
      
      groupsUI("user_groups")
      
    )
  ),
  ## Body content
  dashboardBody(
    tags$head(
      tags$style(HTML("
                      .fa-edit {
                      color: black;
                      }
                      .icon-light {
                      color: white;
                      }
                      .pad {
                      padding: 0 5% 0 5%;
                      }
                      .centerAlign {
                      display: inline-block;
                      width: 100%;
                      text-align: center;
                      }
                      .rightAlign {
                      display: inline-block;
                      width: 100%;
                      text-align: right;
                      }"))
    ),
    tabItems(
      tabItem(tabName = "expenses",
              titlePanel(textOutput("expenses_title")),
              fluidRow(
                box(
                  expenses_tableOutput("expenses_list"),
                  new_item_idOuput("add_expense"),
                  width = 7),
                box(
                  expenses_splitOutput("expenses_summary_plot"),
                  width = 5),
                box(
                  introOutput("expenses_list"),
                  itemOutput("expenses_list"),
                  itemEditUI("expenses_list"),
                  width = 5)
              ),
              fluidRow(
                column(1, offset = 9,
                       div(style = "position: fixed; bottom: 5%;",
                           expenseModalUI("add_expense"),
                           align = "right")
                )
              )
      ),
      tabItem(tabName = "analytics",
              titlePanel(textOutput("analytics_title")),
              fluidRow(
                box(
                  summary_month_personOutput("month_person"),
                  width = 5
                ),
                box(
                  summary_month_categoryUI("month_category"),
                  width = 5
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {

  #### API address ----
  api_host <- "https://housemate.pgstevenson.com/api"
  
  #### Init reactive values ----
  dat <- reactiveValues()
  
  # User's details
  user_information_returned <- callModule(user_information,
                                          "user",
                                          api = api_host)
  observe({
    shiny::validate(need(user_information_returned, F))
    dat$user <- user_information_returned$user
  })
  
  # Groups user belongs to
  groups_returned <- callModule(groups,
                           "user_groups",
                           user = dat$user)
  
  # Expense category drop-down
  categories_returned <- callModule(categories,
                     "cats",
                     api = api_host)
  
  observe({
    shiny::validate(need(groups_returned, F),
                    need(categories_returned, F))
    dat$group <- groups_returned$group
    dat$categories <- categories_returned$categories
  })
  
  # members of selected group
  observe({
    shiny::validate(need(dat$group, F))
    
    df <- GET(paste0(api_host, "/v1/group_members?gid=", dat$group)) %>%
      content(type = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      as_tibble()
    
    li <- df$uid
    names(li) <- paste(df$first, df$last)
    
    dat$members <- list(df = df, li = li)
  })
  
  # Group's expenses
  observe({
    shiny::validate(
      need(dat$group, F),
      need(dat$members, F)
    )
    
    dat_expenses <- GET(paste0(api_host, "/v1/expenses?gid=", dat$group)) %>%
      content(type = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      as_tibble() %>%
      dat_refactor(members = dat$members)
    
    dat$expenses <- dat_expenses
    
  })
  
  #### Expenses pane title text ----
  output$expenses_title <- renderText({
    shiny::validate(
      need(dat$user, F),
      need(dat$group, F)
    )
    paste("Expenses for", names(dat$user$group[dat$group[[1]]])) %>% toTitleCase()
  })
  
  #### Analytics pane title text ----
  output$analytics_title <- renderText({
    shiny::validate(
      need(dat$group, F)
    )
    paste("Analytics for", names(dat$group), "expenses") %>% toTitleCase()
  })
  
  #### print user's name in side bar ----
  output$user_name <- renderUI({
    shiny::validate(
      need(dat$user, F)
    )
    tags$h1(paste("Hi", dat$user$given_name))
  })
  
  #### render data table ----
  observe({
    shiny::validate(need(dat$expenses, F))
    callModule(expenses_table, "expenses_list", dat = dat$expenses)
  })
  
  #### Edit user's expenses
  callModule(expenses,
             "expenses_list",
             dat = dat,
             api = api_host)
 
  #### "Add expense" modal ----
  dat_new_item <- callModule(expenseModal,
                                      "add_expense",
                                      user = dat$user,
                                      group = dat$group,
                                      members = dat$members,
                                      cats = dat$categories,
                                      api = api_host)
  
  # #### master reactive for expenses table ----
  observeEvent(dat_new_item(), {
    dat$expenses <- bind_rows(
      dat$expenses %>% select(-who, -initials, -description),
      dat_new_item() %>% mutate(date = ymd(date))
    ) %>%
      dat_refactor(members = dat$members)
  })
  
  #### Plot of Expenses by person and balance ----
  observe({
    shiny::validate(need(dat$expenses, F),
                    need(dat$members, F))
    
    callModule(expenses_split,
               "expenses_summary_plot",
               dat = dat$expenses,
               members = dat$members,
               api = api_host)
  })
  
  #### Summary of monthly expenses by month and person ----
  observe({
    shiny::validate(need(dat$group, F),
                    need(!(dat$group %in% c("a")), F))
    
    callModule(summary_month_person,
               "month_person",
               group = dat$group,
               api = api_host)
  })
  
  #### Summary of monthly expenses by month and category ----
  observe({
    shiny::validate(need(dat$group, F),
                    need(!(dat$group %in% c("a")), F))
    
    callModule(summary_month_category,
               "month_category",
               group = dat$group,
               api = api_host)
  })
  
  #### add store window (placeholder, doesn't do anyhting PGS 15-Mar-19) ----
  storeModal <- function() {
    modalDialog(
      footer = tagList(
        modalButton("Cancel"),
        actionButton("add_store_ok", "OK")
      )
    )}
  
}

shinyApp(ui, server)
