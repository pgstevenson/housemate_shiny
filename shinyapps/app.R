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

source("02-add_expense_module.R")
source("03-expense_summary_modules.R")
source("04-expense_module.R")
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
                  expensesOutput("expenses_list"),
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
  
  #### User's details ----
  user <- callModule(user_information,
                     "user",
                     api = api_host)
  
  #### groups user belongs to ----
  groups <- callModule(groups,
                       "user_groups",
                       user = user)
  
  #### members of selected group ----
  members <- reactive({
    shiny::validate(
      need(groups(), F)
    )
    df <- GET(paste0(api_host, "/v1/group_members?gid=", groups())) %>%
      content(type = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      tbl_df()
    
    li <- df$uid
    names(li) <- paste(df$first, df$last)
    
    list(df = df, li = li)
    
  })
  
  #### populate category drop-down ----
  cats <- callModule(categories,
                     "cats",
                     api = api_host)
  
  #### Expenses pane title text ----
  output$expenses_title <- renderText({
    shiny::validate(
      need(groups(), F)
    )
    paste("Expenses for", names(user()$group[groups()[[1]]])) %>% toTitleCase()
  })
  
  #### Analytics pane title text ----
  output$analytics_title <- renderText({
    shiny::validate(
      need(groups(), F)
    )
    paste("Analytics for", names(groups()), "expenses") %>% toTitleCase()
  })
  
  #### print user's name in side bar ----
  output$user_name <- renderUI({
    shiny::validate(
      need(user(), F)
    )
    tags$h1(paste("Hi", user()$given_name))
  })

  #### Data Table containing all of group's expenses (returns a dataframe object) ---
  dat_expenses <- callModule(expenses,
                             "expenses_list",
                             user = user,
                             group = groups,
                             members = members,
                             cats = cats,
                             api = api_host)
  
  #### Plot of Expenses by person and balance ----
  callModule(expenses_split,
             "expenses_summary_plot",
             dat = dat_expenses,
             members = members,
             api = api_host)
  
  #### "Add expense" modal ----
  callModule(expenseModal,
             "add_expense",
             user = user,
             group = groups,
             members = members,
             cats = cats,
             api = api_host)
  
  #### Summary of monthly expenses by month and person ----
  callModule(summary_month_person,
             "month_person",
             group = groups,
             api = api_host)
  
  #### Summary of monthly expenses by month and category ----
  callModule(summary_month_category,
             "month_category",
             group = groups,
             api = api_host)
  
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
