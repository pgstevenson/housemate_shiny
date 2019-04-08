#' Shiny modules for housemate app
#' Created by Paul Stevenson 2019-03-10
#' 

#' Access token
#' populates with google oauth2 access token
#' returns reactive
#' 
user_token <- function(input, output, session) {
  # validate
  shiny::validate(
    need(Sys.getenv("SHINYPROXY_OIDC_ACCESS_TOKEN"), F)
  )
  
  # access token
  token <- reactive({ 
    list(Authentication =
           list(public_fields =
                  list(
                    token = list(cache_path = Sys.getenv()["SHINYPROXY_OIDC_ACCESS_TOKEN"],
                                 params = list(scope = options("googleAuthR.scopes.selected")),
                                 app = list(key = options("googleAuthR.client_id"),
                                            secret = options("googleAuthR.webapp.client_secret")))
                    )))
  })
  
  return(token)
  
}

#' User Information
#' get user information from email address supplied at loggin
#' 
#' uses /v1/users and /v1/groups apis
#' 
#' returns reactive
#' 
user_information <- function(input, output, session, api) {
  
  # validate
  shiny::validate(
    need(Sys.getenv("SHINYPROXY_USERNAME"), F)
  )
  
  # get user information from Google with token
  # dat <- GET(paste0("https://www.googleapis.com/plus/v1/people/me?access_token=", Sys.getenv()["SHINYPROXY_OIDC_ACCESS_TOKEN"])) %>%
  #   content(type = "text", encoding = "UTF-8") %>%
  #   fromJSON()
  
  # Populate user information
  user <- reactive({
    
    # get user's information
    user_df <- GET(paste0(api, "/v1/users?email=", Sys.getenv("SHINYPROXY_USERNAME"))) %>%
      content(type = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      tbl_df()
    
    # new user
    if ("code" %in% names(user_df))
      if (user_df$code == 204)
        user_df <- GET(paste0(api_host, "/v1/users_new?email=", Sys.getenv("SHINYPROXY_USERNAME"))) %>%
          content(type = "text", encoding = "UTF-8") %>%
          fromJSON() %>%
          tbl_df()
    
    # get user's groups
    groups_df <- GET(paste0(api, "/v1/groups?email=", Sys.getenv("SHINYPROXY_USERNAME"))) %>%
      content(type = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      tbl_df()
    
    # new group
    if ("code" %in% names(groups_df))
      if (groups_df$code == 204) {
        groups_df <- GET(paste0(api_host, "/v1/group_new")) %>%
          content(type = "text", encoding = "UTF-8") %>%
          fromJSON() %>%
          tbl_df()
        # link user to new group as admin
        GET(paste0(api, "/v1/expenses/new_user_group?uid=", user_df$uid, "&gid=", groups_df$gid))
        # place first item in expenses
        GET(paste0(api, "/v1/expenses/new?uid=", user_df$uid, "&gid=", groups_df$gid, "&date=", gsub("-", "", Sys.Date()), "&cid=9&amount=0&notes=First%20expense"))
      }

    # resturcture groups as a named list
    groups <- as.list(groups_df$gid)
    names(groups) <- groups_df$name
    
    list(
      uid = user_df$uid,
      given_name = user_df$first,
      family_name = user_df$last,
      # dob = ymd(dat$birthday),
      # gender = dat$gender,
      # image_url = dat$image$url,
      email = Sys.getenv("SHINYPROXY_USERNAME"),
      group = groups
    )
  })
  
  return(user)
  
}

#' List of categories to populate select input(s)
#' 
#' Retturns a named list of categories
#' 
#' Gets data from /v1/categories api
#' 
#' Created by Paul Stevenos 16-Mar-2019
#' 
categories <- function(input, output, session, api) {
  # validate
  shiny::validate(
    need(api, F)
  )
  
  cat <- GET(paste0(api, "/v1/categories")) %>%
    content(type = "text", encoding = "UTF-8") %>%
    fromJSON() %>%
    tbl_df()
  cats <- list(cat$cid)[[1]]
  names(cats) <- cat$category
  cats <- c("None" = NA_integer_, cats)
  
  return(cats)
  
}

#' Populate a Select Input with the groups to which the user belongs
#' returns a reactive with the group number as a named vecotr (integer)
#'  
#' Created by Paul Stevenson 15-Mar-2019
#' 
groupsUI <- function(id) {
  ns <- NS(id)
  shinyjs::hidden(
    tags$div(
      id = ns("groups_select"),
      uiOutput(ns("groups"))
    )
  )
}

groups <- function(input, output, session, user) {
  ns <- session$ns
  
  dat <- reactive({
    # Validate
    shiny::validate(
      need(user(), F)
    )
    
    # Get groups that the user belongs to
    user()$group
  })
  
  # render select input
  output$groups <- renderUI({
    selectInput(ns("groups"),
                label = "Your Groups:",
                choices = dat(),
                selected = NULL)
  })
  
  # update the returned value with the input's selected value
  dat2 <- eventReactive(input$groups, {
    dat()[dat() == input$groups]
  })
  
  # By default, the dropdown box with available groups is hidden, if the user
  # belongs to more than 1 group then show the select input
  observe({
    if(length(dat()) > 1)
      shinyjs::show("groups_select")
  })
  
  return(reactive({ ifelse(length(dat()) > 1, dat2(), dat()) }))
  
}

#' Defines the stores selectize input with UI and server functions
#' 
#' Gets data from /v1/stores api
#' 
#' Returns selected store id as character
#' 
#' Created by Paul Stevenson 16-Mar-19
#' 
storesUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("stores"))
}

stores <- function(input, output, session, api) {
  ns <- session$ns
  
  # validate
  shiny::validate(
    need(api, F)
  )
  
  output$stores <- renderUI({
    selectizeInput(ns("stores"), label = "Store", choices = NULL, options = list(
    valueField = "id",
    labelField = "name",
    searchField = c("name", "city", "entity"),
    options = list(),
    create = FALSE,
    maxOptions = 10,
    render = I(
      "{
      option: function(item, escape) {
      return '<div><strong>' + escape(item.name) + '</strong> - <em>' + escape(item.city) + '</em><br />[' + escape(item.entity) + ']</div>';
      }
}"
                     ),
    load = I(paste0(
      "function(query, callback) {
      if (!query.length) return callback();
      $.ajax({
      url: '", api, "/v1/stores?words=' + encodeURIComponent(query),
      type: 'GET',
      dataType: 'json',
      error: function() {
      callback()
      },
      success: function(res) {
      callback(res)
      }
      })
      }")
                     )
    ))
  })
  
  dat <- eventReactive(input$stores, {
    input$stores
  })
  
  return(dat)
  
}


#' Shiny module illustrating the distribution of costs after all expenses/
#' repayments considered
#' 
#' Needs dat_expenses, members
#' 
expenses_splitOutput <- function(id) {
  ns <- NS(id)
  plotOutput(ns("summary_plot"), height = "300px")
  
}

expenses_split <- function(input, output, session, dat, members, api) {
  
  reactive({
    shiny::validate(
      need(dat(), F),
      need(members(), F)
    )
  })
  
  output$summary_plot <- renderPlot({
    members()$df %>%
      left_join(
        # Summary of total individual expenses paid
        dat() %>%
          group_by(uid) %>%
          filter(cid != "12") %>%
          summarise(individual_total_expense = sum(amount, na.rm = T)),
        by = "uid") %>%
      left_join(
        # Summary of who has recieved a repayment
        dat() %>%
          filter(cid == "12") %>%
          group_by(sid) %>%
          summarise(received = sum(amount, na.rm = T)),
        by = c("uid" = "sid")) %>%
      left_join(
        # Summary of who has made repayments
        dat() %>%
          filter(cid == "12") %>%
          group_by(uid) %>%
          summarise(repaid = sum(amount, na.rm = T)),
        by = "uid") %>%
      mutate(group_total = dat() %>% # total amount spent by group
               filter(cid != "12") %>%
               summarise(total = sum(amount, na.rm = T)) %>%
               .$total,
             group_size = nrow(.), # number of people in the group
             individual_owing = group_total/group_size) %>% # split evenly, how much each person owns
      group_by(uid, first, last) %>%
      summarise(balance = sum(individual_total_expense, na.rm = T) -
                  individual_owing - 
                  sum(received, na.rm = T) +
                  sum(repaid, na.rm = T)) %>% # Paid - owing - recieved + repaid
      mutate(name = paste(first, last),
             positive = ifelse(balance >=  0, T, F)) %>%
      ggplot(aes(x = factor(name), y = balance, fill = positive)) +
      geom_col() +
      geom_text(aes(label = paste0(name, "\n$", round(balance, digits = 2)), vjust = ifelse(positive, -0.5, 1.5)), size = 5) +
      geom_hline(aes(yintercept = 0)) +
      labs(x = "", y = "Balance ($)") +
      scale_y_continuous(expand = c(.4, .4)) +
      scale_fill_manual(values=c("#ac1f1f", "#2c8740")) +
      theme_classic(base_size = 20) +
      theme(legend.position="none",
            axis.text.x=element_blank(),
            axis.ticks.x = element_blank())
  })
  
}
