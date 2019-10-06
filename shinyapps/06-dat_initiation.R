#' List of categories to populate select input(s)
#' 
#' Retturns a named list of categories
#' 
#' Gets data from /v1/categories api
#' 
#' Created by Paul Stevenos 16-Mar-2019
#' 
categories <- function(input, output, session, api) {

  dat <- reactiveValues()
  
  # validate
  shiny::validate(
    need(api, F)
  )
  
  cat <- GET(paste0(api, "/v1/categories")) %>%
    content(type = "text", encoding = "UTF-8") %>%
    fromJSON() %>%
    tbl_df()
  cats <- list(cat$category_id)[[1]]
  names(cats) <- cat$category
  cats <- c("None" = NA_integer_, cats)
  
  dat$categories <- cats
  
  return(dat)
  
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
    shiny::validate(
      need(user$group, F)
    )
    user$group 
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
    dat()[dat() == input$groups][[1]]
  })
  
  # By default, the dropdown box with available groups is hidden, if the user
  # belongs to more than 1 group then show the select input
  observe({
    if(length(dat()) > 1)
      shinyjs::show("groups_select")
  })
  
  returns <- reactiveValues()
  
  observe({
    x <- dat()
    y <- dat2()
    returns$group <- x[x == y]
  })
  
  return(returns)
  
}

#' User Information
#' get user information from email address supplied at loggin
#' 
#' uses /v1/users and /v1/groups apis
#' 
#' returns reactive
#' 
user_information <- function(input, output, session, api) {
  
  dat <- reactiveValues()
  
  # validate
  shiny::validate(
    need(Sys.getenv("SHINYPROXY_USERNAME"), F)
  )
  
  # Populate user information
  observe({
    
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
      groups <- as.list(groups_df$group_id)
      names(groups) <- groups_df$name
      
      dat$user <- list(
        uid = user_df$user_id,
        given_name = user_df$first,
        family_name = user_df$last,
        # dob = ymd(dat$birthday),
        # gender = dat$gender,
        # image_url = dat$image$url,
        email = Sys.getenv("SHINYPROXY_USERNAME"),
        group = groups
      )
      
  })
  
  return(dat)
  
}