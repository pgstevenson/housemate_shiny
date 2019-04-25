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
    labelField = "out",
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

  output$summary_plot <- renderPlot({
    members$df %>%
      left_join(
        # Summary of total individual expenses paid
        dat %>%
          group_by(uid) %>%
          filter(cid != "12") %>%
          summarise(individual_total_expense = sum(amount, na.rm = T)),
        by = "uid") %>%
      left_join(
        # Summary of who has recieved a repayment
        dat %>%
          filter(cid == "12") %>%
          group_by(sid) %>%
          summarise(received = sum(amount, na.rm = T)),
        by = c("uid" = "sid")) %>%
      left_join(
        # Summary of who has made repayments
        dat %>%
          filter(cid == "12") %>%
          group_by(uid) %>%
          summarise(repaid = sum(amount, na.rm = T)),
        by = "uid") %>%
      mutate(group_total = dat %>% # total amount spent by group
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

#' Refactor expenses data
#' 
#' input dat tibble
#' return dat tibble
#' 
dat_refactor <- function(dat, members) {
  
  # replace store names with person paid when categroy = 12: Repayment
  stores_refactored <- apply(dat, 1, function(x) {
    if (is.na(x["cid"]) | x["cid"] != "12") return(x["store"])
    strsplit(names(members$li)[members$li == as.numeric(x["sid"])], " ")[[1]][1]
  }) %>%
    unlist() %>%
    unname()
  
  # refactor person name, take from group members list
  who_refactored <- sapply(dat$uid, function(x) strsplit(names(members$li)[members$li == as.numeric(x)], " ")[[1]][1])
  
  who <- sapply(dat$uid, function(x) names(members$li[members$li == x]))
  initials <- sapply(who, function(x) {
    sapply(strsplit(x, " "), function(y) paste0(substr(y, 1, 1), collapse = ""))
  })
  
  dat %>%
    mutate(date = ymd(date),
           who = who,
           initials = initials,
           store = stores_refactored,
           description = ifelse(cid == "12", # T: repayment, # F: expense
                                paste("Repayment:", who, "to", store),
                                paste(category, ifelse(is.na(store), "", paste("at", store))))
    ) %>%
    arrange(desc(date))
}