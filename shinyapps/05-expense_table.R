# main data table listing expenses
expenses_tableOutput <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("expenses"))
}

expenses_table <- function(input, output, session, dat) {
  output$expenses <- DT::renderDataTable({
    DT::datatable(select(dat, id, date, description, initials, amount) %>%
                    rename(Date = date, Store = description, Who = initials, Amount = amount),
                  options = list(lengthMenu = c(5, 10),
                                 pageLength = 10,
                                 columnDefs = list(list(visible = FALSE, targets = c(0)))), # hide id col
                  rownames = F,
                  selection = "single",
                  width = "100%") %>%
      formatCurrency("Amount", currency = "", interval = 3, mark = ",") %>%
      formatDate(
        2, method = 'toLocaleDateString',
        params = list('en-US',  list(year = 'numeric', month = 'long', day = 'numeric'))
      ) },
    server = F)
}

# columnDefs = list(list(visible=FALSE, targets=c(0)))