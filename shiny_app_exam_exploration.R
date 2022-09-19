
###*** App to read data file, provide choices of columns to search and list of 
###* search terms within column. Then filter df by search, provide summary count
###* of groups that contain that search and allow to export file

# Global- Libraries, Password Set-up, Read Data --------------------------------------------

library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(tippy) # Tooltips
library(shinyBS)
library(shinyauthr) # Password

# dataframe that holds usernames, passwords and other user data. Obscure for 
# public posting
user_base <- dplyr::tibble(
  user = c("user_name"),
  password = c("user_password"),
  permissions = c("admin"),
  name = c("User One")
)

## Read exam-level data that consists of all exams given was pulled 9/16/2022

exams <- readxl::read_xlsx("list_of_all_exams_edited.xlsx")

# UI ----------------------------------------------------------------------

 # Most UI rendered in server to use password
ui <- fluidPage(
  # This script gets browser window size, to be returned in server as
  # input$dimension[1] for width and [2] for height. Used for media rules
  tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
  titlePanel("Exam Search"),
## Password set-up
  # add logout button UI
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  # add login panel UI function
  shinyauthr::loginUI(id = "login"),
             uiOutput("master_renderUI"))

# Server ------------------------------------------------------------------


server <- function(input, output, session) {

# Password Set-up ---------------------------------------------------------

  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  ## Get choices in column to search for
  options_from_choice <- reactive({
    exams %>%
      dplyr::select(input$search_columns)
  })

# Reactive Functions for Getting Search Columns Terms -----------------

  ## Filters df by search term entered in input$search_term selectize
  ## searched within input$search_columns, which is list of column names to search
  filter_func <- reactive({
    exams %>%
      filter(grepl(
        # gsub to avoid escape characters being entered by user
        gsub("[^A-Za-z0-9 ]", "", input$search_term),
        gsub("[^A-Za-z0-9 ]", "", !!sym(input$search_columns)),
        ignore.case = T
      )) %>%
      # Groups by searched term
      arrange(input$search_term)
  })
  
  ## Generates datatable from the search conducted above
  filter_func_table <- reactive({
    req(input$search_columns) # Wait for search_columns to load
    DT::datatable(
      # Filters by search
      filter_func(),
      options = list(
        # Scroll bar and reduce size of header and body
        scrollX = TRUE,
        # Function to reduce size, used in Rmarkdown version as no renderDT there
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-size': '75%'});",
          "$(this.api().table().header()).css({'font-size': '75%'});",
          "}"
        ),
        pageLength = 5
      ), # end options
      rownames = FALSE
    ) # end dataTable
  })
  
  ## From filtered df, this groups and counts by the search
  summary_func <- reactive({
    req(input$search_columns) # Wait for columns to load
    DT::datatable(
      exams %>%
        filter(grepl(
          gsub("[^A-Za-z0-9 ]", "", input$search_term),
          gsub("[^A-Za-z0-9 ]", "", !!sym(input$search_columns)),
          ignore.case = T
        )) %>%
        group_by(!!sym(input$search_columns)) %>%
        count %>%
        arrange(desc(n)),
      options = list(
        # Scroll bar and reduce size of header and body
        scrollX = TRUE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-size': '75%'});",
          "$(this.api().table().header()).css({'font-size': '75%'});",
          "}"
        ),
        pageLength = 5
      ),
      # end options
      rownames = FALSE
    ) # end dataTable
  })
  
  ## This done to improve speed, selectizeInput initialized in UI
  observe({
  updateSelectizeInput(
    session,
    "search_term",
    # Gives choices of search terms in search column
    choices = c("*", options_from_choice()),
    server = TRUE
  )
  })

  output$master_renderUI <- renderUI({
  # use req to only render results when credentials()$user_auth is TRUE
  req(credentials()$user_auth)
  sidebarLayout(
    sidebarPanel(
      # Sidebar stays in view when scrolling down in mainPanel
      # when width > 1200. If not then allow position to be relative so
      # not overlap
      style = if (input$dimension[1] > 1200){"position:fixed; width: 300px"},
      
      h2("Search Terms"),
      
      # Search panel that includes choice of columns and term to search within that column
        # Column choices
        selectInput(
          "search_columns",
          label = "Select column to search",
          choices = colnames(exams)
        ),
      # Within given column, give choices
        tipify(
          selectizeInput(
            "search_term",
            label = "Term to include to search for (choose or input)",
            choices = NULL,
            width = '400px',
            options = list(create = TRUE)
          ),
          "To add partial search term, click Add search term. Use * to search all",
          placement = "right"
        ),
      br(),
      tipify(
        downloadButton("download_search", "Download Search"),
        "Download filtered search results",
        placement = "right"
      )
    ),
    
    mainPanel(
      # Panel should scroll down, 'width' necessary to avoid overlap
      style = if (input$dimension[1] > 1200){"position:relative; width:700px"},
      
      h2("Complete Table of All Tests Given"),
      
      renderDT(DT::datatable(
        # All exam records
        exams,
        options = list(
          # Scroll bar and reduce size of header and body
          scrollX = TRUE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().body()).css({'font-size': '75%'});",
            "$(this.api().table().header()).css({'font-size': '75%'});",
            "}"
          ),
          pageLength = 5
        ),
        rownames = FALSE
      )),
      
      hr(),
      h2("Search Results"),
      h3("All Fields Search-Limited"),
      
      renderDT(filter_func_table()),
      
      hr(),
      h3("Exam Count By Search"),
      
      renderDT(summary_func())
    )
  ) # sidebarPanelLayout end
  }) # renderUI
  
  # Download searched file
  output$download_search<- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filter_func(), file)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
