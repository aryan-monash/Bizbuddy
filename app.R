# Loading Libraries
library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)
library(lubridate)
library(eeptools) # age_calc
library(scales)


# Reading Files
cust = read.csv("data/Customer.csv", na.strings=c("", "NA"))
prod = read.csv("data/prod_cat_info.csv", na.strings=c("", "NA"))
trans = read.csv("data/Transactions.csv", na.strings=c("", "NA"))

# Merging datasets
cust_trans = merge(cust,trans, by.x = 'customer_Id', by.y = 'cust_id')
customer_final = merge(cust_trans, prod, by.x = c('prod_cat_code', 'prod_subcat_code'), by.y = c('prod_cat_code','prod_sub_cat_code'))

# Data Cleaning
customer_final <- mutate(customer_final, DOB = as.Date(DOB, "%d-%m-%Y"))
customer_final['tran_date'] = parse_date_time(customer_final$tran_date, orders = c('dmy', 'mdy'))
customer_final <- distinct(customer_final)

customer_final <- customer_final %>%
  mutate(age = floor(age_calc(DOB, units='years')))
customer_final$age_group <- cut(customer_final$age, breaks=c(25,35,45,55), labels = c('25-34','35-44','45-54'), include.lowest = FALSE, right = FALSE)

customer_final$Gender[customer_final$Gender == "M"] <- "Male"
customer_final$Gender[customer_final$Gender == "F"] <- "Female"
data <- customer_final

# User interface
ui <- navbarPage(title = div(img(src='logo3.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
                 theme = shinytheme("flatly"),
                 tabPanel("About",
                          titlePanel("BizBuddy"),
                          p("Your Retain Chain Assistant"),
                          p("FIT5147: Data Vizulalization"),
                          sidebarLayout(
                            sidebarPanel(
                              h2("The Creator"),
                              br(),
                              h4("Aryan Jain"),
                              p("31418600"),
                              br(),
                              h4("Monash University"),
                              p("Business Analytics"),
                              br(),
                              br(),
                              br(),
                              br(),
                              p("Social Profiles:"),
                              a("Github",
                                href = "https://github.com/aryan-monash"), "|",
                              a("LinkedIn",
                                href = "https://www.linkedin.com/in/jainaryan/"), "|",
                              a("Kaggle",
                                href = "https://www.kaggle.com/jaryan")
                            ),
                            mainPanel(
                              h1("Introducing the App"),
                              p("This is retail chain assistance app designed with ",
                                strong("Shiny "),
                                "R package."),
                              br(),
                              p(""),
                              br(),
                              h2("Features"),
                              br(),
                              h4(strong("Product Analysis")),
                              p("- Explore sales and returns for particular product categories and sub-categories and a selectable time range."),
                              h4(strong("Business Analysis")),
                              p("- Explores the progression of the business over the years in all the product categories as well as exploration of seasonility."),
                              h4(strong("Customer Retention")),
                              p("- For the first part we sort the customers based on their age group and then explore the returns for gender and age as well. This will help us build a customer retention plan.")
                            )
                          )
                 ),
                 tabPanel("Product Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              h2("User Inputs"),
                              br(),
                              selectInput(inputId = "prod_cat",
                                             label = "Select Category",
                                             choices = unique(data$prod_cat),
                                             selected = "Clothing"
                              ),
                              p("Select a product category from the list"),
                              br(),
                              dateRangeInput(
                                inputId = "tran_date",
                                label = "Transaction Period",
                                start = as.Date("2011-03-01"),
                                end = as.Date("2013-09-01"),
                                min = min(data$tran_date),
                                max = max(data$tran_date)),
                              p("Select a date range for transactions"),
                              br(),
                              br(),
                              h2("Description"),
                              p("This page gives a visual representation of how each product category performed in a particular time period including the sales and returns.")
                            ),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Sales",
                                                   fluidRow(
                                                     splitLayout(cellWidths = c("50%", "50%"),
                                                                 plotlyOutput("barplot"), plotOutput("piechart"))
                                                   ), plotlyOutput("barplot2")),
                                          tabPanel("Returns", plotOutput("piechart2"), plotlyOutput("barplot_ret"))
                              )

                            )
                          )
                 ),

                 tabPanel("Business Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              h2("User Inputs"),
                              br(),
                              selectizeInput(inputId = "prod_cat2",
                                          label = "Select Category",
                                          choices = unique(data$prod_cat),
                                          selected = c("Clothing", "Bags", "Electronics"),
                                          options = list(maxItems = 6)
                              ),
                              p("Select a product category from the list"),
                              br(),
                              checkboxGroupInput(inputId = "checkGroup",
                                                 label = "Select Season",
                                                 choices = c("Jan", "Feb","Sep", "Oct", "Nov","Mar", "Apr", "May","Jun", "Jul", "Aug","Dec"),
                                                 selected = c("Jun", "Jul", "Aug")),
                              p("Select a date range for transactions"),
                              br(),
                              h2("Description"),
                              p("This page explores the progress of the business using the line plot with user inputable product categories and also seasonality using checkboxes and bar plots.")
                            ),
                            mainPanel(tabPanel("Sales",
                                                plotOutput("lineplot"),
                                               p("Time series line plot (Trend Line)"),
                                                   plotOutput("barplot_bus"),
                                               p("Seasonility Plot"),
                                          tabPanel("Returns", plotOutput("piechart2wt"))
                              )
                            )

                 )),
                 tabPanel("Customer Segmentation",
                          sidebarLayout(
                            sidebarPanel(
                              h2("User Inputs"),
                              br(),
                              selectizeInput(inputId = "agegroup",
                                             label = "Select Age Group",
                                             choices = unique(data$age_group),
                                             selected = "25-34",
                                             options = list(maxItems = 3)
                              ),
                              p("Select age group from the list"),
                              br(),
                              checkboxGroupInput(inputId = "gender",
                                                 label = "Select Gender",
                                                 choices = c("Male", "Female"),
                                                 selected = "Male"),
                              p("Select one or both genders"),
                              br(),
                              br(),
                              h2("Description"),
                              p("For the first bar plot we sort the customers based on their age group and then explore the returns for gender and city code for the second faceted bar plot. This will help us build a customer retention plan.")
                            ),
                            mainPanel(tabPanel("Sales",
                                               h3("Customer Segmentation"),
                                               p("Here we explore the sales numbers."),
                                               plotOutput("barplot_cust"),
                                               h3("Customer Retention"),
                                               p("Here we are looking at returns"),
                                               plotOutput("barplot_cust_returns")
                            )

                            )

                          ))

)


# Server
server <- function(input, output, session) {

  # Product Analysis
  selected_cat <- reactive({
    data %>%
      filter(prod_cat == input$prod_cat)
  })

  selected_date <- reactive({
    data <- selected_cat()
    start_date <- input$tran_date[1]
    end_date <- input$tran_date[2]
    data %>%
      filter(between(as.Date(tran_date), start_date, end_date))
  })

  output$barplot <- renderPlotly({
    output_plot <- selected_date()
    p1 <- output_plot %>%
      filter(Qty > 0) %>%
      group_by(prod_subcat) %>%
      mutate(total_amt = sum(total_amt)) %>%
      ggplot(aes(y = total_amt, x = prod_subcat, fill = prod_subcat)) +
      geom_col(position = 'Dodge') +
      xlab("") +
      ylab("Total Sales (In Dollars)") +
      coord_flip() +
      theme_minimal() +
      theme(legend.title = element_blank(), legend.position = "none") +
      scale_fill_brewer(palette="Set2")
    ggplotly(p1)

  })

  output$barplot2 <- renderPlotly({
    # output_plot <- selected_date()
    # p2 <- output_plot %>%
    #   filter(Qty > 0) %>%
    #   group_by(prod_subcat) %>%
    #   mutate(Rate = mean(Rate)) %>%
    #   ggplot(aes(y = prod_subcat, fill = Gender)) +
    #   geom_bar(position = 'Dodge') +
    #   xlab("Number of Customers") +
    #   ylab("") +
    #   theme_minimal() +
    #   theme(legend.title = element_blank(), legend.position = "top") +
    #   scale_fill_brewer(palette="Set2")
    # ggplotly(p2)
    p2 <- data %>%
      ggplot(aes(x = prod_cat, fill = "lightblue")) +
      geom_bar(position = 'Dodge') +
      theme_light() +
      xlab("") +
      ylab("") +
      theme(legend.title = element_blank(), legend.position = "none")
    ggplotly(p2)

  })

  output$piechart <- renderPlot({
    output_plot <- selected_date()
    output_plot %>%
      filter(Qty > 0) %>%
      group_by(prod_subcat) %>%
      summarise(value = n()) %>%
      mutate(prop = 100 * value / sum(value)) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop ) %>%
      ggplot(aes(x = "", y = prop, fill = prod_subcat)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      #theme_void() +
      theme_minimal() +
      theme(legend.title = element_blank(), legend.position = "left") +
      #geom_text(aes(y = ypos, label = label_percent()(round(prop)/100)), color = "white", size=4) +
      scale_fill_brewer(palette="Set2")
  })

  output$barplot_ret <- renderPlotly({
    output_plot <- selected_date()
    p1 <- output_plot %>%
      filter(Qty < 0) %>%
      group_by(prod_subcat) %>%
      mutate(total_amt = sum(total_amt)) %>%
      ggplot(aes(y = total_amt, x = prod_subcat, fill = prod_subcat)) +
      geom_col(position = 'Dodge') +
      xlab("") +
      ylab("Total Returns (In Dollars)") +
      theme_minimal() +
      theme(legend.title = element_blank(), legend.position = "none") +
      scale_fill_brewer(palette="Set2")
    ggplotly(p1)

  })

  output$piechart2 <- renderPlot({
    output_plot <- selected_date()
    output_plot %>%
      filter(Qty < 0) %>%
      group_by(prod_subcat) %>%
      summarise(value = n()) %>%
      mutate(prop = 100 * value / sum(value)) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop ) %>%
      ggplot(aes(x = "", y = prop, fill = prod_subcat)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      #theme_void() +
      theme_minimal() +
      theme(legend.title = element_blank(), legend.position = "none") +

      #geom_text(aes(y = ypos, label = label_percent()(round(prop)/100)), color = "white", size=4) +
      scale_fill_brewer(palette="Set2")
  })

  # Business Analysis
  selected_cat2 <- reactive({
    data %>%
      filter(prod_cat == input$prod_cat2)
  })

  selected_season <- reactive({
    data %>%
      filter(Qty >= 0) %>%
      mutate(month = month(tran_date, label = TRUE)) %>%
      filter(month %in% input$checkGroup)
  })

  output$lineplot<- renderPlot({
    output_plot <- selected_cat2()
    output_plot %>%
      filter(Qty >= 0) %>%
      mutate(year = year(tran_date)) %>%
      group_by(year, prod_cat) %>%
      summarise(total_amt = sum(total_amt)) %>%
      ggplot(aes(x = year, y = total_amt, color = prod_cat)) +
      geom_line() +
      theme_light() +
      theme(legend.title = element_blank(), legend.position = "top") +
      ylab("") +
      xlab("Total Sales")+
      scale_y_continuous(labels = scales::label_number_si(accuracy=0.1))
  })

  output$barplot_bus <- renderPlot({
    output_plot <- selected_season()
    output_plot %>%
      group_by(month, prod_cat) %>%
      summarise(Qty = sum(Qty)) %>%
      ggplot(aes(x = prod_cat, y = Qty, fill = month)) +
      geom_col(position = "dodge2") +
      theme_light() +
      theme(legend.title = element_blank()) +
      xlab("") +
      ylab("Number of Products Sold")
  })

  # Customer Segmentation

  selected_age <- reactive({
    data %>%
      filter(age_group == input$agegroup)
  })

  selected_gender <- reactive({
    data <- selected_age()
    data %>%
      filter(Gender == input$gender)
  })

  output$barplot_cust <- renderPlot({
    output_plot <- selected_gender()
    output_plot %>%
    ggplot(aes(x = prod_cat, fill = age_group)) +
      geom_bar(position = 'dodge') +
      theme_light() +
      xlab("") +
      ylab("count of sales") +
      theme(legend.title = element_blank(), legend.position = "top") +
      facet_wrap(~Gender)
  })

  output$barplot_cust_returns <- renderPlot({
    output_plot <- selected_gender()
    output_plot %>%
      filter(Qty < 0) %>%
      ggplot(aes(x = Gender, fill = Gender)) +
      geom_bar(position = 'dodge') +
      theme_light() +
      xlab("") +
      ylab("count of returns") +
      theme(legend.title = element_blank(), legend.position = "top") +
      facet_wrap(~city_code)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
