library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

prop_data <- read.csv(file.choose())
prop_data <- subset(prop_data, YR_BUILT>1700)
prop_data <- subset(prop_data, YR_REMOD>1900)

condition<-c("E","A","P")
prop_data <- subset(prop_data, prop_data$R_OVRALL_CND%in%condition)

prop_data<-prop_data %>% 
  mutate(R_OVRALL_CND = case_when(
    R_OVRALL_CND == "P"  ~ "Poor",
    R_OVRALL_CND == "E"  ~ "Excellent",
    R_OVRALL_CND == "A" ~ "Average"
  )
  )

mycss <- "
#z ~ .selectize-control .selectize-input {
  display: none;
}
"

mycss1 <- "
#a ~ .selectize-control .selectize-input {
  display: none;
}
"

# Define UI for application inspecting Boston property values
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
  
    # Inputs
    sidebarPanel(
      
      # Select variable for y-axis
      conditionalPanel(condition = "input.tabs != 'bar'",selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Property Value" = "AV_TOTAL", 
                              "Land Value" = "AV_LAND"),
                  selected = "AV_TOTAL")),
      
      # Select variable for x-axis
      conditionalPanel(condition = "input.tabs != 'bar'",selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Year Built" = "YR_BUILT", 
                              "Year Remodeled" = "YR_REMOD"), 
                  selected = "YR_BUILT")),
      
      tags$style(mycss),
      tags$style(mycss1),
      
      selectInput(inputId = "z",
                  label = NULL,
                  choices = c("Condition" = "R_OVRALL_CND")
                  ),
      
      selectInput(inputId = "a",
                  label = NULL,
                  choices = c("Area Name" = "Area_Name")
                 ),
      
      # Select value for year on x-axis
      conditionalPanel(condition = "input.tabs != 'bar'",sliderInput("year", "Choose Year Range",
                  min = 1800, max = 2018,value = c(1902,2018))),
      
      # Select values for condition for plotting
      conditionalPanel(condition = "input.tabs != 'line'",checkboxGroupInput("cond", "Select codition(s) of Property" , c("Excellent","Average","Poor"), selected = c("Excellent","Average","Poor"), inline = FALSE,
                         width = NULL))
    ),
    
    # Output
    mainPanel(id = 'tabs',
      tabsetPanel(
                  tabPanel("Boston Property Assessment", value ='line', plotOutput("lineChart")),
                  tabPanel("Geographical Comparisons", value ='bar', plotOutput("barChart"))
      )
    )
  )
)

# Define server function required to create the lineChart
server <- function(input, output) {
  
  # data pre-processing
  property_data_filter <-reactive({
    prop_data <- subset(prop_data, YR_BUILT > input$year[1] & YR_BUILT < input$year[2])
    prop_data <- subset(prop_data, YR_REMOD > input$year[1] & YR_REMOD < input$year[2])
    prop_data <- subset(prop_data, prop_data$R_OVRALL_CND%in%input$cond)
    return(prop_data)
  })
  
  # data pre-processing
  property_data_filter1 <-reactive({
    area_cond<-c("Mission Main","Fenway")
    prop_data <- subset(prop_data, prop_data$Area_Name%in%area_cond)
    return(prop_data)
  })
  
  # Create the line object the plotOutput function is expecting
  output$lineChart <- renderPlot({
    prop_data = property_data_filter()
    Disp_plot <- ggplot(data = prop_data, aes_string(x = input$x, y = input$y, colour = input$z)) +
      geom_line(size=1) +
      ggtitle("Boston Property Assessment") +
      xlab("Years")  +
      ylab("Assessed Value")
    library(scales)
    Disp_plot <- Disp_plot + scale_y_continuous(labels = comma)
    Disp_plot <- Disp_plot + theme(legend.title = element_text(colour="blue", size=10, face="bold"))
    Disp_plot <- Disp_plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    Disp_plot
  })
  
  # Create the bar object the plotOutput function is expecting
  output$barChart <- renderPlot({
    prop_data = property_data_filter()
    prop_data = property_data_filter1()
    bar_plot <- ggplot(data = prop_data, aes_string(x = input$z, fill = input$a)) +
      geom_bar(position = 'dodge') +
      ggtitle("Geographical Comparisons") +
      xlab("Condition")  +
      ylab("Assessed Value")
    library(scales)
    bar_plot <- bar_plot + scale_y_continuous(labels = comma)
    bar_plot <- bar_plot + theme(legend.title = element_text(colour="blue", size=10, face="bold"))
    bar_plot <- bar_plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    bar_plot
  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)