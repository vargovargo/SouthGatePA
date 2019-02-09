#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(xts)
library(data.table)
library(dygraphs)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Purple Air Upload Attempt"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3, 
            fileInput('file1', 'Load Purple Air File',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           dygraphOutput("timeseries")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
   
    
    output$timeseries <- renderDygraph({
        
        
        foo <- fread(input$file1$datapath) %>%
            .[, dateTime := as.POSIXct(created_at)] %>% 
            .[,.(dateTime, `PM1.0_CF_ATM_ug/m3`,  `PM2.5_CF_ATM_ug/m3` , `PM10.0_CF_ATM_ug/m3`)]
        
        lastDay <- max(foo$dateTime) - 1
        
    
    dygraph(xts(foo, order.by = foo$dateTime), main = "PA Sensor")  %>%
        dySeries("PM1.0_CF_ATM_ug/m3", fillGraph = T, drawPoints = F, color = "red", label = "ultrafine")  %>%
        dySeries("PM2.5_CF_ATM_ug/m3", drawPoints = F, color = "gray", label = "fine") %>%
        dySeries("PM10.0_CF_ATM_ug/m3", drawPoints = F, color = "blue", label = "coarse")  %>%
        dyRangeSelector(dateWindow = c({lastDay-40000}, lastDay), height = 50)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

