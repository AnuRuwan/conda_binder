library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Linear Regression data"),
    
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            # adding an action button
            actionButton("go", "Linear model", style = "color:#FFFFFF;background-color:#009dff"),
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("linearplot"),
            h5("Intercept",style="color:#009dff"),
            textOutput("Interceptoutput"),
            br(), #added a space
            h5("Slope",style="color:#009dff"),
            textOutput("slopeoutput"),
            br(),
            h5("Correlation_coefficient",style="color:#009dff"),
            textOutput("correlation_coefficientoutput"),
            br(),
            tableOutput("contents")
           
            
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    
    
    
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })
    
    observeEvent(input$go, {
        dataset <- dataInput()
        x <- dataset$x
        y <- dataset$y
        reg <- lm(y ~ x)
        output$linearplot <- renderPlot({
            
            plot(x,y)
            
            abline(reg)
            #print(reg)
        
        })
        
        corr <- cor(x,y)
        
        model_summary <- summary(reg)
        #print(model_summary)
        
        intercept_value <- model_summary$coefficients[1,1] 
        Slope_value <- model_summary$coefficients[2,1]
        
        #Intercept <- lm(x ~ y,dataset)
        output$Interceptoutput <- renderText({intercept_value})
        output$slopeoutput <- renderText({Slope_value})
        
        output$correlation_coefficientoutput <- renderText(corr)
        
        
        
    })
    
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
