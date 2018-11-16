library(shiny)

ui <- fluidPage(
  # Title Frame
  titlePanel("BC Liquor Data Webpage"),

  # Sidebar Frame
  sidebarPanel(
    img(src="BCLiquor.jpg", width = "100%"),
    br(),br(),
    sliderInput("PriceIn",
                label = "Price Range",
                min = 1.99,
                max = 30250,
                value = c(min,max),
                pre = "CAD"),
    checkboxGroupInput("TypeIn",
                       label = "Product Type",
                       choices = c("WINE","SPIRITS","BEER","REFRESHMENT"),
                       selected = "WINE"),
    uiOutput("SelectByCountry"),
    span("Data Source:",
         a("OpenDataBC",
           href = "http://www.opendatabc.ca/dataset/bc-liquor-store-product-price-list-current-prices/resource/3e5059d0-5f46-4356-8e91-42a3150589fd"))
  ),

  # Main Frame
  mainPanel(
    h3(textOutput("summary")),
    downloadButton("download", "Download Results"),
    br(),br(),
    tabsetPanel(
      tabPanel("Alcohol Content Histogram", plotOutput("hist")),
      tabPanel("Result Table", DT::dataTableOutput("table"))
    )
  )

)

server <- function(input, output) {
  library(tidyverse)
  library(stringr)
  library(DT)

  bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

  output$SelectByCountry<-renderUI({
    selectInput("SelCountry",
                label = "Select By Country",
                choices = sort(unique(bcl$Country)),
                selected = "CANADA")
  })

  filter_data<-reactive({
    if (is.null(input$SelCountry)){
      return(NULL)
    }
    bcl%>%filter(Price >= input$PriceIn[1],
                 Price <= input$PriceIn[2],
                 Type %in% input$TypeIn,
                 Country == input$SelCountry)
  })

  output$hist<-renderPlot({
    if(is.null(filter_data())){
      return(NULL)
    }
    filter_data()%>%ggplot(aes(x=Alcohol_Content))+
      geom_histogram(aes(fill=Type))+
      scale_x_continuous(breaks = seq(0, 100, 10),limits = c(0,100))+
      labs(x="Alcohol Content",
           y="Number of Products")
  })

  output$table <- DT::renderDataTable({
    if(is.null(filter_data())){
      return(NULL)
    }else{
      return(filter_data())
    }
  })

  output$summary<-renderText({
    numofoptions<-nrow(filter_data())
    paste0("We found ", numofoptions, " options for you")
  })

  output$download <- downloadHandler(
    filename = function(){
      "bcl-data.csv"
    },
    content = function(con){
      write.csv(filter_data(),con)
    }
  )
}

# Run the application
shinyApp(ui = ui,server = server)

