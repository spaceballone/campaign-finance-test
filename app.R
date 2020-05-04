#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RColorBrewer)
library(shinyjs)
library(leaflet)
library(plotly)
library(lubridate)
library(dplyr)
library(forcats)
library(stringr)
library(scales)
library(shiny.semantic)

# df <- read.csv("~/Downloads/campaign_finance.csv") %>%
#     rename(`amount` = `Tran_Amt1`,
#            `election_date` = `Elect_Date`,
#            `campaign` = `Filer_NamL`,
#            `employer` = `Tran_Emp`,
#            `donation_date` = `Tran_Date`,
#            `first_name` = `Tran_NamF`,
#            `last_name` = `Tran_NamL`,
#            `location` = `Tran_Location`) 
# 
simplified_path <- "./dat/sf_campaigns.csv"
# 
# df %>%
#     select(campaign,election_date,amount,first_name,last_name,employer,donation_date,location) %>%
#     write.csv(simplified_path)
# 
df_donations <- read.csv(simplified_path, stringsAsFactors = FALSE) %>%
    #filter(grepl("*Newsom*",campaign)) %>%
    mutate(election_date = as.Date(election_date,'%m/%d/%Y')) %>%
    mutate(donation_date = as.Date(donation_date,'%m/%d/%Y')) %>%
    mutate(election_year = year(election_date)) 
#%>%
#    filter(election_year == "1998")
# 
# df_donations %>% write.csv("~/Downloads/test.csv")

# df_donations <- read.csv("~/Downloads/test.csv", stringsAsFactors = FALSE) %>%
#     mutate(election_date = as.Date(election_date,'%m/%d/%Y')) %>%
#     mutate(donation_date = as.Date(donation_date, '%m/%d/%Y'))
    
# Define UI for application that draws a histogram
ui <- navbarPage("Campaign Finance App",
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

    # Application title
  #  titlePanel("Campaign Finance App"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3, style = "height: 1024px",
            tagList(
                div(
                    div(style="display: inline-block;","Election Year"),
                    div(style="display: inline-block;background-color: transparent;border-color: transparent;",
                        uiOutput(outputId = "yearSelector")
                    )
                ),
                uiOutput(outputId = "selector"),
                uiOutput(outputId = "candidateDetails")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 9, 
           fluidRow(width = "100%",
                    div(style="display: inline-block;vertical-align:top","Campaign Contributions Controller"),
                    div(style="display: inline-block;vertical-align:top",
                        dateRangeInput('dateRange',
                                    label = NULL,
                                    start = "1998-01-01", end = "2000-01-01"
                        ))
                   ),
           fluidRow(width = "100%",
                    uiOutput(outputId = "legend"),align = "right"),
           fluidRow(width = "100%",
               # column(width = 7,
               #        wellPanel(leafletOutput(outputId = "map"))
               #        ),
               div(id='mapContainer',leafletOutput(outputId = "map")),
#                      wellPanel(leafletOutput(outputId = "map"))
#               ),
               div(id='contributorsContainer',
                          div(class="chartHeader","Top Contributors"),
                          div(style='max-height:500px; overflow-y: scroll; position: relative',plotlyOutput(outputId = "topContributors",width="100%"))
                          
               )
           ),
           fluidRow(width = "100%",
                    div(id='contributionsHistogramContainer',
                          div(class="chartHeader","Contributions Sizes Grouped by Amount"),
                          plotlyOutput(outputId = "contributionsHistogram",width="100%")
                                ),
               div(id='contributionsHistogramContainer',
                          div(class="chartHeader","Contributions Overtime"),
                          plotlyOutput(outputId = "contributionsTimeline",width="100%")
               )
           )
        )
    )
)


get_color <- function(candidate_input, thing){
    #Really should just be passing the input here, and filtering on that instead of the whole dataset
    keys <- candidate_input
    palette <- viridisLite::viridis(n = length(keys))
    names(palette) <- keys
    return(palette)
}
# Define server logic required to draw a histogram
server <- function(input, output) {
    
   unique_campaigns <- reactive({
       df_uniques <- df_donations %>%
             filter(election_year == input$electionYearFilter)
       unique(df_uniques$campaign)
   })
   
   # unique_campaigns <- reactive({unique(sort(df_donations$campaign))
   # })
   # 
   default_campaign <- c("Supervisor Gavin Newsom Campaign Committee")
    
    filtered_donations <- reactive({
        df_donations %>%
            filter(campaign %in% input$candidate1) %>%
            filter(election_year == input$electionYearFilter) %>%
            filter(donation_date > input$dateRange[1] & donation_date < input$dateRange[2]) 
    })
    
    
    unique_years <- sort(as.numeric(unique(df_donations$election_year),rm.na = T))
    
    output$yearSelector <- renderUI({
        selectInput(
            inputId = "electionYearFilter", 
            label = NULL, 
            width="auto",
            choices = unique_years,
            selected = "1998")
    })
    
    output$selector <- renderUI({
        
        tagList(
            div(
                selectInput(inputId = "candidate1", 
                            label = NULL, 
                            width="100%",
                            unique_campaigns(),
                            #sort(unique_campaigns),
                            selected = default_campaign,
                            multiple = TRUE)
            )
        )
    })
    
    color_palette <- reactive({
        campaigns <- input$candidate1
        palette <- viridisLite::viridis(n = length(campaigns))
        names(palette) <- campaigns
        palette
    })
    
    output$candidateDetails <- renderUI({
        total_donations <- filtered_donations() %>% 
            group_by(campaign) %>%
            summarize(total_amount = sum(amount))
        
        tags <- tagList()
        
        colors <- color_palette()
        
        for (row in 1:nrow(total_donations)){
            campaign <- total_donations[[row,"campaign"]]
            amount <- total_donations[[row,"total_amount"]]
            tags <- tagList(tags,
                            tagList(div(style = "background-color: white;",
                                        div(style = paste0("display: inline-block;height: 20px;width: 20px; background-color: ",colors[[as.character(campaign)]]),""),
                                        div(style = "display: inline-block;",campaign),
                                        div(style = "display: inline-block;",dollar(amount))
                                    )
                                )
                            )
        }
        
        tags
    })
    
    output$map <- renderLeaflet({
        lat_regex <-  "-?[0-9]+\\.[0-9]+"
        lon_regex <- "(?<=\\s)[0-9]+\\.[0-9]+"
        map_data <- filtered_donations() %>%
            mutate(lat = as.numeric(str_extract(location,lat_regex))) %>%
            mutate(lon = as.numeric(str_extract(location,lon_regex)))
        mapout <- leaflet_static <- leaflet() %>% addProviderTiles(providers$ CartoDB.Positron) %>%
            setView(-98.58,39.82,3) %>%
            addCircles(data = map_data, lng = ~lat, lat = ~lon, layerId = ~employer,
              popup = ~employer, color = color_palette())
            
        
        return(mapout)
    })
    
    output$contributionsTimeline <- renderPlotly({
        contributions_data <- filtered_donations() %>% group_by(donation_date,campaign) %>% summarize(total_amount = sum(amount))
        
        plt_out <- contributions_data %>%
            group_by(campaign) %>%
            plot_ly(
                x = ~donation_date,
                y = ~total_amount,
                showlegend = FALSE
            ) %>%
            add_lines(color = ~campaign, colors = color_palette())
            
        
        return(plt_out)
    })
    
    bins <- c(0,200,500,1000,2000)
    
    output$legend <- renderUI({
        palette <- color_palette()
        tagList(
            lapply(seq_along(palette),function(i){
                tagList(div(style=paste("width: 20px;height: 20px;display: inline-block;background-color: ",palette[[i]]),""),
                        div(style="display: inline-block;",paste0("",names(palette)[[i]]))
                )
            })
        )
    })
    
    output$contributionsHistogram <- renderPlotly({
        df_histogram_data <- filtered_donations() %>% group_by(campaign,cut(amount,bins)) %>% summarize(total_count = n())

        plt_out <- df_histogram_data %>%
            group_by(campaign) %>%
            plot_ly(
                x = ~`cut(amount, bins)`,
                y = ~total_count,
                # hoverinfo = "",
                # mode = 'lines',
                color = ~campaign,
                colors = color_palette(),
                showlegend = FALSE
        )
        return(plt_out)
    })
    
    output$topContributors <- renderPlotly({
        df_top_contributors <- filtered_donations() %>% group_by(employer,campaign) %>% summarize(total_amount = sum(amount)) %>% ungroup() %>% mutate(employer = fct_reorder(employer,total_amount))
        
        plot_height <- 500 + 30*nrow(df_top_contributors)*length(input$candidate1)
        
        m <- list(l=150, r=20, b=10, t=1)
        
        plt_out <- df_top_contributors %>%
            group_by(campaign) %>%
            plot_ly(
                x = ~total_amount,
                y = ~employer,
                orientation='h',
                color = ~campaign,
                colors = color_palette(),
                showlegend = FALSE,
                height=plot_height
            ) %>%
            layout(margin = m,bargap=0.5)
        return(plt_out)
    })
}

options(shiny.autoreload = TRUE)
        # Run the application 
shinyApp(ui = ui, server = server)
