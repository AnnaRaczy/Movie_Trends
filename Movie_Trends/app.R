library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(leaflet)
#library(utils)
library(plotly)
library(dplyr)
library(base)
library(data.table)
library(expss)
library(readxl)
library(janitor)
library(tidyverse)
library(shinyjs)
library(viridis)
library(readr)
library(DT)
library(hrbrthemes)
library(showtext)

#font_add_google("Roboto", "Roboto")


ui <- fluidPage(tags$head(
  tags$script(
    HTML("
                                $(document).ready(function(){
                                  // Mark columns we want to toggle
                                  $('body').find('div [class=col-sm-4]').addClass('sidebarPanel');
                                  $('body').find('div [class=col-sm-8]').addClass('mainPanel');
                                })
                    
                    
                                Shiny.addCustomMessageHandler ('resize',function (message) {
                                  $('.sidebarPanel').toggle();
                                  $('.mainPanel').toggleClass('col-sm-8 col-sm-12');
                                  $(window).trigger('resize')
                                });
                    
                               ")
  )
),
navbarPage("Movie Trends", theme = shinytheme("sandstone"),
           tabPanel("Plots",
                    h1("Movie Trends in Years 2010 - 2020", align='center'),
                    br(),
                    br(),
                    fluidRow(
                      
                      column(6, 
                             h3("Earnings", align='center'),
                             plotlyOutput(outputId='plot1')),
                      
                      column(6,
                             h3('Box-Offices', align='center'),
                             plotOutput(outputId='plot5'))
                    ),
                    br(),
                    br(),
                    fluidRow(
                      
                      column(2),
                      
                      column(8,
                             h3("Popularity by Genre", align='center'),
                             plotlyOutput(outputId='plot2')),
                      
                      column(2)),
                    
                    br(),
                    br(),
                    fluidRow(
                      
                      column(6,
                             h3("Number of Votes", align='center'),
                             plotlyOutput(outputId='plot4')),
                      
                      column(6,
                             h3("Runtime", align='center'),
                             plotlyOutput(outputId='plot3'))),
                    br(),
                    br(),
                    br(),
                    fluidRow(
                      
                      column(2),
                      
                      column(8),
                      
                      column(2)
                      
                    ),
                    br(),
                    br(),
                    br()
           ),
           
           
           
           
           tabPanel("Map",
                    
                    leafletOutput("mymap", width = "200%", height = 1500),
                    p())
           
           
           
           ,
           tabPanel("Data",
                    sidebarLayout(
                      position = "left",
                      div( id ="Sidebar",
                           sidebarPanel(
                             div(style="display: inline;vertical-align:top; width: auto;", 
                                 selectInput(inputId = "selectTitle",
                                             label = "Select Title",
                                             choices = movies_df$Title,
                                             multiple = TRUE)), 
                             br(),
                             div(style="display: inline;vertical-align:top; width: auto;", 
                                 selectInput(inputId = "selectGenre",
                                             label = "Select Genre",
                                             choices = movies_df$Genre,
                                             multiple = TRUE)),
                             br(),
                             div(style="display: inline;vertical-align:top; width: auto;", 
                                 selectInput(inputId = "selectYear",
                                             label = "Select Year",
                                             choices = movies_df$Year,
                                             multiple = TRUE)),
                             linebreaks(21)),
                           
                           br(),
                           br()
                           
                      ),
                      mainPanel(title = "Title", 
                                br(),
                                useShinyjs(),
                                actionButton("showpanel", "Show/Hide Sidebar"),
                                tags$style(type='text/css', "#GA { width:20%; right: 5px;}"),
                                br(),
                                br(),
                                div(style="display: inline;vertical-align:top; width: auto;", 
                                    DTOutput(outputId = "table"))
                                
                      ) # mainPanel
                    )  #Sidebar Layout
           ) #tabPanel
           
) #navbarPage
) # fluidPage





server <- function(input, output, session) {
  
  
  #### Show/Hide Sidebar ####
  
  observeEvent(input$showpanel,{
    session$sendCustomMessage(type = 'resize', message = 1)
    
  })
  
  
  
  #### Plots ####
  
  
  output$plot1 <- renderPlotly({
    
    options(scipen=10000)
    
    plot1_data <- genre_earnings2 %>%
      arrange(desc(Genre)) %>%
      mutate(Year= as.factor(Year)) %>%
      mutate(text = paste("Genre: ", Genre, "\nEarning in Millions: ", Gross_Earning_in_Mil)) %>%
      
      ggplot(mapping = aes(x=Year,y=Gross_Earning_in_Mil, text=text))+
      geom_point(aes(size=Gross_Earning_in_Mil,col=Genre), alpha=0.4, fill='purple')+
      scale_size(range = c(.5, 7), name="Earning")+
      scale_color_viridis(discrete=TRUE)+
      theme(plot.title.position = element_text(vjust = -3, hjust= 0.5))+
      theme_ipsum() +
      theme(axis.text.x = element_text(angle = 90),
            axis.title.x = element_text(size = 13, vjust = -5, hjust= 0.5),
            axis.title.y = element_text(size=13, vjust = 5, hjust= 0.5))+
      theme(legend.text=element_text(size=10),
            legend.title=element_blank())+
      expand_limits(y = 5000)+
      scale_y_continuous(labels = scales::comma)+
      labs(y = "Gross Earning in Millions")
    
    
    plotly_plot <- ggplotly(plot1_data, tooltip="text")
    plotly_plot
    
    
  }
  )
  
  
  
  
  output$plot2 <- renderPlotly({
    
    plot2_data <- genre_count3 %>%
      arrange(desc(Genre)) %>%
      mutate(Genre = as.factor(Genre)) %>%
      mutate(Year= as.factor(Year)) %>%
      mutate(text = paste("Genre: ", Genre, "\nNumber of Movies: ", Count)) %>%
      
      
      ggplot(mapping = aes(x=Year,y=Count, text=text))+
      geom_point(aes(size=Count,col=Genre), alpha=0.4, fill='purple' )+
      scale_size(range = c(.5, 7), name="Popularity")+
      scale_color_viridis(discrete=TRUE)+
      theme_ipsum() +
      theme(axis.text.x = element_text(angle = 90),
            axis.title.x = element_text(size = 13, vjust = -5, hjust= 0.5),
            axis.title.y = element_text(size=13, vjust = 5, hjust= 0.5))+
      theme(legend.text=element_text(size=10),
            legend.title=element_blank())+
      expand_limits(y = 30)+
      scale_y_continuous(labels = scales::comma)+
      labs(y = "Number of Movies")
    
    plotly_plot <- ggplotly(plot2_data, tooltip="text")
    plotly_plot
    
  }
  )
  
  output$plot3 <- renderPlotly({
    
    plot3_data <- genre_runtime %>%
      arrange(desc(Genre)) %>%
      mutate(Genre = as.factor(Genre)) %>%
      mutate(Year= as.factor(Year)) %>%
      mutate(text = paste("Genre: ", Genre, "\nRuntime: ", Runtime)) %>%
      
      
      ggplot(mapping = aes(x=Year,y=Runtime, text=text))+
      geom_point(aes(size=Runtime,col=Genre), alpha=0.4, fill='purple' )+
      scale_size(range = c(.5, 7), name="Runtime")+
      scale_color_viridis(discrete=TRUE)+
      theme_ipsum() +
      theme(axis.text.x = element_text(angle = 90),
            axis.title.x = element_text(size = 13, vjust = -5, hjust= 0.5),
            axis.title.y = element_text(size=13, vjust = 5, hjust= 0.5))+
      theme(legend.text=element_text(size=10),
            legend.title=element_blank())+
      expand_limits(y = 170)+
      scale_y_continuous(labels = scales::comma)+
      labs(y = "Average Runtime")
    
    plotly_plot <- ggplotly(plot3_data, tooltip="text")
    plotly_plot
    
  }
  )
  
  
 
  
  
  output$plot4 <- renderPlotly({
    
    options(scipen=10000)
    
    plot4_data <- genre_votes %>%
      arrange(desc(Votes)) %>%
      mutate(Genre = as.factor(Genre)) %>%
      mutate(Year= as.factor(Year)) %>%
      mutate(text = paste("Genre: ", Genre, "\nAverage Number per Genre: ", scales::comma(Votes, 1))) %>%
      
      ggplot(mapping = aes(x=Year,y=Votes, text=text))+
      geom_point(aes(size=Votes,col=Genre), alpha=0.4, fill='purple' )+
      scale_size(range = c(.5, 5), name='Votes')+
      scale_color_viridis(discrete=TRUE)+
      theme_ipsum() +
      theme(axis.text.x = element_text(angle = 90),
            axis.title.x = element_text(size=13, vjust = -5, hjust= 0.5),
            axis.title.y = element_text(size=13, vjust = 5, hjust= 0.5))+
      theme(legend.text=element_text(size=10),
            legend.title=element_blank())+
      scale_y_continuous(labels = scales::comma)+
      coord_cartesian(clip = 'off') 
    
    plotly_plot <- ggplotly(plot4_data, tooltip="text")
    plotly_plot
    
  }
  )
  
  
  output$plot5 <- renderPlot({


    ggplot(box_office_movies, aes(x=reorder(Director, Gross_Earning_in_Mil),y=Gross_Earning_in_Mil)) +
      geom_bar(stat="identity", fill="#440154FF", alpha=.6, width=.4) +
      theme(axis.text.y = element_text(size= 15),
             axis.text.x = element_text(size= 15)) +
      labs(y='Gross Earning in Millions', element_text(size= 15)) +
      scale_y_continuous(labels = scales::comma)+
      coord_flip() +
      xlab("") +
      theme_ipsum(base_family = "Arial", base_size = 15, axis_title_size = 15, axis_title_just = "mc") 

  }
  )
  
  
  ### Map ###
  
  
  choice <- ifelse(mapped_country2$Density > 1, 'directors', 'director')
  
  
  output$mymap <- renderLeaflet({
    
    mybins <- c(1, 6, 10, 100, 200)
    mypalette <- colorBin( palette="YlOrBr", domain=mapped_country2$Density, bins=mybins)
    
    
    m <- leaflet(mapped_country2) %>% 
      addTiles()  %>% 
      setView( lat=-27, lng=120 , zoom=3) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addCircleMarkers(~Longitude, ~Latitude,  
                       fillOpacity = 0.9, 
                       stroke=TRUE, weight = 1, color = '#8f3704',
                       fillColor=~mypalette(Density), 
                       radius=15,
                       label = paste(mapped_country2$Country,":", mapped_country2$Density, choice),
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>%
      
      addLegend(pal=mypalette, values=~Density, opacity=0.9, title = "Directors over the world", position = "topleft" )
    
    m
    
    
  }
  )
  
  
  #### Data ####
  
  
  observeEvent(input$selectTitle,{
    choice1 <- sort(unique(as.character(movies_df_app$Genre[which(movies_df_app$Title %in% input$selectTitle)])))
    
    updateSelectizeInput(session = session,
                         inputId = "selectGenre",
                         choices = choice1
    )
  })
  
  
  observeEvent(input$selectGenre,{
    choice2 <- sort(unique(as.numeric(movies_df_app$Year[which(movies_df_app$Title %in% input$selectTitle &
                                                                 movies_df_app$Genre %in% input$selectGenre)])))
    
    updateSelectizeInput(session = session,
                         inputId = "selectYear",
                         choices = choice2
    )
  })
  
  
  
  output$table <- DT::renderDataTable({
    
    
    
    options = list(
      autoWidth = TRUE)
    
    filtered <- movies_df_app
    if (!is.null(input$selectTitle)) {
      filtered <- filtered %>% filter(Title %in% input$selectTitle)
    }
    if (!is.null(input$selectGenre)) {
      filtered <- filtered %>% filter(Genre %in% input$selectGenre)
    }
    if (!is.null(input$selectYear)) {
      filtered <- filtered %>% filter(Year %in% input$selectYear)
    }
    
    
    DT::datatable(
      filtered
      
    )
    
    js <- as.datatable_widget(
      cbind(' ' = '&oplus;', filtered), escape = -2,
      options = list(
        columnDefs = list(
          list(visible = FALSE, targets = c(0,2)),    # child rows columns
          list(orderable = FALSE, className = 'details-control', targets = 1)
        )
      ),
      callback = JS("
  table.column(1).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {
    return '<div style=\"background-color:#eee; padding: .5em;\"> ' +
            d[2]  + '</div>';
  };
  table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&ominus;');
      format_datatable(row.data())
    }
  });"
      )
    )
    
    
    js
    
    
    
  })
  
  
  
  
}

shinyApp(ui, server)