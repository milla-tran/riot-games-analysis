# Load Libraries for this app
library(shiny) ; library(fpp3) ; library(seasonal) ; library(tsibble) ; library(ggeasy) ;
library(ggpubr) ; library(shinyWidgets) ; library(shinythemes) ; library(plotly) ;
library(shinydashboard) ; library(dplyr) ; library(seasonal) ; library(tsibble) ;
library(tidyverse) ; library(ggplot2) ; library(lubridate) ; library(timevis)

# Path where data is
file_path <- "C:\\Users\\Milla\\Downloads\\BAS475\\riot games app\\RiotData.csv"

# Data starts in 3rd row, skip first 2 rows
riot_trends <- read.csv(file_path, skip = 2)
# Rename columns
names(riot_trends) <- c("Month", "Interest")
# Convert Month to date
riot_trends$Month <- yearmonth(riot_trends$Month)
# Changing interest data into numeric instead of characters
riot_trends$Interest <- as.numeric(ifelse(riot_trends$Interest == "<1",
                                          0,
                                          riot_trends$Interest))
# Convert to tsibble
riot_trends <- tsibble(riot_trends)


# Introduction message upon opening the app
INTRODUCTION <- c("You will find that there are two tabs available to you on the left, one titled 'plots' and the other 'timeline.'
                    In the timeline tab you will find an interactive timeline presenting the largest events and releases Riot has worked on.
                    This timeline is to help provide context for the data presented in the graphics.
                    To begin analyzing the data, select which graphic you would like to study under the plot tab. 
                    The options include: seasonality, autocorrelation, and three types of decomposition which are multiplicative, additive, and X11.
                    There is also the option to choose the time interval of the data you would like to see. 
                    The default time interval is from the founding of Riot Games to the present day.
                    Once you select what you would like to study click the 'submit' button.
                    A general interpretation of the graphic will be provided beneath the graph.
                    When you are finished feel free to check out any of the other options as well!")


# Output interpretations
INTERPRETATION <- c("Generally, it can be seen that interest from 2006 to 2022 has increased.
                    It also noticeable that in April of 2020 there is huge spike in interest. 
                    This makes sense in context as that was when Legends of Runeterra was released which has many features based on League of Legends, which is Riot's flagship game.
                    The strategy cardgame became very popular and won a number of awards.
                    Notice, however, that there is not a consistent pattern in interest. 
                    The data seems mostly stagnant within each year but increases between years.
                    The spikes in interest are likely due to big releases but the interest is maintained as updates and patches for their already existing franchises are announced.
                    Since each of their games are updated regularly (typically every few weeks) the company stays relevant in Google searches.",
                    "There is significant autocorrelation for lags 1, 5, 6, 7, 19. 
                    Since there are many lag spikes outside of the bounds it is likely that the series is not attributed to white noise.
                    Also since lag 1 is of a larger magnitude in the negative direction troughs tend to be one quarter behind peaks.
                    Similarly since lag 6 is highest out of the other lags, the peaks tend to be six quarters apart and the troughs are six quarters apart as well.
                    From the residual plot, we can see that the residuals are generally small which is a great sign, however there is a huge spike a little after the beginning of 2020.
                    The large residual coincides with the release of Legends of Runeterra which explains the increase in interest.",
                    "The first thing to notice is that the trend and randomness components seem to be particularly influential.
                    We can see this by the size of the reference bar provided on the left side of the graphic.
                    The trend seems to be fairly consistent in that it seems to be increasing through time.
                    There is little indication of seasonality since seasonality seems to be a small part of the decomposition shown through the reference bar.
                    Randomness seems to be the largest component which makes sense since the interest in Riot Games seems to fluctuate in a jagged pattern.
                    Contextually this makes sense as Riot games interest could stem from any of its popular games or events connected with its franchises.
                    Huge annual events such as the League of Legends world championships keeps the games relevant and popular which explains the upward trend",
                    "In the multiplicative decomposition it can be concluded by the reference bar on the left of the graphic that the trend is the largest component of the data.
                    Seasonality, on the other hand is a very small part of the data, with randomness being in between the two.
                    The decomposition makes sense with the overall data since interest has increased throughout the years as Riot games continues operations,
                    but within each year there does not seem to be any sort of seasonality. 
                    Riot does release new content reguarly but the magnitude is on varying scales.
                    A new patch or update will not gain as much interest as release of a new game.
                    This decomposition differs from the additive and X11 decompositions as it shows that the trend component as slightly more influential than the randomness part.",
                    "In this decomposition, randomness is the largest component which makes sense since the graphic of the plain data seems to fluctuate randomly.
                    Trend also seems to be influential but seasonality does not.
                    Similarly with the other decompositions, the graphics make sense contextually since Riot games has continued to grow since its founding which coincides with the increasing trend.
                    However, since Riot games can not continuously release games and other huge content projects the interest in the company can not be consistent.
                    The seasonality portion is very small and the graph does not show a pattern which further shows how little seasonality is present in the data.
                    Overall the decomposition does suport the graph with the main components being trend and randomness.")

# Create the dataframe for the timeline
timeline <- data.frame(
  id      = 1:19,
  content = c("League of Legends released", 
              "First League of Legends World Championship",
              "2012 League of Legends World Championship", 
              "2013 League of Legends World Championship",
              "2014 League of Legends World Championship", 
              "Imagine Dragons released their first collaboration with Riot Games for the League of Legends World Championships",
              "2015 League of Legends World Championship", 
              "2016 League of Legends World Championship",
              "2017 League of Legends World Championship", 
              "2018 League of Legends World Championship",
              "Teamfight Tactics, a League of Legends spinoff auto battler game released", 
              "2019 League of Legends World Championship",
              "Legends of Runeterra, a digital collectible card game based on League of Legends lore was released",
              "Valorant, Riot Games first first person shooter released", 
              "2020 League of Legends World Championship",
              "Wild Rift, a mobile version of League of Legends was released", 
              "2021 League of Legends World Championship",
              "Netflix collaboration series, Arcane, released based on League of Legends lore",
              "First Valorant Championship Tournament"),
  start   = c("2009-10-27", "2011-06-18", "2012-10-04",
              "2013-09-15", "2014-09-18","2015-09-18", 
              "2015-10-01","2016-09-29","2017-09-23",
              "2018-10-01","2019-06-26", "2019-10-02",
              "2020-04-29", "2020-06-02", "2020-09-25",
              "2020-10-27", "2021-10-05", "2021-11-06",
              "2021-12-12"),
  style = c("border-color: #163832; color: #163832;")
)


# Define UI for application helps user explore insurance data
ui <- fluidPage(
  theme = bslib::bs_theme(bg = "#ece8db",
                          fg = "black",
                          base_font = "Times New Roman"),
  
  # Application title
  titlePanel("Riot Games from Creation to the Current Day"),
  
  # Sidebar with radio buttons for user input for choice of graph displayed 
  sidebarLayout(position = "right",
                sidebarPanel(
                  
                  radioButtons("radio", label = "Please select one of the following plots",
                               choices = c("seasonality", "autocorrelation", "additive decomposition", 
                                           "multiplicative decomposition", "X11 decomposition")),
                  
                  dateInput(inputId = "begin", label = "Select start date", 
                            value = today()-months(188), 
                            min = today()-months(200), 
                            max = today(),
                            format = "yyyy-mm-dd", 
                            startview = "month", 
                            weekstart = 0,
                            language = "en", width = NULL),
                  
                  dateInput(inputId = "end", label = "Select end date", 
                            value = today(), 
                            min = today()-months(200), 
                            max = today(),
                            format = "yyyy-mm-dd", 
                            startview = "month", 
                            weekstart = 0,
                            language = "en", width = NULL),
                  
                  submitButton(text = "Submit")
                  
                ),
                
                # Show a plot of the generated distribution
                mainPanel( position = "left",
                           tabsetPanel(
                             tabPanel(
                               "Plots",
                               textOutput("intro"),
                               plotlyOutput("mainPlot", height = 350),
                               br(),
                               plotOutput("otherPlot", height = 350),
                               h3("Interpretation"),
                               textOutput("explanation"),
                               br(),
                               br()  
                             ),
                             tabPanel(
                               "Timeline",
                               timevisOutput("timeline")
                             )
                           )
                           
                           
                )
  )
)


# Define server logic required to draw plots desired by the user
server <- function(input, output) {
  
  output$mainPlot <- 
    renderPlotly(
      riot_trends %>% 
        filter(Month >= as.Date(input$begin), Month <= as.Date(input$end)) %>% 
        ggplot(aes(x = Month, y = Interest)) +
        geom_line(aes(y = Interest), col = "Black") +
        labs(title = "Riot Games Interest Over the Years", y = "Interest", x = "Date") +
        theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
              axis.title.x = element_text(size = 15, colour = "black"),
              axis.title.y = element_text(size = 15, colour = "black"),
              plot.background = element_rect(fill = "#d2c8a9"),
              panel.background = element_rect(fill="#d2c8a9")) 
    )
  
  output$otherPlot <- 
    renderPlot({
      if(input$radio == "seasonality") { 
        riot_trends %>% 
          filter(Month >= as.Date(input$begin), Month <= as.Date(input$end)) %>% 
          gg_season(Interest) +
          geom_line(size=1) +
          ggtitle("Riot Games Interest over Time") +
          xlab("Date") +
          theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
                axis.title.x = element_text(size = 15, colour = "black"),
                axis.title.y = element_text(size = 15, colour = "black"),
                plot.background = element_rect(fill = "#d2c8a9"),
                panel.background = element_rect(fill="#d2c8a9"),
                legend.text = element_text(colour="black"),
                legend.background = element_rect(fill = "#d2c8a9"),
                legend.key = element_blank())  
      }
      
      else if(input$radio == "autocorrelation") {
        riot_trends %>%
          filter(Month >= as.Date(input$begin), Month <= as.Date(input$end)) %>% 
          model(NAIVE(Interest)) %>%
          gg_tsresiduals() +
          ggtitle("Riot Games Interest over Time") +
          xlab("Date")
      }
      
      else if(input$radio == "additive decomposition") {
        riot_trends %>%
          filter(Month >= as.Date(input$begin), Month <= as.Date(input$end)) %>% 
          model(
            classical_decomposition(Interest, type = "additive")
          ) %>%
          components() %>%
          autoplot() +
          labs(title = "Additive Decomposition of Riot Interest") +
          xlab("Date") +
          theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
                axis.title = element_blank(),
                axis.title.x = element_text(size = 15, colour = "black"),
                axis.title.y = element_text(size = 15, colour = "black"),
                plot.background = element_rect(fill = "#d2c8a9"),
                panel.background = element_rect(fill="#d2c8a9"))
      }
      
      else if(input$radio == "multiplicative decomposition") {
        riot_trends %>%
          filter(Month >= as.Date(input$begin), Month <= as.Date(input$end)) %>% 
          model(
            classical_decomposition(Interest, type = "multiplicative")
          ) %>%
          components() %>%
          autoplot() +
          labs(title = "Multiplicative Decomposition of Riot Interest") +
          xlab("Date") +
          theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
                axis.title = element_blank(),
                axis.title.x = element_text(size = 15, colour = "black"),
                axis.title.y = element_text(size = 15, colour = "black"),
                plot.background = element_rect(fill = "#d2c8a9"),
                panel.background = element_rect(fill="#d2c8a9"))
      }
      
      else if(input$radio == "X11 decomposition") {
        riot_trends %>%
          filter(Month >= as.Date(input$begin), Month <= as.Date(input$end)) %>% 
          model(
            x11 = X_13ARIMA_SEATS(Interest ~ x11())) %>%
          components() %>%
          autoplot() +
          labs(title = "X11 Decomposition of Riot Interest") +
          xlab("Date") +
          theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
                axis.title = element_blank(),
                axis.title.x = element_text(size = 15, colour = "black"),
                axis.title.y = element_text(size = 15, colour = "black"),
                plot.background = element_rect(fill = "#d2c8a9"),
                panel.background = element_rect(fill="#d2c8a9"))
      }
      
    })
  
  
  output$explanation <- renderText({
    if(input$radio == "seasonality") { 
      paste(INTERPRETATION[1])
    }
    
    else if(input$radio == "autocorrelation") {
      paste(INTERPRETATION[2])
    }
    
    else if(input$radio == "additive decomposition") {
      paste(INTERPRETATION[3])
    }
    
    else if(input$radio == "multiplicative decomposition") {
      paste(INTERPRETATION[4])
    }
    
    else if(input$radio == "X11 decomposition") {
      paste(INTERPRETATION[5])
    }
    
  })
  
  
  output$timeline <- renderTimevis({
    timevis(timeline)
  })
  
  output$intro <- renderText({
    showModal(modalDialog(
      title = "Hello! 
                    This app is to help you explore how interest in the company Riot Games has changed since it was founded.",
      type = "info",
      paste0(INTRODUCTION[1]),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
