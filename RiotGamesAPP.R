# Load Libraries for this app
library(shiny) ; library(fpp3) ; library(seasonal) ; library(tsibble) ; library(ggeasy) ;
library(ggpubr) ; library(shinyWidgets) ; library(shinythemes) ; library(plotly) ;
library(shinydashboard) ; library(dplyr) ; library(seasonal) ; library(tsibble) ;
library(tidyverse) ; library(ggplot2) ; library(lubridate) ; library(timevis)

# Path where data is
file_path <- "C:\\Users\\milla\\Downloads\\BAS475\\riot games app\\RiotData.csv"

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


# Introduction message upon opening the app -----------------------------------
INTRODUCTION <- c("You will find that there are three tabs available to you on the left, one titled 'plots', another 'models', and the last one 'timeline.'
                    To begin analyzing the data, select which graphic you would like to study in the 'plots' tab. 
                    The options include: seasonality, autocorrelation, and three types of decomposition which are multiplicative, additive, and X11.
                    There is also the option to choose the time interval of the data you would like to see. 
                    The default time interval is from the founding of Riot Games to the present day.
                    Once you select what you would like to study click the 'submit' button.
                    A general interpretation of the graphic will be provided beneath the graph.
                    In the 'models' tab you will find a variety of options for models including simple models, exponential smoothing models, and ARIMA models. 
                    In the 'timeline' tab you will find an interactive timeline presenting the largest events and releases Riot has worked on.
                    This timeline is to help provide context for the data presented in the graphics.
                    When you are finished feel free to check out any of the other options as well!")


# Output interpretations ------------------------------------------------------
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

# Output notes for the models tab ---------------------------------------------
MNOTES <- c("The naive model sets all future forecasts to be equal to the last observation in the data given. This method works surprisingly well for 
            economic and financial timer series. However, it is not likely to be a good fit here since Riot Games continues to evolve and change depending 
            on how much work is done at the company and if any new releases are coming up.",
            "The seasonal naive model uses a similar method to the naive model. This model is very effective for forecasting highly seasonal data. It sets each 
            forecast to be equal to the last observed value from the same season. Since the interest in Riot Games is not highly seasonal, this model is
            likely not a great fit.",
            "The mean model makes predictions based on the mean of the data that we have so far. The mean model is good for seeing how much interest in Riot Games
            fluctuates around the average. It is not likely to be a good model for predicting what the interest in Riot will look like though since it is not able to 
            predict events that could heigten interest in the company.",
            "The drift model is a variation of the nave model and utilizes the average change seen in historical data. This prediction will have the same slope
            as a line that connects the first and last observations of our data. Like with the other simple models, this will likely not be able to capture
            the fluctuation in Riot Games interest.",
            "The Holt's model is an extended simple exponential smoothing model which allows for forecasting data with a trend. The method utilizes a forecast
            equation, and two smoothing equations. One of the smoothing equations is for level and the other is for trend. Unlike the simple models, the Holt's
            model captures some trend from the data and uses that to make its forecasts.",
            "The Holt-Winters method is an extension of Holt's method and captures seasonality. Instead of two smoothing equations, the Holt-Winter's method uses
            three equations. The third equation is for the seasonal component of the data. This can be seen the model of the data. Unforunately the predictions do
            not seem to capture much trend, but it does depict the seasonality component.",
            "ARIMA stands for AutoRegressive Integrated Moving Average, the ARIMA model uses predicted including lagged values and lagged errors. While the 
            parameters are chosen for you, be wary and of the predictions. It is important to understand the behaviour of the model.",
            "The parameters for the ARIMA models are as follows: the lower case p,d, and q are variables for the non-seasonal part of the model where 
            p = order of the autoregressive part, d = degree of first differencing involved, q = order of the moving average part, and the capitalized 
            P, D, and Q represent the same variables but are part of the seasonal part of the model. The default ARIMA model is a white noise case where 
            all parameters are 0.")

# Create the dataframe for the timeline ---------------------------------------
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
  style = c("border-color: #aac9dd; color: #163832;")
)

# setting up the ui -----------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Riot Games"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plots", tabName = "Plots", icon = icon("table")),
      menuItem("Models", tabName = "Models", icon = icon("bar-chart-o")),
      menuItem("Timeline", tabName = "Timeline", icon = icon("calendar"))
    )
  ),
    dashboardBody(
      tabItems(
        # First tab content ---------------------------------------------------
        tabItem(tabName = "Plots",
                h2("Plots"),
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
                                             textOutput("intro"),
                                             plotlyOutput("mainPlot", height = 350),
                                             br(),
                                             plotOutput("otherPlot", height = 350),
                                             h3("Interpretation"),
                                             textOutput("explanation"),
                                             br(),
                                             br()  
                              )
                )
        ),
        
        # Second tab content --------------------------------------------------
        tabItem(tabName = "Models",
                h2("Models"),
                sidebarLayout(position = "right",
                              sidebarPanel(
                                sliderInput("slider", label = ("Please select how many months after the data you would like to forecast"), min = 0, 
                                            max = 100, value = 12),
                                
                                radioButtons("radiom", label = "Please select one of the following models and then click submit",
                                             choices = c("Naive", "Seasonal Naive", "Mean", 
                                                         "Drift", "Holts", "Holts/Winters", "Auto ARIMA", "ARIMA")
                                ),
                                strong("If you choose to do an ARIMA model, please select the parameters you would like to use:"),
                                numericInput("lp", label = ("Please select a p value"), value = 0),
                                numericInput("ld", label = ("Please select a d value"), value = 0),
                                numericInput("lq", label = ("Please select a q value"), value = 0),
                                numericInput("bp", label = ("Please select a P value"), value = 0),
                                numericInput("bd", label = ("Please select a D value"), value = 0),
                                numericInput("bq", label = ("Please select a Q value"), value = 0),
                                submitButton(text = "Submit")
                              ),
                              
                              mainPanel( position = "left",
                                         plotlyOutput("mainPlotm", height = 350),
                                         br(),
                                         plotOutput("otherPlotm", height = 350),
                                         h3("Notes"),
                                         textOutput("notes"),
                                         br(),
                                         br()
                                         )
                )
        ),
        
        # Third tab content ---------------------------------------------------
        tabItem(tabName = "Timeline",
                h2("Timeline"),
                timevisOutput("timeline")
        )
      )
  )
)


# setting up the server -------------------------------------------------------
server <- function(input, output) {

  # building the plots --------------------------------------------------------
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
              plot.background = element_rect(fill = "#aac9dd"),
              panel.background = element_rect(fill="#aac9dd")) 
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
                plot.background = element_rect(fill = "#aac9dd"),
                panel.background = element_rect(fill="#aac9dd"),
                legend.text = element_text(colour="black"),
                legend.background = element_rect(fill = "#aac9dd"),
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
                plot.background = element_rect(fill = "#aac9dd"),
                panel.background = element_rect(fill="#aac9dd"))
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
                plot.background = element_rect(fill = "#aac9dd"),
                panel.background = element_rect(fill="#aac9dd"))
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
                plot.background = element_rect(fill = "#aac9dd"),
                panel.background = element_rect(fill="#aac9dd"))
      }
      
    })
  
  # interpretations for the plots ---------------------------------------------
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
  
  output$mainPlotm <- 
    renderPlotly(
      riot_trends %>% 
        filter(Month >= as.Date(input$begin), Month <= as.Date(input$end)) %>% 
        ggplot(aes(x = Month, y = Interest)) +
        geom_line(aes(y = Interest), col = "Black") +
        labs(title = "Riot Games Interest Over the Years", y = "Interest", x = "Date") +
        theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
              axis.title.x = element_text(size = 15, colour = "black"),
              axis.title.y = element_text(size = 15, colour = "black"),
              plot.background = element_rect(fill = "#aac9dd"),
              panel.background = element_rect(fill="#aac9dd")) 
    )
  
  
  
  # building the models -------------------------------------------------------
  output$otherPlotm <- renderPlot({
    if(input$radiom == "Naive") { 
      riot_trends %>% 
        model(NAIVE(Interest)) %>% 
        forecast(h = as.numeric(input$slider)) %>% 
        autoplot(riot_trends) +
        ggtitle("Naive Model of Riot Games Interest") +
        theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
              axis.title = element_blank(),
              axis.title.x = element_text(size = 15, colour = "black"),
              axis.title.y = element_text(size = 15, colour = "black"),
              plot.background = element_rect(fill = "#aac9dd"),
              panel.background = element_rect(fill="#aac9dd"))
    }
    
    else if(input$radiom == "Seasonal Naive") {
      riot_trends %>% 
        model(SNAIVE(Interest)) %>% 
        forecast(h = as.numeric(input$slider)) %>% 
        autoplot(riot_trends) +
        ggtitle("Seasonal Naive Model of Riot Games Interest") +
        theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
              axis.title = element_blank(),
              axis.title.x = element_text(size = 15, colour = "black"),
              axis.title.y = element_text(size = 15, colour = "black"),
              plot.background = element_rect(fill = "#aac9dd"),
              panel.background = element_rect(fill="#aac9dd"))
    }
    
    else if(input$radiom == "Mean") {
      riot_trends %>% 
        model(MEAN(Interest)) %>% 
        forecast(h = as.numeric(input$slider)) %>% 
        autoplot(riot_trends) +
        ggtitle("Mean Model of Riot Games Interest") +
        theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
              axis.title = element_blank(),
              axis.title.x = element_text(size = 15, colour = "black"),
              axis.title.y = element_text(size = 15, colour = "black"),
              plot.background = element_rect(fill = "#aac9dd"),
              panel.background = element_rect(fill="#aac9dd"))
    }
    
    else if(input$radiom == "Drift") {
      riot_trends %>% 
        model(RW(Interest ~ drift())) %>% 
        forecast(h = as.numeric(input$slider)) %>% 
        autoplot(riot_trends) +
        ggtitle("Drift Model of Riot Games Interest") +
        theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
              axis.title = element_blank(),
              axis.title.x = element_text(size = 15, colour = "black"),
              axis.title.y = element_text(size = 15, colour = "black"),
              plot.background = element_rect(fill = "#aac9dd"),
              panel.background = element_rect(fill="#aac9dd"))
    }
    
    else if(input$radiom == "Holts") {
      riot_trends %>% 
        model(ETS(Interest ~ error("A") + trend("A") + season("N"))) %>% 
        forecast(h = as.numeric(input$slider)) %>% 
        autoplot(riot_trends) +
        ggtitle("Holts Model of Riot Games Interest") +
        theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
              axis.title = element_blank(),
              axis.title.x = element_text(size = 15, colour = "black"),
              axis.title.y = element_text(size = 15, colour = "black"),
              plot.background = element_rect(fill = "#aac9dd"),
              panel.background = element_rect(fill="#aac9dd"))
    }
    
    else if(input$radiom == "Holts/Winters") {
      riot_trends %>% 
        model(ETS(Interest ~ error("M") + trend("Ad") + season("M"))) %>% 
        forecast(h = as.numeric(input$slider)) %>% 
        autoplot(riot_trends) +
        ggtitle("Holts-Winters Model of Riot Games Interest") +
        theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
              axis.title = element_blank(),
              axis.title.x = element_text(size = 15, colour = "black"),
              axis.title.y = element_text(size = 15, colour = "black"),
              plot.background = element_rect(fill = "#aac9dd"),
              panel.background = element_rect(fill="#aac9dd"))
    }
    
    else if(input$radiom == "Auto ARIMA") {
      riot_trends %>% 
        model(ARIMA(Interest)) %>% 
        forecast(h = as.numeric(input$slider)) %>% 
        autoplot(riot_trends) +
        ggtitle("Auto ARIMA Model of Riot Games Interest") +
        theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
              axis.title = element_blank(),
              axis.title.x = element_text(size = 15, colour = "black"),
              axis.title.y = element_text(size = 15, colour = "black"),
              plot.background = element_rect(fill = "#aac9dd"),
              panel.background = element_rect(fill="#aac9dd"))
    }
    
    else if(input$radiom == "ARIMA") {
      riot_trends %>% 
        model(ARIMA(Interest ~ pdq(input$lp,input$ld,input$lq) + PDQ(input$bp,input$bd,input$bq))) %>% 
        forecast(h = as.numeric(input$slider)) %>% 
        autoplot(riot_trends) +
        ggtitle("Manual ARIMA Model of Riot Games Interest") +
        theme(plot.title = element_text(size = 20, colour = "black", family = "Times New Roman"),
              axis.title = element_blank(),
              axis.title.x = element_text(size = 15, colour = "black"),
              axis.title.y = element_text(size = 15, colour = "black"),
              plot.background = element_rect(fill = "#aac9dd"),
              panel.background = element_rect(fill="#aac9dd"))
    }
    
})
  

  # notes for the models ------------------------------------------------------
  output$notes <- renderText({
    if(input$radiom == "Naive") { 
      paste(MNOTES[1])
    }
    else if(input$radiom == "Seasonal Naive") {
      paste(MNOTES[2])
    }
    else if(input$radiom == "Mean") {
      paste(MNOTES[3])
    }
    else if(input$radiom == "Drift") {
      paste(MNOTES[4])
    }
    else if(input$radiom == "Holts") {
      paste(MNOTES[5])
    }
    else if(input$radiom == "Holts/Winters") {
      paste(MNOTES[6])
    }
    else if(input$radiom == "Auto ARIMA") {
      paste(MNOTES[7])
    }
    else if(input$radiom == "ARIMA") {
      paste(MNOTES[8])
    }
    
  })
  # building the timeline -----------------------------------------------------
  output$timeline <- renderTimevis({
    timevis(timeline)
  })
  
 
   
  # outputting the introduction -----------------------------------------------
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

shinyApp(ui, server)