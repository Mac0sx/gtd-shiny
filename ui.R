# ShinyApp for analysing global terrorism dataset
#
# ui.R defines the ui of the shiny app which is rendered as html/css
# the inputs and outputs for the gui are defined here
#
# Author: Carlo Michaelis
# License Attribution-ShareAlike 4.0 International
# Date:   Aug. 2016

ui <- fixedPage(
  
  # define title, favicon and css 
  tags$head(
    tags$title("Understand Terrorism - A Statistical Approch"),
    tags$link(rel="shortcut icon", href="www/favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", ref="www/style.css")
  ),
  h1("Understand Terrorism - A Statistical Approch"),
  
  # define row with two columns, one for input, one for output
  # as input it is possible to choose between number of incidents and
  # number of victims, it is optionally possible to filter by region
  # as ouput a barplot is shown
  h3("Chronological number of indcidents/victims"),
  fixedRow(
    column(4, wellPanel(
      radioButtons("incRadBox", NULL,
                   c("Number of incidents" = "Incidents",
                     "Number of victims" = "Victims")),
      checkboxInput("regionIncBox", "Filter by region"),
      conditionalPanel(
        condition = "input.regionIncBox == true",
        selectInput("region", "Region:", dataLabels$regions))
    )
    ),
    column(8, plotlyOutput(outputId="hist"))
  ),
  
  # define row with two columns, one for input, one for output
  # as input it is possible to choose a characteristic of the incidents
  # it is optionally possible to filter by region or time
  # (decade or year, where year can shown be animated)
  # as ouput a pieplot is shown
  h3("Characteristics of attacks"),
  fixedRow(
    column(8, plotOutput(outputId="pie")),
    column(4, wellPanel(
      selectInput("char", "Characteristic:",
                  setNames(names(dataLabels$characteristics),
                           unlist(lapply(dataLabels$characteristics,
                                         function(x) x$name)))),
      checkboxInput("regionCharBox", "Filter by region"),
      conditionalPanel(
        condition = "input.regionCharBox == true",
        selectInput("regionChar", "Region:", dataLabels$regions)
      ),
      checkboxInput("timeCharBox", "Filter by time"),
      conditionalPanel(
        condition = "input.timeCharBox == true",
        radioButtons("timeRadBox", NULL,
                     c("Filter by decade" = "decade",
                       "Filter by year" = "iyear")),
        conditionalPanel(
          condition = "input.timeRadBox == 'decade'",
          selectInput("decChar", "Decade:", dataLabels$decades)
        ),
        conditionalPanel(
          condition = "input.timeRadBox == 'iyear'",
          sliderInput("yearChar", "Years:",
                      min(charData$iyear), max(charData$iyear),
                      min(charData$iyear), step = 1,
                      animate = animationOptions(interval = 750))
        )
      ))
    )
  ),
  
  # define row with two columns, one for input, one for output
  # as input it is possible to choose an indicator, a response and a year
  # where the year can be shown animated
  # it is optionally possible to filter by some characteristics
  # as ouput a pieplot is shown
  h3("Indicators"),
  fixedRow(
    column(4, wellPanel(
      sliderInput("yearIndi", "Years:",
                  min(indiData$all$Year), max(indiData$all$Year),
                  min(indiData$all$Year), step = 1,
                  animate = animationOptions(interval = 1000)),
      hr(),
      selectInput("indicator", "Choose indicator:", dataLabels$indicators),
      selectInput("response", "Choose response:", c("Incidents", "Victims")),
      checkboxInput("charIndiBox", "Filter by characteristic"),
      conditionalPanel(
        condition = "input.charIndiBox == true",
        selectInput("charIndi", "Characteristic:",
                    c("International Attack" = "INT_ANY",
                      "Successful attack" = "success",
                      "Attack with suicide" = "suicide"))
      ),
      hr(),
      strong("Correlation:"),
      p("Correlation between indicator and response with respect to the filter."),
      strong(textOutput("corr"))
    )),
    column(8, plotlyOutput(outputId="scatt"))
  )
)
