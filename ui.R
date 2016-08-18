# ShinyApp for analysing global terrorism dataset
#
# ui.R defines the ui of the shiny app which is rendered as html/css
# the inputs and outputs for the gui are defined here
#
# Author:  Carlo Michaelis
# License: Attribution-ShareAlike 4.0 International
# Date:    Aug. 2016

ui <- fixedPage(
  
  # define title, theme and css
  tags$head(
    tags$title("Understand Terrorism - A Statistical Approch")
  ),
  includeCSS("www/bootstrap.min.css"),
  includeCSS("www/style.css"),
  
  h1("Understand Terrorism - A Statistical Approch"),
  p(HTML(pasteq(
    "For this analysis there is mainly the ",
    a(href = "https://www.start.umd.edu/gtd/",
      "Global Terrorism Database (GTD)"),
    " in use. In the last plot, there is some more date in use. Namely the ",
    a(href = "http://data.worldbank.org/indicator/SP.POP.TOTL",
      "population"), ", ",
    a(href = "http://data.worldbank.org/indicator/SP.URB.TOTL.IN.ZS",
      "urban percentage"), " and ",
    a(href = "http://data.worldbank.org/indicator/NY.GDP.MKTP.CD", "GDP"),
    "from worldbank, the ",
    a(href = "https://www.sipri.org/databases/milex", "military expenditure"),
    " from Stockholm International Peace Research Institute and data from the ",
    a(href = "http://fsolt.org/swiid/",
      "Standardized World Income Inequality Database.")))),
  p("In the first step we want to analyse the frequencies of terroristic
    incidents and the amount of victims. In the second step the characteristics
    of terroristic attacks are focused and in the last step we have a look at
    possible indicators of terroristic incidents or victims of terroristic
    attacks."),
  
  # define row with two columns, one for input, one for output
  # as input it is possible to choose between number of incidents and
  # number of victims, it is optionally possible to filter by region
  # as ouput a barplot is shown
  h2("Chronological number of indcidents/victims"),
  div(class="jumbotron",
  p("First we simply want to have a look at the frequency of terroristic attacks
    by year. Therefore you can choose between the frequency of incidents or the
    total amount of victims by year. For further inspection you can use a
    filter to restrict by region, which summarieses over some countries."),
  p("Mind that if you choose the filter, the y-axis will stay constant. This
    will help to compare the regions.")),
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
  h2("Characteristics of attacks"),
  div(class="jumbotron",
  p("In this step we want to get some information about the characteristics of
    terroristic attacks. The pie will show how the characteristics are
    spread. Optionally you can filter by region (as above) and/or by time. Using
    the time filter you can choose between filtering by decade and by single
    years. If you choose years, you can also animate the pie by clicking the
    play button.")),
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
  h2("Indicators"),
  div(class="jumbotron",
  p("In this third step, we want to find out something about relations between
    some indicators and the responses. You can choose the response: frequency of
    incidents or amount of victims. And you can choose the indicator:
    population, urban percentage (how many percent live in cities), GDP,
    military expenditure, inequallity (gini index, where 100 is maximum of
    inequality and 0 is maximum of equality). Optionally you can fliter by
    some characteristics as above. All data will be filtered by year, which you
    can animate by using the play button."),
  p("Beside the scatter plot you can also find the correlation of current
    data below the filters."),
  p(HTML(pasteq(strong("Background:")), "Some studies (see below) indicate that
    there are relations between the amount of terroristic attacks and inequality
    in a country. The population, the urban percentage and the GDP where just
    for fun (like standard data to compare with). The military expenditure
    was chosen to check if military expenditures can reduce terrorism."))),
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
      p("Correlation between indicator and response
        with respect to the filter."),
      strong(textOutput("corr"))
    )),
    column(8, plotlyOutput(outputId="scatt"))
  ),
  h2("References and datasets"),
  h3("Datasets"),
  p("The used datasets are:"),
  tags$ul(
    tags$li("Global Terrorism Database (GTD): https://www.start.umd.edu/gtd/"),
    tags$li("GDP: http://data.worldbank.org/indicator/NY.GDP.MKTP.CD"),
    tags$li("Population: http://data.worldbank.org/indicator/SP.POP.TOTL"),
    tags$li("Urban: http://data.worldbank.org/indicator/SP.URB.TOTL.IN.ZS"),
    tags$li("Military Expenditure: https://www.sipri.org/databases/milex"),
    tags$li("The Standardized World Income Inequality Database:
            http://fsolt.org/swiid/")
  ),
  h3("References"),
  p("There are two papers which leaded to the idea of
    searching for an indicator:"),
  tags$ul(
    tags$li("Piazza, J. A. (2011).
            Poverty, minority economic discrimination, and domestic terrorism.
            Journal of Peace Research, 48(3), 339-353."), 
    tags$li("Goldstein, K. B. (2005).
            Unemployment, inequality and terrorism:
            Another look at the relationship between economics and terrorism.
            Undergraduate Economic Review, 1(1), 6.")
  )
)
