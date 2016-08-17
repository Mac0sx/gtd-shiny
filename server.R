# ShinyApp for analysing global terrorism dataset
#
# server.R defines the server of the shiny app which reacts to user inputs
# and produces corresponding outputs
#
# Author: Carlo Michaelis
# License Attribution-ShareAlike 4.0 International
# Date:   Aug. 2016

server <- function(input, output) {
  # created server function to run shinyApp()
  # handles user inputs und create corresponding outputs
  #
  # Args:
  #   input: inputs from shiny ui
  #   output: outputs for shiny ui
  #
  # Returns:
  #   server function for shiny app
  
  # ----- histogram - frequency data -----
  
  currFreqData <- reactive({
    # reactive which builds the selected data using freqData
    selector <- input$incRadBox
    if(input$regionIncBox == FALSE) {
      # if no region is chosen, return global data, containing the max
      freqData[[pasteq(selector, "All")]]
    } else {
      # if a region is chosen, return filtered data and max of all regions
      # to keep the maximum for all regions constant (for comparison reasons)
      list(
        "data" = filter(freqData[[selector]]$data, region == input$region),
        "max" = freqData[[selector]]$max
      )
    }
  })
  
  output$hist <- renderPlotly({
    # create ggplot object, using geom_bar instead of geom_histogram
    # to have one bar per year, expand limits to maximum (see above in reactive)
    # and theme background and grid lines
    g <- ggplot(currFreqData()$data,
                aes_string(x = "Year", y = input$incRadBox)) +
      geom_bar(stat = "identity") +
      expand_limits(y = c(0, currFreqData()$max)) +
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            panel.grid.major = element_line(colour = "gray90", size = 0.5))
    
    # create plotly object to have dynamic elements, like tooltip
    # and remove navigation elements with config, to have a clean plot
    gg <- ggplotly(g)
    config(gg, showLink = FALSE, sendData = FALSE,
           displaylogo = FALSE, displayModeBar = FALSE)
  })
  
  # ----- pie - characteristics data -----
  
  # reactive which builds the selected data using charData
  currChar <- reactive({
    # first filter by input characteristic
    tmpData <- select(charData, region, iyear, decade, one_of(input$char))
    
    # evaluate the current input condition and filter temporary dataset
    # by time (year or decade) and region otherwise return global data
    if(input$regionCharBox == TRUE & input$timeCharBox == TRUE) {
      if(input$timeRadBox == 'decade') {
        subset(tmpData, region == input$regionChar & decade == input$decChar)
      } else if(input$timeRadBox == 'iyear') {
        subset(tmpData, region == input$regionChar & iyear == input$yearChar)
      }
    } else if(input$regionCharBox == TRUE) {
      subset(tmpData, region == input$regionChar)
    } else if(input$timeCharBox == TRUE) {
      if(input$timeRadBox == 'decade') {
        subset(tmpData, decade == input$decChar)
      } else if(input$timeRadBox == 'iyear') {
        subset(tmpData, iyear == input$yearChar)
      }
    } else {
      tmpData
    }
  })
  
  output$pie <- renderPlot({
    data <- currChar()
    
    # if no data is available for this characteristic, show an error message
    if(length(data[[input$char]]) == 0) {
      stop("No data available")
    }
    
    # find missing factor labels in data and remove them from label list
    missingLabels <- setdiff(dataLabels$characteristics[[input$char]]$labels,
                             sort(unique(data[[input$char]])))
    if(length(missingLabels) != 0) {
      currLabels <-
        names(subset(dataLabels$characteristics[[input$char]]$labels,
                     !(as.integer(
                       dataLabels$characteristics[[input$char]]$labels
                       %in% missingLabels))))
    } else {
      currLabels <- names(dataLabels$characteristics[[input$char]]$labels)
    }
    
    # prepare data for pie (create factor with data and labels and order it)
    pieces <- factor(data[[input$char]], labels = currLabels)
    pieces <- reorder(pieces, X = pieces, FUN = function(x) -length(x))
    
    # calculate percentage values for pie pieces to show on every piece
    at <- nrow(data) - as.numeric(cumsum(sort(table(pieces)))
                                  - 0.5*sort(table(pieces)))
    perLabel <- paste0(round(sort(table(pieces))/
                               sum(table(pieces)),2) * 100,"%")
    
    # palette function for more than 9 colors (used in scale_fill_manual below)
    cols <- colorRampPalette(rev(brewer.pal(9, "OrRd")))
    
    # create ggplot object, using geom_bar, theme it and
    # transform it into polar coordinates to get it "round"
    # with annotate the percentage of the pieces are shown
    ggplot(data, aes(x = factor(1), fill = pieces)) +
      geom_bar(width = 1) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
            axis.ticks.x = element_blank(), axis.title.y = element_blank(),
            axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            legend.title = element_blank(),
            panel.background = element_rect(fill = "transparent",
                                            colour = NA)) +
      scale_fill_manual(values = cols(length(unique(pieces)))) +
      coord_polar(theta = "y") +
      annotate(geom = "text", y = at, x = 1.25, label = perLabel)
  })
  
  # ----- scatter - indicators data -----
  
  currIndiData <- reactive({
    # define selector for filtering data
    if(input$charIndiBox == FALSE) {
      selector <- "all"
    } else {
      selector <- input$charIndi
    }
    
    # filter data using selector and select chosen indicator and
    # response column, finally filter by chosen year
    indiData[[selector]] %>%
      select(Country, Region, Year,
             one_of(c(input$indicator, input$response))) %>%
      filter(Year == input$yearIndi)
  })
  
  output$scatt <- renderPlotly({
    # if no data is available for this indicator, show an error message
    if(length(currIndiData()[[input$indicator]]) == 0) {
      stop("No data available")
    }
    
    # create ggplot object, using geom_point, choose xlim and ylim to have
    # constant axis (better for comaprison between the years)
    g <- ggplot(currIndiData(),
                aes_string(x = input$indicator, y = input$response,
                           text = "Country", colour = "Region")) +
      xlim(c(0, max(na.omit(indiData[[input$charIndi]][[input$indicator]])))) +
      ylim(c(0, max(na.omit(indiData[[input$charIndi]][[input$response]])))) +
      geom_point()
    
    # create plotly object to have dynamic elements, like tooltip
    # and remove navigation elements with config, to have a clean plot
    gg <- ggplotly(g)
    config(gg, showLink = FALSE, sendData = FALSE,
           displaylogo = FALSE, displayModeBar = FALSE)
  })
  
  output$corr <- renderText({
    # show correlation for current data
    # if no data are available show a message instead of the correlation
    if(length(currIndiData()[[input$indicator]]) != 0) {
      cor(currIndiData()[[input$indicator]], currIndiData()[[input$response]],
          use="complete")
    } else {
      "No data available"
    }
  })
}
