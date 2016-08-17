server <- function(input, output) {
  # ----- step one ----- histogram -----
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
    # first create ggplot object, using geom_bar instead of geom_histogram
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
  
  # ----- step two ----- pie -----
  currChar <- reactive({
    if(input$timeCharBox == TRUE) {
      tmpData <- select(charData, one_of(input$timeRadBox),
                        region, one_of(input$char))
    } else {
      tmpData <- select(charData, region, one_of(input$char))
    }
    
    if(input$regionCharBox == TRUE & input$timeCharBox == TRUE) {
      filter(tmpData, region == input$regionChar & decade == input$decCha)
    } else if(input$regionCharBox == TRUE) {
      filter(tmpData, region == input$regionChar)
    } else if(input$timeCharBox == TRUE & input$timeRadBox == 'decade') {
      filter(tmpData, region == input$regionChar & decade == input$decCha)
    } else if(input$timeCharBox == TRUE & input$timeRadBox == 'iyear') {
      filter(tmpData, region == input$regionChar & iyear == input$yearChar)
    } else {
      tmpData
    }
  })
  
  output$pie <- renderPlot({
    #colNum <- 3  # number of column in data (fixed)
    #currCharLabelNum <- currCharColNum()-colNum
    data <- currChar()
    
    #if(length(data[[colNum]]) == 0) {
    #  stop("No data available")
    #}
    
    if(length(data[[input$char]]) == 0) {
      stop("No data available")
    }
    
    #missingLabels <- setdiff(charLabels[[currCharLabelNum]],
    #                         sort(unique(data[[colNum]])))
    #if(length(missingLabels) != 0) {
      #currLabels <- names(charLabels[[currCharLabelNum]][-missingLabels])
    #  currLabels <- names(subset(charLabels[[currCharLabelNum]],
    #                             !(charLabels[[currCharLabelNum]] %in% missingLabels)))
    #} else {
    #  currLabels <- names(charLabels[[currCharLabelNum]])
    #}
    
    # find missing factor labels in data and remove them from label list
    missingLabels <- setdiff(dataLabels$characteristics[[input$char]]$labels,
                             sort(unique(data[[input$char]])))
    if(length(missingLabels) != 0) {
      currLabels <-
        names(subset(dataLabels$characteristics[[input$char]]$labels,
                     !(dataLabels$characteristics[[input$char]]$labels
                       %in% missingLabels)))
    } else {
      currLabels <- names(dataLabels$characteristics[[input$char]]$labels)
    }
    
    # data for pie
    #pieces <- factor(data[[colNum]], labels = currLabels)
    #pieces <- reorder(pieces, X = pieces, FUN = function(x) -length(x))
    
    pieces <- factor(data[[input$char]], labels = currLabels)
    pieces <- reorder(pieces, X = pieces, FUN = function(x) -length(x))
    
    # calculate percentage values for pie pieces
    at <- nrow(data) - as.numeric(cumsum(sort(table(pieces)))
                                  - 0.5*sort(table(pieces)))
    perLabel <- paste0(round(sort(table(pieces))/
                               sum(table(pieces)),2) * 100,"%")
    
    # palette function for more than 9 colors
    cols <- colorRampPalette(rev(brewer.pal(9, "OrRd")))
    
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
  
  # ----- step three ----- scatter -----
  currIndiData <- reactive({
    if(input$charIndiBox == FALSE) {
      selector <- "all"
    } else {
      selector <- input$charIndi
    }
    
    indiData[[selector]] %>%
      select(Country, Region, Year,
             one_of(c(input$indicator, input$response))) %>%
      subset(Year == input$yearIndi)
  })
  
  output$scatt <- renderPlotly({
    g <- ggplot(currIndiData(),
                aes_string(x = input$indicator, y = input$response,
                           text = "Country", colour = "Region")) +
      xlim(c(0, max(na.omit(indiData[[input$charIndi]][[input$indicator]])))) +
      ylim(c(0, max(na.omit(indiData[[input$charIndi]][[input$response]])))) +
      geom_point()
    gg <- ggplotly(g)
    config(gg, showLink = FALSE, sendData = FALSE,
           displaylogo = FALSE, displayModeBar = FALSE)
  })
  
  output$corr <- renderText({
    cor(currIndiData()[[input$indicator]], currIndiData()[[input$response]],
        use="complete")
  })
}
