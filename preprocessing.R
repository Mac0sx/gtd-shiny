# ShinyApp for analysing global terrorism dataset
#
# preprocessing.R has helper function, hanlding function for the
# main management, like loading data from files or define labels
# it also defines a set of functions to prepare the datasets for
# fluid shiny handling
#
# Author: Carlo Michaelis
# License Attribution-ShareAlike 4.0 International
# Date:   Aug. 2016

# ----- helper functions -----

pasteq <- function(...) {
  # helper function to paste strings with no sep
  #
  # Args:
  #   ...: multiple strings
  #
  # Returns:
  #   the pasted string with no separation
  
  paste(..., sep = "")
}

# ----- handling functions -----

loadData <- function(prepDir = "dataset", rawDir = "raw") {
  # higher order function to load all data
  # if is already precomputed, just load the data, if not create it from raw
  #
  # Args:
  #   prepDir: directory where the prepared dataset should be
  #   rawDir: directory where the raw datasets should be
  #
  # Returns:
  #   nothing, but stores the datasets directly into global environment
  
  dataLabels <<- getDataLabels()
  
  filepath = pasteq(prepDir, "/prepared_data.RData")
  if(file.exists(filepath)) {
    load(filepath)
    freqData <<- freqData
    charData <<- charData
    indiData <<- indiData
  } else {
    rawData <- loadRawResponseFile()
    rawIndi <- loadRawIndicatorFiles()
    
    freqData <<- getFreqData(rawData)
    charData <<- getCharData(rawData, names(dataLabels$characteristics))
    indiData <<- getIndiData(rawData, rawIndi)
    
    save(freqData, charData, indiData, file = pasteq(prepDir, "/prepared_data.RData"))
  }
}

loadRawResponseFile <- function(rawDir = "raw") {
  # loads the main terrorism dataset
  #
  # Args:
  #   rawDir: directory where the dataset should be
  #
  # Returns:
  #   the raw dataset
  
  # old code for reading data from xlsx sheet
  # but because this takes much longer than reading RData,
  # this is just for historical reasons
  #
  # ###
  # dat <- read.xlsx(pasteq(getwd(), "/datasets/globalterrorismdb_0616dist.xlsx"),
  #                 sheet=1, colNames = TRUE)
  # save(dat, file = pasteq(getwd(), "/datasets/gtd.RData"))
  # ###
  
  # load data and return dataset
  # source: https://www.start.umd.edu/gtd/
  load(file = pasteq(rawDir, "/gtd.RData"))
  return(get(ls()[ls() == "dat"]))
}

loadRawIndicatorFiles <- function(rawDir = "raw") {
  # loads different datasets, which should later have the role of indicators
  #
  # Args:
  #   rawDir: directory where the datasets should be
  #
  # Returns:
  #   a list which contains the datasets
  
  # read dataset from csv, melt, select and mutate to have the preferred form
  # source: http://data.worldbank.org/indicator/SP.POP.TOTL
  pop <-
    read.csv(file = pasteq(rawDir, "/population.csv"),
             dec = ".", check.names = FALSE) %>%
    melt(id=c("country_txt","country_iso")) %>%
    select(country_iso, Year = variable, Population = value) %>%
    mutate(country_iso = as.character(country_iso),
           Year = as.integer(as.character(Year)),
           Population = as.numeric(Population))
  
  # read dataset from csv, melt, select and mutate to have the preferred form
  # http://data.worldbank.org/indicator/SP.URB.TOTL.IN.ZS
  urban <-
    read.csv(file = pasteq(rawDir, "/urban.csv"),
             dec = ",", check.names = FALSE) %>%
    melt(id=c("country_txt","country_iso")) %>%
    select(country_iso, Year = variable, Urban = value) %>%
    mutate(country_iso = as.character(country_iso),
           Year = as.integer(as.character(Year)))
  
  # read dataset from csv, melt, select and mutate to have the preferred form
  # source: http://data.worldbank.org/indicator/NY.GDP.MKTP.CD
  gdp <-
    read.csv(file = pasteq(rawDir, "/gdp.csv"),
             dec = ".", check.names = FALSE) %>%
    melt(id=c("country_txt","country_iso")) %>%
    select(country_iso, Year = variable, GDP = value) %>%
    mutate(country_iso = as.character(country_iso),
           Year = as.integer(as.character(Year)))
  
  # read dataset from csv, melt, select and mutate
  # finally create country_iso (which is not available in this dataset directly)
  # afterwards omit all data which is NA, which is necessary here, because
  # this dataset contains many empty years (1970 to 1987)
  # source: https://www.sipri.org/databases/milex
  military <-
    read.csv(file = pasteq(rawDir, "/military.csv"),
             dec = ",", check.names = FALSE) %>%
    melt(id=c("country_txt")) %>%
    select(country_txt, Year = variable, Military = value) %>%
    mutate(Year = as.integer(as.character(Year)))
  military[,"country_iso"] <- countrycode(military$country_txt,
                                          "country.name", "iso3c")
  military <- na.omit(select(military, country_iso, Year, Military))
  
  # - load dataset directly from RData file
  # - to capture uncertainty this dataset contains 100 data frames
  #   with slightly variing data, so the mean is calculated,
  #   follwing the instructions of the dataset
  # - afterwards add country_iso and omit na
  # source: http://fsolt.org/swiid/
  load(file = pasteq(rawDir, "/inequality.RData"))
  inequality <- swiid %>%
    bind_rows() %>%
    group_by(country, year) %>%
    summarize_each(funs(mean)) %>%
    ungroup() %>%
    filter(year >= 1970) %>%
    select(country_txt = country, Year = year, Gini = gini_net)
  inequality[,"country_iso"] <- countrycode(inequality$country_txt,
                                            "country.name", "iso3c")
  inequality <- na.omit(select(inequality, country_iso, Year, Gini))
  
  # return alls datasets as a list
  return(
    list(
      "pop" = pop, "urban" = urban, "gdp" = gdp,
      "military" = military, "inequality" = inequality
      ))
}

getDataLabels <- function() {
  # define data labels of the available data to control data and
  # to use it for input and output in ui
  # if data is added, this function needs an edit
  #
  # Args:
  #   no arguments
  #
  # Returns:
  #   all labels as a list
  
  # build named vector with avaialable regions and order by name
  availableRegions <-
    setNames(1:12, c("North America", "Central America & Caribbean",
                     "South America", "East Asia", "Southeast Asia",
                     "South Asia", "Central Asia", "Western Europe",
                     "Eastern Europe", "Middle East & North Africa",
                     "Sub‐Saharan Africa", "Australasia & Oceania"))
  availableRegions <- availableRegions[order(names(availableRegions))]
  
  # build named vector with available decades
  availableDecades <- c("1970s" = 1970, "1980s" = 1980, "1990s" = 1990,
                        "2000s" = 2000, "2010s" = 2010)
  
  # build named vector with used indicators
  availableIndicators <- c("Population" = "Population",
                           "Urban Percantage" = "Urban",
                           "GDP" = "GDP",
                           "Military Expenditure" = "Military",
                           "Inequality" = "Gini")
  
  # build named vector with used characteristics and their labels
  availableCharacteristics <-
    list(
      "attacktype1" =
        list("name" = "Type of attack",
             "labels" =
               setNames(1:9, c("Assassination", "Hijacking", "Kidnapping",
                               "Barricade Incident", "Bombing/Explosion",
                               "Armed Assault", "Unarmed Assault",
                               "Facility/Infrastructure Attack", "Unknown"))),
      "success" =
        list("name" = "Success of attack",
             "labels" = setNames(0:1, c("No", "Yes"))),
      "suicide" =
        list("name" = "Attack with suicide",
             "labels" = setNames(0:1, c("No", "Yes"))),
      "weaptype1" =
        list("name" = "Type of weapon",
             "labels" =
               setNames(1:13, c("Biological", "Chemical", "Radiological",
                                "Nuclear", "Firearms", "Explosives",
                                "Fake Weapons", "Incendiary", "Melee", "Vehicle",
                                "Sabotage Equipment", "Other", "Unknown"))),
      "targtype1" =
        list("name" = "Type of victim",
             "labels" =
               setNames(1:22, c("Business", "Governmental (General)", "Police",
                                "Military", "Abortion Related",
                                "Airports & Aircrafts", "Government (Diplomatic)",
                                "Educational Institution", "Food & Water Supply",
                                "Journalists & Media", "Maritime", "NGO",
                                "Other", "Citizens & Propoerty",
                                "Religoius Figures & Insitutions",
                                "Telecommunication", "Terrorists & Militia",
                                "Tourists", "Other Transportation", "Unknown",
                                "Utilities", "Violent Political Parties"))),
      "nperps" =
        list("name" = "Number of attackers",
             "labels" =
               setNames(c(0:5,99),
                        c("0", "1", "2", "3", "4", "5 or more", "Unknown"))),
      "claimed" =
        list("name" = "Claimed attack",
             "labels" = setNames(c(0:1,99), c("No", "Yes", "Unknown"))),
      "INT_ANY" =
        list("name" = "International attack",
             "labels" = setNames(c(0:1,99), c("No", "Yes", "Unknown"))))
  
  # return all available labels as list
  return(
    list(
      "regions" = availableRegions,
      "decades" = availableDecades,
      "indicators" = availableIndicators,
      "characteristics" = availableCharacteristics
    )
  )
}

# ----- data preparation functions -----

getFreqData <- function(data) {
  # prepare frequency data to show frequencies of incidents
  # and number of victims per incident in later use
  #
  # Args:
  #   data: the data where incidence and victim counts are calculated with
  #         argument has to be the whole or part of the global terrorism dataset
  #
  # Returns:
  #   the prepared datasets as a list
  
  incidents <- data %>%
    select(region, Year = iyear) %>%
    group_by(region, Year) %>%
    tally() %>%
    ungroup() %>%
    rename(Incidents = n)
  
  incidentsAll <- data %>%
    select(Year = iyear) %>%
    group_by(Year) %>%
    tally() %>%
    ungroup() %>%
    rename(Incidents = n)
  
  victims <- data %>%
    select(region, Year = iyear, nkill) %>%
    group_by(region, Year) %>%
    na.omit() %>%
    tally(wt = nkill) %>%
    ungroup() %>%
    rename(Victims = n)
  
  victimsAll <- data %>%
    select(Year = iyear, nkill) %>%
    group_by(Year) %>%
    na.omit() %>%
    tally(wt = nkill) %>%
    ungroup() %>%
    rename(Victims = n)
  
  # return a list with processed data and max values
  return(list(
    "Incidents" = list("data" = incidents, "max" = max(incidents)),
    "IncidentsAll" = list("data" = incidentsAll, "max" = max(incidentsAll)),
    "Victims" = list("data" = victims, "max" = max(victims)),
    "VictimsAll" = list("data" = victimsAll, "max" = max(victimsAll))
  ))
}

getCharData <- function(data, selection) {
  # prepare characteristics dataset to filter incidents by their
  # chacteristics later
  #
  # Args:
  #   data: the data where chararcteristics are extracted from
  #         argument has to be the whole or part of the global terrorism dataset
  #   selection: vector of all characteristics of interest
  #         caution: possibly this function will not work for all
  #         characteristics and it's possibly necessary to edit the labels
  #
  # Returns:
  #   a dataset which is ready to filter incidents by characteristic
  
  yearToDecade <- function(year) {
    # helper function to transform years to decades (used below)
    #
    # Args:
    #   year: a year which should be transformed to decade
    #
    # Returns:
    #   the decade, where the year is in
    
    # split the year into a vector of single numbers
    vec <- as.numeric(strsplit(as.character(year), "")[[1]])
    
    # combine these numbers to a whle year again,
    # while replacng the last digit by 0
    comb <- paste(append(head(vec, length(vec)-1), 0), collapse = "")
    
    # return the decade as numberic
    return(as.numeric(comb))
  }
  
  # select the selection (argument) and add decade with "yearToDecade" function
  if("nperps" %in% selection) {
    # if nperps is part of the selection it needs another transformation where
    # numbers are summarised in categories
    charData <- data %>%
      select(iyear, region, one_of(selection)) %>%
      mutate(nperps = ifelse(nperps >= 5, 5, nperps)) %>%
      rowwise() %>%
      mutate(decade = yearToDecade(iyear))
  } else {
    charData <- data %>%
      select(iyear, region, one_of(selection)) %>%
      rowwise() %>%
      mutate(decade = yearToDecade(iyear))
  }
  
  # change order
  #charData <- charData[c(1,11,2:10)]  # obsolete ???
  
  # some corretions of NA's
  charData[charData == -9] <- 99  # no data = unknown
  charData[charData == -99] <- 99  # no data = unknown
  charData[is.na(charData)] <- 99  # no data = unknown
  if("claimed" %in% selection) {
    # correction of error in data
    charData$claimed[charData$claimed == 2] <- 99 
  }
  
  return(charData)
}

getIndiData <- function(data, indicators, characteristics =
                          c("INT_ANY", "success", "suicide")) {
  # prepare indicators/response dataset to find out relations later
  # one dataset contains all information, the other datasets
  # (if characteristics argument is chosen) contain filtered data
  #
  # Args:
  #   data: the data where responses are extracted from
  #         argument has to be the whole or part of the global terrorism dataset
  #   indicators: a list of datasets where indicators are extracted from
  #         beside the data column they need to contain "country_iso" and "Year"
  #   characteristics: for filtering indicators/response data
  #         just characteristics with "yes", "no", "unknown" levels are possible,
  #         where "yes" is coded with "1"
  #
  # Returns:
  #   a list of multiple datasets, one for all data and the others
  #   with filtered data
  
  getIncidents <-
    function(data, characteristic = NULL,
             selection = c("country_iso", "region_num", "Region", "Year")) {
      # calulates the number of incidents for given data
      #
      # Args:
      #   data: the data where responses are extracted from
      #         argument has to be the whole or part of
      #         the global terrorism dataset
      #   characteristic: characteristic to group by
      #   selection: selected columns to group by
      #
      # Returns:
      #   the dataset with the number of incidents
      
      selection = c(selection, characteristic)
      return(
        data %>%
          select_(.dots = selection) %>%
          group_by_(.dots = selection) %>%
          tally() %>%
          ungroup() %>%
          rename(Incidents = n)
      )
    }
  
  getVictims <-
    function(data, characteristic = NULL,
             selection = c("country_iso", "region_num", "Region", "Year")) {
      # calulates the number of victims for given data
      #
      # Args:
      #   data: the data where responses are extracted from
      #         argument has to be the whole or part of
      #         the global terrorism dataset
      #   characteristic: characteristic to group by
      #   selection: selected columns to group by
      #
      # Returns:
      #   the dataset with the number of victims
      
      return(
        data %>%
          select_(.dots = c(selection, characteristic, "nkill")) %>%
          group_by_(.dots = c(selection, characteristic)) %>%
          tally(wt = nkill) %>%
          ungroup() %>%
          rename(Victims = n)
      )
    }
  
  joinNPerCountry <-
    function(data, characteristic = NULL,
             by = c("country_iso", "Year", "region_num", "Region")) {
      # higher order function using "getIncidents" and "getVictims"
      # full_join incidents count and victims count to one dataset
      #
      # Args:
      #   data: the data where responses are extracted from
      #         argument has to be the whole or part of
      #         the global terrorism dataset
      #   characteristic: characteristic to group by
      #   by: selected columns to group by
      #
      # Returns:
      #   joined dataset with number of incidents and victims
      
      return(full_join(getIncidents(data, characteristic),
                       getVictims(data, characteristic),
                       by = c(by, characteristic)))
    }
  
  # select everything necessary from dataset
  # country/region information, number of victims (nkill) and characteristics
  responseSelect <- select(data, country_txt, region_num = region,
                           Region = region_txt, Year = iyear, nkill,
                           one_of(characteristics))
  
  # get country iso3 values for later join (to have unambiguous names)
  responseSelect[,"country_iso"] <- countrycode(responseSelect$country_txt,
                                                "country.name", "iso3c")
  
  # omit missing values
  # further calculations is not possible with missing values
  responseSelect <- na.omit(responseSelect)
  
  # combine all responses using "joinNPerCountry" function
  # create a list and add responses without characterstic
  responsesJoined <- list("all" = joinNPerCountry(responseSelect))
  # add all responses which are restricted to characteristcs
  for(i in 1:length(characteristics)) {
    responsesJoined[[characteristics[i]]] <-
      filter_(joinNPerCountry(responseSelect , characteristics[i]),
              paste(characteristics[i], "==", 1))
  }
  
  # combine all incidators with full_join
  indicatorsJoined <- indicators[[1]]
  if(length(indicators) > 1) {
    for(i in 2:length(indicators)) {
      indicatorsJoined <- full_join(indicatorsJoined, indicators[[i]],
                                    by=c("country_iso","Year"))
    }
  }
  
  # do right_join (not full_join) to combine indicators and responses
  # because indicators have some more information then responses
  # especially indicators contain data from grouped countries
  # like eu, oecd or the whole world which are not necessary
  indiData <- lapply(responsesJoined, right_join, x = indicatorsJoined,
                     by = c("country_iso", "Year"))
  
  # add country names from country iso to have human readable country identifier
  indiData <- lapply(indiData, mutate,
                     Country = countrycode(country_iso,
                                           "iso3c", "country.name"))
  
  # return the baby
  return(indiData)
}
