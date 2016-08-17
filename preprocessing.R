
rawData <- loadRawFiles()
freqData <- getFreqData(rawData$dat)
charData <- getCharData(rawData$dat, names(dataLabels$characteristics))

loadRawFiles <- function() {
  # Load all datasets in environment ---------------------------
  
  #dat <- read.xlsx(pasteq(getwd(), "/datasets/globalterrorismdb_0616dist.xlsx"),
  #                 sheet=1, colNames = TRUE)
  #save(dat, file=pasteq(getwd(), "/datasets/gtd.RData"))
  
  load(file = pasteq(getwd(), "/datasets/gtd.RData"))
  
  pop <-
    read.csv(file = pasteq(getwd(), "/datasets/population.csv"),
             dec = ".", check.names = FALSE) %>%
    melt(id=c("country_txt","country_iso")) %>%
    select(country_iso, Year = variable, Population = value) %>%
    mutate(country_iso = as.character(country_iso),
           Year = as.integer(as.character(Year)), Population = as.numeric(Population))
  
  urban <-
    read.csv(file = pasteq(getwd(), "/datasets/urban.csv"),
             dec = ",", check.names = FALSE) %>%
    melt(id=c("country_txt","country_iso")) %>%
    select(country_iso, Year = variable, Urban = value) %>%
    mutate(country_iso = as.character(country_iso),
           Year = as.integer(as.character(Year)))
  
  gdp <-
    read.csv(file = pasteq(getwd(), "/datasets/gdp.csv"),
             dec = ".", check.names = FALSE) %>%
    melt(id=c("country_txt","country_iso")) %>%
    select(country_iso, Year = variable, GDP = value) %>%
    mutate(country_iso = as.character(country_iso),
           Year = as.integer(as.character(Year)))
  
  military <-
    read.csv(file = pasteq(getwd(),"/datasets/military.csv"),
             dec = ",", check.names = FALSE) %>%
    melt(id=c("country_txt")) %>%
    select(country_txt, Year = variable, Military = value) %>%
    mutate(Year = as.integer(as.character(Year)))
  military[,"country_iso"] <- countrycode(military$country_txt,
                                          "country.name", "iso3c")
  inequality <- na.omit(select(military, country_iso, Year, Military))
  
  load(file = pasteq(getwd(), "/datasets/inequality.RData"))
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
  
  return(
    list(
      "dat" = dat, "pop" = pop, "urban" = urban, "gdp" = gdp,
      "military" = military, "inequality" = inequality
      ))
}

# Prepare bar plot dataset (#incidents / #victims) ---------------------------

getFreqData <- function(data) {
  # prepare frequency data for incidents and victims
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

# Prepare pie plot dataset (characteristics) ---------------------------

getCharData <- function(data, selection) {
  # define function to transform years to decades (used below)
  yearToDecade <- function(year) {
    vec <- as.numeric(strsplit(as.character(year), "")[[1]])
    comb <- paste(append(head(vec, length(vec)-1), 0), collapse = "")
    return(as.numeric(comb))
  }
  
  # build dataset for pie plot with characteristics
  if("nperps" %in% selection) {
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
  charData <- charData[c(1,11,2:10)]  # obsolete ???
  
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

# Prepare scatter plot dataset (indicators) ---------------------------

#getIndiData <- function(data, indicators, ) {
#  
#}

responseSelect <- select(rawData$dat, country_txt, region_num = region,
                         Region = region_txt, Year = iyear, nkill,
                         INT_ANY, success, suicide)
responseSelect[,"country_iso"] <- countrycode(responseSelect$country_txt,
                                        "country.name", "iso3c")
responseSelect <- na.omit(responseSelect)

getIncPerCountry <-
  function(data, characteristic = NULL,
           selection = c("country_iso", "region_num", "Region", "Year")) {
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

getVicPerCountry <-
  function(data, characteristic = NULL,
           selection = c("country_iso", "region_num", "Region", "Year")) {
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
    return(full_join(getIncPerCountry(data, characteristic),
                     getVicPerCountry(data, characteristic),
                     by = c(by, characteristic)))
  }

responses <- list("all" = joinNPerCountry(responseSelect),
                  "INT_ANY" = filter(joinNPerCountry(
                    responseSelect ,"INT_ANY"), INT_ANY == 1),
                  "success" = filter(joinNPerCountry(
                    responseSelect ,"success"), success == 1),
                  "suicide" = filter(joinNPerCountry(
                    responseSelect ,"suicide"), suicide == 1))

indicators <-
  full_join(rawData$pop, rawData$urban, by=c("country_iso","Year")) %>%
  full_join(rawData$gdp, by=c("country_iso","Year")) %>%
  full_join(rawData$military, by=c("country_iso","Year")) %>%
  full_join(rawData$inequality, by=c("country_iso","Year"))

# delete all rows with special data
# it does not fit to terrorism data set
indicators <- filter(indicators, country_iso != "WLD", country_iso != "IBT",
                   country_iso != "IDA", country_iso != "IDB",
                   country_iso != "IDX", country_iso != "OED",
                   country_iso != "LMC", country_iso != "LMY",
                   country_iso != "MIC", country_iso != "IBD",
                   country_iso != "EAR", country_iso != "UMC",
                   country_iso != "EAS", country_iso != "LTE",
                   country_iso != "EAP", country_iso != "EMU",
                   country_iso != "EUU", country_iso != "TEA",
                   country_iso != "TEC", country_iso != "ARB",
                   country_iso != "ECA", country_iso != "ECS",
                   country_iso != "HPC", country_iso != "PRE",
                   country_iso != "SAS", country_iso != "TSA")

indiData <- lapply(responses, right_join, x = indicators,
                   by = c("country_iso", "Year"))
indiData <- lapply(indiData, mutate,
                   Country = countrycode(country_iso, "iso3c", "country.name"))

# clean memory from temporary objects
#rm(swiid, pop, urban, gdp, military, inequality)
#rm(responseSelect, responses, indicators)
#rm(dat)

