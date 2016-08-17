# contains helpful functions and objects



getDataLabels <- function() {
  # build named vector with avaialable refions and order by name
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

# 
# availableCharacteristics <- c("Type of attack" = "attacktype1",
#                               "Success of attack" = "success",
#                               "Attack with suicide" = "suicide",
#                               "Type of weapon" = "weaptype1",
#                               "Type of victim" = "targtype1",
#                               "Number of attackers" = "nperps",
#                               "Claimed attack" = "claimed",
#                               "International attack" = "INT_ANY")
# 
# charLabels <- list(
#   setNames(1:9, c("Assassination", "Hijacking", "Kidnapping",
#                   "Barricade Incident", "Bombing/Explosion", "Armed Assault",
#                   "Unarmed Assault", "Facility/Infrastructure Attack",
#                   "Unknown")),
#   setNames(0:1, c("No", "Yes")),
#   setNames(0:1, c("No", "Yes")),
#   setNames(1:13, c("Biological", "Chemical", "Radiological", "Nuclear",
#                    "Firearms", "Explosives", "Fake Weapons", "Incendiary",
#                    "Melee", "Vehicle", "Sabotage Equipment", "Other",
#                    "Unknown")),
#   setNames(1:22, c("Business", "Governmental (General)", "Police",
#                    "Military", "Abortion Related", "Airports & Aircrafts",
#                    "Government (Diplomatic)", "Educational Institution",
#                    "Food & Water Supply", "Journalists & Media", "Maritime",
#                    "NGO", "Other", "Citizens & Propoerty",
#                    "Religoius Figures & Insitutions", "Telecommunication",
#                    "Terrorists & Militia", "Tourists", "Other Transportation",
#                    "Unknown", "Utilities", "Violent Political Parties")),
#   setNames(c(0:5,99), c("0", "1", "2", "3", "4", "5 or more", "Unknown")),
#   setNames(c(0:1,99), c("No", "Yes", "Unknown")),
#   setNames(c(0:1,99), c("No", "Yes", "Unknown")))