# analysis.R file

# A function that takes a dataframe and two column names, drops the first
#   column and create a type column filled with the second column name,
#   summarise by state, and return the mutated dataframe
DropAddMutate <- function(data, drop.col, add.col) {
  data[, colnames(data) != drop.col] %>%
    MonthMutateGroup() %>% summarise(
    casualties = sum(!!sym(add.col))
  ) %>% with(
    ., aggregate(
      x = list(casualties = casualties),
      by = list(month = factor(
        month,
        levels = c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
        )
      )),
      FUN = sum
    )
  ) %>% mutate(type = add.col)
}

# A function that takes in a dataframe and returns a barplot
FetchBarplot <- function(data) {
  state.summary <- rbind(
    DropAddMutate(data, 'injured', 'killed'),
    DropAddMutate(data, 'killed', 'injured')
  )
  ggplot(data = state.summary, aes(
    x = month, y = casualties, fill = type)
  ) + geom_bar(stat="identity") +
    ggtitle("US Shootings Casualty Barplot By Month In 2017")
}

# A function that takes in a dataframe and returns a info package
FetchInfoPkg <- function(data, n) {
  casualty <- GetCasualtyDF(data)
  abbr.casualty <- GetAbbrCasualtyDF(casualty, n)
  list(
    total.killed = sum(data$killed),
    total.injured = sum(data$injured),
    casualty.df = casualty,
    top.states = casualty %>% filter(
      casualty >= 100
    ) %>% .[['state']],
    top.pct = 100 - round(
      (abbr.casualty %>% filter(
        state == 'Other'
      ) %>% .[['casualty']]) / sum(abbr.casualty$casualty), 3
    ) * 100,
    st.nd.times = round(
      abbr.casualty$casualty[1] / abbr.casualty$casualty[2]
    ),
    leat.casulty.state = casualty %>% filter(
      casualty == min(casualty)
    ),
    most.death.month = data %>% MonthMutateGroup() %>% summarise(
      killed = sum(killed)
    ) %>% filter(killed == max(killed)),
    most.death.city = data %>% group_by(city) %>% summarise(
      killed = sum(killed),
      injured = sum(injured)
    ) %>% filter(killed == max(killed)),
    mass.shooting.date = data %>% filter(
      city == 'Las Vegas'
    ) %>% .[['date']]
  )
}

# A function that takes in a dataframe and return a map
FetchMap <- function(data) {
  data <- data %>% mutate(mag = (injured * 0.1 + killed))
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = GetMapColor(data$mag)
  )
  return(
    leaflet(data) %>%
      addTiles() %>%
      addAwesomeMarkers(
        ~lng, ~lat,
        icon = icons,
        label = paste0("Magnitude: ", data$mag),
        clusterOptions = markerClusterOptions(),
        popup = paste0(
          "Date: ", data$date,
          "<br>City: ", data$city,
          "<br>State: ", data$state,
          "<br>Killed: ", data$killed,
          "<br>Injured: ", data$injured
        )
      )
  )
}

# A function that takes a dataframe and an integer, and returns 
#   a pie chart of each state's percentage of shootings incidents
FetchPiechart <- function(data, n) {
  data %>% GetCasualtyDF() %>% GetAbbrCasualtyDF(n) %>% plot_ly(
    labels = ~state,
    values = ~casualty,
    textfont = list(color = '#FFFFFF'),
    textinfo = 'label+percent',
    text = ~paste0(
      "State: ", .[['state']],
      "<br>Casualty: ", .[['casualty']]
    ),
    hoverinfo = 'text+percent'
  ) %>% add_pie(hole = 0.6) %>% layout(
    title = "US Shootings Casualty Percentages By State In 2017",
    showlegend = FALSE,
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
  )
}

# A function that takes in a dataframe and returns the
#   summarised casualty dataframe by state
GetCasualtyDF <- function(data) {
  data %>% group_by(state) %>% summarise(
    casualty = sum(injured) + sum(killed)
  ) %>% arrange(-casualty)
}

# A function that takes in a dataframe and an integer n, and returns
#   the abbreviated casualty dataframe by combining the states other
#   than the first n ones together
GetAbbrCasualtyDF <- function(data, n) {
  data.frame(
    state = c(data$state[1:n], 'Other'),
    casualty = c(
      data$casualty[1:n],
      sum(data$casualty[(n + 1):nrow(data)])
    )
  )
}

# A function that takes in a magnitude and returns a corresponding color
GetMapColor <- function(mag) {
  sapply(
    mag, function(mag) {
      if (mag <= 5) { "green" }
      else if (mag <= 10) { "orange" }
      else { "red" }
    }
  )
}

# A function that takes a dataframe, reformat the date,
#   and group by month, and returns the dataframe
MonthMutateGroup <- function(data) {
  data %>% mutate(
    month = format(as.Date(.[['date']], '%B %d, %Y'), '%b')
  ) %>% group_by(month)
}
