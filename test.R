str(mtcars)

ggplot(mtcars, aes(x = factor(1), fill = factor(mtcars[[2]]))) +
  geom_bar(width = 1) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
      axis.ticks.x=element_blank(), axis.title.y=element_blank(),
      axis.text.y=element_blank(), axis.ticks.y=element_blank(),
      legend.title = element_blank(),
      panel.background=element_rect(fill="transparent",colour=NA)) +
  coord_polar(theta = "y")


League<-c("A","B","A","C","D","E","A","E","D","A","D")
data<-data.frame(League) # I have more variables 

data$League <- reorder(data$League, X = data$League, FUN = function(x) -length(x))

at <- nrow(data) - as.numeric(cumsum(sort(table(data)))-0.5*sort(table(data)))

label=paste0(round(sort(table(data))/sum(table(data)),2) * 100,"%")

cols <- colorRampPalette(rev(brewer.pal(9, "OrRd")))
myPal <- cols(length(unique(League)))

ggplot(data,aes(x="", fill = League)) +
  geom_bar(width = 1) +
  coord_polar(theta="y") +
  theme(panel.grid.major = element_line(colour="gray", size=0.5),
        panel.grid.minor = element_line(colour="gray", size=0.5)) +
  #scale_fill_brewer(palette = "OrRd", direction = -1) +
  #scale_fill_manual(values = myPal) +
  #annotate(geom = "text", y = at, x = 1.25, label = label)
tt <- c(0)
names(subset(charLabels[[8]], !(charLabels[[8]] %in% tt)))

g <- ggplot(filter(indiData, indiData$Year == 2015),
       aes(x = Population, y = Incidents, colour = Region, text = paste("Country:", Country))) +
  geom_point()
gg <- ggplotly(g)
config(gg, showLink = FALSE, sendData = FALSE,
       displaylogo = FALSE, displayModeBar = FALSE)


ggplot(iris, aes(x=Sepal.Length)) + geom_histogram() + 
  theme(panel.grid.minor = element_line(colour="black", size=0.5)) + 
  scale_x_continuous(minor_breaks = seq(1, 10, 1))

