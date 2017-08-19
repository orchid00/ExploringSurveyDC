
# Dowloaded file from repo https://github.com/carpentries/assessment-projects/blob/master/joint-carpentry-projects/data.csv

datasmall <- read.csv(file = "data/data.csv")
#str(datasmall)
dim(datasmall)
View(datasmall)

plot(datasmall$Country, las = 2 )

# clean NA
levels(datasmall$Country)
levels(datasmall$Country)[1] <- "No Answer"
levels(datasmall$Country)
datasmall$Country <- factor(datasmall$Country, levels = rev(levels(datasmall$Country)))
levels(datasmall$Country)
# ggplot2

# install.packages("ggplot2")
library("ggplot2")

##################################################################v  
# Option 1

install.packages("extrafont")
library(extrafont)
font_import(pattern="[C/c]omic") # y
loadfonts(device="win")



ggplot(data = datasmall, aes( x = Country )) +
  coord_flip() +
  geom_bar(fill = "tomato" ) +
  theme_light() +
  ggtitle( "Surveys per country" ) +
  theme(text=element_text(size=12, family="Comic Sans MS"))
####################################################################



####################################################################
# Option 2
font_import(pattern="[H/h]umor")
loadfonts(device="win")

theme_xkcd <- theme(
  panel.background = element_rect(fill = "white"),
  #axis.ticks = element_line(colour = NA),
  panel.grid = element_line(colour="white"),
  #axis.text.y = element_text(colour = NA),
  axis.text.x = element_text(colour="black"),
  text = element_text(size=12, family = "Humor Sans")
)

p <- ggplot(data = datasmall, aes( x = Country )) +
  coord_flip() +
  geom_bar(fill = "tomato" ) +
  theme_light() +
  ggtitle( "Surveys per country" ) +
  theme_xkcd  
p
####################################################################