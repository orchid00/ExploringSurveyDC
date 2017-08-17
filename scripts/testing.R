

datasmall <- read.csv(file = "~/paula/DataSoftwareCarpentry/Survey/Data/data.csv")
str(datasmall)
View(datasmall)


plot(datasmall$Country, las = 2 )

# ggplot2

# install.packages("ggplot2")
# library("ggplot2")

##################################################################v  
# Option 1

# install.packages("extrafont")
# library(extrafont)
# font_import(pattern="[C/c]omic")
# loadfonts(device="win")


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