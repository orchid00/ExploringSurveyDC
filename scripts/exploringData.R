#! /usr/bin/env Rscript

# Author: Paula Andrea Martinez
# Date: 2017, August 09
# Version 0.1
# Analyzing pre and post survey results from Data Carpentry
# Repo: https://github.com/carpentries/assessment-projects


#####################################
# download files

download.file("https://raw.githubusercontent.com/carpentries/assessment-projects/master/data-carpentry-projects/postworkshop-public-archived.csv", 
               destfile = "./Data/postworkshopDC.csv", method = "wininet")


download.file("https://raw.githubusercontent.com/carpentries/assessment-projects/master/data-carpentry-projects/preworkshop-public-archived.csv", 
              destfile = "./Data/preworkshopDC.csv", method = "wininet")

######################################
# Exploring the dataset

Exploring <- function(filecsv){
    ps <- read.csv(filecsv)
    print(paste("Data contains", 
                 dim(ps)[1], "rows and", 
                 dim(ps)[2], "columns")) 

    colnames_ps <- colnames(ps)
    print(colnames_ps)
    return(ps)
}

# Calling exploring 
postworkshop <- Exploring("./Data/postworkshopDC.csv") 
preworkshop <- Exploring("./Data/preworkshopDC.csv")  

# files sent my email have more colums
Epostworkshop <- Exploring("./Data/postworkshop-public-archived.csv") 
Epreworkshop <- Exploring("./Data/preworkshop-public-archived.csv") 

colnames(Epreworkshop)

levels(Epreworkshop$Gender)
####Epreworkshop$Gender.Other ### only NA
####levels(Epreworkshop$Gender.Other)
#class(Epreworkshop$Gender.Other)  # logical
#levels(Epreworkshop$Status.Other) # 87 options
levels(Epreworkshop$Status)
##########################################################
#Update blank cells to No Answer
levels(Epreworkshop$Gender)[1] <- "No Answer"
levels(Epreworkshop$Status)[1] <- "No Answer"
levels(Epreworkshop$Status)[5] <- "Other"

#check
levels(Epreworkshop$Gender)
levels(Epreworkshop$Status)
## reorder levels for factors
reorderLevels <- function(x, vec){
  x <- factor(x,levels(x)[vec])
  print(levels(x))
  return(x)
}

g <- Epreworkshop$Gender
levels(g)
g <- reorderLevels(g, c(2,3,4,1))
Epreworkshop$Gender <- g
levels(Epreworkshop$Gender)

Epreworkshop$Status <- reorderLevels(Epreworkshop$Status, c(8,3,6,2,7,4,5,1))

#postworkshop cleaning data
levels(Epostworkshop$Gender)
levels(Epostworkshop$Status)

levels(Epostworkshop$Gender)[1] <- "No Answer"
levels(Epostworkshop$Status)[1] <- "No Answer"
levels(Epostworkshop$Status)[5] <- "Other"

#check
levels(Epostworkshop$Gender)
levels(Epostworkshop$Status)

# Postworkshop Status has one extra level "Response" changing this to Other
rm(g)
g <- Epostworkshop$Status
levels(g)
levels(g)[7] <- "Other"
levels(g)
Epostworkshop$Status <- g

Epostworkshop$Gender <- reorderLevels(Epostworkshop$Gender, c(2,3,4,1))
levels(Epostworkshop$Gender)
Epostworkshop$Status <- reorderLevels(Epostworkshop$Status, c(8,3,6,2,7,4,5,1))
levels(Epostworkshop$Status)
#############################


######################################
#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
###############################################################################
#Colour Palette
# The palette with grey:
cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#999999","#E69F00", "#D55E00", "#CC79A7")
# The palette with black:
cbbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2","#000000", "#E69F00", "#D55E00", "#CC79A7")
# To use for fills, add
scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
scale_colour_manual(values=cbPalette)
######################################
# plotting
#install.packages("ggplot2")
library("ggplot2")

plotByGender <- function(df, ti){ 
  ps <- ggplot(data = df, aes( x = Gender, fill = Gender )) +
    #coord_flip() +
    geom_bar(aes(y = (..count..)/sum(..count..)) ) +
    scale_fill_manual(values = cbPalette) +
    theme_light() +
    labs(x = "Gender", y = paste("Total respondents", dim(df)[1], "in percentage ")) +
    ggtitle(paste(ti, "responses by gender") ) +
    scale_y_continuous(labels = scales::percent) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="bottom",
          legend.title = element_blank())
  #print(ps)
}
#plotByGender(Epreworkshop, "Pre- survey")

multiplot(plotByGender(Epreworkshop, "Pre- survey"), 
          plotByGender(Epostworkshop, "Post- survey"), cols=2)


# ps <- ggplot(data = Epreworkshop, aes( x = Gender, fill= Gender)) +
#   coord_flip() +
#   geom_bar( ) +
#   theme_light() +
#   ggtitle(paste("Pre", "Responses by Gender and Status")) +
#   theme(axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         legend.position="bottom") +
#   facet_grid(. ~ Status)
# print(ps)


# ps <- ggplot(data = Epreworkshop, aes( x = Gender, fill= Gender)) +
#   #coord_flip() +
#   geom_bar( ) +
#   theme_light() +
#   ggtitle(paste("Pre", "Responses by Gender and Status")) +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         legend.position="bottom") +
#   facet_grid(. ~ Status)
# print(ps)

plotByGenderStatus <- function(df, ti){
  ps <- ggplot(data = df, aes( x = Gender, fill= Gender)) +
    #coord_flip() +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_fill_manual(values = cbPalette) +
    theme_light() +
    labs(x = "Gender", y = paste("Total respondents", dim(df)[1], "in percentage ")) +
    ggtitle(paste(ti, "responses by gender and status")) +
    scale_y_continuous(labels = scales::percent) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",
        legend.title = element_blank())+
    facet_grid(. ~ Status)
  print(ps)
  ggsave(filename =  paste("./plots/GendervsStatus-",ti,".png", sep = ""), 
         width = 12, height = 6, dpi = 120)
}

plotByGenderStatus(Epreworkshop, "Pre-survey")
plotByGenderStatus(Epostworkshop, "Post-survey")

########################################################
#excluding "Prefer not to say and No answer from Gender and Status"

ExcludeNANotGiven <- function(df){
  newpredf <- data.frame(Gender = factor(df$Gender), 
                  Status = factor(df$Status))
  print(head(newpredf))
  print(str(newpredf))
  print(table(newpredf$Gender))
  print(table(newpredf))
  x <- subset(newpredf, newpredf$Gender == "Male" |newpredf$Gender == "Female", drop = T) 
  x <- subset(x, x$Status != "No Answer", drop = T) 
  print(table(x))
  y <- droplevels(x)
  print(table(y))
  print(dim(y))
  return(y) 
}
newpreGS <- ExcludeNANotGiven(Epreworkshop)
newpostGS <- ExcludeNANotGiven(Epostworkshop)
plotByGenderStatus(newpreGS, "Pre-survey filtered")
plotByGenderStatus(newpostGS, "Post-survey filtered")
