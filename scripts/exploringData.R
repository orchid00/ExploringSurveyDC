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

multiplot(plotByGender(newpreGS, "Pre- survey"), 
          plotByGender(newpostGS, "Post- survey"), cols=2)

######
colnames(Epreworkshop)
head(Epreworkshop[,1:2])
class(Epreworkshop[,1])

levels(Epreworkshop$Race)
table(Epreworkshop$Race)


plotByRace <- function(df, ti){ 
  ps <- ggplot(data = df, aes( x = Race, fill = Race )) +
    #coord_flip() +
    geom_bar(aes(y = (..count..)/sum(..count..)) ) +
    scale_fill_manual(values = cbPalette) +
    theme_light() +
    labs(x = "Race", y = paste("Total respondents", dim(df)[1], "in percentage ")) +
    ggtitle(paste(ti, "responses by race") ) +
    scale_y_continuous(labels = scales::percent) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="bottom",
          legend.title = element_blank())
  print(ps)
}

plotByRace(Epreworkshop, "Pre- survey")

dim(Epreworkshop)
dim(subset(Epreworkshop, Race != ""))
a <- subset(Epreworkshop, Race != "")
plotByRace(a, "Pre- survey")

colnames(Epreworkshop)
table(Epreworkshop$Gender)
table(Epreworkshop$With.Friend)
table(Epreworkshop$Gender, Epreworkshop$With.Friend)




#############################################
plotByGenderGeneric <- function(df, ti, colna, colstr){
  #StartbyFiltering Gender Not Answered or not provided
  print(dim(df))
  x <- subset(df, df$Gender == "Male" | df$Gender == "Female") 
  x <- droplevels(x)
  print(dim(x))
  print(colna)
  y <- droplevels(subset(x, x[[colna]] != ""))
  print(dim(y))
  print(table(y[[colna]]))
  #####Plot Function Generic
  ps <- ggplot(data = y, aes( x = Gender, fill= Gender)) +

    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_fill_manual(values = cbPalette) +
    theme_light() +
    labs(x = "Gender", y = paste("Total respondents", dim(y)[1], "in percentage ")) +
    ggtitle(paste(ti, "responses by gender and", colstr)) +
    scale_y_continuous(labels = scales::percent) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="bottom",
          legend.title = element_blank())+
    facet_grid(reformulate(colna), ".") # . ~ colna
  print(ps)
  ggsave(filename =  paste("./plots/FilteredGendervs", gsub("\\s", "",colstr),
                           "-",ti,".png", sep = ""),
         width = 12, height = 6, dpi = 120)
}

#plotByGenderGeneric <- function(df, ti, colna, colstr)
plotByGenderGeneric(Epreworkshop, "Pre-survey", "With.Friend" , "attended the workshop with a friend")
plotByGenderGeneric(Epreworkshop, "Pre-survey", "Discipline" , "discipline")
plotByGenderGeneric(Epreworkshop, "Pre-survey", "OS" , "operative system")
plotByGenderGeneric(Epreworkshop, "Pre-survey", "Programming.Usage" , "programming usage")

### I'm not sure why there is 7 columns for current tools "Current.Tools.1" to "Current.Tools.7"

plotByGenderGeneric(Epreworkshop, "Pre-survey", "Have.Dataset" , "have dataset")
plotByGenderGeneric(Epreworkshop, "Pre-survey", "Data.Organization" , "importance of data organization")
plotByGenderGeneric(Epreworkshop, "Pre-survey", "Using.Scripting.Language" , "importance of using scripting language")
plotByGenderGeneric(Epreworkshop, "Pre-survey", "Using.R.or.Python" , "importance of using R or Python")
plotByGenderGeneric(Epreworkshop, "Pre-survey", "Value.of.SQL.or.Python" , "importance of using SQL or Python")
plotByGenderGeneric(Epreworkshop, "Pre-survey", "Age" , "age")
plotByGenderGeneric(Epreworkshop, "Pre-survey", "Race" , "race")
plotByGenderGeneric(Epreworkshop, "Pre-survey", "First.Time" , "first time taking a DC as learner")
plotByGenderGeneric(Epreworkshop, "Pre-survey", "Workshop.in.US" , "workshop taken in the US")
### Very important the question of gender was only asked to people in the US
# table(Epreworkshop$Workshop.in.US, Epreworkshop$Gender)
# 
# Female Male Prefer not to say No Answer
# 0    0                 0       446
# No       0    0                 0       590
# Yes    717  540                22        28


#############################################
plotByStatusGeneric <- function(df, ti, colna, colstr){
  #StartbyFiltering colna Not Answered
  print(dim(df))
  print(colna)
  y <- droplevels(subset(df, df[[colna]] != ""))
  print(dim(y))
  print(table(y[[colna]]))
  #####Plot Function Generic
  ps <- ggplot(data = y, aes( x = Status, fill= Status)) +
    #coord_flip() +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_fill_manual(values = cbPalette) +
    theme_light() +
    labs(x = "Status", y = paste("Total respondents", dim(y)[1], "in percentage ")) +
    ggtitle(paste(ti, "responses by status and", colstr)) +
    scale_y_continuous(labels = scales::percent) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="bottom",
          legend.title = element_blank())+
    facet_grid(reformulate(colna), ".") # . ~ colna
  print(ps)
  ggsave(filename =  paste("./plots/Statusvs", gsub("\\s", "",colstr),
                           "-",ti,".png", sep = ""),
         width = 12, height = 6, dpi = 120)
}

#plotByStatusGeneric(df, ti, colna, colstr)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Gender" , "gender")
#not very useful as gender is only asked in the US
plotByStatusGeneric(Epreworkshop, "Pre-survey", "With.Friend" , "attended with a friend")
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Discipline" , "discipline")
# too many variables
plotByStatusGeneric(Epreworkshop, "Pre-survey", "OS" , "operative system")
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Programming.Usage" , "programming usage")

### I'm not sure why there is 7 columns for current tools "Current.Tools.1" to "Current.Tools.7"

plotByStatusGeneric(Epreworkshop, "Pre-survey", "Have.Dataset" , "have dataset")
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Data.Organization" , "importance of data organization")
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Using.Scripting.Language" , "importance of using scripting language")
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Using.R.or.Python" , "importance of using R or Python")
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Value.of.SQL.or.Python" , "importance of using SQL or Python")
plotByStatusGeneric(Epreworkshop, "Pre-survey", "First.Time" , "first time taking a DC as learner")
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Workshop.in.US" , "workshop taken in the US")
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Age" , "age")
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Race" , "race")



