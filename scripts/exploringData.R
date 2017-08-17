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
dim(Epostworkshop)
levels(Epostworkshop$Gender)
levels(Epostworkshop$Status)
Epostworkshop[1, 1:10]
# Postworkshop Status has one extra level "Response" only in row 1, now removed 
Epostworkshop <- Epostworkshop[-c(1), ]
Epostworkshop <- droplevels(Epostworkshop)
dim(Epostworkshop)

levels(Epostworkshop$Gender)
levels(Epostworkshop$Status)

levels(Epostworkshop$Gender)[1] <- "No Answer"
levels(Epostworkshop$Status)[1] <- "No Answer"
levels(Epostworkshop$Status)[5] <- "Other"

#check
levels(Epostworkshop$Gender)
levels(Epostworkshop$Status)


#Reorder levels in Gender and Status
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

multiplot(plotByGender(newpreGS, "Pre-survey"), 
          plotByGender(newpostGS, "Post-survey"), cols=2)

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
plotByStatusGeneric <- function(df, ti, colna, colstr, reorderingvec = NULL){
  #StartbyFiltering colna Not Answered
  print(dim(df))
  print(colna)
  y <- droplevels(subset(df, df[[colna]] != ""))
  print(dim(y))
  print(table(y[[colna]]))
  if(!is.null(reorderingvec)){
    y[[colna]] <- reorderLevels(y[[colna]], reorderingvec)
  }
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

#plotByStatusGeneric(df, ti, colna, colstr, order)
table(Epreworkshop$First.Time)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "First.Time" , "first time taking a DC as learner", c(2,1))

table(Epreworkshop$Discipline) # too many variables
table(Epreworkshop$OS)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "OS" , "operative system", c(4,1,2,3))
table(Epreworkshop$With.Friend)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "With.Friend" , "attended with a friend", c(3,1,2))
table(Epreworkshop$Programming.Usage)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Programming.Usage" , "programming usage", c(2, 3, 6, 4, 7, 1, 5))
### I'm not sure why there is 7 columns for current tools "Current.Tools.1" to "Current.Tools.7"
table(Epreworkshop$Have.Dataset)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Have.Dataset" , "have dataset", c(3, 4, 1, 2))
table(Epreworkshop$Data.Organization)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Data.Organization" , "importance of data organization", c(4, 1, 3, 2, 5))
table(Epreworkshop$Using.Scripting.Language)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Using.Scripting.Language" , "importance of using scripting language", c(4, 1, 3, 2, 5))
table(Epreworkshop$Using.R.or.Python)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Using.R.or.Python" , "importance of using R or Python", c(4, 1, 3, 2, 5))
table(Epreworkshop$Value.of.SQL.or.Python)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Value.of.SQL.or.Python" , "accidentaly changing data in R, SQL or Python", c(4, 1, 3, 2, 5))

table(Epreworkshop$Workshop.in.US)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Workshop.in.US" , "workshop taken in the US", c(2,1))
table(Epreworkshop$Age)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Age" , "age")
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Gender" , "gender")
table(Epreworkshop$Race)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Race" , "race", c(8, 2, 1,3,4,5,6,7))

#####################################
#Comments on postsurvey data
# there is not much to do with columns that are repeated with the pre-survey 
# for example
# "Start.Date"                "End.Date"                  "When.Taking.Survey"        "First.Time"  
# "Research" is a text field wich is difficult to categorize
# Status can be ploted easily I already order it and removed not answered, plus added Response into Other
# Undergraduate student      Graduate student              Post-doc               Faculty                 Staff 
# 60                   435                   144                    96                   184 
# Industry                 Other             No Answer 
# 7                    80                    76 

# Rate your level of involvement
table(Epostworkshop$Involvement)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Involvement" , "level of involvement", c(1,3,2))
table(Epostworkshop$Practical.Knowledge)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Practical.Knowledge" , "practical knowledge gained", c(1, 3, 2))
table(Epostworkshop$Organize.Data)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Organize.Data" , "better understanding on how to organize data in spreadsheets", c(5, 1, 4, 2, 6, 3))
# has a weird extra category Effectively organize data in spreadsheets
table(Epostworkshop$Use.OpenRefine)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Use.OpenRefine" , "better understanding on how to use OpenRefine for data cleaning", c(5, 1, 4, 2, 6, 3))
# has a weird extra category Use OpenRefine for data cleaning
table(Epostworkshop$Import.Python)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Import.Python" , "better understanding on how to import a file in Python and work with the data", c(5, 1, 4, 2, 6, 3))
table(Epostworkshop$Import.R)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Import.R" , "better understanding on how to to import a file into R and with the data", c(5, 1, 4, 2, 6, 3))
table(Epostworkshop$Visualizations.in.Python)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Visualizations.in.Python" , "better understanding on how to do visualizations in Python", c(5, 1, 4, 2, 6, 3))
table(Epostworkshop$Visualizations.in.R)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Visualizations.in.R" , "better understanding on how to do visualizations in R", c(5, 1, 4, 2, 6, 3))
table(Epostworkshop$Construct.SQL)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Construct.SQL" , "better understanding on how to construct SQL queries", c(5, 1, 4, 2, 6, 3))
table(Epostworkshop$Use.command.line)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Use.command.line" , "better understanding on how to use command line", c(5, 1, 4, 2, 6, 3))
table(Epostworkshop$Skill.Level.Prior)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Skill.Level.Prior" , "data management and analysis skills prior the workshop", c(5, 2, 3, 1, 4))
table(Epostworkshop$Skill.Level.Following)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Skill.Level.Following" , "data management and analysis skills following the workshop", c(1, 4, 2, 3))
table(Epostworkshop$Application)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Application" , "can immediately applied what was learned at the workshop", c(4,1,3,2,5))
table(Epostworkshop$Worth.My.Time)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Worth.My.Time" , "the workshop was worth my time", c(4,1,3,2,5))
table(Epostworkshop$Worth.My.Time)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Worth.My.Time" , "the workshop was worth my time", c(4,1,3,2,5))
table(Epostworkshop$Worth.My.Time)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Worth.My.Time" , "the workshop was worth my time", c(4,1,3,2,5))
table(Epostworkshop$Worth.My.Time)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Worth.My.Time" , "the workshop was worth my time", c(4,1,3,2,5))
table(Epostworkshop$Worth.My.Time)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Worth.My.Time" , "the workshop was worth my time", c(4,1,3,2,5))
table(Epostworkshop$Worth.My.Time)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Worth.My.Time" , "the workshop was worth my time", c(4,1,3,2,5))
table(Epostworkshop$Worth.My.Time)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Worth.My.Time" , "the workshop was worth my time", c(4,1,3,2,5))
table(Epostworkshop$Worth.My.Time)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Worth.My.Time" , "the workshop was worth my time", c(4,1,3,2,5))
table(Epostworkshop$Material)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Material" , "the material matched the workshop description", c(4,1,3,2,5))
table(Epostworkshop$Recommend)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Recommend" , "would recommend this workshop", c(4,1,3,2,5))
table(Epostworkshop$Instructors.Effective)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Instructors.Effective" , "were the instructors effective in teaching the workshop", c(1,2,5, 4, 3))
table(Epostworkshop$Instructors.Enthusiastic)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Instructors.Enthusiastic" , "were the instructors enthusiastic about the workshop", c(1, 2,4, 3))
table(Epostworkshop$Workshop.in.US)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Workshop.in.US" , "the workshop was in the US", c(2,1))
# age and gender are not shown in the PDF questionnaire
table(Epostworkshop$Age)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Age" , "age") # not very necesary as given in the presurvey
table(Epostworkshop$Gender)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Gender" , "gender")
table(Epostworkshop$Race.White) # given that is the majority
plotByStatusGeneric(Epostworkshop, "Post-survey", "Race.White" , "white")
