#! /usr/bin/env Rscript

#
#   Description: report ploting functions
#   Analyzing pre and post survey results from Data Carpentry 
#   Repo: https://github.com/carpentries/assessment-projects/tree/master/data-carpentry-projects
#   Date: 2017, August 09 - 19
#   Copyright (C) 2017 Paula Andrea Martinez
#
source(file = "scripts/installpkg.R")
source(file = "scripts/usefulFunctions.R")

#####################################
# download files

#download.file("https://raw.githubusercontent.com/carpentries/assessment-projects/master/data-carpentry-projects/postworkshop-public-archived.csv", 
#                destfile = "../data/postworkshopDC.csv", method = "wininet")
# 
# 
# download.file("https://raw.githubusercontent.com/carpentries/assessment-projects/master/data-carpentry-projects/preworkshop-public-archived.csv", 
#               destfile = "../data/preworkshopDC.csv", method = "wininet")


############################################################################
#Set Colour Palette
# The palette with grey:
cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#999999","#E69F00", "#D55E00", "#CC79A7", "#90EE90")
# The palette with black:
cbbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2","#000000", "#E69F00", "#D55E00", "#CC79A7", "#90EE90")
# To use for fills, add
scale_fill_manual(values = cbPalette)
# To use for line and point colors, add
scale_colour_manual(values = cbPalette)

# ###############################################################################
# Exploring the dataset
Exploring <- function(filecsv){
    ps <- read.csv(filecsv)
    print(filecsv)
    print(paste("Data contains", 
                 dim(ps)[1], "rows and", 
                 dim(ps)[2], "columns")) 

    return(ps)
}
######################################
# Reordering levels of a factor
reorderLevels <- function(x, vec){
  x <- factor(x,levels(x)[vec])
  print(levels(x))
  return(x)
}
######################################
#Cleaning Epreworkshop
cleanPreworkshopdata <- function(df){
  print(colnames(df))
  # "Start.Date" # munging to year.survey
  # "End.Date"                 "When.Taking.Survey" # ignored      
  # "First.Time" # ignored
  # "Status"   #munging and reorder
  # "Status.Other"   # ignored          
  # "Department"   # ignored 
  # "Discipline"   #munging and reorder
  # "Discipline.Other"   # ignored       
  # [10] "OS"                       "With.Friend"              "Programming.Usage"       
  # [13] "Current.Tools.1"          "Current.Tools.2"          "Current.Tools.3"         
  # [16] "Current.Tools.4"          "Current.Tools.5"          "Current.Tools.6"         
  # [19] "Current.Tools.7"      # ignored       
  # "Have.Dataset"  #munging names          "Data.Management.Strategy"
  # [22] "Data.Analysis.Workflow"   "Data.Organization"        "Using.Scripting.Language"
  # [25] "Using.R.or.Python"        "Value.of.SQL.or.Python"   "Hope.to.Learn"           
  # [28] "Workshop.in.US"           "Age"                      "Gender"                  
  # [31] "Gender.Other"             "Race"                     "Race.Other"  
  df$year.survey <- as.factor(format(as.Date(as.character(df$Start.Date), "%m/%d/%y" ),"%Y"))
  #print(levels(df$year.survey ))
  levels(df$Status)[1] <- "No Answer"
  levels(df$Status)[5] <- "Other"
  levels(df$Status)[8] <- "Undergraduate Student"
  df$Status <- reorderLevels(df$Status, c(8,3,6,2,7,4,5,1))
  levels(df$Discipline)[1] <- "No Answer"
  levels(df$Discipline)[3] <- "Neurosciences"
  levels(df$Discipline)[5] <- "Computer sciences, Electrical Eng"
  levels(df$Discipline)[6] <- "Geology, Oceanography, Meteorology"
  levels(df$Discipline)[8] <- "Civil, Mechanical, Chemical Eng"
  levels(df$Discipline)[10] <- "Biology and Genetics"
  levels(df$Discipline)[11] <- "Botany, Ecology, Zoology"
  levels(df$Discipline)[13] <- "Other"
  levels(df$Discipline)[18] <- "Tech support/Lab tech/support prog"
  #df$Discipline <- reorderLevels(df$Discipline, c(2,10, 11,4,8,5,7,6, 9, 12,3, 14, 15, 16, 17,18, 13,1))
  levels(df$Have.Dataset)[2] <- "I am working on generating data"
  levels(df$Have.Dataset)[3] <- "I do not have data yet"
  levels(df$Have.Dataset)[4] <- "I have data and done a fair bit of analysis"
  levels(df$Have.Dataset)[5] <- "I have data but haven't started analyzing it"
  levels(df$Gender)[1] <- "No Answer"
  df$Gender <- reorderLevels(df$Gender, c(2,3,4,1))
  levels(df$Race)[1] <- "No Answer"
  levels(df$Race)[7] <- "Other"
  df$Race <- reorderLevels(df$Race, c(2,3,4,5,6,9,8,7,1))
  return(df)

}
######################################
#Cleaning Epostworkshop
cleanPostworkshopdata <- function(df){
  # # Postworkshop Status has one extra level "Response" only in row 1, now removed 
  df <- df[-c(1), ]
  df <- droplevels(df)
  print(dim(df))
  print(colnames(df))
  # [1] "Start.Date"                "End.Date"                  "When.Taking.Survey"        "First.Time"               
  # [5] "Research"                  "Status"                    "Status.Other"              "Involvement"              
  # [9] "Practical.Knowledge"       "Organize.Data"             "Use.OpenRefine"            "Import.Python"            
  # [13] "Import.R"                  "Visualizations.in.Python"  "Visualizations.in.R"       "Construct.SQL"            
  # [17] "Use.command.line"          "Skill.Level.Prior"         "Skill.Level.Following"     "Data.Organization"        
  # [21] "Using.Scripting.Language"  "Using.R.or.Python"         "Value.of.SQL.or.Python"    "Application"              
  # [25] "Worth.My.Time"             "Material"                  "Recommend"                 "Instructors.Clear.Answers"
  # [29] "Instructors.Considerate"   "Instructors.Effective"     "Instructors.Enthusiastic"  "Workshop.in.US"           
  # [33] "Age"                       "Gender"                    "Gender.Other"              "Race.American.Indian"     
  # [37] "Race.Asian"                "Race.Black"                "Race.Hispanic"             "Race.Islander"            
  # [41] "Race.White"                "Race.Prefer.Not"           "Race.Other"          
  df$year.survey <- as.factor(format(as.Date(as.character(df$Start.Date), "%m/%d/%y" ),"%Y"))
  #print(levels(df$year.survey ))
  levels(df$Status)[1] <- "No Answer"
  levels(df$Status)[5] <- "Other"
  levels(df$Status)[8] <- "Undergraduate Student"
  df$Status <- reorderLevels(df$Status, c(8,3,6,2,7,4,5,1))
  levels(df$Gender)[1] <- "No Answer"
  df$Gender <- reorderLevels(df$Gender, c(2,3,4,1))
  return(df)
  
}

# # files sent by email have more colums
Epreworkshop <- Exploring("./data/preworkshop-public-archived.csv")
Epreworkshop <- cleanPreworkshopdata(Epreworkshop)

Epostworkshop <- Exploring("./data/postworkshop-public-archived.csv")
Epostworkshop <- cleanPostworkshopdata(Epostworkshop)



############################################################################
########## plotting ##############
library("ggplot2")
############################################################################
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
  ggsave(filename =  paste("./plots/", ti, "_","Status_", gsub("\\.", "",colna),
                            ".png", sep = ""),
        width = 15, height = 8, dpi = 200)
}
############################################################################
# Calling the function # plotByStatusGeneric(df, ti, colna, colstr, order)
table(Epreworkshop$year.survey)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "year.survey" , "year of survey response")
table(Epreworkshop$First.Time)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "First.Time" , "first time taking a DC as learner", c(2,3,1))
table(Epreworkshop$Discipline) # too many variables to plot by Biology and Genetics is the discipline with the majority of answers
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Discipline" , "discipline", c(2,10, 11,4,8,5,7,6, 9, 12,3, 14, 15, 16, 17,18, 13,1))
table(Epreworkshop$OS)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "OS" , "operative system", c(1,2,4,3))
table(Epreworkshop$With.Friend)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "With.Friend" , "attended with a friend", c(1,2,3))
table(Epreworkshop$Programming.Usage)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Programming.Usage" , "programming usage", c(2, 3, 6, 4, 7, 1, 5))
### skipping current tools "Current.Tools.1" to "Current.Tools.7"
table(Epreworkshop$Have.Dataset)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Have.Dataset" , "have dataset", c(2,1,4,3))

## "Data.Management.Strategy" "Data.Analysis.Workflow" are better plotted with a Likert plot 
table(Epreworkshop$Data.Management.Strategy)
table(Epreworkshop$Data.Analysis.Workflow)

table(Epreworkshop$Data.Organization)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Data.Organization" , "importance of data organization", c(5,2,3,1,4))
table(Epreworkshop$Using.Scripting.Language)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Using.Scripting.Language" , "importance of using scripting language",  c(5,2,3,1,4))

### Skipped because question is not clear
table(Epreworkshop$Using.R.or.Python)
table(Epreworkshop$Value.of.SQL.or.Python)

table(Epreworkshop$Workshop.in.US)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Workshop.in.US" , "workshop taken in the US")

table(Epreworkshop$Age)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Age" , "age")
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Gender" , "gender")
table(Epreworkshop$Race)
plotByStatusGeneric(Epreworkshop, "Pre-survey", "Race" , "race")
### I will suggest to do the next plots with a subset of US 
EpreworkshopUS <- subset(Epreworkshop, Workshop.in.US == "Yes")
dim(EpreworkshopUS)

plotByStatusGeneric(EpreworkshopUS, "Pre-surveyUS", "Age" , "age")
plotByStatusGeneric(EpreworkshopUS, "Pre-surveyUS", "Gender" , "gender")
plotByStatusGeneric(EpreworkshopUS, "Pre-surveyUS", "Race" , "race")

############################################################################
# Post- survey data
table(Epostworkshop$year.survey)
plotByStatusGeneric(Epostworkshop, "Post-survey", "year.survey" , "year of survey response")
# there is not much to do with columns that are repeated with the pre-survey
#  "When.Taking.Survey"        "First.Time"
# "Research" is a text field wich is difficult to categorize
table(Epostworkshop$Involvement)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Involvement" , "level of involvement", c(2,3,1))
table(Epostworkshop$Practical.Knowledge)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Practical.Knowledge" , "practical knowledge gained", c(2, 3, 1))

#####The folowwing will be better with a Likert plot
# "Organize.Data"             "Use.OpenRefine"            "Import.Python"            
# [13] "Import.R"                  "Visualizations.in.Python"  "Visualizations.in.R"       "Construct.SQL"            
# [17] "Use.command.line" 

# For information of each question
table(Epostworkshop$Organize.Data)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Organize.Data" , "better understanding on how to organize data in spreadsheets", c(6,2,4,1,5,3)) 
# has a weird extra category Effectively organize data in spreadsheets
table(Epostworkshop$Use.OpenRefine)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Use.OpenRefine" , "better understanding on how to use OpenRefine for data cleaning", c(6,2,4,1,5,3)) 
# has a weird extra category Use OpenRefine for data cleaning
table(Epostworkshop$Import.Python)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Import.Python" , "better understanding on how to import a file in Python and work with the data",  c(6,2,4,1,5,3)) 
table(Epostworkshop$Import.R)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Import.R" , "better understanding on how to import a file in R and work with the data",  c(6,2,4,1,5,3)) 
table(Epostworkshop$Visualizations.in.Python)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Visualizations.in.Python" , "better understanding on how to do visualizations in Python",  c(6,2,4,1,5,3)) 
table(Epostworkshop$Visualizations.in.R)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Visualizations.in.R" , "better understanding on how to do visualizations in R",  c(6,2,4,1,5,3)) 
table(Epostworkshop$Construct.SQL)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Construct.SQL" , "better understanding on how to construct SQL queries",  c(6,2,4,1,5,3)) 
table(Epostworkshop$Use.command.line)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Use.command.line" , "better understanding on how to use command line",  c(6,2,4,1,5,3)) 


table(Epostworkshop$Skill.Level.Prior)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Skill.Level.Prior" , "data management and analysis skills prior the workshop", c(5, 2, 3, 1, 4))
table(Epostworkshop$Skill.Level.Following)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Skill.Level.Following" , "data management and analysis skills following the workshop", c(1, 4, 2, 3))
table(Epostworkshop$Application)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Application" , "can immediately applied what was learned at the workshop", c(5,2,3,1,4))
table(Epostworkshop$Worth.My.Time)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Worth.My.Time" , "the workshop was worth my time", c(5,2,3,1,4))
 
table(Epostworkshop$Material)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Material" , "the material matched the workshop description", c(5,2,3,1,4))
table(Epostworkshop$Recommend)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Recommend" , "would recommend this workshop", c(5,2,3,1,4))
table(Epostworkshop$Instructors.Effective)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Instructors.Effective" , "were the instructors effective in teaching the workshop?", c(3, 4,5,2, 1))
table(Epostworkshop$Instructors.Enthusiastic)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Instructors.Enthusiastic" , "were the instructors enthusiastic about the workshop?", c(3,4,2,1))


table(Epostworkshop$Workshop.in.US) # should be No and Yes but it is all small letters
plotByStatusGeneric(Epostworkshop, "Post-survey", "Workshop.in.US" , "the workshop was in the US")
# age and gender are not shown in the PDF questionnaire
# not very necesary as given in the presurvey
table(Epostworkshop$Age)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Age" , "age") 
table(Epostworkshop$Gender)
plotByStatusGeneric(Epostworkshop, "Post-survey", "Gender" , "gender") # needs subsetting
table(Epostworkshop$Race.White) # given that is the majority
plotByStatusGeneric(Epostworkshop, "Post-survey", "Race.White" , "white")

###New Combinations
# wort my time vs With a friend
# involvement vs with a friend
#Recomended vs worth my time
# recomended vs material
# material vs worth my time

table(Epreworkshop$Status, Epreworkshop$With.Friend)











# Plots by Gender should be only taken for answers within US 

plotByGender <- function(df, ti, reorderingvec=NULL){
  if(!is.null(reorderingvec)){
      df[["Gender"]] <- reorderLevels(df[["Gender"]], reorderingvec)
  }
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

multiplot(plotByGender(Epreworkshop, "Pre- survey"),
          plotByGender(Epostworkshop, "Post- survey"), cols=2)


###########################################################################
plotByGenderStatus <- function(df, ti){
  ps <- ggplot(data = df, aes( x = Gender, fill= Gender)) +
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
  #ggsave(filename =  paste("./plots/GendervsStatus-",ti,".png", sep = ""),
  #       width = 12, height = 6, dpi = 120)
}

plotByGenderStatus(Epreworkshop, "Pre-survey")
plotByGenderStatus(Epostworkshop, "Post-survey")
# 
###########################################################################
# # #excluding "Prefer not to say and No answer from Gender and Status"
# # 
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

############################################################################
plotByRace <- function(df, ti){
  x <- subset(df, df$Race != "No Answer", drop = T)
  y <- droplevels(x)
  ps <- ggplot(data = y, aes( x = Race, fill = Race )) +
    #coord_flip() +
    geom_bar(aes(y = (..count..)/sum(..count..)) ) +
    scale_fill_manual(values = cbPalette) +
    theme_light() +
    labs(x = "Race", y = paste("Total respondents", dim(y)[1], "in percentage ")) +
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
############################################################################


colnames(Epreworkshop)
table(Epreworkshop$Gender)
table(Epreworkshop$With.Friend)
table(Epreworkshop$Gender, Epreworkshop$With.Friend)


############################################################################
# plotByGenderGeneric <- function(df, ti, colna, colstr){
#   #StartbyFiltering Gender Not Answered or not provided
#   print(dim(df))
#   x <- subset(df, df$Gender == "Male" | df$Gender == "Female") 
#   x <- droplevels(x)
#   print(dim(x))
#   print(colna)
#   y <- droplevels(subset(x, x[[colna]] != ""))
#   print(dim(y))
#   print(table(y[[colna]]))
#   #####Plot Function Generic
#   ps <- ggplot(data = y, aes( x = Gender, fill= Gender)) +
# 
#     geom_bar(aes(y = (..count..)/sum(..count..))) +
#     scale_fill_manual(values = cbPalette) +
#     theme_light() +
#     labs(x = "Gender", y = paste("Total respondents", dim(y)[1], "in percentage ")) +
#     ggtitle(paste(ti, "responses by gender and", colstr)) +
#     scale_y_continuous(labels = scales::percent) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     theme(axis.text.x=element_blank(),
#           axis.ticks.x=element_blank(),
#           legend.position="bottom",
#           legend.title = element_blank())+
#     facet_grid(reformulate(colna), ".") # . ~ colna
#   print(ps)
#   ggsave(filename =  paste("./plots/FilteredGendervs", gsub("\\s", "",colstr),
#                            "-",ti,".png", sep = ""),
#          width = 12, height = 6, dpi = 120)
# }
# 
# #plotByGenderGeneric <- function(df, ti, colna, colstr)
# plotByGenderGeneric(Epreworkshop, "Pre-survey", "With.Friend" , "attended the workshop with a friend")
# plotByGenderGeneric(Epreworkshop, "Pre-survey", "Discipline" , "discipline")
# plotByGenderGeneric(Epreworkshop, "Pre-survey", "OS" , "operative system")
# plotByGenderGeneric(Epreworkshop, "Pre-survey", "Programming.Usage" , "programming usage")
# 
# ### I'm not sure why there is 7 columns for current tools "Current.Tools.1" to "Current.Tools.7"
# 
# plotByGenderGeneric(Epreworkshop, "Pre-survey", "Have.Dataset" , "have dataset")
# plotByGenderGeneric(Epreworkshop, "Pre-survey", "Data.Organization" , "importance of data organization")
# plotByGenderGeneric(Epreworkshop, "Pre-survey", "Using.Scripting.Language" , "importance of using scripting language")
# plotByGenderGeneric(Epreworkshop, "Pre-survey", "Using.R.or.Python" , "importance of using R or Python")
# plotByGenderGeneric(Epreworkshop, "Pre-survey", "Value.of.SQL.or.Python" , "importance of using SQL or Python")
# plotByGenderGeneric(Epreworkshop, "Pre-survey", "Age" , "age")
# plotByGenderGeneric(Epreworkshop, "Pre-survey", "Race" , "race")
# plotByGenderGeneric(Epreworkshop, "Pre-survey", "First.Time" , "first time taking a DC as learner")
# plotByGenderGeneric(Epreworkshop, "Pre-survey", "Workshop.in.US" , "workshop taken in the US")
# ### Very important the question of gender was only asked to people in the US
# # table(Epreworkshop$Workshop.in.US, Epreworkshop$Gender)
# # 
# # Female Male Prefer not to say No Answer
# # 0    0                 0       446
# # No       0    0                 0       590
# # Yes    717  540                22        28
# 
# 
# 