#! /usr/bin/env Rscript

#
#   Description: report plotting functions
#   Analyzing pre and post survey results from Data Carpentry 
#   Repo: https://github.com/carpentries/assessment-projects/tree/master/data-carpentry-projects
#   Date: 2017, August 09 - 27
#   Copyright (C) 2017 Paula Andrea Martinez
#   ORCID iD 0000-0002-8990-1985

source(file = "scripts/installpkg.R")

#####################################
# download files

#download.file("https://raw.githubusercontent.com/carpentries/assessment-projects/master/data-carpentry-projects/preworkshop_public_archived.csv", 
#                destfile = "data/preworkshop_public_archived.csv", method = "wininet")

 
#download.file("https://raw.githubusercontent.com/carpentries/assessment-projects/master/data-carpentry-projects/postworkshop_public_archived.csv", 
#              destfile = "data/postworkshop_public_archived.csv", method = "wininet")


############################################################################
# Set blind-friendly colour Palette
# http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/
# The palette with grey:
cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#999999","#E69F00", "#D55E00", "#CC79A7", "#90EE90")
# To use for fills, add
library("ggplot2")
scale_fill_manual(values = cbPalette)
# To use for line and point colors, add
scale_colour_manual(values = cbPalette)

# ###########################################################################
# Exploring the dataset
Exploring <- function(filecsv){
    ps <- read.csv(filecsv)
    print(filecsv)
    print(paste("Data contains", 
                 dim(ps)[1], "rows and", 
                 dim(ps)[2], "columns")) 

    return(ps)
}
# ###########################################################################
# Reordering levels of a factor
reorderLevels <- function(x, vec){
  x <- factor(x,levels(x)[vec])
  print(levels(x))
  return(x)
}
# ###########################################################################
# Define a helper function to change NAs from factor to character
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, "No answer")
}
# ###########################################################################
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
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
# ###########################################################################
# Cleaning Epreworkshop
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
  print(paste("Data contains", 
              dim(df)[1], "rows and", 
              dim(df)[2], "columns")) 
  return(df)
}
# ###########################################################################
# Cleaning Epostworkshop
cleanPostworkshopdata <- function(df){
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
  # capitalize levels
  levels(df$Workshop.in.US)[2] <- "No"
  levels(df$Workshop.in.US)[3] <- "Yes"
  print(paste("Data contains", 
              dim(df)[1], "rows and", 
              dim(df)[2], "columns")) 
  return(df)
  
}
# ###########################################################################
# ######### plotting ##############
# load all necesary packages
library("ggplot2")
library("scales")
library("extrafont")
font_import(pattern="[T/t]ahoma", prompt = FALSE)
loadfonts()
# ###########################################################################
# Make barplots by Status 
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
  numoffacets <- length((table(y[[colna]])))
  perc <- c()
  for (f in 1:numoffacets){
    perc[f] <- round((table(y[[colna]])[f] * 100 / dim(y)[1]), digits = 2)
  }
  #####Plot Function Generic
  ps <- ggplot(data = y, aes( x = Status, fill= Status)) +
    #coord_flip() +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_fill_manual(values = cbPalette) +
    theme_light() +
    labs(x = "Status", y = paste("Total respondents", dim(y)[1], "shown in percentages")) +
    ggtitle(paste(ti, "responses by status and", colstr)) +
    scale_y_continuous(labels = scales::percent) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(panel.grid.major.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          strip.background = element_rect(fill =  "#888888"),
          strip.placement = "outside",
          strip.switch.pad.grid = unit(0.1, "cm"),
          text = element_text(family="Tahoma", size=12))+
    facet_grid(reformulate(colna), ".") # . ~ colna
  ps <- ps +  annotate(geom = "text", label = paste0(perc, "%"), size = 4, 
                       x = 4.5, y = dim(y)[1]/6000, fontface="bold",
                       colour = "#888888", family="Tahoma")
  print(ps)
   ggsave(filename =  paste("./plots/", ti, "_","Status_", gsub("\\.", "",colna),
                             ".png", sep = ""),
         width = 15, height = 8, dpi = 200)
}
# ###########################################################################
# A try to make a generic plot
plotGeneric <- function(df, ti, colna, colstr, reorderingvec = NULL, ext,reorderingvec2 = NULL ){
  #StartbyFiltering colna Not Answered
  print(dim(df))
  print(colna)
  print(ext)
  y <- droplevels(subset(df, df[[colna]] != ""))
  y <- droplevels(subset(y, y[[ext]] != ""))
  print(dim(y))
  if(!is.null(reorderingvec)){
    y[[colna]] <- reorderLevels(y[[colna]], reorderingvec)
  }
  if(!is.null(reorderingvec2)){
    y[[ext]] <- reorderLevels(y[[ext]], reorderingvec2)
  }
  print(table(y[[colna]],y[[ext]]))
  numoffacets <- length((table(y[[colna]],y[[ext]])))
  perc <- c()
  for (f in 1:numoffacets){
    perc[f] <- round((table(y[[colna]],y[[ext]])[f] * 100 / dim(y)[1]), digits = 2)
  }
  #####Plot Function Generic
  ps <- ggplot(data = y, aes( x = Status, fill= Status) )+
    #coord_flip() +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_fill_manual(values = cbPalette) +
    theme_light() + 
    labs(x = "Status", y = paste("Total respondents", dim(y)[1], "shown in percentages")) +
    ggtitle(paste(ti, "responses by status and", colstr)) +
    scale_y_continuous( labels = percent_format())+
    theme(panel.grid.major.x =  element_blank(),
          plot.title   = element_text(hjust = 0.5),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom",
          legend.title    = element_blank(),
          strip.background = element_rect(fill =  "#888888"),
          strip.placement = "outside",
          strip.switch.pad.grid = unit(0.1, "cm"),
          text=element_text(family="Tahoma", size=12))+
    #facet_grid(reformulate(colna), ".") # . ~ colna
    facet_grid(eval(reformulate(colna, ext)))
  ps <- ps +  annotate(geom = "text", label = paste0(perc, "%"), size = 4, 
                       x = 4.5, y = dim(y)[1]/6000, fontface="bold",
                       colour = "#888888", family="Tahoma")
  ggsave(filename =  paste("./plots/", ti, "_",ext, "_", gsub("\\.", "",colna),
                            ".png", sep = ""),
        width = 15, height = 8, dpi = 200)
  print(ps)
}
# ###########################################################################
# Plots for Gender, Race and Age should be only from US respondents
# ###########################################################################
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
    labs(x = "Gender", y = paste("Total respondents", dim(df)[1], "shown in percentages")) +
    ggtitle(paste(ti, "responses by gender") ) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.7)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="bottom",
          legend.title = element_blank())
  #print(ps)
}
# ###########################################################################
# Plot by Gender and Status
plotByGenderStatus <- function(df, ti){
  ps <- ggplot(data = df, aes( x = Gender, fill= Gender)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_fill_manual(values = cbPalette) +
    theme_light() +
    labs(x = "Gender", y = paste("Total respondents", dim(df)[1], "shown in percentages")) +
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
# ###########################################################################
# Excluding "Prefer not to say and No answer from Gender and Status"
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
# ###########################################################################

