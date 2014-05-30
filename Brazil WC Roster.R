setwd("~/Documents/Blog/Brazil World Cup Roster")

library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("scales", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

bzlroster <- read.csv("Brazil WC Roster, 1930-2014.csv")
onename <- read.csv("Brazil WC One Name, 1930-2014.csv")
age <- read.csv("Brazil WC Age, 1930-2014.csv")

### One Name Graph

Year <- onename$Year
OneName <- onename$OneName
roster <- onename$Roster
Percent <- onename$Percent

df <- data.frame(Year,OneName,roster,Percent)

ggplot(df, aes(x=Year, y=Percent)) +
    theme_bw() +
    geom_bar(fill="green4",stat="identity") +
    scale_x_continuous(breaks=Year, labels=Year) +
    scale_y_continuous(expand = c(0,0),limits = c(0,1), labels=percent) + # scales package
    theme(axis.text.x = element_text(angle=90, vjust=0.5),
          panel.grid.minor=element_blank(), # hides minor gridliness
          panel.grid.major.x=element_blank()) + # hides vertical major grid lines
    labs(x="Year", y= "Percentage of Players with One Name", 
               title = "Brazil World Cup Rosters: One Name Players")

### Roster Age Graph

Year <- age$Year
AvgAge <- age$AvgAge

dfage <- data.frame(Year,AvgAge)

ggplot(dfage, aes(x=Year, y=AvgAge)) +
    theme_bw()+
    geom_bar(fill="green4",stat="identity") +
    scale_x_continuous(breaks=Year, labels=Year) +
    coord_cartesian(ylim=c(20, 30)) + # view bars between 20 and 30 on y axis
    theme(axis.text.x = element_text(angle=90, vjust=0.5),
          panel.grid.minor=element_blank(), # hides minor gridliness
          panel.grid.major.x=element_blank()) + # hides vertical major grid lines
    labs(x="Year", y= "Average Age", 
         title = "Brazil World Cup Rosters: Average Age")

### Foreign Club Graph

Year <- bzlroster$Year
Country <- bzlroster$ClubCountry

dfb <- data.frame(Year,Country)

# For each year, count number of "Brazil"
# Two columns - Year | Brazil Count

x <- count(dfb, c("Year","Country")) # plyr package; counts no. of WC players per country per WC year
x <- x[1:66,]
bzl <- subset(x,Country=="Brazil",select=c(Year,Country,freq)) # subset of rows with Brazil as country

ggplot(bzl, aes(x=Year, y=freq)) +
    theme_bw() +
    geom_bar(fill="green4",stat="identity") +
    scale_x_continuous(breaks=Year, labels=Year) +
    scale_y_continuous(limits=c(0,22),breaks=c(0, 5, 10, 15,20,22)) + 
    theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10),
          panel.grid.minor=element_blank(), # hides minor gridliness
          panel.grid.major.x=element_blank()) + # hides vertical major grid lines
    labs(x="Year", y= "Number of Players", 
         title = "Brazil World Cup Rosters: Number of Players on \nBrazilian Club Teams")