
# Roger IL Grande
# EM-622 Midterm Project


# Load the necessary libraries
library(ggplot2)
library(RColorBrewer)
library(maps)
library(mapproj)
library(treemap)
library(dplyr)
library(reshape)
library(grid)
library(treemapify)

# Set working directory
setwd("~/Documents/R Projects")

COVID_Cases_Data <- read.csv("COVID-19-Confirmed (10th March2020).csv", header = TRUE) # Import the cases dataset

View(COVID_Cases_Data) # View the data

# Create a scatter plot showing the number of confirmed cases in Mainland China vs everywhere else in the world combined 

# Create a new data frame with only the country and cases data
Location_and_Cases <- subset(COVID_Cases_Data, select = -c(Province.State,Lat,Long))

# Count the number of cases within Mainland China only on each date
Cases_China_Only <- data.frame(colSums((filter(Location_and_Cases, Country.Region == "Mainland China")[-1]), na.rm = TRUE))

names(Cases_China_Only) <- NULL # Remove un-needed header name

Cases_China_Only <- t(Cases_China_Only) # Transpose into original wide format

row.names(Cases_China_Only)[1] <- "Mainland_China"

View(Cases_China_Only)


# Count the number of cases outside of Mainland China only on each date
Cases_Outside_China <- data.frame(colSums((filter(Location_and_Cases, Country.Region != "Mainland China")[-1]), na.rm = TRUE))

colnames(Cases_Outside_China) <- NULL # Remove un-needed header name

Cases_Outside_China <- t(Cases_Outside_China) # Transpose into original wide format

row.names(Cases_Outside_China)[1] <- "Outside_of_China"

View(Cases_Outside_China)


# Combine the two data frames together - Cases in Mainland China and cases outside of China
Cases_by_Date <- rbind(Cases_China_Only, Cases_Outside_China)


Date <- colnames(Cases_by_Date) # Extract the dates as they are currently formatted in the column names portion
Date <- as.Date(Date, format = "X%m.%d.%y") # Convert to date format by specifying the format the dates are currently in
Date <- as.character(Date)


Cases_by_Date <- rbind(Date, Cases_by_Date) # Combine the formatted dates into the data frame
colnames(Cases_by_Date) <- NULL # Remove un-needed header name
Cases_by_Date <- t(Cases_by_Date) # Transpose to long format
Cases_by_Date <- as.data.frame(Cases_by_Date)
View(Cases_by_Date)


Cases_by_Date$Mainland_China <- as.numeric(Cases_by_Date$Mainland_China)
Cases_by_Date$Outside_of_China <- as.numeric(Cases_by_Date$Outside_of_China)
str(Cases_by_Date) # Check data types

# Create a scatter plot

ggplot(data = Cases_by_Date) + geom_point(aes(x = as.Date(Date),y = Mainland_China, colour = "Mainland_China")) + geom_point(aes(x = as.Date(Date),y = Outside_of_China, colour = "Outside_of_China")) + scale_y_continuous(limits = c(0, 81000)) +
  labs(title = "COVID Cases Inside and Outside of China", x = "Date", y = "Cases") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Mainland_China" = "red", "Outside_of_China"="blue")) +
  labs(colour = "Location")




# Create a table heat map showing COVID cases from February 1-March 1 within Mainland China

# Manipulation: Create a subset of the cases data
Mainland_China_Cases_Feb <- subset(COVID_Cases_Data, select = c(-(X1.22.20:X1.31.20), -(Lat:Long), -(X03.02.2020:X03.10.2020)), Country.Region == "Mainland China")
row.names(Mainland_China_Cases_Feb) <- Mainland_China_Cases_Feb$Province.State
Mainland_China_Cases_Feb <- subset(Mainland_China_Cases_Feb, select = c(-(Country.Region:Province.State)))
Mainland_China_Cases_Feb <- filter(Mainland_China_Cases_Feb, X03.01.2020 > 1)

Date <- colnames(Mainland_China_Cases_Feb) # Extract the dates as they are currently formatted in the column names portion
Date <- as.Date(Date, format = "X%m.%d.%y") # Convert to date format by specifying the format the dates are currently in
Date <- as.character(Date) # Convert to character format
colnames(Mainland_China_Cases_Feb) <- Date # Set column names as properly-formatted dates

View(Mainland_China_Cases_Feb)

# Convert the dataset to matrix form
Mainland_China_Cases_Feb_matrix <- data.matrix(Mainland_China_Cases_Feb)

# Clean the canvas
dev.off()

dev.set(dev.next())

# Create a table heat map
China_heatmap <- heatmap(Mainland_China_Cases_Feb_matrix, Rowv = NA, Colv = NA, margins = c(1, 1),
                         col = brewer.pal(9, "Reds"), scale = "row",
                         main = "COVID Cases in Mainland China from Feb 1 - Mar 1, 2020")




# Create a tree map showing where the most/least COVID recoveries were by 3/10/2020 outside of Mainland China

COVID_Recoveries_Data <- read.csv("COVID-19-Recovered (10th March2020).csv", header = TRUE) # Import the recoveries dataset

View(COVID_Recoveries_Data)

treemap_COVID_recoveries <- subset(COVID_Recoveries_Data, select = c(Country.Region, X03.10.2020)) # Create a subset of data with only country and number of recoveries
treemap_COVID_recoveries <- filter(treemap_COVID_recoveries, Country.Region != "Mainland China", X03.10.2020 > 1) # Filter out Mainland China and recoveries less than 1
colnames(treemap_COVID_recoveries) <- c("Country.Region", "Recoveries")

treemap_COVID_recoveries$logRecoveries <- log10(treemap_COVID_recoveries$Recoveries) # Normalize recoveries to create better color contrast

View(treemap_COVID_recoveries)

# Create the tree map
ggplot(treemap_COVID_recoveries, aes(area = Recoveries, fill = logRecoveries, label = Country.Region)) +
  geom_treemap() + geom_treemap_text(colour = "white",
                                     place = "centre", size = 0.5, grow = TRUE) +
  labs(title = "COVID Recoveries Outside of China") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")




# Create a geographic map showing number of COVID cases worldwide on 3/10/20

# Create a new data frame with only the country and cases data
Geographic_subset <- subset(COVID_Cases_Data, select = c(Lat,Long, X03.10.2020))
colnames(Geographic_subset) <- c("Lat", "Long", "Cases")
Geographic_subset$logCases <- log(Geographic_subset$Cases)
View(Geographic_subset)

Geographic_subset$colorBuckets <- as.numeric(cut(Geographic_subset$logCases, # Setting up color buckets based on these ranges of cases
                                       c(-1,2,4,6,8,10,12)))

newpalette <- brewer.pal(6, "Reds") # Six color degrees on the red scale

Geographic_subset$colorCode <- newpalette[Geographic_subset$colorBuckets]
Geographic_subset <- na.omit(Geographic_subset) # Remove NAs

world <- map_data("world") # Creating world map
ggplot() +
  geom_map(aes(map_id = region), map = world, data = world, color = "black", fill = "lightgray", size = 0.1) + # Setting world map properties
  expand_limits(x = world$long, y = world$lat) +
  geom_point(data = Geographic_subset, aes(Long, Lat, color = colorCode, size = logCases), # Setting point properties to follow color code and size
    alpha = 0.7) +
  theme_void() + labs(color = "Severity", size = "Number of Cases - Log Scale") +
  scale_size_continuous(range = c(1, 8)) + # Continuous scale of point sizes
  scale_color_discrete(name = "Severity", labels= c("6", "5", "4", "3", "2", "1"))




# Make a scatter plot of total worldwide cases vs deaths

COVID_Deaths_Data <- read.csv("COVID-19-Death (10th March2020).csv", header = TRUE) # Import the deaths dataset


# Create a new data frame with only the country and cases data
Location_and_Cases <- subset(COVID_Cases_Data, select = -c(Province.State,Lat,Long))
# Count the number of cases on each date
total_cases <- data.frame(colSums((Location_and_Cases)[-1]))

names(total_cases) <- NULL # Remove un-needed header name

total_cases <- t(total_cases) # Transpose into original wide format

row.names(total_cases)[1] <- "Total_Cases"
View(total_cases)


# Create a new data frame with only the country and deaths data
Location_and_Deaths <- subset(COVID_Deaths_Data, select = -c(Province.State,Lat,Long))
# Count the number of deaths on each date
total_deaths <- data.frame(colSums((Location_and_Deaths)[-1]))

names(total_deaths) <- NULL # Remove un-needed header name

total_deaths <- t(total_deaths) # Transpose into original wide format

row.names(total_deaths)[1] <- "Total_Deaths"
View(total_deaths)


# Combine the two data frames together - Cases in Mainland China and cases outside of China
Cases_vs_Deaths <- rbind(total_cases, total_deaths)


Date <- colnames(Cases_vs_Deaths) # Extract the dates as they are currently formatted in the column names portion
Date <- as.Date(Date, format = "X%m.%d.%y") # Convert to date format by specifying the format the dates are currently in
Date <- as.character(Date)


Cases_vs_Deaths_by_Date <- rbind(Date, Cases_vs_Deaths) # Combine the formatted dates into the data frame
colnames(Cases_vs_Deaths_by_Date) <- NULL # Remove un-needed header name
Cases_vs_Deaths_by_Date <- t(Cases_vs_Deaths_by_Date) # Transpose to long format
Cases_vs_Deaths_by_Date <- as.data.frame(Cases_vs_Deaths_by_Date)
View(Cases_vs_Deaths_by_Date)


Cases_vs_Deaths_by_Date$Total_Cases <- as.numeric(Cases_vs_Deaths_by_Date$Total_Cases)
Cases_vs_Deaths_by_Date$Total_Deaths <- as.numeric(Cases_vs_Deaths_by_Date$Total_Deaths)


ggplot(data = Cases_vs_Deaths_by_Date) + geom_point(aes(x = as.Date(Date),y = Total_Cases, colour = "Total_Cases")) + geom_point(aes(x = as.Date(Date),y = Total_Deaths, colour = "Total_Deaths")) + scale_y_continuous(limits = c(0,120000)) +
  labs(title = "COVID Cases vs Deaths", x = "Date", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Total_Cases" = "red", "Total_Deaths"="black")) +
  labs(colour = "Key")

