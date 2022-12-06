library(readr)
library(ggplot2)
library(dplyr)
data <- read.csv("D:\\cmi\\SEM I\\VISU\\IndiaAffectedWaterQualityAreas.csv")
data$State.Name <- gsub(pattern = "CHATTISGARH", replacement = "CHHATTISGARH", data$State.Name)
data$Block.Name <-gsub(pattern = "RAJAHMUNDRY\xae (29)", replacement = "RAJAHMUNDRY(29)", data$Block.Name)
data$Year <- as.Date(data$Year, format = "%d-%m-%Y")
data$Year <- format(data$Year, format = "%Y")
data$Quality.Parameter <- as.factor(data$Quality.Parameter)


freq <- as.data.frame(table(data$Year, data$Quality.Parameter))
names(freq) <- c("Year", "Parameter","Freqs" )

c1 <- distinct(data, Quality.Parameter)

states <-distinct(data[c("State.Name")])




freq2 <- as.data.frame(table(data$Year,data$District.Name, data$Quality.Parameter))
names(freq2) <- c("Year", "District", "Parameter", "Freq")

data[data == "RAJAHMUNDRY\xae (29)"] <- "RAJAHMUNDRY(29)"

freq3 <- as.data.frame(table(data$Year,data$Block.Name, data$Quality.Parameter))
names(freq3) <- c("Year", "Block", "Parameter", "Freq")

url <- a(" Indian Water Quality Dataset", href="https://www.kaggle.com/datasets/venkatramakrishnan/india-water-quality-data")
