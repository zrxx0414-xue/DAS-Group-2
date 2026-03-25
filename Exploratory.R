# Exploratory Analysis

library(tidyverse)
library(ggplot2)

BMIdata <- read.csv("DAProject2.csv", stringsAsFactors = TRUE)

# Create a useful column for exploratory plots
BMIdata$FruitVeg <- interaction(BMIdata$Fruit, BMIdata$Veg)


# Organise Education level in a more interpretable manner (ranging from lowest to highest level of prestige)
BMIdata$Education <- factor(BMIdata$Education, 
                            levels = c("No qualifications", "Other school level", 
                                       "Standard grade or equiv", "Higher grade or equiv", 
                                       "HNC/D or equiv", "Degree or higher"))


# Subset of the data which will help to create a more readable exploratory plot
BMIdata_subset <- BMIdata %>%
  filter(Education %in% c("No qualifications", "Degree or higher"))


# Creates a normalised table indicating the portions of participants at each education level who meet each category of fruit
healthEdu <- table(BMIdata$FruitVeg, BMIdata$Education)
healthEduNorm <- sweep(healthEdu, 2, colSums(healthEdu), FUN = "/")
rownames(healthEduNorm) <- c("Neither", "Fruit only", "Veg only", "Both")
healthEduNorm

# Boxplot indicating constant BMI across the years of the study
ggplot(BMIdata, aes(x = Year, y = BMI)) +
  
  # Making the plot more readable by reducing it's range on the BMI axis, and excluding outlier points
  geom_boxplot(aes(group = Year), outlier.shape = NA) +
  coord_cartesian(ylim = c(10, 45))



ggplot(BMIdata_subset, aes(x = Age, y = BMI)) + 
  
  geom_point(aes(colour = FruitVeg), size = 1.0) +
  
  facet_grid(Sex ~ Education) +
  
  # Mean line for reference
  stat_summary(
    aes(linetype = "Mean BMI"),       # maps the line to a new legend 
    fun = mean,
    geom = "line",
    colour = "grey20",
    size = 1.0) +
  
  # legend labels
  scale_colour_manual(
    values = c(
      "No.No" = "red",
      "Yes.No" = "orange",
      "No.Yes" = "yellow",
      "Yes.Yes" = "green"),
    
    labels = c(
      "No.No" = "Neither",
      "Yes.No" = "Fruit Only",
      "No.Yes" = "Veg Only",
      "Yes.Yes" = "Both")) +
  
  scale_linetype_manual(
    values = c("Mean BMI" = "solid")) +
  
  # Legend title
  labs(colour = "Recommended fruit/veg intake?", linetype = "")

# Histogram potentially demonstrating normality
ggplot(BMIdata, aes(x = BMI)) +
  geom_histogram(binwidth = 1 , fill = "yellow4", colour = "grey") +
  labs(title = "Histogram of BMI scores", xlab = "BMI", ylab = "Frequency")
  


## This is unlikely to be useful
# ggplot(BMIdata, aes(x = Age, y = BMI)) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = "loess", color = "blue", se = TRUE) +
#   labs(
#     title = "BMI vs Age",
#     x = "Age",
#     y = "BMI") +
#   theme_minimal()




