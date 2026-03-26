------------------------------------------------------------------------

**Question 1**

```{r}
library(tidyverse)
library(ggplot2)
library(performance)
library(see)
library(sjPlot)
library(kableExtra)
library(broom)
library(knitr)
library(car)
```

```{r}
#| eval: false
mean <- dataset%>%
  summarise(mean_BMI = mean(BMI), 
            .by = Year)
ggplot(mean, aes(x = Year, y = mean_BMI))+
  geom_line()
```

From the plot the it is suggested that there is no significant trend followed by the observation over 2008-2012

```{r}
#The Model for question 1 
time_model <- lm(BMI~Year, data = dataset)
summary(time_model)
#we will chech the model diagnostics 
check_model(time_model)


#complementary model and its check no need to include it 
#model_q1 <-lm(formula = BMI ~ Age + I(Age^2) + Education +
    #Veg + Year + Age:Sex + Sex:Education + Fruit:Education, data = dataset)
#Note that I have added the year term in the final model becuase we are checking prevelance with time. 
#summary(model_q1)
#check_model(model_q1)
#check_model(model_q1,check = c("linearity","homogeneity"))
#check_model(model_q1,check = c("qq","normality"))


```

```{r}
tidy(time_model, conf.int = TRUE) %>%
  select(term, estimate, p.value, conf.low, conf.high) %>%
  kable(
    digits = 3, 
    booktabs = TRUE, 
    col.names = c("Term", "Estimate", "P-Value", "Lower CI", "Upper CI") ) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"), full_width = FALSE, font_size = 8) %>%
  # Adjust column 1 (Predictors) to be 4cm wide
  column_spec(1, width = "4cm") %>%
  # Adjust the rest to be narrower (e.g., 2cm)
  column_spec(2:5, width = "2cm")
```

Here year is not a significant predictor of BMI as the p-value is greater than 0.05, however we may still see an increase in BMI over the years due to the coefficient term being positive. The confidence interval contains 0, further suggesting that there is no prevalence in BMI from year to another.

In conclusion there have been changes in BMI over time as seen from the plot, but changes in year is not what is causing the change in BMI. Thus, variation in BMI is explained by peoples education and lifestyle status rather than the year when data was collected.
