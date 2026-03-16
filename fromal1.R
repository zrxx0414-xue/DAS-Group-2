df=read.csv("C:/Users/86185/Downloads/DAProject2.csv",stringsAsFactors = T)
#question1
fit_year <- lm(BMI ~ factor(Year), data = df)
summary(fit_year)
anova(fit_year)
#no changes for BMI over years

#question2
#smooth curve from EDA showed a non-linear relationship btw BMI and Age, so quadratic age term was added to the model.
fit_age=lm(BMI ~ Age + I(Age^2),data=df)
summary(fit_age)
#including Age^2 is significant
fit_linear <- lm(BMI ~ Age + Sex + Education + Veg + Fruit, data = df)
fit_quad_1   <- lm(BMI ~ Age + I(Age^2) + Sex + Education + Veg+ Fruit, data = df)
anova(fit_linear, fit_quad_1)
summary(fit_quad_1)
AIC(fit_quad_1)
#where sex and fruit are not significant

fit_sex=lm(BMI ~ Sex,data=df)
summary(fit_sex)
#sex not significant by itself
fit_fruit=lm(BMI ~ Fruit,data=df)
summary(fit_fruit)
#fruit significant by itself
#cuz there might be strong correlation btw Veg and Fruit

#EDA (Veg~Fruit)
chisq.test(table(df$Fruit, df$Veg))
ggplot(df, aes(x = Fruit, fill = Veg)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion") +
  theme_minimal()



fit_quad_2   <- lm(BMI ~ Age + I(Age^2) +  Education + Veg + Fruit, data = df)
summary(fit_quad_2)
AIC(fit_quad_2)
#model is improved by dropping sex
fit_quad_3   <- lm(BMI ~ Age + I(Age^2) +  Education + Veg, data = df)
summary(fit_quad_3)
AIC(fit_quad_2)
#model is not improved by dropping fruit

#we can plot a comparsion for fit_quad_1&fit_quad_2&fit_quad_3
#where we can conclude fit_quad_2 is the best fit
#i.e. BMI is relative with age,edu,veg,fruit--but not--sex,year