# Load Necessary Libraries
library(ggplot2)       # Visualization
library(dplyr)         # Data manipulation
library(MASS)          
library(sandwich)      
library(readr)
library(psych)
library(lmtest)
library(PerformanceAnalytics)

# Load Dataset
lending_dataset <- read_csv("lending_dataset1_aggr.csv")

# summary statistics
sum(is.na(lending_dataset))
colnames(lending_dataset)
str(lending_dataset)
describe(lending_dataset)

# 1. Descriptive Analysis
# Summary Statistics
numeric_summary <- lending_dataset %>% select_if(is.numeric) %>% describe()
print(numeric_summary)
sum(is.na(lending_dataset))# there is no null values

#categorical variable

# Get the summary of categorical variables
summary_categorical <- lending_dataset %>% select_if(~is.factor(.) | is.character(.)) %>% lapply(table)  # Get the summary statistics for categorical variables
print(summary_categorical)



# Correlation Analysis
cor_matrix <- lending_dataset %>% select_if(is.numeric) %>% cor()
print(cor_matrix)

# Visualizations

# Plot for each variable

# 1. Plot for  histogram for `Experience'
ggplot(lending_dataset, aes(x = Experience)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Experience", x = "Experience", y = "Frequency")

# 2. Plot for  histogram for `Earnings`)
ggplot(lending_dataset, aes(x = Earnings)) + 
  geom_histogram(binwidth = 10000, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Earnings", x = "Earnings", y = "Frequency")

# 3. Plot bar plot for `HomeOwner`
ggplot(lending_dataset, aes(x = HomeOwner)) + 
  geom_bar(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Bar Plot of Home Ownership", x = "Homeowner (Yes/No)", y = "Count")

# 4. Box plot for `BadPastRecords`
ggplot(lending_dataset, aes(x = BadPastRecords)) + 
  geom_bar(binwidth = 10000, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Bar plot  of BadPastRecords", x = "BadPastRecords", y = "Frequency")

# 5. Box plot for `ObsRate` (default rate)
ggplot(lending_dataset, aes(x = ObsRate)) + 
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black", alpha = 0.8, boundary = 0) +
  labs(title = "Histogram of Default Rate (ObsRate)", 
       x = "Observed Default Rate", 
       y = "Frequency")
# obsrate are normaly distributed
# 6. Plot for categorical variable (e.g., bar plot for `Gender`)
ggplot(lending_dataset, aes(x = Gender)) + 
  geom_bar(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Bar Plot of Gender Distribution", x = "Gender", y = "Count")

# 7. Plot for  `Age` distribution)
ggplot(lending_dataset, aes(x = Age)) + 
  geom_histogram(binwidth = 1, fill = "cyan", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")


# Earnings vs Defaults
ggplot(lending_dataset, aes(x = Earnings, y = Defaults)) +
  geom_point(color = "darkred") +
  labs(title = "Earnings vs Defaults", x = "Earnings", y = "Defaults")




# Correlation graph summarize all  about correletion of variables
chart.Correlation(lending_dataset %>% select_if(is.numeric), histogram = TRUE, pch = 19)



## model fitting
# 2. Initial generalized Linear Regression Model
## Glm model1 (since the response variable is bounded between 0 and 1 we are obiged to fit the generalized linear model)
glm_model1 <- glm(ObsRate ~ Age + Gender + Experience + Earnings + Residence +
                   HomeOwner + LandOwner + BadPastRecords + CarOwner  + Accounts,data = lending_dataset,family = quasibinomial(link = "logit"))
summary(glm_model1)


# modified model

# Fit the modified model with significant predictors
glm_modified <- glm(ObsRate ~ Experience + Earnings + HomeOwner + BadPastRecords,
                    family = quasibinomial(link = "logit"),
                    data = lending_dataset)

# View the summary of the modified model
summary(glm_modified)

# the variable Earnings  and Experiences are highly correlated so 
#  let check if we can modify the model  again.

glm_modified_new <- glm(ObsRate ~ Experience + HomeOwner + BadPastRecords, 
                    family = quasibinomial(link = "logit"), 
                    data = lending_dataset)

summary(glm_modified_new)


# let test the model to be preferred using deviance and likelhood ratio test

dv_modified<- glm_modified$deviance
dv_modified_new<- glm_modified_new$deviance
cat("Residual Deviance (Initial Model with 4 Parameters):",dv_modified , "\n")
cat("Residual Deviance (Modified Model with 3 Parameters):", dv_modified_new, "\n")

#  likelhood ratio test

lr_test <- anova(glm_modified, glm_modified_new, test = "Chisq")
print(lr_test) # this test shows that the model with four parameter is significant than one with three parameter.

### The test shows that the model with four parameter would be preferred.

# model assumption check

par(mfrow = c(2, 2))

# 1. Plot Pearson residuals vs Fitted values (Homoscedasticity check)
plot(glm_modified$fitted.values, residuals(glm_modified, type = "pearson"),
     xlab = "Fitted Values", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs Fitted Values")
abline(h = 0, col = "red")

# 2. Plot Deviance residuals vs Fitted values (Alternative Residuals)
plot(glm_modified$fitted.values, residuals(glm_modified, type = "deviance"),
     xlab = "Fitted Values", ylab = "Deviance Residuals", 
     main = "Deviance Residuals vs Fitted Values")
abline(h = 0, col = "red")

# 3. Cook's Distance Plot (Check for Influential Points)
cooks_distance <- cooks.distance(glm_modified)
plot(cooks_distance, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4 / nrow(lending_dataset), col = "red")

# 4. Q-Q plot of Pearson Residuals (Normality Check)
qqnorm(residuals(glm_modified, type = "pearson"), main = "Q-Q plot of Pearson Residuals")
qqline(residuals(glm_modified, type = "pearson"), col = "red")

#5.Durbin-Watson test for autocorrelation
dwtest(glm_modified) #There  is no  auto correlation

#6 Calculate VIF to check multicollinearity
vif(glm_modified)



