
## reads in a dataset of all residential property transactions from 2014 in 
## Seattle, creates dummy variables for zip code, property type, and whether a
## property has been remodelled. 

# Clearing environment
remove(list = ls())

gc()

# Importing necessary packages:
library(stargazer)

# Read-in data set
shv <- read.csv("Redlining_Data.csv", header = TRUE)


# Subset the data to include only the relevant variables
shv_data<- shv[, c("salespriceamount", "redlined", "buildingareasqft","BC_Excellent","BC_Average","BC_Good","BC_Fair","Home_Age","Home_Age")]

# Remove rows with missing values in the subset
shv_clean<- na.omit(shv_data)

# Descriptive statistics for the subset of properties without missing values
vtable(shv_clean, lush = TRUE) 
str(shv)
summary(shv_clean)

#Creating a summary statistic table
stargazer(shv_clean,title="Summary Statistics",type="text")
#subset rows
stargazer(shv_clean[1:4,],summary=FALSE,rownames=FALSE,title="First 4 Rows of Data")

#Run a regression model
# Linear regression model
model <- lm(salespriceamount ~ redlined + buildingareasqft + BC_Excellent+ BC_Average+ BC_Good+ BC_Fair+ Home_Age,data=shv_clean)

# Generate regression table
stargazer(model, title="Regression Results", align=TRUE, type="text")

shv_clean$redlined <- as.numeric(as.character(shv_clean$redlined))
# Repeat for other variables as needed
stargazer(model, 
          title = "Regression Results with Custom Labels",
          dep.var.labels = "Sales Price Amount",
          covariate.labels = c("Redlined", "Building Area (sqft)", "Lot Size (sqft)",
                               "Tract Non-White", "BC Excellent", "BC Average", 
                               "BC Good", "BC Fair", "Home Age"),
          type = "text",
          omit.coef = "NA")  # This will exclude coefficients with NA

#correlation matrix
cor_matrix <- cor(shv_clean[, sapply(shv_clean, is.numeric)], use="complete.obs")
stargazer(cor_matrix, title="Correlation Matrix", type="text")
install.packages("sandwich")

library(sandwich)

# Robust standard errors
cov <- vcovHC(model, type="HC")
robust_se <- sqrt(diag(cov))

# Display the regression table with robust SE
stargazer(model, model, 
          se=list(NULL, robust_se), 
          column.labels=c("Default", "Robust"),
          title="Regression with Robust SE", 
          type="text")


# Model A - redlined indicator - sales price in its original form is the y
Model_A <- lm(salespriceamount ~ redlined + buildingareasqft+ TractNonWhite+ BC_Excellent+ BC_Average+ BC_Good+ BC_Fair+ Home_Age,data = shv_clean, x = TRUE)

# Check the summary of the model
summary(Model_A)

#Generate standardized residuals
standardized_residuals <- rstandard(Model_A)


# Plot standardized residuals against fitted values
ggplot(data = data.frame(fitted = fitted(Model_A), residuals = standardized_residuals), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Standardized Residual Plot", x = "Predicted Sales Price", y = "Standardized Residuals") +
  theme_minimal()

# Save the plot
ggsave("Standardized_Residual_Plot.png")

# Load necessary library
library(readr)

# Load the data
data <- read_csv("Redlining_Data.csv")

# Transform the dependent variable (e.g., salespriceamount)
data$log_salespriceamount <- log(data$salespriceamount)

# Define the regression model
model <- lm(log_salespriceamount ~ buildingareasqft+TractNonWhite+ redlined + BC_Excellent + BC_Average+ BC_Good+ BC_Fair + Home_Age, data = data)

# Summarize the model
summary(model)

#Generate standardized residuals
standardized_residuals <- rstandard(model)

# Plot standardized residuals against fitted values
ggplot(data = data.frame(fitted = fitted(model), residuals = standardized_residuals), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Standardized Residual Plot", x = "Predicted Sales Price", y = "Standardized Residuals") +
  theme_minimal()

stargazer(Model_A, Model_B, type = "text", title="Regression Summary",
          align=TRUE,  dep.var.labels=c("Sales price","Log of sales price"),
          covariate.labels=c("Redlined","Building Square Footage", "Total Bathrooms - Count",
                             "Single Family", "Townhome",
                             "Building Condition- Excellent","Building Condition-Good",
                             "Building Condition-Average","Building Condition-Fair",
                             "Home Age", "Home Age - squared",),
          omit.stat=c("LL","ser","f"), no.space=TRUE)

