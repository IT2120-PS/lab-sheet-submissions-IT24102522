setwd("C:\\Users\\it24102522\\Desktop\\PS_Lab_04")

branch_data <- read.table("Exercise.txt", header = TRUE, sep= ",")

#2

str(branch_data)

summary(branch_data)

#3
# Boxplot for sales
boxplot(branch_data$Sales_X1, main = "Boxplot of Sales", ylab = "Sales")

#4
# summary of the Five-number and IQR for advertising
summary(branch_data$Advertising)

# IQR
IQR(branch_data$Advertising_X2)


#5
# Finding the outliers
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- IQR(x)
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}

# Check for outliers in the "Years" variable
find_outliers(branch_data$Years)
