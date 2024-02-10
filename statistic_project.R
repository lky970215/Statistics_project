# Install necessary libraries
install.packages("ggplot2")

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("https://raw.githubusercontent.com/lky970215/Statistics_project/main/laptop_price.csv")

# Explore the size of the data
print(dim(data))

# Check the structure and size of the dataset
str(data)
head(data,10)

# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))

# Determine if there are any missing values
if (sum(missing_values) == 0) {
  print("There are no missing values in the dataset.")
} else {
  print("There are missing values in the dataset.")
}

# Determine the type of each data variable
data_types <- sapply(data, class)
print(data_types)

#Part 1 : Data Descriptive

# bar chart
company_counts <- table(data$Company)

company_counts_df <- as.data.frame(company_counts)

names(company_counts_df) <- c("Company", "Number of Laptops Sold")

ggplot(company_counts_df, aes(x = reorder(Company, -`Number of Laptops Sold`), y = `Number of Laptops Sold`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "The number of laptop models launched by major companies", x = "Company", y = "Number of Laptop Models") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = `Number of Laptops Sold`), vjust = -0.5, size = 3)

# Pie chart
total_prices <- tapply(data$Price, data$Company, sum)

total_prices_df <- data.frame(Company = names(total_prices), Total_Price = as.numeric(total_prices))

colors <- rainbow(length(total_prices_df$Company))

pie(total_prices_df$Total_Price, labels = total_prices_df$Company, main = "Total Prices of Laptops by Company", col = colors)

legend("topright", legend = total_prices_df$Company, fill = colors, cex = 0.8)

# Stem and leaf plot
stem(data$Price)

# Histogram
hist(data$Price, breaks = 20, xlab = "Price Range", ylab = "Quantity", main = "Laptop price distribution chart")

# Box plot
boxplot(data$Price, ylab = "Price", main = "Laptop Prices")

ggplot(data, aes(x = Company, y = Price_euros)) +
  geom_boxplot() +
  labs(x = "Company", y = "Laptop price", title = "Laptop price by Company")

# Descriptive statistics
summary(data$Price)

# Part 2 : inferential Analysis

# 1. Hypothesis Testing (1-sample or 2-sample)
# 1-sample t-test
t.test(data$Price, mu = 1000)  # Assuming the null hypothesis that the mean price is $1000

# 2-sample t-test
anova_model <- aov(Price_euros ~ Company, data = data)
summary(anova_model)  # Assuming two groups of laptops (different companies)

# 2. Goodness of Fit Test
# Chi-square goodness of fit test
chisq.test(table(cut(data$Price_euros, breaks = 5)))  # Assuming 5 price ranges

# 3. Chi-Square Test of Independence
# Chi-square test of independence between Company and other categorical variables
chisq.test(table(data$Company, data$Cpu))  # Example with Processor variable

# 4. Correlation
# Pearson correlation coefficient between Price and Screen Size
cor(data$Price_euros, data$Inches)

# 5. Regression
# Simple linear regression
lm_model <- lm(Price_euros ~ Inches, data = data)
summary(lm_model)

# scatter plot about Correlation and Regression
correlation <- cor(data$Price_euros, data$Inches)
plot(data$Inches, data$Price_euros, xlab = "Screen Size", ylab = "Price", main = "Scatter Plot of Screen Size vs. Price")
abline(lm(data$Price_euros ~ data$Inches), col = "red")  # Add linear regression line
legend("topright", legend = paste("Correlation:", round(correlation, 2)), bty = "n")  # Add correlation to legend

# 6. ANOVA
# One-way ANOVA
anova_model <- aov(Price_euros ~ Company, data = data)
summary(anova_model)