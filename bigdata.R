setwd("C:/Users/eduar/OneDrive/Documents/R")

fat <- read.csv('fat.csv')

fix(fat)#Exploratory Data Analysis (EDA):
#Generate summary statistics (mean, median, standard deviation, etc.) for each variable.
summary(fat)
cor(fat)
summary(fat$brozek)
summary(fat$siri)
hist(fat$brozek, col="#0073C2FF", main = "Histogram Of Brozek's Body Fat Percentage", xlab='Body Fat Percentage' )
hist(fat$siri, col= "#EFC000FF", main = "Histogram Of Siri's Body Fat Percentage", xlab='Body Fat Percentage')

#Identify any correlations between body fat and other measurements using scatter plots or correlation matrices.
correlation_matrix <- cor(fat)

get_top_correlations_abs <- function(cor_vector, variable_name, top_n = 11) {
  sorted_cor <- sort(abs(cor_vector), decreasing = TRUE)
  top_vars <- head(sorted_cor, n = top_n)
  return(data.frame(
    Correlation = cor_vector[names(top_vars)],
    VariableOfInterest = rep(variable_name, each = top_n)
  ))
}

top_correlations_brozek <- get_top_correlations_abs(correlation_matrix[,'brozek'], 'brozek')

top_correlations_siri <- get_top_correlations_abs(correlation_matrix[,'siri'], 'siri')

library(knitr)
print(top_correlations_brozek)
print(top_correlations_siri)

library(ggplot2)

# Extract the top correlated variables
top_variables_brozek <- names(top_correlations_brozek$Correlation)
top_variables_siri <- names(top_correlations_siri$Correlation)

# Create scatter plots for 'brozek'
scatter_plots_brozek <- lapply(top_variables_brozek, function(var) {
  ggplot(fat, aes(x = fat[['brozek']], y = fat[[var]])) +
    geom_point() +
    ggtitle(paste("Scatter Plot for Brozek vs", var)) +
    theme_minimal()
})

# Create scatter plots for 'siri'
scatter_plots_siri <- lapply(top_variables_siri, function(var) {
  ggplot(fat, aes(x = fat[['siri']], y = fat[[var]])) +
    geom_point() +
    ggtitle(paste("Scatter Plot for Siri vs", var)) +
    theme_minimal()
})

# Print the scatter plots
scatter_plots_brozek
scatter_plots_siri


library(ggplot2)

# Scatter plot for chest vs brozek
ggplot(fat, aes(x = knee, y = brozek)) +
  geom_point(color = "#0073C2FF", size = 3) +
  ggtitle("Scatter Plot for Abdomen vs Brozek") +
  xlab("Knee Circumference (cm)") +
  ylab("Brozek's Body Fat Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))


ggplot(fat, aes(x = knee, y = siri)) +
  geom_point(color = "#EFC000FF", size = 3) +
  ggtitle("Scatter Plot for Knee vs Siri") +
  xlab("Knee Circumference (cm)") +
  ylab("Siri's Body Fat Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# multiple regression to fit body fat percentage to other body measurements.

brozeklm <- lm(brozek ~ . -siri, data = fat)
summary(brozeklm)

sirilm <- lm(siri ~ . -brozek, data = fat)
summary(sirilm)
str(fat)
brozeklm_revised <- lm(brozek~ .-siri -adiposity -density - weight -fatfree -chest -abdomen -hip, data = fat)
sirilm_revised <- lm(siri~ .-brozek -adiposity -density - weight -fatfree -chest -abdomen -hip, data = fat)


# Check the summary of the new model
summary(brozeklm_revised)
summary(sirilm_revised)
par(mfrow =c(2, 2))
plot(brozeklm_revised)

fat$age_group <- cut(fat$age, 
                     breaks = c(20, 40, 65, Inf), 
                     labels = c("group1", "group2", "group3"), 
                     right = FALSE, # This means the intervals are [20,40), [40,65), [65, Inf)
                     include.lowest = TRUE) 

fix(fat)

anova_model_siri <- aov(siri ~ age_group, data = fat)
anova_model_brozek <- aov(brozek ~ age_group, data = fat)

# View the summary of the ANOVA model
summary(anova_model_siri)
summary(anova_model_brozek)

TukeyHSD(anova_model_siri)

TukeyHSD(anova_model_brozek)
plot(brozeklm)

library(car)
vif_values <- vif(sirilm)

# Display the VIF values
print(vif_values > 10)

aov(brozek~age_group)


library(ggplot2)

# Assuming your data frame is named 'fat' and it has the 'age_group' variable you created
# Boxplot for Brozek body fat percentage
ggplot(fat, aes(x = age_group, y = brozek)) +
  geom_boxplot() +
  labs(title = "Boxplot of Brozek Body Fat Percentage by Age Group",
       x = "Age Group",
       y = "Brozek Body Fat Percentage")
ggplot(fat, aes(x = age_group, y = siri)) +
  geom_boxplot() +
  labs(title = "Boxplot of Siri Body Fat Percentage by Age Group",
       x = "Age Group",
       y = "Siri Body Fat Percentage")


fat$weight_group <- cut(fat$weight, 
                        breaks = c(100, 160, 220, Inf), 
                        labels = c("Slim", "normal", "fatsos"), 
                        right = FALSE, # This means the intervals are [100,180), [180,260), [260, Inf)
                        include.lowest = TRUE) # This includes the lowest value in the first interval

anova_model_siriw <- aov(brozek ~ weight_group, data = fat)
summary(anova_model_siriw)
TukeyHSD(anova_model_siriw)


modified_model_brozek <- lm(brozek ~ age * weight_group + height + neck + thigh + knee + ankle + biceps + forearm + wrist, data = fat)
summary(modified_model_brozek)

# Check for multicollinearity
vif(modified_model_brozek)

# Residual analysis
plot(modified_model_brozek)




ggplot(fat, aes(x = height, y = brozek)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line
  labs(title = "Relationship between Height and Brozek Body Fat Percentage",
       x = "Height",
       y = "Brozek Body Fat Percentage") +
  theme_minimal() +
  scale_x_continuous(limits = c(60, max(fat$height))) 

fat$weight_group <- cut(fat$weight, 
                        breaks = c(100, 180, 260, Inf), 
                        labels = c("100-180", "180-260", "260+"), 
                        right = FALSE, # This means the intervals are [100,180), [180,260), [260, Inf)
                        include.lowest = TRUE) # This includes the lowest value in the first interval


fix(fat)
view(fat)


library(ggplot2)
ggplot(fat, aes(x = age, y = siri, color = weight_group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Scatter Plot of Age vs Brozek Body Fat Percentage by Weight Group", x="Age", y="Brozek Body Fat Percentage")


pairs(fat[, c("age", "height", "neck", "thigh", "knee", "ankle", "biceps", "forearm", "wrist", "brozek", "siri")], main="Pairwise Scatter Plots")




fit_brozek <- lm(brozek ~ ., data = fat)

# Test for outliers
outliers <- outlierTest(fit_brozek)

# Check the results
print(outliers)

str(fat)

body_circumference <- fat[, c('siri', 'brozek', 'neck', 'chest', 'abdomen', 'hip', 'thigh', 'knee', 'ankle', 'biceps', 'forearm', 'wrist')]

# Calculate the correlation matrix for body circumference measurements
correlation_matrix <- cor(body_circumference)

# Print the correlation matrix
print(correlation_matrix)

