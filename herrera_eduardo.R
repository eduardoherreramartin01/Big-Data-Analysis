setwd("C:/Users/eduar/OneDrive/Documents/bigdata")
fat <- read.table("C:/Users/eduar/OneDrive/Documents/bigdata/fat.dat.txt", header = FALSE, sep = "", stringsAsFactors = FALSE)
fix(fat) #to change variable names
fat$height[fat$case==42] <- 69.5
fat[fat$brozek < 0, 'brozek'] <- 0    
fat[fat$siri < 0, 'siri'] <- 0  
fix(fat)

---------------------------------------------------------------------------------------------------------------------
#EXPLORATORY ANALYSIS :

library(ggplot2)
library(dplyr)
library(car)

  
summary(fat)
cor(fat)
summary(fat$brozek)
summary(fat$siri)
par(mfrow = c(1, 1))
hist(fat$brozek, col="dodgerblue4", main = "Histogram Of Brozek's Body Fat Percentage", xlab='Body Fat Percentage', prob=TRUE )
density_brozek <- density(fat$brozek)
lines(density_brozek, col="red", lwd=2)
hist(fat$siri, col= "gold3", main = "Histogram Of Siri's Body Fat Percentage", xlab='Body Fat Percentage', prob=TRUE)
density_siri <- density(fat$siri)
lines(density_siri, col="red", lwd=2)
selected_variables <- c("brozek", "age", "height", "neck", "chest", "abdomen", "hip", "thigh", "knee", "ankle", "biceps", "forearm", "wrist")
selected_data <- fat[selected_variables]


scatterplotMatrix(~brozek + age, data=selected_data)

selected_variables <- c("siri", "age", "height", "neck", "chest", "abdomen", "hip", "thigh", "knee", "ankle", "biceps", "forearm", "wrist")
selected_data <- fat[selected_variables]


scatterplotMatrix(~siri + age, data=selected_data)


pairs(fat[, c("age", "height", "neck", "thigh", "knee", "ankle", "biceps", "forearm", "wrist")], main="Pairwise Scatter Plots")

correlation_matrix <- cor(fat)

brozek_correlations <- correlation_matrix['brozek',]
brozek_correlations['siri'] <- NA 
brozek_sorted <- brozek_correlations[order(-abs(brozek_correlations), na.last = TRUE)]
top_brozek <- head(brozek_sorted, 11)

siri_correlations <- correlation_matrix['siri',]
siri_correlations['brozek'] <- NA  
siri_sorted <- siri_correlations[order(-abs(siri_correlations), na.last = TRUE)]
top_siri<- head(siri_sorted, 11)

top_brozek_df <- data.frame(Variable = names(top_brozek), Correlation = top_brozek)
top_siri_df <- data.frame(Variable = names(top_siri), Correlation = top_siri)

top_brozek_df
top_siri_df




----------------------------------------------------------------------------------------------------------------------
  
#PREDICTIVE MODEL


( cutoff <- 4/(nrow(fat)-length(model_siri$coefficients)-2) )
plot(model_siri, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

outlierTest(model_brozek)


fat_brozek <- fat[-216, ]
fat_brozek <- fat[-86, ]
fat_brozek <- fat[-39, ]


fat_siri <- fat[-86, ]
fat_siri <- fat[-39, ]



model_siri <- lm(siri ~ neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, data = fat_siri)
summary(model_siri)

model_brozek <- lm(brozek ~ neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, 
                               data = fat_brozek)
summary(model_brozek)

plot(model_brozek)

par(mfrow =c(2, 2))



qqPlot(model_brozek, simulate=TRUE, labels=row.names(fat),
       id=list(method="identify"), main="Q-Q Plot")


library (gvlma)
gvmodel <- gvlma(model_siri)
summary(gvmodel)

library(dplyr)
brozek_coef <- as.data.frame(coef(summary(model_brozek)))
siri_coef <- as.data.frame(coef(summary(model_siri)))



brozek_coef$Model <- 'Brozek'
siri_coef$Model <- 'Siri'
coef_comparison <- bind_rows(brozek_coef, siri_coef)
coef_comparison$Term <- rownames(coef_comparison)
ggplot(coef_comparison, aes(x = Term, y = Estimate, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Coefficient Comparison between Brozek and Siri Models",
       x = "Predictors", y = "Coefficient Estimate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

----------------------------------------------------------------------------------------------------------------------

# ANOVA model
  

fat$age_group <- cut(fat$age, 
                       breaks = c(20, 40, 65, Inf), 
                       labels = c("group1", "group2", "group3"), 
                       right = FALSE,
                       include.lowest = TRUE) 

fat_brozekaov <- fat[-216, ]
fat_brozekaov <- fat[-86, ]
fat_brozekaov <- fat[-39, ]


fat_siriaov <- fat[-86, ]
fat_siriaov <- fat[-39, ]


anova_model_siri <- aov(siri ~ age_group, data = fat_siriaov)
anova_model_brozek <- aov(brozek ~ age_group, data = fat_brozekaov)


summary(anova_model_siri)
TukeyHSD(anova_model_siri)


summary(anova_model_brozek)
TukeyHSD(anova_model_brozek)




library(ggplot2)
ggplot(fat_brozekaov, aes(x = age_group, y = brozek, fill = age_group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Brozek Body Fat Percentage by Age Group",
       x = "Age Group",
       y = "Brozek Body Fat Percentage") +
  scale_fill_manual(values = c("lightyellow2", "lightyellow3", "lightyellow4"))
ggplot(fat_siriaov, aes(x = age_group, y = siri, fill = age_group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Siri Body Fat Percentage by Age Group",
       x = "Age Group",
       y = "Siri Body Fat Percentage")+
  scale_fill_manual(values = c("darkseagreen2", "darkseagreen", "darkseagreen4"))















