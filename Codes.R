



#loading requred packages
library(ggplot2)
library(ggExtra)
library(gridExtra)
library(scatterplot3d)
library(PerformanceAnalytics)
library(corrplot)
library(readxl)

#loading data file in excel format
commit <- read_excel("D:/My Documents/pdf/R/data.R/commit.xlsx")

#checking the data structure
str(commit)

head(commit)

attach(commit)
##looking for missing values (NA)
apply(commit, 2, function(x) sum(is.na(x)))

summary(commit)



#visualization


g3 <- ggplot(data = commit, aes(x = commitment)) +
  geom_histogram(aes(y =..density..), col="red",
                 fill="skyblue",
                 alpha = .2,
                 binwidth = 5) +
  stat_function(fun = dnorm, args = list(mean = mean(commitment),
                                         sd = sd(commitment))
                , color = "darkorange", size = 1) +
  ggtitle("histogram of commitment")

g4 <- ggplot(data = commit, aes(sample = commitment)) + 
  geom_qq(color = "dodgerblue", pch = 20, cex = 3) +
  geom_qq_line(color = "darkorange", size = 1) +
  labs(y = "commitment") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  ggtitle("Normal Q-Q plot of commitment")

grid.arrange(g3, g4, nrow = 2)



#scatter plot of commitment vs leadership
ggplot(commit, aes(x = leadership, y = commitment)) + 
  geom_point(col = "dodgerblue", cex = 4, pch = 20) +
  
  stat_smooth(method = "lm", col = "darkorange",
              se = TRUE) 





#scatter plot of commitment vs justice
ggplot(commit, aes(x = justice, y = commitment)) + 
  geom_point(col = "dodgerblue", cex = 4, pch = 20) +
  
  stat_smooth(method = "lm", col = "darkorange",
              se = TRUE)




chart.Correlation(commit, histogram = TRUE, method = "pearson")
#correlation test
cor.test(commitment, leadership, method = "pearson")
cor.test(commitment, justice, method = "pearson")


commit.M <- cor(commit)
corrplot(commit.M)

#3 dimensional scatter plot of data
scatterplot3d(commit[, 1:3], pch = 16, color = "dodgerblue",
              angle = 55,grid = TRUE, box = FALSE,
              main = "3d scatterplot of data")


s3d <- scatterplot3d(commit, type = "h", color = "blue", 
                     angle = 55, pch = 16, 
                     main = "regression plane for data", box = F)
s3d$plane3d(lm(commitment ~ leadership + justice, 
               data = commit), col = "red")


#fitting a linear regression model 
commit_lm <- lm(commitment ~ leadership + justice, data = commit)


summary(commit_lm)
residuals <- commit_lm$residuals
fitted <- commit_lm$fitted.values

#checking model adequacy 
par(mfrow = c(2, 2))

#scatter plot of fitted values versus residuals
plot(fitted, residuals, col = "blue", pch = 20, main = 
       "scatter plot of residuals vs fitted values", cex = 2)


plot.ts(residuals, col = "darkorange", lwd = 2, main = 
          "time series plot of residuals")
abline(h = 0, col = "dodgerblue", lwd = 2)


hist(residuals, col = "skyblue", border = "darkorange")


qqnorm(residuals, pch = 20, col = "dodgerblue", cex = 2)
qqline(residuals, lwd = 2, col = "darkorange")

