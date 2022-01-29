#Interesting code from Chapter 2
# 1. Plot example with titles and labels
# 2. Create PDF with figure 
# 3. 3D plotting: contour, image, and persp 
# 4. Plots and corresponding ggplot 

#install.packages("ISLR")
library(ISLR)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("GGally")
library(GGally)

# 1. Plot example ---------------------------------------------------------
x <- rnorm(100)
y <- rnorm(100)
plot(x, y)
plot(x, y, xlab = "this is the x-axis",
     ylab = "this is the y-axis",
     main = "Plot of X vs Y")

# 2. PDF with figure ------------------------------------------------------
pdf("Figure.pdf")
plot(x, y, col = "green")
dev.off()

# 3. 3D plotting: contour, image, and persp ------------------------------
x <- seq(-pi, pi, length = 50)
y <- x
f <- outer(x, y, function(x, y) cos(y) / (1 + x^2))
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)
fa <- (f - t(f)) / 2
contour(x, y, fa, nlevels = 15)
###
image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)

# 4. Plots and corresponding ggplot  --------------------------------------
#used for base R plots
attach(Auto)  

#used for ggplot
auto <- Auto %>% 
    mutate(cylinders = as.factor(cylinders))

Auto$cylinders <- as.factor(cylinders)

auto %>% 
  ggplot(aes(cylinders, mpg)) +
  geom_point()

###
auto %>% 
  ggplot(aes(cylinders, mpg)) +
  geom_boxplot(fill='red', varwidth = TRUE) +
  coord_flip()

plot(cylinders, mpg, col = "red", varwidth = t,
     xlab = "cylinders", ylab = "mpg")

auto %>% 
    ggplot(aes(mpg)) +
    geom_histogram(binwidth = 5, fill="#FF9999", colour="black")

hist(Auto$mpg, col = 2, breaks = 15)
###
pairs(Auto)
pairs(
  ~ mpg + displacement + horsepower + weight + acceleration,
  data = Auto
)

auto %>% 
  select(mpg, displacement, horsepower, weight, acceleration) %>% 
  ggpairs(auto)

###
summary(auto)
###
summary(mpg)
###