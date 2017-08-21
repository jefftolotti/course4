# Exploratory Graphs

pollution <- read.csv("./data/avgpm25.csv", colClasses=c("numeric", "character",
  "factor", "numeric", "numeric"))
head(pollution)

summary(pollution$pm25)

boxplot(pollution$pm25, col = "blue")

hist(pollution$pm25, col = "green", breaks = 100)
rug(pollution$pm25)

boxplot(pollution$pm25, col = "blue")
abline(h = 12)

hist(pollution$pm25, col = "green")
abline(v = 12, lwd = 2)
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)

barplot(table(pollution$region), col = "wheat", main = "Number of Counties in Each Region")

boxplot(pm25 ~ region, data = pollution, col = "red")

par(mfrow = c(2,1), mar = c(4, 4, 2, 1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)

par(mfrow = c(1,2), mar = c(5,4,2,1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))


# Plotting Systems in R

par(mfrow = c(1,1), mar = c(5,4,4,2))
library(datasets)
data(cars)
with(cars, plot(speed, dist))

library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))

library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)


# Base Plotting System

library(datasets)
hist(airquality$Ozone)

with(airquality, plot(Wind, Ozone))

airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in NYC"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in NYC",type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other"))

with(airquality, plot(Wind, Ozone, main = "NYC", pch = 20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)

par(mfrow = c(1, 2))
with(airquality, { plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")})

par(mfrow = c(1, 3), mar = c(4,4,2,1), oma = c(0,0,2,0))
with(airquality, { plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
  plot(Temp, Ozone, main = "Ozone and Temp")
  mtext("Ozone and Weather in NYC", outer = TRUE)
  })


# Base Plotting Demonstration

x <- rnorm(100)
hist(x)
y <- rnorm(100)
plot(x,y)
z <- rnorm(100)
plot(x, z)
par(mar = c(4,4,2,2))

plot(x,y,pch = 20)
plot(x,y,pch = 19)
plot(x,y,pch = 2)
example(points)

plot(x, y, pch = 20)
title("Scatterplot")
text(-2, -2, "label")
legend("topleft", legend= "Data", pch = 20)
fit <- lm(y ~ x)
abline(fit)
abline(fit, lwd = 3)
abline(fit, lwd = 3, col = "blue")
plot(x, y, xlab = "Weight", ylab = "Height", main = "Scatterplot", pch = 20)
legend("topright", legend = "Data", pch = 20)
abline(fit, lwd = 3, col = "red")
z <- rpois(100, 2)
par(mfrow = c(2,1))
plot(x, y, pch = 20)
plot(x, z, pch = 19)
par(mar = c(2,2,1,1))

par(mfrow = c(1,1))
x <- rnorm(100)
y <- x + rnorm(100)
g <- gl(2, 50)
g <- gl(2, 50, labels = c("Male", "Female"))
str(g)
plot(x,y)
plot(x, y, type = "n")
points(x[g=="Male"], y[g=="Male"], col = "Green")
points(x[g=="Female"], y[g=="Female"], col = "Blue", pch = 19)


# Graphics devices in R

library(datasets)
with(faithful, plot(eruptions, waiting)) #Make plot appear on screen
title(main = "Old Faithful Geyer data") #annotate with title
dev.copy(png, file = "geyserplot.png") #copy to PNG file
dev.off() #don't forget to close the png device

pdf(file = "myplot.pdf") ## Open PDF device; create pdf in working directory
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful geyser data") #annotate, still nothing on screen
dev.off() #now it's closed and can view on comp
