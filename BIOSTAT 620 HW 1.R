install.packages("dplyr")
library(dplyr)
library(readxl)
Screentime_SPH<- read_excel("Screentime_SPH.xlsx")
attach(Screentime_SPH)

#create and add 2 new variables
Screentime_SPH= Screentime_SPH %>%
  slice(7:27)%>%
  mutate(daily_use_min= Total.ST.min / Pickups, Prop_social= Social.ST.min/Pickups)
head(Screentime_SPH)


#distinguishing weekday, weekend
Screentime_SPH$weekday <- weekdays(Screentime_SPH$Date, abbreviate = TRUE)

Screentime_SPH <- Screentime_SPH %>%
  mutate(
    if_weekend = as.factor(ifelse(weekday %in% c("Sun", "Sat"), "weekend", "weekday"))
  )

#TIME SERIES PLOTS
library(ggplot2)

ggplot(Screentime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend)) +
  geom_line(color = "steelblue") +
  geom_point() +
  labs(x = "Date", y = "Total screentime in min") +
  scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
  scale_color_manual(labels = c("weekdays", "weekends"), values = c("pink", "black")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())





ggplot(Screentime_SPH, aes(x = Date, y = Social.ST.min, color = if_weekend)) +
  geom_line(color = "steelblue") +
  geom_point() +
  labs(x = "Date", y = "Social.ST.min") +
  scale_y_continuous(limits = c(50, 250), breaks = seq(50, 250, by = 50)) +
  scale_color_manual(labels = c("weekdays", "weekends"), values = c("pink", "black")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())



ggplot(Screentime_SPH, aes(x = Date, y = Pickups, color = if_weekend)) +
  geom_line(color = "steelblue") +
  geom_point() +
  labs(x = "Date", y = "Pickups") +
  scale_y_continuous(limits = c(75, 350), breaks = seq(75, 350, by = 25)) +
  scale_color_manual(labels = c("weekdays", "weekends"), values = c("pink", "black")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())


ggplot(Screentime_SPH, aes(x = Date, y = Prop_social, color = if_weekend)) +
  geom_line(color = "steelblue") +
  geom_point() +
  labs(x = "Date", y = "Prop_social") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual(labels = c("weekdays", "weekends"), values = c("pink", "black")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())





ggplot(Screentime_SPH, aes(x = Date, y = daily_use_min, color = if_weekend)) +
  geom_line(color = "steelblue") +
  geom_point() +
  labs(x = "Date", y = "daily duration") +
  scale_y_continuous(limits = c(0.5, 3.5), breaks = seq(0.5, 3.5, by = 0.5)) +
  scale_color_manual(labels = c("weekdays", "weekends"), values = c("pink", "black")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())

#Occupation time curve
# 1. Adding cumulative sum columns for all your variables
Screentime_SPH <- Screentime_SPH %>%
  arrange(Date) %>%
  mutate(Cumulative_Total.ST.min = cumsum(Total.ST.min),
         Cumulative_Social.ST.min = cumsum(Social.ST.min),
         Cumulative_Pickups = cumsum(Pickups),
         Cumulative_Prop_social = cumsum(Prop_social),
         Cumulative_daily_use_min = cumsum(daily_use_min))

# 2. Plotting occupation time curves
# A. Total screen time
ggplot(Screentime_SPH, aes(x = Date, y = Cumulative_Total.ST.min )) +
  geom_line() +
  labs(x = "Date", y = "Total.ST.min ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# b. Total social screen time
ggplot(Screentime_SPH, aes(x = Date, y = Cumulative_Social.ST.min )) +
  geom_line() +
  labs(x = "Date", y = "Social.ST.min ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# c. pickups
ggplot(Screentime_SPH, aes(x = Date, y = Cumulative_Pickups )) +
  geom_line() +
  labs(x = "Date", y = "Pickups") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# d. duration per use
ggplot(Screentime_SPH, aes(x = Date, y = Cumulative_daily_use_min)) +
  geom_line() +
  labs(x = "Date", y = "daily_use_min") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# proportion of social..
ggplot(Screentime_SPH, aes(x = Date, y = Cumulative_Prop_social)) +
  geom_line() +
  labs(x = "Date", y = "Prop_social") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Install the GGally package (if not already installed)
install.packages("GGally")

# Load the GGally package
library(GGally)
 

Screentime_SPH %>%
  ggpairs(columns = c("Total.ST.min","Social.ST.min","Pickups","daily_use_min","Prop_social"),columnLabels = c("Total.ST.min","Social.ST.min","Pickups","daily_use_min","Prop_social"),
          progress = FALSE) + theme_bw()
#Autocorrelations
acf(Screentime_SPH$Total.ST.min)
acf(Screentime_SPH$Social.ST.min)
acf(Screentime_SPH$Pickups)
acf(Screentime_SPH$daily_use_min)
acf(Screentime_SPH$Prop_social)

# Transorm the first pickup
library(circular)
 Screentime_SPH = Screentime_SPH %>%
   mutate(Pickup_1st_angular = (hour(Pickup.1st)*60 + minute(Pickup.1st))/(24*60)*360)

 first_pickup_cir = circular(Screentime_SPH$Pickup_1st_angular,units= "degrees",
                             template = "clock24")
  #scatterplot
 
 first.pickup.cir <- circular(Screentime_SPH$Pickup_1st_angular, units = "degrees", template = "clock24")
 plot(first.pickup.cir, col = "blue")

 # Histogram
 plot(first_pickup_cir, main = "Circular Scatterplot of Pickup_1st_angular")
 plot (first_pickup_cir, stack = TRUE, bins = 30, col = "pink")

 
install.packages("stats")
library(stats)
library(MASS)
 # Poisson
 model<- glm(Pickups ~ offset(log(Total.ST.min)), data = Screentime_SPH, family = poisson)
 
 # Display model summary
 summary(model)
 
#for dummy variables for testing B1 and B2
model <- glm(Pickups ~ offset(log(Total.ST.min)) + if_weekend +
         ifelse(Date >= as.Date("2024-01-10"), 1, 0), data = Screentime_SPH, family = poisson)
     # Summary of the model
  summary(model)
  
  # von mises distribution
  
 circular= mle.vonmises( first_pickup_cir-180)  
 circular_mu = circular$mu
 circularK=circular$kappa
 
 first_pickup_rad = circular((first_pickup_cir)*pi/180 - pi,units = "radians")
 rad = mle.vonmises(first_pickup_rad)
 rad_mu =rad$mu
 rad_Kappa=rad$kappa
 
 
 # Estimate parameters for von Mises distribution (degrees)
 est_circular <- mle.vonmises(first.pickup.cir - 180)  # Subtracting 180 to center the distribution
 est_circular_mu <- est_circular$mu
 est_circular_Kappa <- est_circular$kappa
 
 
 
 
 ang = (8.5*60)/(24*60)*360
 rad = (-ang * (pi/180))-pi
 
 1-pvonmises(circular(rad),mu = rad_mu, kappa = rad_Kappa)  
 
 
 
 
 
 
 


 
 
 
 
 
 
  