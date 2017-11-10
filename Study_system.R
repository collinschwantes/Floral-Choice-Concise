
list.dirs()

FilePath <- list.files(path = "./figures",pattern = "Sunflower Densities_040615a.csv",full.names = T)

floral_data<- read.csv(file = "/Users/featherlite569/Documents/Floral_Choice /floral_choice/Figures/Sunflower Densities_040615a.csv")

str(floral_data)

## floral densities ##

mean(na.omit(floral_data$Density))

std_er <- function(x) {
  sqrt(var(x)/length(x))
}

std_er(x = na.omit(floral_data$Density))

## Beetle densities ##

Beetle_density <- floral_data$Sum_beetles/floral_data$flowers_plant

Beetle_density[is.nan(Bug_density)] <- 0

mean(Beetle_density)

std_er(x = na.omit(Beetle_density))

## Ambush bug density ##

Bug_density <- floral_data$Presence/floral_data$flowers_plant

Bug_density[is.nan(Bug_density)] <- 0

mean(Bug_density)

std_er <- function(x) {
  sqrt(var(x)/length(x))
}

std_er(x = na.omit(Bug_density))


Bug_percent <- sum(floral_data$Presence)/length(floral_data$flowers_plant)


