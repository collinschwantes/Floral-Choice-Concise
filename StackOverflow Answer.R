#The unequal interval between days means that 0 and 1 not be separated
days <- data.frame(
  day=c(0,1,8,15)
  )
str(days)

groups <- data.frame(
    group=c("A","B","C","D", "E"), 
    means=seq(0,1,length=5)
    )

my_data <- merge(days, groups)

my_data$mid <- exp(my_data$means+rnorm(nrow(my_data), sd=0.25))
my_data$sigma = 0.1

str(my_data)

my_data$day <- as.factor(my_data$day)

levels(my_data$day)

#
ggplot(data = my_data, aes(x=day, ymin=mid-sigma, ymax=mid+sigma, y= mid, fill=group)) +
       geom_bar(position="dodge", stat = "identity") + 
       geom_errorbar( position = position_dodge(), colour="black") +
       geom_point(position=position_dodge(.9), aes(y=mid, colour=group)) +
       coord_flip()

a <- ggplot(mtcars, aes(gear ,fill=gear))+ geom_bar() + 
  theme(legend.position="none") + 
          coord_flip() + scale_y_reverse()

str(a)
a$scales$add

