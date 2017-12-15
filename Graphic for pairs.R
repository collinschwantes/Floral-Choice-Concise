## Diagram for paired observation

# Grid with 20 cells
# Number in each grid
# Pair of camera's observing
# Color indicating control vs treated

a<-sample(1:1000,1,replace = T)
set.seed(a)
df <- data.frame(
FID = c(1:20),
Side = rep(c("Left","Right"),10),
Pair = sample(rep(1:10, each = 2), size = 20, replace = F)
) 
df <- arrange(df,Pair)

df$Treatment <-  rep(c("Present","Absent"),10)

df <- arrange(df,FID)
df$Position_Pair <- rep(c(1:10),each = 2)

Ran_treat_df <- function(plants, days){
  
  x <- plants
  pair <- x/2
  FID <-sample(1:x,x,replace = F)
  
  df1 <- data.frame(
  FID = FID,
  Treatment =  rep(c("Present","Absent"), each = pair)
  )
  
  cdf <- function(x){
  
  x$Pair <- c(sample(1:pair,size = pair, replace = F), sample(1:pair,size = pair, replace = F))
  
  df <- arrange(x,FID)
  df$Side <- rep(c("Left","Right"),pair)
  df$Position_Pair <- rep(c(1:pair),each = 2)
  
  df  
  }
  
  dfs <- list()
  for( i in 1:days){
    df<- cdf(x = df1)
    df$Day <- rep(paste("Day",i, sep = " "),plants)
    dfs[[i]] <- df
    }
  
  dff  <- do.call("rbind", dfs)
}

df1 <- Ran_treat_df(plants = 20, days = 3)

df2 <- Ran_treat_df(plants = 20, days = 3)



 block1 <- ggplot(df1,aes(Side,Position_Pair)) +
  geom_tile(aes(fill = as.factor(Treatment)), colour = "black") + 
  geom_text(aes(label = Pair)) +
   scale_fill_manual(values = c("#ffffff","#999999"), name = "Treatment") +
  theme(axis.text= element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank()) +
  ylab("") +
  xlab("") +
  facet_grid(facets = ~Day) +
  theme(strip.background = element_rect(colour="#111111", fill="#ffffff")) 
 
   
block2 <- ggplot(df2,aes(Side,Position_Pair)) +
  geom_tile(aes(fill = as.factor(Treatment)), colour = "white") + 
  geom_text(aes(label = Pair)) +
  scale_fill_manual(values = c("#b3cde3","#fbb4ae"), name = "Treatment") +
  theme(axis.text= element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank()) +
  ylab("") +
  xlab("") +
  facet_grid(facets = ~Day) +
  theme(strip.background = element_rect(colour="#111111", fill="#ffffff")) 
 
 
block1
ggsave(filename = "Designvisb1.eps",width = 6,height = 5,units = "in")
block2
