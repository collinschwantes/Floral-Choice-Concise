## So i want to randomize treatments on 20 flowers per block.
## Block 1 is the Norther block and block 2 is the southern flower number 1 is in the north east corner
flower_ID1<-c(1:20)
length(flower_ID1)
FL_ID1<-data.frame(flower_ID1)
flower_ID2<-c(1:20)
FL_ID2<-data.frame(flower_ID2)

## Adding random treatment to block 1 (north bed)
a<-sample(1:1000,1,replace = T)
set.seed(a)
block1<-sample(flower_ID1,20, replace = F)
treated<-block1[1:10]
control<-block1[11:20]
order1<-sample(1:10,10,replace = F)
order2<-sample(1:10,10,replace = F)
order3<-sample(1:10,10,replace = F)
treatment_block1<-data.frame(cbind(treated,control,order1,order2,order3))

treatment_block1
write.csv(treatment_block1,file = "Block 1.csv")
## block 2 (south bed)
a<-sample(1:1000,1,replace = T)
set.seed(a)
block2<-sample(flower_ID2,20, replace = F)
treated2<-block2[1:10]
control2<-block2[11:20]
order1<-sample(1:10,10,replace = F)
order2<-sample(1:10,10,replace = F)
order3<-sample(1:10,10,replace = F)
treatment_block2<-data.frame(cbind(treated2,control2,order1,order2,order3))
treatment_block2

write.csv(treatment_block2,file = "Block 2.csv")
## selecting by treatment, use this to pair plants assign new random ID 1 - 20 
?Syntax
