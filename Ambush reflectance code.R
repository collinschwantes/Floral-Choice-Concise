# This is code for the ambush bug and sun flower reflectance

par(mar=c(5,5,1,1))
par(mfrow=c(1,1))

#    an example of the spectral data   ##############
dorsal<-read.csv("dorsal45.csv")
attach(dorsal)
plot(d_Reflectance~Wavelength, col="red",cex=.35,ylim=c(0,70), xlab="Wavelength", ylab="Reflectance", cex.lab=1.5)
points(d45_Reflectance~Wavelength,col="dark red",cex=.35)
points(v_Reflectance~Wavelength,col="blue",cex=.35)
points(v45_Reflectance~Wavelength,col="dark blue", cex=.35)
legend(300,70, c("dorsal", "dorsal 45", "ventral", "ventral 45"), pch=19, col=c("red", "dark red", "blue","dark blue"), bty="n")

#with some lowess smoothing
plot(d_Reflectance~Wavelength, cex=.25,main = "lowess(d_Reflectance~Wavelength)")
lines(lowess(d_Reflectance~Wavelength), col = 2)
lines(lowess(cars, f = .2), col = 3)
legend(5, 120, c(paste("f = ", c("2/3", ".2"))), lty = 1, col = 2:3)


#################   down to bidness   ################
#### I think in the end we sould just report female color ###########

#Female reflectance from copiled data
ref<-read.csv("Reflectance bugs.csv")

#cut to visible spectrum
spec1<-ref[ref$Wavelength>300,]
spec<-spec1[spec1$Wavelength<700,]
attach(spec)


# package for organizing, analyzing, and visualizing spec data
## paper describing package and methods https://besjournals.onlinelibrary.wiley.com/doi/epdf/10.1111/2041-210X.12069

library('pavo')


#differnet spots of color, DT: dorsal thorax, DA: dorsal abdomen, VT: ventral thorax, VA: ventral abdomen
par(mar=c(5,5,1,1))
par(mfrow=c(1,1))
plot(Wavelength,DT,type="l",col="red",ylim=c(0,80),xlab="Wavelength", ylab="Reflectance", cex.lab=1.5 )
lines(Wavelength,DA,col="dark red")
lines(Wavelength,VT,col="blue")
lines(Wavelength,VA,col="dark blue")

# bug colors

# female reflectance

DTm <- DT + outer(seDT, c(1,-1))
DAm <- DA + outer(seDA, c(1,-1))
VTm <- VT + outer(seVT, c(1,-1))
VAm <- VA + outer(seVA, c(1,-1))

plot(Wavelength, DT, type="l", xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,60),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(DTm[,1],rev(DTm[,2])),border=NA, col="darkgoldenrod"))
lines(Wavelength, DA, type="l",xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,60),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(DAm[,1],rev(DAm[,2])),border=NA, col="darkgoldenrod4"))
lines(Wavelength, VT, type="l",xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,60),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(VTm[,1],rev(VTm[,2])),border=NA, col="darkgoldenrod2"))
lines(Wavelength, VA, type="l",xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,60),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(VAm[,1],rev(VAm[,2])),border=NA, col="darkgoldenrod1"))
legend(300,60, c("dorsal thorax", "dorsal abdomen", "ventral thorax", "ventral abdomen"), fill=c("darkgoldenrod", "darkgoldenrod4", "darkgoldenrod2","darkgoldenrod1"), bty="n")

#Male reflectance
mDTm <- mDT + outer(semDT, c(1,-1))
mDAm <- mDA + outer(semDA, c(1,-1))
mVTm <- mVT + outer(semVT, c(1,-1))
mVAm <- mVA + outer(semVA, c(1,-1))

plot(Wavelength, mDT, type="l", xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,60),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(mDTm[,1],rev(mDTm[,2])),border=NA, col="darkgoldenrod"))
lines(Wavelength, mDA, type="l",xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,60),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(mDAm[,1],rev(mDAm[,2])),border=NA, col="darkgoldenrod4"))
lines(Wavelength, mVT, type="l",xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,60),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(mVTm[,1],rev(mVTm[,2])),border=NA, col="darkgoldenrod2"))
lines(Wavelength, mVA, type="l",xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,60),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(mVAm[,1],rev(mVAm[,2])),border=NA, col="darkgoldenrod1"))
legend(300,60, c("dorsal thorax", "dorsal abdomen", "ventral thorax", "ventral abdomen"), fill=c("darkgoldenrod", "darkgoldenrod4", "darkgoldenrod2","darkgoldenrod1"), bty="n")

#flower refectance
SDAm <- SDA + outer(seSDA, c(1,-1))
SHAm <- SHA + outer(seSHA, c(1,-1))
SRAm <- SRA + outer(seSRA, c(1,-1))

plot(Wavelength, SDA, type="l", xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,70),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(SDAm[,1],rev(SDAm[,2])),border=NA, col="darkgoldenrod"))
lines(Wavelength, SRA, type="l",xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,70),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(SRAm[,1],rev(SRAm[,2])),border=NA, col="darkgoldenrod2"))

legend(300,60, c("ray flowers", "disc flowers"), fill=c("darkgoldenrod", "darkgoldenrod2"), bty="n")

# Combined for female and flowers, AKA Figure 3 

dir.create("./Figures")
png(filename = "./Figures/Reflectance_FemaleFlowerf3.png",width = 5,height = 4,units = "in",res = 300)

plot(Wavelength, SDA, type="l", xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,60),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(SDAm[,1],rev(SDAm[,2])),border=NA, col=rgb(.55,.28,0.03, 0.75)))
lines(Wavelength, SRA, type="l",xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,60),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(SRAm[,1],rev(SRAm[,2])),border=NA, col=rgb(1,.84,0, 0.75)))
lines(Wavelength, DT, type="l", xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,60),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(DTm[,1],rev(DTm[,2])),border=NA, col=rgb(.55,.41,.08, 0.75)))
lines(Wavelength, VA, type="l",xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.lab=1.5,ylim=c(0,60),panel.first=polygon(c(Wavelength,rev(Wavelength)),c(VAm[,1],rev(VAm[,2])),border=NA, col=rgb(.80,.58,0,0.75)))
legend(300,60, 
       c("ray petals",  "sternum", "notum", "disc petals"), 
       fill= c(rgb(1,.84,0, 0.75),
               rgb(.80,.58,0,0.75),
               rgb(.55,.41,.08, 0.75),
               rgb(.55,.28,0.03, 0.75)),
       bty="n")

dev.off()

## Very interesting that ventral coloration is more like the ray flower
## Imagine seeing the ambush bug on the plane of the sunflower disc
## looking at an ambush bug from the plane of the flower, a visitor 
## would likely see the ventral surface and ray petals. 

#time to try pavo
str(ref)

reflec<-ref[,c(1:53)]
is.rspec(reflec)
dat.spec<-as.rspec(reflec,lim=c(300,700))
is.rspec(dat.spec)
head(dat.spec)
tail(dat.spec)

str(dat.spec)

#subset
FDA<-dat.spec[,1:9]
FDT<-dat.spec[,c(1,10:17)]
FVA<-dat.spec[,c(1,18:25)]
FVT<-dat.spec[,c(1,26:33)]
SD<-dat.spec[,c(1,34:43)]
SR<-dat.spec[,c(1,44:53)]


explorespec(FVA)

mFVA<-aggspec(FVA, FUN=mean)
mFVA
dim(FVA)
explorespec(mFVA)
plotsmooth(mFVA, minsmooth=0.05, maxsmooth=0.5,curves=4,ask=F)
mFVA.sm<-procspec(mFVA, opt='smooth', span=0.2)

#plotting average with aggspec
plot(mFVA[, 2] ~ mFVA[, 1], type = 'l', lwd = 10, col = 'grey', xlab = "Wavelength (nm)", ylab = "Reflectance (%)")
lines(mFVA.sm[, 2] ~ mFVA[, 1], col = 'red', lwd = 2)


#colorspace hexagon
vis.FVA<-vismodel(FVA,visual='apis',qcatch = 'Ei', relative = FALSE, vonkries = TRUE, bkg = 'green')
vis.FDT<-vismodel(FDT,visual='apis',qcatch = 'Ei', relative = FALSE, vonkries = TRUE, bkg = 'green')
vis.SD<-vismodel(SD,visual='apis',qcatch = 'Ei', relative = FALSE, vonkries = TRUE, bkg = 'green')
vis.SR<-vismodel(SR,visual='apis',qcatch = 'Ei', relative = FALSE, vonkries = TRUE, bkg = 'green')
hex.FVA<-colspace(vis.FVA,space='hexagon')
hex.FDT<-colspace(vis.FDT,space='hexagon')
hex.SD<-colspace(vis.SD,space='hexagon')
hex.SR<-colspace(vis.SR,space='hexagon')

head(hex.FVA)

par(mar=c(0,0,0,0))
par(mfrow=c(1,1))
plot(hex.FVA, sectors='coarse', pch=21, col=rgb(.80,.58,0,0), cex=1.5)
points(hex.FVA, pch=22, bg=rgb(.80,.58,0,0.75),cex=1.5)
points(hex.FDT, pch=24, bg=rgb(.55,.41,.08, 0.75),cex=1.5)
points(hex.SD, pch=21, bg=rgb(.55,.28,0.03, 0.75),cex=1.5)
points(hex.SR, pch=23, bg=rgb(1,.84,0, 0.75), cex=1.5)
legend(-1.15,1.275, c("sternum", "notum", "disc petal", "ray petal"),bty='n',pch=c(22,24,21,23), pt.bg=c(rgb(.80,.58,0,0.75),rgb(.55,.41,.08, 0.75),rgb(.55,.28,0.03, 0.75),rgb(1,.84,0, 0.75)),cex=1.25)


par(mar=c(5,5,1,1))
par(mfrow=c(1,1))

#messy actual data
attach(spec)
plot(F1VA~Wavelength)
plot(Wavelength,F1VA,type="l",col="red",ylim=c(0,80),xlab="Wavelength", ylab="Reflectance", cex.lab=1.5 )
lines(Wavelength,F1VT,col="dark red")
lines(Wavelength,F1DA,col="blue")
lines(Wavelength,F1DT,col="dark blue")
lines(Wavelength,F2VA,col="red")
lines(Wavelength,F2VT,col="dark red")
lines(Wavelength,F2DA,col="blue")
lines(Wavelength,F2DT,col="dark blue")
lines(Wavelength,F3VA,col="red")
lines(Wavelength,F3VT,col="dark red")
lines(Wavelength,F3DA,col="blue")
lines(Wavelength,F3DT,col="dark blue")
lines(Wavelength,F4VA,col="red")
lines(Wavelength,F4VT,col="dark red")
lines(Wavelength,F4DA,col="blue")
lines(Wavelength,F4DT,col="dark blue")
lines(Wavelength,F5VA,col="red")
lines(Wavelength,F5VT,col="dark red")
lines(Wavelength,F5DA,col="blue")
lines(Wavelength,F5DT,col="dark blue")
lines(Wavelength,F6VA,col="red")
lines(Wavelength,F6VT,col="dark red")
lines(Wavelength,F6DA,col="blue")
lines(Wavelength,F6DT,col="dark blue")
lines(Wavelength,F7VA,col="red")
lines(Wavelength,F7VT,col="dark red")
lines(Wavelength,F7DA,col="blue")
lines(Wavelength,F7DT,col="dark blue")
lines(Wavelength,F8VA,col="red")
lines(Wavelength,F8VT,col="dark red")
lines(Wavelength,F8DA,col="blue")
lines(Wavelength,F8DT,col="dark blue")


