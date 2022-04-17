# Coskun Kucukkaragoz

# Girdle Lengths
girdle <- read.csv("girdle.csv",stringsAsFactors = TRUE)
attach(girdle)
plot(girdle$distance_cm[girdle$limb_type=="Manus"]~girdle$track_set[girdle$limb_type=="Manus"],col="blue",pch=16)
points(girdle$distance_cm[girdle$limb_type=="Pes"]~girdle$track_set[girdle$limb_type=="Pes"],col="red",pch=16)

M <- lm(girdle$distance_cm[girdle$limb_type=="Manus"]~girdle$track_set[girdle$limb_type=="Manus"])
P <- lm(girdle$distance_cm[girdle$limb_type=="Pes"]~girdle$track_set[girdle$limb_type=="Pes"])
abline(M,col="blue")
abline(P,col="red")

girdleM <- subset(girdle,girdle$limb_type=="Manus")
girdleP <- subset(girdle,girdle$limb_type=="Pes")
mean(girdleM$distance_cm,na.rm = TRUE)
mean(girdleP$distance_cm,na.rm = TRUE)
