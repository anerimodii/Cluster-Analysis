#For Modifying the data base#
#to change City types "A" to numeric values "1"
Data.clean$City_Category<-factor(Data.clean$City_Category)
as.character(Data.clean$City_Category)
as.numeric(Data.clean$City_Category)
#to change Gender types "M" to numeric value "1"
Data.clean$Gender<-factor(Data.clean$Gender)
as.character(Data.clean$Gender)
as.numeric(Data.clean$Gender)


#For Clustering#


Data <- read.csv(file="BlackFriday1.csv", header=TRUE, sep=",") #read the csv
#for omitting NA values
Data.clean <- na.omit(Data)


#subsetting for occupation
Data.Occ0 <- subset(Data.clean,Occupation == "0") #subsets the occupation
head(Data.Occ0)#head of the subset
Data.Occ0$Occupation <- NULL
Data.Occ.1 <- subset(Data.clean, Occupation == "1")
Data.Occ2 <- subset(Data.clean, Occupation == "2")
Data.Occ3 <- subset(Data.clean, Occupation == "3")
Data.Occ4 <- subset(Data.clean, Occupation == "4")
Data.Occ5 <- subset(Data.clean, Occupation == "5")
Data.Occ6 <- subset(Data.clean, Occupation == "6") 
Data.Occ7 <- subset(Data.clean, Occupation == "7")
Data.Occ8 <- subset(Data.clean, Occupation == "8")
Data.Occ9 <- subset(Data.clean, Occupation == "9")
Data.Occ10 <- subset(Data.clean, Occupation == "10")
Data.Occ11 <- subset(Data.clean, Occupation == "11")
Data.Occ13 <- subset(Data.clean, Occupation == "13")
Data.Occ14 <- subset(Data.clean, Occupation == "14")
Data.Occ15 <- subset(Data.clean, Occupation == "15")
Data.Occ16 <- subset(Data.clean, Occupation == "16")
Data.Occ17 <- subset(Data.clean, Occupation == "17")
Data.Occ18 <- subset(Data.clean, Occupation == "18")
Data.Occ19 <- subset(Data.clean, Occupation == "19")
Data.Occ20 <- subset(Data.clean, Occupation == "20")


#subset of martial status and then subsetting by age


Data.Mart1 <- subset(Data.clean, Marital_Status == "1")
Data1.Age17 <- subset(Data.Mart1,Age=="17")
Data1.Age25 <- subset(Data.Mart1,Age=="25")
Data1.Age35 <- subset(Data.Mart1,Age=="35")
Data1.Age45 <- subset(Data.Mart1,Age=="45")
Data1.Age50 <- subset(Data.Mart1,Age=="50")
Data1.Age55 <- subset(Data.Mart1,Age=="55")
Data1.Age56 <- subset(Data.Mart1,Age=="56")
Data.Mart0 <- subset(Data.clean,Marital_Status=="0")
Data0.Age17 <- subset(Data.Mart0,Age=="17")
Data0.Age25 <- subset(Data.Mart0,Age=="25")
Data0.Age35 <- subset(Data.Mart0,Age=="35")
Data0.Age45 <- subset(Data.Mart0,Age=="45")
Data0.Age50 <- subset(Data.Mart0,Age=="50")
Data0.Age55 <- subset(Data.Mart0,Age=="55")
Data0.Age56 <- subset(Data.Mart0,Age=="56")


#subset of City by category and then by age
Data.CityA <- subset(Data.clean,City_Category == "1")
Data.A.Age17 <- subset(Data.CityA,Age=="17")
Data.A.Age25 <- subset(Data.CityA,Age=="25")
Data.A.Age35 <- subset(Data.CityA,Age=="35")
Data.A.Age45 <- subset(Data.CityA,Age=="45")
Data.A.Age50 <- subset(Data.CityA,Age=="50")
Data.A.Age55 <- subset(Data.CityA,Age=="55")
Data.A.Age56 <- subset(Data.CityA,Age=="56")


Data.CityB <- subset(Data.clean,City_Category == "2")
Data.B.Age17 <- subset(Data.CityB,Age=="17")
Data.B.Age25 <- subset(Data.CityB,Age=="25")
Data.B.Age35 <- subset(Data.CityB,Age=="35")
Data.B.Age45 <- subset(Data.CityB,Age=="45")
Data.B.Age50 <- subset(Data.CityB,Age=="50")
Data.B.Age55 <- subset(Data.CityB,Age=="55")
Data.B.Age56 <- subset(Data.CityB,Age=="56")


Data.CityC <- subset(Data.clean,City_Category == "3")
Data.C.Age17 <- subset(Data.CityC,Age=="17")
Data.C.Age25 <- subset(Data.CityC,Age=="25")
Data.C.Age35 <- subset(Data.CityC,Age=="35")
Data.C.Age45 <- subset(Data.CityC,Age=="45")
Data.C.Age50 <- subset(Data.CityC,Age=="50")
Data.C.Age55 <- subset(Data.CityC,Age=="55")
Data.C.Age56 <- subset(Data.CityC,Age=="56")


#for occupation 0
scaled_data.Occ0 = as.matrix(scale(Data.Occ0))


#K Means#
#OCCUPATION 0#
#K means for cluster N = 2 
km.Occ0.2 = kmeans(Data.Occ0,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Occ0.3 = kmeans(Data.Occ0,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Occ0.4 = kmeans(Data.Occ0,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5
km.Occ0.5 = kmeans(Data.Occ0,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Occ0.6 = kmeans(Data.Occ0,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Occ0.7 = kmeans(Data.Occ0,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Occ0.9 = kmeans(Data.Occ0,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Occ0.9 = kmeans(Data.Occ0,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Occ0.10 = kmeans(Data.Occ0,10,nstart = 25,iter.max = 15)


K-median
#loading Gmedian library
install.packages("Gmedian")
library(Gmedian)


Occ0 <- as.matrix(Data.Occ0)


#K-median for 2-10 Clusters
kmed.Occ0.2 <- kGmedian(Occ0, ncenters = 2)
kmed.Occ0.3 <- kGmedian(Occ0, ncenters = 3)
kmed.Occ0.4 <- kGmedian(Occ0, ncenters = 4)
kmed.Occ0.5 <- kGmedian(Occ0, ncenters = 5)
kmed.Occ0.6 <- kGmedian(Occ0, ncenters = 6)
kmed.Occ0.7 <- kGmedian(Occ0, ncenters = 7)
kmed.Occ0.8 <- kGmedian(Occ0, ncenters = 8)
kmed.Occ0.9 <- kGmedian(Occ0, ncenters = 9)
kmed.Occ0.10 <- kGmedian(Occ0, ncenters = 10)


KNN
set.seed(1234)
data_norm<-function(x){((x-min(x)/ (max(x)-min(x))))}
Data.Occ0_Norm<-as.data.frame(lapply(Data.Occ0, data_norm))
ind<-sample(2, nrow(Data.Occ0_Norm), replace=TRUE, prob = c(0.7,0.3))
train.data<-Data.Occ0_Norm[ind==1,]
test.data<-Data.Occ0_Norm[ind==2,]
predict.data<-knn(train.data, test.data, train.data[,6], k =50 , prob=TRUE)
table(predict.data,test.data[,6])




#iclust for N = 2
iclust(Data.Occ0,nclusters=3)
#iclust for N = 3
iclust(Data.Occ0,nclusters=3)
#iclust for N = 4
iclust(Data.Occ0,nclusters=4)
#iclust for N = 5
iclust(Data.Occ0,nclusters=5)
#iclust for N = 6
iclust(Data.Occ0,nclusters=6)
#iclust for N = 7
iclust(Data.Occ0,nclusters=7)
#iclust for N = 8
iclust(Data.Occ0,nclusters=8)
#iclust for N = 9
iclust(Data.Occ0,nclusters=9)
#iclust for N = 10
iclust(Data.Occ0,nclusters=10)


#OCCUPATION 1#
#K means for cluster N = 2 
km.Occ1.2 = kmeans(Data.Occ1,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Occ1.3 = kmeans(Data.Occ1,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Occ1.4 = kmeans(Data.Occ1,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Occ1.5 = kmeans(Data.Occ1,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Occ1.6 = kmeans(Data.Occ1,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Occ1.7 = kmeans(Data.Occ1,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Occ1.8 = kmeans(Data.Occ1,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Occ1.9 = kmeans(Data.Occ1,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Occ1.10 = kmeans(Data.Occ1,10,nstart = 25,iter.max = 15)




#iclust for N = 2
iclust(Data.Occ1,nclusters=2)
#iclust for N = 3
iclust(Data.Occ1,nclusters=3)
#iclust for N = 4
iclust(Data.Occ1,nclusters=4)
#iclust for N = 5
iclust(Data.Occ1,nclusters=5)
#iclust for N = 6
iclust(Data.Occ1,nclusters=6)
#iclust for N = 7
iclust(Data.Occ1,nclusters=7)
#iclust for N = 8
iclust(Data.Occ1,nclusters=8)
#iclust for N = 9
iclust(Data.Occ1,nclusters=9)
#iclust for N = 10
iclust(Data.Occ1,nclusters=10)


#OCCUPATION 2#
#K means for cluster N = 2
km.Occ2.2 = kmeans(Data.Occ2,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Occ2.3 = kmeans(Data.Occ2,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Occ2.4 = kmeans(Data.Occ2,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Occ2.5 = kmeans(Data.Occ2,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Occ2.6 = kmeans(Data.Occ2,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Occ2.7 = kmeans(Data.Occ2,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Occ2.8 = kmeans(Data.Occ2,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Occ2.9 = kmeans(Data.Occ2,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Occ2.10 = kmeans(Data.Occ2,10,nstart = 25,iter.max = 15)




#iclust for N = 2
iclust(Data.Occ2,nclusters=2)
#iclust for N = 3
iclust(Data.Occ2,nclusters=3)
#iclust for N = 4
iclust(Data.Occ2,nclusters=4)
#iclust for N = 5
iclust(Data.Occ2,nclusters=5)
#iclust for N = 6
iclust(Data.Occ2,nclusters=6)
#iclust for N = 7
iclust(Data.Occ2,nclusters=7)
#iclust for N = 8
iclust(Data.Occ2,nclusters=8)
#iclust for N = 9
iclust(Data.Occ2,nclusters=9)
#iclust for N = 10
iclust(Data.Occ2,nclusters=10)


#OCCUPATION 3#
#K means for cluster N = 2 
km.Occ3.2 = kmeans(Data.Occ3,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Occ3. = kmeans(Data.Occ3,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Occ3.4 = kmeans(Data.Occ3,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Occ3.5 = kmeans(Data.Occ3,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Occ3.6 = kmeans(Data.Occ3,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Occ3.7 = kmeans(Data.Occ3,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8
km.Occ3.8 = kmeans(Data.Occ3,8,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Occ3.9 = kmeans(Data.Occ3,9,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Occ3.10 = kmeans(Data.Occ3,10,nstart = 25,iter.max = 15)




#iclust for N = 2
iclust(Data.Occ3,nclusters=2)
#iclust for N = 3
iclust(Data.Occ3,nclusters=3)
#iclust for N = 4
iclust(Data.Occ3,nclusters=4)
#iclust for N = 5
iclust(Data.Occ3,nclusters=5)
#iclust for N = 6
iclust(Data.Occ3,nclusters=6)
#iclust for N = 7
iclust(Data.Occ3,nclusters=7)
#iclust for N = 8
iclust(Data.Occ3,nclusters=8)
#iclust for N = 9
iclust(Data.Occ3,nclusters=9)
#iclust for N = 10
iclust(Data.Occ3,nclusters=10)


#OCCUPATION 4#
#K means for cluster N = 2 
km.Occ4.2 = kmeans(Data.Occ4,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Occ4.3 = kmeans(Data.Occ4,3,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Occ4.4 = kmeans(Data.Occ4,7,nstart = 25,iter.max = 15)
#K means for cluster N = 3 
km.Occ4.5 = kmeans(Data.Occ4,3,nstart = 25,iter.max = 15)
#K means for cluster N = 5
km.Occ4.6 = kmeans(Data.Occ4,5,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Occ4.7 = kmeans(Data.Occ4,7,nstart = 25,iter.max = 15)
#K means for cluster N = 3 
km.Occ4.8 = kmeans(Data.Occ4,3,nstart = 25,iter.max = 15)
#K means for cluster N = 5
km.Occ4.9 = kmeans(Data.Occ4,5,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Occ4.10 = kmeans(Data.Occ4,7,nstart = 25,iter.max = 15)


#iclust for N = 2
iclust(Data.Occ4,nclusters=2)
#iclust for N = 3
iclust(Data.Occ4,nclusters=3)
#iclust for N = 4
iclust(Data.Occ4,nclusters=4)
#iclust for N = 5
iclust(Data.Occ4,nclusters=5)
#iclust for N = 6
iclust(Data.Occ4,nclusters=6)
#iclust for N = 7
iclust(Data.Occ4,nclusters=7)
#iclust for N = 8
iclust(Data.Occ4,nclusters=8)
#iclust for N = 9
iclust(Data.Occ4,nclusters=9)
#iclust for N = 10
iclust(Data.Occ4,nclusters=10)


#OCCUPATION 5#
#K means for cluster N = 3 
km.Occ5.3 = kmeans(Data.Occ5,3,nstart = 25,iter.max = 15)
#K means for cluster N = 5
km.Occ5.5 = kmeans(Data.Occ5,5,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Occ5.7 = kmeans(Data.Occ5,7,nstart = 25,iter.max = 15)


#iclust for N = 3
iclust(Data.Occ5,nclusters=3)
#iclust for N = 5
iclust(Data.Occ5,nclusters=5)
#iclust for N = 7
iclust(Data.Occ5,nclusters=7)




#K Means#
#Martial Status 0#
#K Means#
#Martial Status 0#
#AGE 17#
#K means for cluster N = 2 
km.Data0.Age17  = kmeans(Data0.Age17,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data0.Age17 = kmeans(Data0.Age17,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data0.Age17 = kmeans(Data0.Age17,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data0.Age17  = kmeans(Data0.Age17,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data0.Age17 = kmeans(Data0.Age17,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data0.Age17 = kmeans(Data0.Age17,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Data0.Age17  = kmeans(Data0.Age17,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Data0.Age17 = kmeans(Data0.Age17,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Data0.Age17 = kmeans(Data0.Age17,10,nstart = 25,iter.max = 15)


#K Means#
#Martial Status 0#
#K Means#
#Martial Status 0#
#AGE 25#
#K means for cluster N = 2 
km.Data0.Age25  = kmeans(Data0.Age25,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data0.Age25 = kmeans(Data0.Age25,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data0.Age25 = kmeans(Data0.Age25,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data0.Age25  = kmeans(Data0.Age25,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data0.Age25 = kmeans(Data0.Age25,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data0.Age25 = kmeans(Data0.Age25,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Data0.Age25  = kmeans(Data0.Age25,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Data0.Age25 = kmeans(Data0.Age25,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Data0.Age25 = kmeans(Data0.Age25,10,nstart = 25,iter.max = 15)




#K Means#
#Martial Status 0#
#K Means#
#Martial Status 0#
#AGE 35#
#K means for cluster N = 2 
km.Data0.Age35  = kmeans(Data0.Age35,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data0.Age35 = kmeans(Data0.Age35,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data0.Age35 = kmeans(Data0.Age35,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data0.Age35  = kmeans(Data0.Age35,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data0.Age35 = kmeans(Data0.Age35,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data0.Age35 = kmeans(Data0.Age35,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Data0.Age35  = kmeans(Data0.Age35,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Data0.Age35 = kmeans(Data0.Age35,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Data0.Age35 = kmeans(Data0.Age35,10,nstart = 25,iter.max = 15)


#K Means#
#Martial Status 0#
#K Means#
#Martial Status 0#
#AGE 45#
#K means for cluster N = 2 
km.Data0.Age45  = kmeans(Data0.Age45,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data0.Age45 = kmeans(Data0.Age45,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data0.Age45 = kmeans(Data0.Age45,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data0.Age45  = kmeans(Data0.Age45,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data0.Age45 = kmeans(Data0.Age45,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data0.Age45 = kmeans(Data0.Age45,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Data0.Age45  = kmeans(Data0.Age45,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Data0.Age45 = kmeans(Data0.Age45,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Data0.Age45 = kmeans(Data0.Age45,10,nstart = 25,iter.max = 15)


#K Means#
#Martial Status 0#
#K Means#
#Martial Status 0#
#AGE 50#
#K means for cluster N = 2 
km.Data0.Age50  = kmeans(Data0.Age50,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data0.Age50 = kmeans(Data0.Age50,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data0.Age50 = kmeans(Data0.Age50,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data0.Age50  = kmeans(Data0.Age50,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data0.Age50 = kmeans(Data0.Age50,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data0.Age50 = kmeans(Data0.Age50,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Data0.Age50  = kmeans(Data0.Age50,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Data0.Age50 = kmeans(Data0.Age50,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Data0.Age50 = kmeans(Data0.Age50,10,nstart = 25,iter.max = 15)


#K Means#
#Martial Status 0#
#K Means#
#Martial Status 0#
#AGE 55#
#K Means#
#Martial Status 0#
#K Means#
#Martial Status 0#
#AGE 50#
#K means for cluster N = 2 
km.Data0.Age55  = kmeans(Data0.Age55,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data0.Age55 = kmeans(Data0.Age55,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data0.Age55 = kmeans(Data0.Age55,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data0.Age55  = kmeans(Data0.Age55,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data0.Age55 = kmeans(Data0.Age55,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data0.Age55 = kmeans(Data0.Age55,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Data0.Age55  = kmeans(Data0.Age55,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Data0.Age55 = kmeans(Data0.Age55,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Data0.Age55 = kmeans(Data0.Age55,10,nstart = 25,iter.max = 15)


#K Means#
#Martial Status 0#
#K Means#
#Martial Status 0#
#AGE 56#
#K means for cluster N = 2 
km.Data0.Age56  = kmeans(Data0.Age56,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data0.Age56 = kmeans(Data0.Age56,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data0.Age56 = kmeans(Data0.Age56,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data0.Age56  = kmeans(Data0.Age56,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data0.Age56 = kmeans(Data0.Age56,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data0.Age56 = kmeans(Data0.Age56,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Data0.Age56  = kmeans(Data0.Age56,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Data0.Age56 = kmeans(Data0.Age56,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Data0.Age56 = kmeans(Data0.Age56,10,nstart = 25,iter.max = 15)




#K Means#
#Martial Status 1#
#K Means#
#Martial Status 1#
#AGE 17#
#K means for cluster N = 2
km.Data1.Age17  = kmeans(Data1.Age17,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data1.Age17 = kmeans(Data1.Age17,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data1.Age17 = kmeans(Data1.Age17,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data1.Age17  = kmeans(Data1.Age17,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data1.Age17 = kmeans(Data1.Age17,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data1.Age17 = kmeans(Data1.Age17,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8
km.Data1.Age17  = kmeans(Data1.Age17,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Data1.Age17 = kmeans(Data1.Age17,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10
km.Data1.Age17 = kmeans(Data1.Age17,10,nstart = 25,iter.max = 15)


#K Means#
#Martial Status 1#
#K Means#
#Martial Status 1#
#AGE 25#
#K means for cluster N = 2
km.Data1.Age25  = kmeans(Data1.Age25 ,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data1.Age25 = kmeans(Data1.Age25 ,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data1.Age25 = kmeans(Data1.Age25 ,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data1.Age25  = kmeans(Data1.Age25 ,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data1.Age25 = kmeans(Data1.Age25 ,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data1.Age25 = kmeans(Data1.Age25 ,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8
km.Data1.Age25 = kmeans(Data1.Age25 ,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9 
km.Data1.Age25 = kmeans(Data1.Age25 ,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Data1.Age25 = kmeans(Data1.Age25 ,10,nstart = 25,iter.max = 15)




#K Means#
#Martial Status 1#
#K Means#
#Martial Status 1#
#AGE 35#
#K means for cluster N = 2
km.Data1.Age35  = kmeans(Data1.Age35,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data1.Age35 = kmeans(Data1.Age35,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data1.Age35 = kmeans(Data1.Age35,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data1.Age35  = kmeans(Data1.Age35,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data1.Age35 = kmeans(Data1.Age35,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data1.Age35 = kmeans(Data1.Age35,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Data1.Age35  = kmeans(Data1.Age35,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Data1.Age35 = kmeans(Data1.Age35,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10
km.Data1.Age35 = kmeans(Data1.Age35,10,nstart = 25,iter.max = 15)


#K Means#
#Martial Status 1#
#K Means#
#Martial Status 1#
#AGE 45#
#K means for cluster N = 2
km.Data1.Age45  = kmeans(Data1.Age45,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data1.Age45 = kmeans(Data1.Age45,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data1.Age45 = kmeans(Data1.Age45,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data1.Age45  = kmeans(Data1.Age45,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data1.Age45 = kmeans(Data1.Age45,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data1.Age45 = kmeans(Data1.Age45,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8
km.Data1.Age45  = kmeans(Data1.Age45,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Data1.Age45 = kmeans(Data1.Age45,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Data1.Age45 = kmeans(Data1.Age45,10,nstart = 25,iter.max = 15)




K-median
#K-median for 2-10 Clusters
kmed.Mart1.Age45.2 <- kGmedian(Data1.Age45, ncenters = 2)
kmed.Mart1.Age45.3 <- kGmedian(Data1.Age45, ncenters = 3)
kmed.Mart1.Age45.4 <- kGmedian(Data1.Age45, ncenters = 4)
kmed.Mart1.Age45.5 <- kGmedian(Data1.Age45, ncenters = 5)
kmed.Mart1.Age45.6 <- kGmedian(Data1.Age45, ncenters = 6)
kmed.Mart1.Age45.7 <- kGmedian(Data1.Age45, ncenters = 7)
kmed.Mart1.Age45.8 <- kGmedian(Data1.Age45, ncenters = 8)
kmed.Mart1.Age45.9 <- kGmedian(Data1.Age45, ncenters = 9)
kmed.Mart1.Age45.10 <- kGmedian(Data1.Age45, ncenters = 10)


KNN
set.seed(1234)
data_norm<-function(x){((x-min(x)/ (max(x)-min(x))))}
Data1.Age45_Norm<-as.data.frame(lapply(Data1.Age45, data_norm))
ind<-sample(2, nrow(Data1.Age45_Norm), replace=TRUE, prob = c(0.7,0.3))
train.data<-Data1.Age45_Norm[ind==1,]
test.data<-Data1.Age45_Norm[ind==2,]
predict.data<-knn(train.data, test.data, train.data[,6], k =50 , prob=TRUE)
table(predict.data,test.data[,6])




#K Means#
#Martial Status 1#
#K Means#
#Martial Status 1#
#AGE 50#
#K means for cluster N = 2 
km.Data1.Age50  = kmeans(Data1.Age50 ,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data1.Age50 = kmeans(Data1.Age50 ,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data1.Age50 = kmeans(Data1.Age50 ,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data1.Age50  = kmeans(Data1.Age50 ,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data1.Age50 = kmeans(Data1.Age50 ,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data1.Age50 = kmeans(Data1.Age50 ,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Data1.Age50  = kmeans(Data1.Age50 ,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Data1.Age50 = kmeans(Data1.Age50 ,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Data1.Age50 = kmeans(Data1.Age50 ,10,nstart = 25,iter.max = 15)










#K Means#
#Martial Status 1#
#K Means#
#Martial Status 1#
#AGE 55#
#K means for cluster N = 2 
km.Data1.Age55  = kmeans(Data1.Age55,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data1.Age55 = kmeans(Data1.Age55,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data1.Age55 = kmeans(Data1.Age55,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data1.Age55  = kmeans(Data1.Age55,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data1.Age55 = kmeans(Data1.Age55,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data1.Age55 = kmeans(Data1.Age55,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Data1.Age55  = kmeans(Data1.Age55,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Data1.Age55 = kmeans(Data1.Age55,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Data1.Age55 = kmeans(Data1.Age55,10,nstart = 25,iter.max = 15)




#K Means#
#Martial Status 1#
#K Means#
#Martial Status 1#
#AGE 56#
#K means for cluster N = 2 
km.Data1.Age56  = kmeans(Data1.Age56,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.Data1.Age56 = kmeans(Data1.Age56,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.Data1.Age56 = kmeans(Data1.Age56,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.Data1.Age56  = kmeans(Data1.Age56,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.Data1.Age56 = kmeans(Data1.Age56,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.Data1.Age56 = kmeans(Data1.Age56,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.Data1.Age56  = kmeans(Data1.Age56,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.Data1.Age56 = kmeans(Data1.Age56,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.Data1.Age56 = kmeans(Data1.Age56,10,nstart = 25,iter.max = 15)




#iclust for AGE = 17(Marital Status 0 and 1)
#Marital Status = 0
#iclust for N = 2
iclust(Data0.Age17,nclusters=2)
#iclust for N = 3
iclust(Data0.Age17,nclusters=3)
#iclust for N = 4
iclust(Data0.Age17,nclusters=4)
#iclust for N = 5
iclust(Data0.Age17,nclusters=5)
#iclust for N = 6
iclust(Data0.Age171,nclusters=6)
#iclust for N = 7
iclust(Data0.Age17,nclusters=7)
#iclust for N = 8
iclust(Data0.Age17,nclusters=8)
#iclust for N = 9
iclust(Data0.Age17,nclusters=9)
#iclust for N = 10
iclust(Data0.Age17,nclusters=10)




#Marital Status = 1 
#iclust for N = 2
iclust(Data1.Age17,nclusters=2)
#iclust for N = 3
iclust(Data1.Age17,nclusters=3)
#iclust for N = 4
iclust(Data1.Age17,nclusters=4)
#iclust for N = 5
iclust(Data1.Age17,nclusters=5)
#iclust for N = 6
iclust(Data1.Age17,nclusters=6)
#iclust for N = 7
iclust(Data1.Age17,nclusters=7)
#iclust for N = 8
iclust(Data1.Age17,nclusters=8)
#iclust for N = 9
iclust(Data1.Age17,nclusters=9)
#iclust for N = 10
iclust(Data1.Age17,nclusters=10)


#iclust for AGE = 25(Marital Status 0 and 1)
#Marital Status = 0
#iclust for N = 2
iclust(Data0.Age25,nclusters=2)
#iclust for N = 3
iclust(Data0.Age25,nclusters=3)
#iclust for N = 4
iclust(Data0.Age25,nclusters=4)
#iclust for N = 5
iclust(Data0.Age25,nclusters=5)
#iclust for N = 6
iclust(Data0.Age25,nclusters=6)
#iclust for N = 7
iclust(Data0.Age25,nclusters=7)
#iclust for N = 8
iclust(Data0.Age25,nclusters=8)
#iclust for N = 9
iclust(Data0.Age25,nclusters=9)
#iclust for N = 10
iclust(Data0.Age25,nclusters=10)


#Marital Status = 1 
#iclust for N = 2
iclust(Data1.Age25,nclusters=2)
#iclust for N = 3
iclust(Data1.Age25,nclusters=3)
#iclust for N = 4
iclust(Data1.Age25,nclusters=4)
#iclust for N = 5
iclust(Data1.Age25,nclusters=5)
#iclust for N = 6
iclust(Data1.Age25,nclusters=6)
#iclust for N = 7
iclust(Data1.Age25,nclusters=7)
#iclust for N = 8
iclust(Data1.Age25,nclusters=8)
#iclust for N = 9
iclust(Data1.Age25,nclusters=9)
#iclust for N = 10
iclust(Data1.Age25,nclusters=10)


#iclust for AGE = 35(Marital Status 0 and 1)
#Marital Status = 0
#iclust for N = 2
iclust(Data0.Age35,nclusters=2)
#iclust for N = 3
iclust(Data0.Age35,nclusters=3)
#iclust for N = 4
iclust(Data0.Age35,nclusters=4)
#iclust for N = 5
iclust(Data0.Age35,nclusters=5)
#iclust for N = 6
iclust(Data0.Age35,nclusters=6)
#iclust for N = 7
iclust(Data0.Age35,nclusters=7)
#iclust for N = 8
iclust(Data0.Age35,nclusters=8)
#iclust for N = 9
iclust(Data0.Age35,nclusters=9)
#iclust for N = 10
iclust(Data0.Age35,nclusters=10)


#Marital Status = 1 
#iclust for N = 2
iclust(Data1.Age35,nclusters=2)
#iclust for N = 3
iclust(Data1.Age35,nclusters=3)
#iclust for N = 4
iclust(Data1.Age35,nclusters=4)
#iclust for N = 5
iclust(Data1.Age35,nclusters=5)
#iclust for N = 6
iclust(Data1.Age35,nclusters=6)
#iclust for N = 7
iclust(Data1.Age35,nclusters=7)
#iclust for N = 8
iclust(Data1.Age35,nclusters=8)
#iclust for N = 9
iclust(Data1.Age35,nclusters=9)
#iclust for N = 10
iclust(Data1.Age35,nclusters=10)


#iclust for AGE = 45(Marital Status 0 and 1)
#Marital Status = 0
#iclust for N = 2
iclust(Data0.Age45,nclusters=2)
#iclust for N = 3
iclust(Data0.Age45,nclusters=3)
#iclust for N = 4
iclust(Data0.Age45,nclusters=4)
#iclust for N = 5
iclust(Data0.Age45,nclusters=5)
#iclust for N = 6
iclust(Data0.Age45,nclusters=6)
#iclust for N = 7
iclust(Data0.Age45,nclusters=7)
#iclust for N = 8
iclust(Data0.Age45,nclusters=8)
#iclust for N = 9
iclust(Data0.Age45,nclusters=9)
#iclust for N = 10
iclust(Data0.Age45,nclusters=10)


#Marital Status = 1 
#iclust for N = 2
iclust(Data1.Age45,nclusters=2)
#iclust for N = 3
iclust(Data1.Age45,nclusters=3)
#iclust for N = 4
iclust(Data1.Age45,nclusters=4)
#iclust for N = 5
iclust(Data1.Age45,nclusters=5)
#iclust for N = 6
iclust(Data1.Age45,nclusters=6)
#iclust for N = 7
iclust(Data1.Age45,nclusters=7)
#iclust for N = 8
iclust(Data1.Age45,nclusters=8)
#iclust for N = 9
iclust(Data1.Age45,nclusters=9)
#iclust for N = 10
iclust(Data1.Age45,nclusters=10)




#iclust for AGE = 50(Marital Status 0 and 1)
#Marital Status = 0
#iclust for N = 2
iclust(Data0.Age50,nclusters=2)
#iclust for N = 3
iclust(Data0.Age50,nclusters=3)
#iclust for N = 4
iclust(Data0.Age50,nclusters=4)
#iclust for N = 5
iclust(Data0.Age50,nclusters=5)
#iclust for N = 6
iclust(Data0.Age50,nclusters=6)
#iclust for N = 7
iclust(Data0.Age50,nclusters=7)
#iclust for N = 8
iclust(Data0.Age50,nclusters=8)
#iclust for N = 9
iclust(Data0.Age50,nclusters=9)
#iclust for N = 10
iclust(Data0.Age50,nclusters=10)


#Marital Status = 1 
#iclust for N = 2
iclust(Data1.Age50,nclusters=2)
#iclust for N = 3
iclust(Data1.Age50,nclusters=3)
#iclust for N = 4
iclust(Data1.Age50,nclusters=4)
#iclust for N = 5
iclust(Data1.Age50,nclusters=5)
#iclust for N = 6
iclust(Data1.Age50,nclusters=6)
#iclust for N = 7
iclust(Data1.Age50,nclusters=7)
#iclust for N = 8
iclust(Data1.Age50,nclusters=8)
#iclust for N = 9
iclust(Data1.Age50,nclusters=9)
#iclust for N = 10
iclust(Data1.Age50,nclusters=10)


#iclust for AGE = 55(Marital Status 0 and 1)
#Marital Status = 0
#iclust for N = 2
iclust(Data0.Age55,nclusters=2)
#iclust for N = 3
iclust(Data0.Age55,nclusters=3)
#iclust for N = 4
iclust(Data0.Age55,nclusters=4)
#iclust for N = 5
iclust(Data0.Age55,nclusters=5)
#iclust for N = 6
iclust(Data0.Age55,nclusters=6)
#iclust for N = 7
iclust(Data0.Age55,nclusters=7)
#iclust for N = 8
iclust(Data0.Age55,nclusters=8)
#iclust for N = 9
iclust(Data0.Age55,nclusters=9)
#iclust for N = 10
iclust(Data0.Age55,nclusters=10)


#Marital Status = 1 
#iclust for N = 2
iclust(Data1.Age55,nclusters=2)
#iclust for N = 3
iclust(Data1.Age55,nclusters=3)
#iclust for N = 4
iclust(Data1.Age55,nclusters=4)
#iclust for N = 5
iclust(Data1.Age55,nclusters=5)
#iclust for N = 6
iclust(Data1.Age55,nclusters=6)
#iclust for N = 7
iclust(Data1.Age55,nclusters=7)
#iclust for N = 8
iclust(Data1.Age55,nclusters=8)
#iclust for N = 9
iclust(Data1.Age55,nclusters=9)
#iclust for N = 10
iclust(Data1.Age55,nclusters=10)


#iclust for AGE = 56(Marital Status 0 and 1)
#Marital Status = 0
#iclust for N = 2
iclust(Data0.Age56,nclusters=2)
#iclust for N = 3
iclust(Data0.Age56,nclusters=3)
#iclust for N = 4
iclust(Data0.Age56,nclusters=4)
#iclust for N = 5
iclust(Data0.Age56,nclusters=5)
#iclust for N = 6
iclust(Data0.Age56,nclusters=6)
#iclust for N = 7
iclust(Data0.Age56,nclusters=7)
#iclust for N = 8
iclust(Data0.Age56,nclusters=8)
#iclust for N = 9
iclust(Data0.Age56,nclusters=9)
#iclust for N = 10
iclust(Data0.Age56,nclusters=10)




#Marital Status = 1 
#iclust for N = 2
iclust(Data1.Age56,nclusters=2)
#iclust for N = 3
iclust(Data1.Age56,nclusters=3)
#iclust for N = 4
iclust(Data1.Age56,nclusters=4)
#iclust for N = 5
iclust(Data1.Age56,nclusters=5)
#iclust for N = 6
iclust(Data1.Age56,nclusters=6)
#iclust for N = 7
iclust(Data1.Age56,nclusters=7)
#iclust for N = 8
iclust(Data1.Age56,nclusters=8)
#iclust for N = 9
iclust(Data1.Age56,nclusters=9)
#iclust for N = 10
iclust(Data1.Age56,nclusters=10)




#k means clustering for city category A#
#AGE 17#
#K means for cluster N = 2 
km.cityA.age17  = kmeans(Data.A.Age17,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityA.age17 = kmeans(Data.A.Age17,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityA.age17 = kmeans(Data.A.Age17,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityA.age17  = kmeans(Data.A.Age17,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityA.age17 = kmeans(Data.A.Age17,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityA.age17 = kmeans(Data.A.Age17,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityA.age17  = kmeans(Data.A.Age17,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityA.age17 = kmeans(Data.A.Age17,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityA.age17 = kmeans(Data.A.Age17,10,nstart = 25,iter.max = 15)


#K Means#
#AGE 25#
#K means for cluster N = 2 
km.cityA.age25  = kmeans(Data.A.Age25,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityA.age25 = kmeans(Data.A.Age25,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityA.age25 = kmeans(Data.A.Age25,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityA.age25  = kmeans(Data.A.Age25,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityA.age25 = kmeans(Data.A.Age25,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityA.age25 = kmeans(Data.A.Age25,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityA.age25  = kmeans(Data.A.Age25,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityA.age25 = kmeans(Data.A.Age25,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityA.age25 = kmeans(Data.A.Age25,10,nstart = 25,iter.max = 15)


K-median
#K-median for 2-10 Clusters
kmed.CityA.Age25.2 <- kGmedian(Data.A.Age25, ncenters = 2)
kmed.CityA.Age25.3 <- kGmedian(Data.A.Age25, ncenters = 3)
kmed.CityA.Age25.4 <- kGmedian(Data.A.Age25, ncenters = 4)
kmed.CityA.Age25.5 <- kGmedian(Data.A.Age25, ncenters = 5)
kmed.CityA.Age25.6 <- kGmedian(Data.A.Age25, ncenters = 6)
kmed.CityA.Age25.7 <- kGmedian(Data.A.Age25, ncenters = 7)
kmed.CityA.Age25.8 <- kGmedian(Data.A.Age25, ncenters = 8)
kmed.CityA.Age25.9 <- kGmedian(Data.A.Age25, ncenters = 9)
kmed.CityA.Age25.10 <- kGmedian(Data.A.Age25, ncenters = 10)
KNN
set.seed(1234)
data_norm<-function(x){((x-min(x)/ (max(x)-min(x))))}
Data.A.Age25_Norm<-as.data.frame(lapply(Data.A.Age25, data_norm))
ind<-sample(2, nrow(Data.A.Age25_Norm), replace=TRUE, prob = c(0.7,0.3))
train.data<-Data.A.Age25_Norm[ind==1,]
test.data<-Data.A.Age25_Norm[ind==2,]
predict.data<-knn(train.data, test.data, train.data[,5], k =50 , prob=TRUE)
table(predict.data,test.data[,5])




#K Means#
#AGE 35#
#K means for cluster N = 2 
km.cityA.age35  = kmeans(Data.A.Age35,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityA.age35 = kmeans(Data.A.Age35,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityA.age35 = kmeans(Data.A.Age35,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityA.age35  = kmeans(Data.A.Age35,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityA.age35 = kmeans(Data.A.Age35,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityA.age35 = kmeans(Data.A.Age35,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityA.age35  = kmeans(Data.A.Age35,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityA.age35 = kmeans(Data.A.Age35,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityA.age35 = kmeans(Data.A.Age35,10,nstart = 25,iter.max = 15)


#K Means#
#AGE 45#
#K means for cluster N = 2 
km.cityA.age45  = kmeans(Data.A.Age45,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityA.age45 = kmeans(Data.A.Age45,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityA.age45 = kmeans(Data.A.Age45,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityA.age45  = kmeans(Data.A.Age45,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityA.age45 = kmeans(Data.A.Age45,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityA.age45 = kmeans(Data.A.Age45,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityA.age45  = kmeans(Data.A.Age45,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityA.age45 = kmeans(Data.A.Age45,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityA.age45 = kmeans(Data.A.Age45,10,nstart = 25,iter.max = 15)


#K Means#
#AGE 50#
#K means for cluster N = 2 
km.cityA.age50  = kmeans(Data.A.Age50,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityA.age50 = kmeans(Data.A.Age50,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityA.age50 = kmeans(Data.A.Age50,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityA.age50  = kmeans(Data.A.Age50,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityA.age50 = kmeans(Data.A.Age50,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityA.age50 = kmeans(Data.A.Age50,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityA.age50  = kmeans(Data.A.Age50,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityA.age50 = kmeans(Data.A.Age50,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityA.age50 = kmeans(Data.A.Age50,10,nstart = 25,iter.max = 15)


#K Means#
#AGE 55#
#K Means#
#K means for cluster N = 2 
km.cityA.age55  = kmeans(Data.A.Age55,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityA.age55 = kmeans(Data.A.Age55,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityA.age55 = kmeans(Data.A.Age55,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityA.age55  = kmeans(Data.A.Age55,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityA.age55 = kmeans(Data.A.Age55,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityA.age55 = kmeans(Data.A.Age55,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityA.age55  = kmeans(Data.A.Age55,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityA.age55 = kmeans(Data.A.Age55,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityA.age55 = kmeans(Data.A.Age55,10,nstart = 25,iter.max = 15)


#K Means#
#K means for cluster N = 2 
km.cityA.age56  = kmeans(Data0.Age56,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityA.age56 = kmeans(Data0.Age56,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityA.age56 = kmeans(Data0.Age56,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityA.age56  = kmeans(Data0.Age56,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityA.age56 = kmeans(Data0.Age56,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityA.age56 = kmeans(Data0.Age56,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityA.age56  = kmeans(Data0.Age56,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityA.age56 = kmeans(Data0.Age56,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityA.age56 = kmeans(Data0.Age56,10,nstart = 25,iter.max = 15)




#k means for city category B#
#AGE 17#
#K means for cluster N = 2 
km.cityB.age17  = kmeans(Data.B.Age17,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityB.age17 = kmeans(Data.B.Age17,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityB.age17 = kmeans(Data.B.Age17,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityB.age17  = kmeans(Data.B.Age17,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityB.age17 = kmeans(Data.B.Age17,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityB.age17 = kmeans(Data.B.Age17,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityB.age17  = kmeans(Data.B.Age17,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityB.age17 = kmeans(Data.B.Age17,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityB.age17 = kmeans(Data.B.Age17,10,nstart = 25,iter.max = 15)


#K Means#
#AGE 25#
#K means for cluster N = 2 
km.cityB.age25  = kmeans(Data.B.Age25,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityB.age25 = kmeans(Data.B.Age25,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityB.age25 = kmeans(Data.B.Age25,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityB.age25  = kmeans(Data.B.Age25,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityB.age25 = kmeans(Data.B.Age25,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityB.age25 = kmeans(Data.B.Age25,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityB.age25  = kmeans(Data.B.Age25,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityB.age25 = kmeans(Data.B.Age25,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityB.age25 = kmeans(Data.B.Age25,10,nstart = 25,iter.max = 15)




#K Means#
#AGE 35#
#K means for cluster N = 2 
km.cityB.age35  = kmeans(Data.B.Age35,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityB.age35 = kmeans(Data.B.Age35,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityB.age35 = kmeans(Data.B.Age35,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityB.age35  = kmeans(Data.B.Age35,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityB.age35 = kmeans(Data.B.Age35,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityB.age35 = kmeans(Data.B.Age35,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityB.age35  = kmeans(Data.B.Age35,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityB.age35 = kmeans(Data.B.Age35,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityB.age35 = kmeans(Data.B.Age35,10,nstart = 25,iter.max = 15)


#K Means#
#AGE 45#
#K means for cluster N = 2 
km.cityB.age45  = kmeans(Data.B.Age45,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityB.age45 = kmeans(Data.B.Age45,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityB.age45 = kmeans(Data.B.Age45,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityB.age45  = kmeans(Data.B.Age45,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityB.age45 = kmeans(Data.B.Age45,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityB.age45 = kmeans(Data.B.Age45,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityB.age45  = kmeans(Data.B.Age45,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityB.age45 = kmeans(Data.B.Age45,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityB.age45 = kmeans(Data.B.Age45,10,nstart = 25,iter.max = 15)


#K Means#
#AGE 50#
#K means for cluster N = 2 
km.cityB.age50  = kmeans(Data.B.Age50,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityB.age50 = kmeans(Data.B.Age50,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityB.age50 = kmeans(Data.B.Age50,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityB.age50  = kmeans(Data.B.Age50,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityB.age50 = kmeans(Data.B.Age50,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityB.age50 = kmeans(Data.B.Age50,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityB.age50  = kmeans(Data.B.Age50,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityB.age50 = kmeans(Data.B.Age50,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityB.age50 = kmeans(Data.B.Age50,10,nstart = 25,iter.max = 15)


#K Means#
#AGE 55#
#K Means#
#K means for cluster N = 2 
km.cityB.age55  = kmeans(Data.B.Age55,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityB.age55 = kmeans(Data.B.Age55,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityB.age55 = kmeans(Data.B.Age55,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityB.age55  = kmeans(Data.B.Age55,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityB.age55 = kmeans(Data.B.Age55,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityB.age55 = kmeans(Data.B.Age55,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityB.age55  = kmeans(Data.B.Age55,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityB.age55 = kmeans(Data.B.Age55,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityB.age55 = kmeans(Data.B.Age55,10,nstart = 25,iter.max = 15)


#K Means#
#K means for cluster N = 2 
km.cityB.age56  = kmeans(Data0.Age56,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityB.age56 = kmeans(Data0.Age56,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityB.age56 = kmeans(Data0.Age56,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityB.age56  = kmeans(Data0.Age56,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityB.age56 = kmeans(Data0.Age56,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityB.age56 = kmeans(Data0.Age56,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityB.age56  = kmeans(Data0.Age56,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityB.age56 = kmeans(Data0.Age56,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityB.age56 = kmeans(Data0.Age56,10,nstart = 25,iter.max = 15)




#k means for city category C#
#AGE 17#
#K means for cluster N = 2 
km.cityC.age17  = kmeans(Data.C.Age17,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityC.age17 = kmeans(Data.C.Age17,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityC.age17 = kmeans(Data.C.Age17,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityC.age17  = kmeans(Data.C.Age17,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityC.age17 = kmeans(Data.C.Age17,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityC.age17 = kmeans(Data.C.Age17,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityC.age17  = kmeans(Data.C.Age17,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityC.age17 = kmeans(Data.C.Age17,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityC.age17 = kmeans(Data.C.Age17,10,nstart = 25,iter.max = 15)


#K Means#
#AGE 25#
#K means for cluster N = 2 
km.cityC.age25  = kmeans(Data.C.Age25,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityC.age25 = kmeans(Data.C.Age25,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityC.age25 = kmeans(Data.C.Age25,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityC.age25  = kmeans(Data.C.Age25,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityC.age25 = kmeans(Data.C.Age25,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityC.age25 = kmeans(Data.C.Age25,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityC.age25  = kmeans(Data.C.Age25,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityC.age25 = kmeans(Data.C.Age25,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityC.age25 = kmeans(Data.C.Age25,10,nstart = 25,iter.max = 15)




#K Means#
#AGE 35#
#K means for cluster N = 2 
km.cityC.age35  = kmeans(Data.C.Age35,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityC.age35 = kmeans(Data.C.Age35,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityC.age35 = kmeans(Data.C.Age35,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityC.age35  = kmeans(Data.C.Age35,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityC.age35 = kmeans(Data.C.Age35,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityC.age35 = kmeans(Data.C.Age35,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityC.age35  = kmeans(Data.C.Age35,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityC.age35 = kmeans(Data.C.Age35,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityC.age35 = kmeans(Data.C.Age35,10,nstart = 25,iter.max = 15)


#K Means#
#AGE 45#
#K means for cluster N = 2 
km.cityC.age45  = kmeans(Data.C.Age45,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityC.age45 = kmeans(Data.C.Age45,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityC.age45 = kmeans(Data.C.Age45,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityC.age45  = kmeans(Data.C.Age45,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityC.age45 = kmeans(Data.C.Age45,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityC.age45 = kmeans(Data.C.Age45,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityC.age45  = kmeans(Data.C.Age45,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityC.age45 = kmeans(Data.C.Age45,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityC.age45 = kmeans(Data.C.Age45,10,nstart = 25,iter.max = 15)


#K Means#
#AGE 50#
#K means for cluster N = 2 
km.cityC.age50  = kmeans(Data.C.Age50,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityC.age50 = kmeans(Data.C.Age50,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityC.age50 = kmeans(Data.C.Age50,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityC.age50  = kmeans(Data.C.Age50,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityC.age50 = kmeans(Data.C.Age50,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityC.age50 = kmeans(Data.C.Age50,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityC.age50  = kmeans(Data.C.Age50,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityC.age50 = kmeans(Data.C.Age50,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityC.age50 = kmeans(Data.C.Age50,10,nstart = 25,iter.max = 15)


#K Means#
#AGE 55#
#K Means#
#K means for cluster N = 2 
km.cityC.age55  = kmeans(Data.C.Age55,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityC.age55 = kmeans(Data.C.Age55,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityC.age55 = kmeans(Data.C.Age55,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityC.age55  = kmeans(Data.C.Age55,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityC.age55 = kmeans(Data.C.Age55,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityC.age55 = kmeans(Data.C.Age55,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityC.age55  = kmeans(Data.C.Age55,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityC.age55 = kmeans(Data.C.Age55,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityC.age55 = kmeans(Data.C.Age55,10,nstart = 25,iter.max = 15)


#K Means#
#K means for cluster N = 2 
km.cityC.age56  = kmeans(Data0.Age56,2,nstart = 25,iter.max = 15)
#K means for cluster N = 3
km.cityC.age56 = kmeans(Data0.Age56,3,nstart = 25,iter.max = 15)
#K means for cluster N = 4 
km.cityC.age56 = kmeans(Data0.Age56,4,nstart = 25,iter.max = 15)
#K means for cluster N = 5 
km.cityC.age56  = kmeans(Data0.Age56,5,nstart = 25,iter.max = 15)
#K means for cluster N = 6
km.cityC.age56 = kmeans(Data0.Age56,6,nstart = 25,iter.max = 15)
#K means for cluster N = 7 
km.cityC.age56 = kmeans(Data0.Age56,7,nstart = 25,iter.max = 15)
#K means for cluster N = 8 
km.cityC.age56  = kmeans(Data0.Age56,8,nstart = 25,iter.max = 15)
#K means for cluster N = 9
km.cityC.age56 = kmeans(Data0.Age56,9,nstart = 25,iter.max = 15)
#K means for cluster N = 10 
km.cityC.age56 = kmeans(Data0.Age56,10,nstart = 25,iter.max = 15)




KNN




set.seed(1234)
data_norm<-function(x){((x-min(x)/ (max(x)-min(x))))}


Data.Occ0_Norm<-as.data.frame(lapply(Data.Occ0, data_norm))
ind<-sample(2, nrow(Data.Occ0_Norm), replace=TRUE, prob = c(0.7,0.3))
train.data<-Data.Occ0_Norm[ind==1,]
test.data<-Data.Occ0_Norm[ind==2,]
predict.data<-knn(train.data, test.data, train.data[,6], k =50 , prob=TRUE)


For Training and Testing Data sets


Training Data and Test Data for 70% and 30% for Occupation 0
Data.Occ0 <- subset(Data.clean,Occupation == "0")#subsets into occupation 0
head(Data.Occ0)
Occ0.train <- sample(nrow(Data.Occ0),0.7*nrow(Data.Occ0))#sampling of the data
Occ0.clean.train <- Data.Occ0[Occ0.train,]#convert into training
Occ0.test <- Data.Occ0[-Occ0.train,]
Occ0.clean.train$User_ID<- NULL
Occ0.test$User_ID<- NULL#deleting the user id
head(Occ0.clean.train)


Training Data and Test Data for 60% and 40% for Occupation 0
Occ01.train <- sample(nrow(Data.Occ0),0.6*nrow(Data.Occ0)))#sampling of the data
Occ01.clean.train <- Data.Occ0[Occ01.train,])
Occ0.test <- Data.Occ0[-Occ01.train,]
Occ01.clean.train$User_ID<- NULL#deleting the user id
Occ0.test$User_ID<- NULL




Training Data and Test Data for 50% and 50% for Occupation 0
Occ0.train <- sample(nrow(Data.Occ0),0.5*nrow(Data.Occ0))#sampling of the data
OCcc0.clean.train <- Data.Occ0[Occ0.train,]#convert into train sets
Occ0.test <- Data.Occ0[-Occ0.train,]
OCcc0.clean.train$User_ID<- NULL#deleting the user id
Occ0.test$User_ID<- NULL




Training Data and Test Data for 70% and 30% for Marital Status 1 with Age 45
Data.Mart1 <- subset(Data.clean, Marital_Status == "1")
Data1.Age45 <- subset(Data.Mart1,Age=="45")
Data45.train <- sample(nrow(Data1.Age45),0.7*nrow(Data1.Age45))
Data45.clean.train <- Data1.Age45[Data17.train,]
Data45.test <- Data1.Age17[-Data17.train,]
Data45.clean.train$User_ID<- NULL
Data45.test$User_ID<- NULL




Training Data and Test Data for 60% and 40% for Marital Status 1 with Age 45
Data.Mart1 <- subset(Data.clean, Marital_Status == "1")
Data1.Age45 <- subset(Data.Mart1,Age=="45")
Data45.train <- sample(nrow(Data1.Age45),0.6*nrow(Data1.Age45))
Data45.clean.train <- Data1.Age45[Data17.train,]
Data45.test <- Data1.Age17[-Data17.train,]
Data45.clean.train$User_ID<- NULL
Data45.test$User_ID<- NULL




Training Data and Test Data for 50% and 50% for Marital Status 1 with Age 45
Data.Mart1 <- subset(Data.clean, Marital_Status == "1")
Data1.Age45 <- subset(Data.Mart1,Age=="45")
Data45.train <- sample(nrow(Data1.Age45),0.5*nrow(Data1.Age45))
Data45.clean.train <- Data1.Age45[Data17.train,]
Data45.test <- Data1.Age17[-Data17.train,]
Data45.clean.train$User_ID<- NULL
Data45.test$User_ID<- NULL




Training Data and Test Data for 70% and 30% for City Category A with Age 25
Data.CityA <- subset(Data.clean,City_Category == "1")
Data.A.Age25 <- subset(Data.CityA,Age=="25")
Data25.train <- sample(nrow(Data.A.Age25),0.7*nrow(Data.A.Age25))
Data25.clean.train <- Data.A.Age25[Data25.train,]
Data25.test <- Data.A.Age25[-Data25.train,]
Data25.clean.train$User_ID<- NULL
Data25.test$User_ID<- NULL




Training Data and Test Data for 60% and 40% for City Category A with Age 25
Data.CityA <- subset(Data.clean,City_Category == "1")
Data.A.Age25 <- subset(Data.CityA,Age=="25")
Data25.train <- sample(nrow(Data.A.Age25),0.6*nrow(Data.A.Age25))
Data25.clean.train <- Data.A.Age25[Data25.train,]
Data25.test <- Data.A.Age25[-Data25.train,]
Data25.clean.train$User_ID<- NULL
Data25.test$User_ID<- NULL




Training Data and Test Data for 50% and 50% for City Category A with Age 25
Data.CityA <- subset(Data.clean,City_Category == "1")
Data.A.Age25 <- subset(Data.CityA,Age=="25")
Data25.train <- sample(nrow(Data.A.Age25),0.5*nrow(Data.A.Age25))
Data25.clean.train <- Data.A.Age25[Data25.train,]
Data25.test <- Data.A.Age25[-Data25.train,]
Data25.clean.train$User_ID<- NULL
Data25.test$User_ID<- NULL


Occ0.lm <-lm(formula = data[,"Gender"] ~ data[,"Age"]+ data[,"Product_ID"]+data[,"Marital_Status"]+ data[,"Stay_In_Current_City_Years"], data=OCcc0.clean.train) #occupation 0 lm method
Occ0.glm <-glm(formula = data[,"Gender"] ~ data[,"Age"]+ data[,"Product_ID"]+data[,"Marital_Status"]+ data[,"Stay_In_Current_City_Years"], data=Occ0.clean.train,family=gaussian) #occupation 0 glm method
plot(Occ0.lm$fitted.values,Occ0.lm$residuals) #plot fitted and residual values
plot(Occ0.glm$fitted.values,Occ0.glm$residuals)#plot fitted and residual values




Data45.lm <-lm(formula = data[,"Gender"] ~data[,"Product_ID"]+ data[,"Stay_In_Current_City_Years"], data=Data45.clean.train)#lm method for Marital Status 1 with age 45
Data45.glm <-glm(formula = data[,"Gender"] ~data[,"Product_ID"]+ data[,"Stay_In_Current_City_Years"], data=Data45.clean.train,family=gaussian) #glm method for Marital Status 1 with age 45
plot(Data45.glm$fitted.values,Data45.glm$residuals) #plot fitted and residual values


Data25.lm <-lm(formula = data[,"Gender"] ~data[,"Marital_Status"]+data[,"Product_ID"]+ data[,"Stay_In_Current_City_Years"], data=Data25.clean.train) #lm method for City Category A with age 25
Data25.glm <-glm(formula = data[,"Gender"] ~data[,"Marital_Status"]+data[,"Product_ID"]+ data[,"Stay_In_Current_City_Years"], data=Data25.clean.train,family=gaussian)#glm method for City Category A with Age 25
plot(Data25.lm$fitted.values,Data25.lm$residuals) #plot fitted and residual values


autopredict <- predict.lm(Occ0.lm, new, interval = "confidence") #predict lm for Occupation 0
PredictData45<-predict.lm(Data45.lm,new,interval=”confidence”)#predict lm for for Marital Status 1 with age 45
PredictData25<-predict.lm(Data25.lm,new,interval=”confidence”)#predict lm for for City Category A with age 25




PredictOccu0<- predict.glm(Occu0.glm,NULL,type=c("link","response","terms")) #predict glm data for Occupation 0
PredictData45<- predict.glm(Data45.glm,NULL,type=c("link","response","terms")) #predict glm data for Marital Status 1 with age 45
PredictData25 <- predict.glm(Data25.glm,NULL,type=c("link","response","terms")) #predict glm data for City Category A with age 25


#ward hierarchical
fit <- pvclust(Data.Occ0, method.hclust="ward", method.dist="euclidean")
plot(fit) #for occupation 0                


fit <- pvclust(Data.Age45, method.hclust="ward",  method.dist="euclidean")
plot(fit) #for marital status 1 with age 45


fit <- pvclust(Data.A.Age25, method.hclust="ward", method.dist="euclidean")
plot(fit) #for city category A with Age 25