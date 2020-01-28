mpg_cars <- read.csv('MechaCar_mpg.csv',check.names = F,stringsAsFactors = F) #import  car mpg dataset
scatter.smooth(x=mpg_cars$mpg,y=mpg_cars$`vehicle weight`)
scatter.smooth(x=mpg_cars$mpg,y=mpg_cars$`vehicle length`)
scatter.smooth(x=mpg_cars$mpg,y=mpg_cars$`spoiler angle`)
scatter.smooth(x=mpg_cars$mpg,y=mpg_cars$`ground clearance`)
scatter.smooth(x=mpg_cars$mpg,y=mpg_cars$`AWD`)
cor(mpg_cars$mpg, mpg_cars$`vehicle weight`)
cor(mpg_cars$mpg, mpg_cars$`vehicle length`)
cor(mpg_cars$mpg, mpg_cars$`spoiler angle`).
cor(mpg_cars$mpg, mpg_cars$`ground clearance`)
cor(mpg_cars$mpg, mpg_cars$`AWD`)
linearMod = lm(mpg ~ vehicle length, data=MechaCar_mpg.csv) 
fit <- lm(mpg~vehicle length+AWD,data=mpg_cars)
fit<- lm(mpg~mpg_cars$`vehicle weight`+mpg_cars$`vehicle length`+mpg_cars$`spoiler angle`+mpg_cars$`ground clearance`+mpg_cars$`AWD`,data=mpg_cars)
print(fit) # linear regression model with multiple variables.
a <- coef(fit)
print(a)
XWeight <- coef(fit)[2]
XLength <- coef(fit)[3]
XAngle <- coef(fit)[4]
XClearnace <- coef(fit)[5]
XAWD <- coef(fit)[6]
MPG Predictions 



S_Coil <- read.csv('Suspension_Coil.csv',check.names = F,stringsAsFactors = F) #import suspenstion coil data
describe(S_coil)
mean(S_coil$'PSI')
library(Hmisc)
library(psych)
describe(S_Coil$PSI)
var(S_Coil$PSI)
sd(S_Coil$PSI)
t.test(log10(S_Coil$PSI),log10(sample_table2$Miles_Driven)) #compare means of two samples
T_test<-t.test(S_Coil$PSI,mu=1500)
print(T_test)