car_mpg<-read.csv('MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
head(car_mpg)
#Multiple regression
model<-lm(mpg~ vehicle.length+vehicle.weight+spoiler.angle+ground.clearance+AWD,data=car_mpg)
summary(lm(mpg~ vehicle.length+vehicle.weight+spoiler.angle+ground.clearance+AWD,data=car_mpg))

# Vehicle length & MPG
variable1<-lm(mpg~vehicle.length,data=car_mpg)
yval1<-variable1$coefficients['vehicle.length']*car_mpg$vehicle.length+variable1$coefficients['(Intercept)']
plt1<-ggplot(car_mpg,aes(x=vehicle.length,y=mpg))
plt1+geom_point()+geom_line(aes(x=vehicle.length,y=yval1),color="red")


# Vehicle weight & MPG
variable2<-lm(mpg~vehicle.weight,data=car_mpg)
yval2<-variable2$coefficients['vehicle.weight']*car_mpg$vehicle.weight+variable2$coefficients['(Intercept)']
plt2<-ggplot(car_mpg,aes(x=vehicle.weight,y=mpg))
plt2+geom_point()+geom_line(aes(x=vehicle.weight,y=yval2),color="red")

#Spoiler angle & MPG
variable3<-lm(mpg~spoiler.angle,data=car_mpg)
yval3<-variable3$coefficients['spoiler.angle']*car_mpg$spoiler.angle+variable3$coefficients['(Intercept)']
plt3<-ggplot(car_mpg,aes(x=spoiler.angle,y=mpg))
plt3+geom_point()+geom_line(aes(x=spoiler.angle,y=yval3),color="red")#Spoiler angle & MPG

#Ground Clearance & MPG
variable4<-lm(mpg~ground.clearance,data=car_mpg)
yval4<-variable4$coefficients['ground.clearance']*car_mpg$ground.clearance+variable4$coefficients['(Intercept)']
plt4<-ggplot(car_mpg,aes(x=ground.clearance,y=mpg))
plt4+geom_point()+geom_line(aes(x=ground.clearance,y=yval4),color="red")

#AWD & MPG
variable5<-lm(mpg~AWD,data=car_mpg)
yval5<-variable5$coefficients['AWD']*car_mpg$AWD+variable5$coefficients['(Intercept)']
plt5<-ggplot(car_mpg,aes(x=AWD,y=mpg))
plt5+geom_point()+geom_line(aes(x=AWD,y=yval5),color="red")

#Suspension Coil summary
sus_coil<- read.csv('Suspension_Coil.csv',check.names = F,stringsAsFactors = F)
summary_table <-sus_coil %>% summarize(PSI_Mean=mean(PSI),PSI_Median=median(PSI),PSI_SD=sd(PSI))

plt6<-ggplot(sus_coil,aes(x=PSI))
plt6+geom_density()
sus_coil_samp<-sus_coil %>% sample_n(50)
sus_coil_samp2<-sus_coil %>% sample_n(50)
t.test(sus_coil_samp$PSI,sus_coil_samp2$PSI)