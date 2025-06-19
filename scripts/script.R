data<- read.csv("C:/Users/edlsx/Desktop/retour entretien - volontaires.csv")
data<-data[,3:7]

data[,1]<-as.factor(data[,1])
data[,5]<-as.factor(data[,5])
data[,3]<-as.factor(data[,3])
data[,4]<-as.factor(data[,4])

data<-as.data.frame(data)
summary(data)

dh <- data[data$Sex == "M", ]
df <- data[data$Sex == "F", ]

data<-data[,2:5]
dh<-dh[,2:5]
df<-df[,2:5]

s<-as.data.frame(summary(data))
sf<-as.data.frame(summary(df))
sh<-as.data.frame(summary(dh))
sfi<-cbind(s,sf,sh)
sfinal<-sfi[,c(2,3,6,9)]
sfinal<-sfinal[1:23,]
sfinal<-sfinal[-c(7,21),]


year_birth<-c("1998(1989-1999)","1998(1992-1999)","1994(1978-1998)")
medical_student<-c("20(59)","13(73)","7(44)")
nurse<-c("3(9)","3(17)","0(0)")
anesth<-c("2(6)","1(5)","1(6)")
huma<-c("2(6)","0(0)","2(12)")
others<-c("7(20)","1(5)","6(38)")
lille<-c("28(82)","16(90)","12(76)")
waziers<-c("1(3)","0(0)","1(6)")
lomme<-c("2(6)","1(5)","1(6)")
other<-c("3(9)","1(5)","2(12)")
ukrainian<-c("3(9)","1(5)","2(12)")
prof<-c("","","")
town<-c("","","")

tablefinal<-as.data.frame(rbind(year_birth,prof,medical_student,nurse,anesth,huma,others,town,lille,waziers,lomme,other,ukrainian))
rownames(tablefinal)<-c("Median year of birth (range)-yr","Profession-no.(%)","Medical student","Nurse","Anesthesiologist","Humanitarian worker","Others","Living town-no.(%)","Lille","Waziers","Lomme","Other towns","Ukrainian naionality-no.(%)")
colnames(tablefinal)<-c("All (n=34)","Women (n=18)","Men (n=16)")
gridExtra::grid.table(tablefinal)
