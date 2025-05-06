library(readxl)
library(mlogit)
library(ggplot2)

#Load dataset
data <- read_excel("data.xlsx")

#create dataset for control words only
data_control <- data[is.element(data$Stimuli, c("jurlaka","pujikat","warlaku","makin","juwelap")),]
mlogit_data_control <- mlogit.data(data_control, shape = "wide", choice = "Answer", idx = list(c("Stimuliid","Participant")))
mlogit_data_control$Origin <- relevel(mlogit_data_control$Origin, ref="K")

#run the simpler model for age and gender effect
model <- mlogit(Answer ~ 1| Origin/Gender/Age,data = mlogit_data_control)
summary(model)

#create dataset for test words only
mlogit_data <- mlogit.data(data, shape = "wide", choice = "Answer", idx = list(c("Stimuliid","Participant")))
mlogit_data$Origin <- relevel(mlogit_data$Origin, ref="K")
for (i in c("jurlaka","pujikat","warlaku","makin","juwelap")) {
	mlogit_data <- mlogit_data[-which(mlogit_data$Stimuli==i),]
}

#run the simpler model for age and gender effect
model <- mlogit(Answer ~ 1| Origin/Gender/Age,data=mlogit_data)
summary(model)

#Create intercept stimuli by combining jurlaka, pujikat, warlaku, makin, juwelap
intercept <- rbind(data[data$Stimuli=="jurlaka",],data[data$Stimuli=="pujikat",],data[data$Stimuli=="warlaku",],data[data$Stimuli=="makin",],data[data$Stimuli=="juwelap",])
intercept$Stimuli <- "1_intercept"
intercept$Stimuliid <- c(1:length(intercept$Stimuliid))
data <- rbind(intercept,data)

#create dataset for full model
mlogit_data <- mlogit.data(data, shape = "wide", choice = "Answer", idx = list(c("Stimuliid","Participant")))
mlogit_data$Origin <- relevel(mlogit_data$Origin, ref="K")
mlogit_data$Participant <- relevel(mlogit_data$Participant, ref="cassandra")
Stimuli <- levels(mlogit_data$Stimuli)[-1]
nStimuli <- length(Stimuli)
Participant <- levels(mlogit_data$Participant)[-1]
nParticipant <- length(Participant)

#run the full model
model.full <- mlogit(Answer ~ 1 | Stimuli + Participant, data = mlogit_data) #Intercept is Cassandra's answers to the five control words

#interpret coefficient for each word variant
pvalues <- t(matrix(summary(model.full)$CoefTable[2+c(1:(nStimuli*2)),4],nrow=2))
p <- min(c(pvalues[Stimuli=="jurlaka",],pvalues[Stimuli=="pujikat",],pvalues[Stimuli=="warlaku",],pvalues[Stimuli=="makin",],pvalues[Stimuli=="juwelap",]))
coef <- t(matrix(model.full$coefficient[2+c(1:(nStimuli*2))],nrow=2))
coef <- data.frame(coef)
rownames(coef) <- Stimuli
colnames(coef) <- c("Old","Young")
coef$Old <- exp(model.full$coefficient[1] + coef$Old)
coef$Young <- exp(model.full$coefficient[2] + coef$Young)
coef$Old <- coef$Old / (1+coef$Old+coef$Young) / (exp(model.full$coefficient[1])/(1+exp(model.full$coefficient[1])+exp(model.full$coefficient[2])))
coef$Young <- coef$Young / (1+coef$Old+coef$Young) / (exp(model.full$coefficient[2])/(1+exp(model.full$coefficient[1])+exp(model.full$coefficient[2])))
Origin <- read.csv("origin.csv")
coef$Origin <- Origin$Origin
coef$Reference <- Origin$Reference
CIlower <- t(matrix(confint(model.full,level=1-p)[,1][2+c(1:(nStimuli*2))],nrow=2))
CIupper <- t(matrix(confint(model.full,level=1-p)[,2][2+c(1:(nStimuli*2))],nrow=2))
coef$OldCIlower <- exp(model.full$coefficient[1] + CIlower[,1]) #2.5%
coef$OldCIupper <- exp(model.full$coefficient[1] + CIupper[,1]) #97.5%
coef$YoungCIlower <- exp(model.full$coefficient[2] + CIlower[,2]) #2.5%
coef$YoungCIupper <- exp(model.full$coefficient[2] + CIupper[,2]) #97.5%
coef$OldCIlower <- coef$OldCIlower / (1+coef$OldCIlower+coef$Young) / (exp(model.full$coefficient[1])/(1+exp(model.full$coefficient[1])+exp(model.full$coefficient[2])))
coef$OldCIupper <- coef$OldCIupper / (1+coef$OldCIupper+coef$Young) / (exp(model.full$coefficient[1])/(1+exp(model.full$coefficient[1])+exp(model.full$coefficient[2])))
coef$YoungCIlower <- coef$YoungCIlower / (1+coef$YoungCIlower+coef$Old) / (exp(model.full$coefficient[2])/(1+exp(model.full$coefficient[1])+exp(model.full$coefficient[2])))
coef$YoungCIupper <- coef$YoungCIupper / (1+coef$YoungCIupper+coef$Old) / (exp(model.full$coefficient[2])/(1+exp(model.full$coefficient[1])+exp(model.full$coefficient[2])))
coef$Oldpvalues <- pvalues[,1]
coef$Youngpvalues <- pvalues[,2]
coef$OldCIlower[coef$Oldpvalues>=p] <- 0
coef$YoungCIlower[coef$Youngpvalues>=p] <- 0
coef$OldCIupper[coef$Oldpvalues>=p] <- 0
coef$YoungCIupper[coef$Youngpvalues>=p] <- 0
coef$Oldsig <- 1
coef$Youngsig <- 1
coef$Oldsig[coef$Oldpvalues>=p] <- 0
coef$Youngsig[coef$Youngpvalues>=p] <- 0

#plot 
sp <- ggplot(coef,aes(x=Old,y=Young)) +
geom_errorbar(aes(ymin=YoungCIlower,ymax=YoungCIupper), color="dark grey") + 
geom_errorbarh(aes(xmin=OldCIlower,xmax=OldCIupper), color="dark grey") + 
geom_hline(yintercept=1,linetype="dashed") +
geom_vline(xintercept=1,linetype="dashed") + 
geom_point(aes(color=Origin,shape=factor(Reference)),size=5) +
geom_text(label=rownames(coef),vjust=-1,size=4)

sp + 
scale_x_continuous(name="Multiple of Propbability of Answering \"Old\" relative to \"Everybody\"", trans="log",breaks=seq(0,ceiling(max(coef$OldCIupper)),1),limits=c(-0.1,ceiling(max(coef$OldCIupper)))) + 
scale_y_continuous(name="Multiple of Propbability of Answering \"Young\" relative to \"Everybody\"",trans="log",breaks=seq(0,ceiling(max(coef$YoungCIupper)),1),limits=c(-0.1,ceiling(max(coef$YoungCIupper)))) +
theme_bw()


#interpret participant coefficient
who <- data.frame(t(matrix(summary(model.full)$coefficient[(nStimuli+1)*2 + c(1:(nParticipant*2))],nrow=2)))
rownames(who) <- Participant
colnames(who) <- c("Old","Young")
who$Age <- data$Age[2:(nParticipant+1)]
who$Gender <- data$Gender[2:(nParticipant+1)]
who$Age_group <- data$Age_group[2:(nParticipant+1)]

#plot
ggplot(who,aes(x=sign(Old)*(abs(Old)^(1/2)),y=sign(Young)*(abs(Young))^(1/2))) +
geom_point(aes(color=Age_group,shape=Gender),size=5) +
#geom_text(label=rownames(who),vjust=-1,size=4) +
theme_bw()

# "Joanne", "rhonda", "Cedrina", "GRoy" have very different answers to other people, compare results before and after removing these people

#create dataset for full model without the 4 participants
mlogit_data_out <- mlogit_data
mlogit_data_out <- mlogit_data_out[-which(mlogit_data_out$Participant=="Joanne"),]
mlogit_data_out <- mlogit_data_out[-which(mlogit_data_out$Participant=="rhonda"),]
mlogit_data_out <- mlogit_data_out[-which(mlogit_data_out$Participant=="Cedrina"),]
mlogit_data_out <- mlogit_data_out[-which(mlogit_data_out$Participant=="GRoy"),]
mlogit_data_out$Participant <- factor(mlogit_data_out$Participant)
mlogit_data_out$Participant <- relevel(mlogit_data_out$Participant, ref="cassandra")

#rerun the full model
model.out <- mlogit(Answer ~ 1 | Stimuli + Participant, data = mlogit_data_out) #Intercept is Cassandra's answers to the five control words

#interpret coefficient for each word variant
coef <- t(matrix(model.out$coefficient[2+c(1:(nStimuli*2))],nrow=2))
coef <- data.frame(coef)
rownames(coef) <- Stimuli
colnames(coef) <- c("Old","Young")
coef$Old <- exp(model.out$coefficient[1] + coef$Old)
coef$Young <- exp(model.out$coefficient[2] + coef$Young)
coef$Old <- coef$Old / (1+coef$Old+coef$Young) / (exp(model.out$coefficient[1])/(1+exp(model.out$coefficient[1])+exp(model.out$coefficient[2])))
coef$Young <- coef$Young / (1+coef$Old+coef$Young) / (exp(model.out$coefficient[2])/(1+exp(model.out$coefficient[1])+exp(model.out$coefficient[2])))
Origin <- read.csv("origin.csv")
coef$Origin <- Origin$Origin
coef$Reference <- Origin$Reference
CIlower <- t(matrix(confint(model.out)[,1][2+c(1:(nStimuli*2))],nrow=2))
CIupper <- t(matrix(confint(model.out)[,2][2+c(1:(nStimuli*2))],nrow=2))
coef$OldCIlower <- exp(model.out$coefficient[1] + CIlower[,1]) #2.5%
coef$OldCIupper <- exp(model.out$coefficient[1] + CIupper[,1]) #97.5%
coef$YoungCIlower <- exp(model.out$coefficient[2] + CIlower[,2]) #2.5%
coef$YoungCIupper <- exp(model.out$coefficient[2] + CIupper[,2]) #97.5%
coef$OldCIlower <- coef$OldCIlower / (1+coef$OldCIlower+coef$Young) / (exp(model.out$coefficient[1])/(1+exp(model.out$coefficient[1])+exp(model.out$coefficient[2])))
coef$OldCIupper <- coef$OldCIupper / (1+coef$OldCIupper+coef$Young) / (exp(model.out$coefficient[1])/(1+exp(model.out$coefficient[1])+exp(model.out$coefficient[2])))
coef$YoungCIlower <- coef$YoungCIlower / (1+coef$YoungCIlower+coef$Old) / (exp(model.out$coefficient[2])/(1+exp(model.out$coefficient[1])+exp(model.out$coefficient[2])))
coef$YoungCIupper <- coef$YoungCIupper / (1+coef$YoungCIupper+coef$Old) / (exp(model.out$coefficient[2])/(1+exp(model.out$coefficient[1])+exp(model.out$coefficient[2])))
pvalues <- t(matrix(summary(model.out)$CoefTable[2+c(1:(nStimuli*2)),4],nrow=2))
coef$Oldpvalues <- pvalues[,1]
coef$Youngpvalues <- pvalues[,2]
p <- min(c(pvalues[Stimuli=="jurlaka",],pvalues[Stimuli=="pujikat",],pvalues[Stimuli=="warlaku",],pvalues[Stimuli=="makin",],pvalues[Stimuli=="juwelap",])) #p=0.0019
coef$OldCIlower[coef$Oldpvalues>=p] <- 0
coef$YoungCIlower[coef$Youngpvalues>=p] <- 0
coef$OldCIupper[coef$Oldpvalues>=p] <- 0
coef$YoungCIupper[coef$Youngpvalues>=p] <- 0
coef$Oldsig <- 1
coef$Youngsig <- 1
coef$Oldsig[coef$Oldpvalues>=p] <- 0
coef$Youngsig[coef$Youngpvalues>=p] <- 0

#plot 
sp <- ggplot(coef,aes(x=Old,y=Young)) +
geom_errorbar(aes(ymin=YoungCIlower,ymax=YoungCIupper), color="dark grey") + 
geom_errorbarh(aes(xmin=OldCIlower,xmax=OldCIupper), color="dark grey") + 
geom_hline(yintercept=1,linetype="dashed") +
geom_vline(xintercept=1,linetype="dashed") + 
geom_point(aes(color=Origin,shape=factor(Reference)),size=5) +
geom_text(label=rownames(coef),vjust=-1,size=4)

sp + 
scale_x_continuous(name="Multiple of Propbability of Answering \"Old\" relative to \"Everybody\"", trans="log",breaks=seq(0,ceiling(max(coef$OldCIupper)),1),limits=c(-0.1,ceiling(max(coef$OldCIupper)))) + 
scale_y_continuous(name="Multiple of Propbability of Answering \"Young\" relative to \"Everybody\"",trans="log",breaks=seq(0,ceiling(max(coef$YoungCIupper)),1),limits=c(-0.1,ceiling(max(coef$YoungCIupper)))) +
theme_bw()

#interpret participant coefficient
Participant <- levels(mlogit_data_out$Participant)[-1]
nParticipant <- length(Participant)

#plot
who <- data.frame(t(matrix(summary(model.out)$coefficient[(nStimuli+1)*2 + c(1:(nParticipant*2))],nrow=2)))
rownames(who) <- Participant
colnames(who) <- c("Old","Young")
who$Age <- data$Age[2:(nParticipant+1)]
who$Gender <- data$Gender[2:(nParticipant+1)]
who$Age_group <- data$Age_group[2:(nParticipant+1)]

ggplot(who,aes(x=sign(Old)*(abs(Old)^(1/2)),y=sign(Young)*(abs(Young))^(1/2))) +
geom_point(aes(color=Age_group,shape=Gender),size=5) +
#geom_text(label=rownames(who),vjust=-1,size=4) +
theme_bw()

#create dataset for the control words only without the 4 participants
data <- read_excel("data.xlsx")
data_control <- data[is.element(data$Stimuli, c("jurlaka","pujikat","warlaku","makin","juwelap")),]
mlogit_data_control <- mlogit.data(data_control, shape = "wide", choice = "Answer", idx = list(c("Stimuliid","Participant")))
mlogit_data_control$Origin <- relevel(mlogit_data_control$Origin, ref="K")
mlogit_data_out <- mlogit_data_control
mlogit_data_out <- mlogit_data_out[-which(mlogit_data_out$Participant=="Joanne"),]
mlogit_data_out <- mlogit_data_out[-which(mlogit_data_out$Participant=="rhonda"),]
mlogit_data_out <- mlogit_data_out[-which(mlogit_data_out$Participant=="Cedrina"),]
mlogit_data_out <- mlogit_data_out[-which(mlogit_data_out$Participant=="GRoy"),]
mlogit_data_out$Participant <- factor(mlogit_data_out$Participant)
mlogit_data_out$Participant <- relevel(mlogit_data_out$Participant, ref="cassandra")

#rerun the simpler model on control words
model <- mlogit(Answer ~ 1| Origin/Gender/Age,data = mlogit_data_out)
summary(model)

#create dataset for the test words only without the 4 participants
mlogit_data <- mlogit.data(data, shape = "wide", choice = "Answer", idx = list(c("Stimuliid","Participant")))
for (i in c("jurlaka","pujikat","warlaku","makin","juwelap")) {
	mlogit_data <- mlogit_data[-which(mlogit_data$Stimuli==i),]
}
mlogit_data$Origin <- relevel(mlogit_data$Origin, ref="K")
mlogit_data_out <- mlogit_data
mlogit_data_out <- mlogit_data_out[-which(mlogit_data_out$Participant=="Joanne"),]
mlogit_data_out <- mlogit_data_out[-which(mlogit_data_out$Participant=="rhonda"),]
mlogit_data_out <- mlogit_data_out[-which(mlogit_data_out$Participant=="Cedrina"),]
mlogit_data_out <- mlogit_data_out[-which(mlogit_data_out$Participant=="GRoy"),]
mlogit_data_out$Participant <- factor(mlogit_data_out$Participant)
mlogit_data_out$Participant <- relevel(mlogit_data_out$Participant, ref="cassandra")

#rerun the simpler model on test words
model <- mlogit(Answer ~ 1| Origin/Gender/Age,data=mlogit_data_out)
summary(model)

#plot count data
tmp <- table(data[,c(4,7,8,3)])
Age <- c(rep("G1_Young",3),rep("G2_TEEN",3),rep("G3_MID",3),rep("G4_OLD",3))
Anwser <- rep(c("Everyone","Elder","Youth"),4)
count <- as.numeric(c(tmp[,,2,1])) #En, female
tmp_En <- data.frame(Age,Anwser,count)
en_f <- ggplot(tmp_En,aes(fill=Anwser,y=count,x=Age)) +
geom_bar(position="fill",stat="identity")

count <- as.numeric(c(tmp[,,1,1])) #G, female
tmp_En <- data.frame(Age,Anwser,count)
g_f <- ggplot(tmp_En,aes(fill=Anwser,y=count,x=Age)) +
geom_bar(position="fill",stat="identity")

count <- as.numeric(c(tmp[,,2,2])) #En, male
tmp_En <- data.frame(Age,Anwser,count)
en_m <- ggplot(tmp_En,aes(fill=Anwser,y=count,x=Age)) +
geom_bar(position="fill",stat="identity")

count <- as.numeric(c(tmp[,,1,2])) #G, male
tmp_En <- data.frame(Age,Anwser,count)
g_m <- ggplot(tmp_En,aes(fill=Anwser,y=count,x=Age)) +
geom_bar(position="fill",stat="identity")

library(ggpubr)
ggarrange(en_f,g_f,en_m,g_m,labels=c("A)    Kriol-origin, Female", "B)    Gurindji-origin, Female", "C)    Kriol-origin, Male", "D)    Gurindji-origin, Male"),ncol=2, nrow=2)
