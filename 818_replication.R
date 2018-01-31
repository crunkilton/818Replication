## Cody Crunkilton
## MLE Final Project
## Fall 2017

setwd("C:/Users/Cody/Dropbox/1school17_18/fall semester/MLE/replication/drones")

#libraries
library(sandwich)
library(lmtest)
library(Amelia)
library(bucky)
library(readstata13)
library(logistf)
library(lme4)
library(igraph)
library(stargazer)
library(xtable)
library(reshape2)
library(ggplot2)
library(coefplot)
library(bucky)

## Read in data
origdrones <- read.dta13("UAVIOReplication.dta") # pre-vdem merge
drones <- read.csv("newdrones.csv") # has VDEM attached too. 

####################################################################
####################################################################
########################## Replication #############################
####################################################################
####################################################################

###############################################
###############Table 1 Replication
###############################################

## Original paper used clustered standard errors, which should not matter given there is only one observation per country. I also did that, though it should not be necessary

table1_adv<- glm(advanced~terrdisputes+lnnterr5+polity2+polity2_squared+lngdpcap+defense,family=binomial(link="probit"),data=drones)
robust.summary(table1_adv,cowcc)

table1_armed <- glm(armedprogram~terrdisputes+lnnterr5+polity2+polity2_squared+lngdpcap+defense,family=binomial(link="probit"),data=origdrones)
table1_armed <- robustify(table1_armed,cowcc)
summary(table1_armed)

possession <- glm(armedpossession~terrdisputes+lnnterr5+polity2+polity2_squared+lngdpcap+defense,family=binomial(link="probit"),data=drones)
robust.summary(possession,cowcc)

#### For Latex:
stargazer(table1_armed)

## Coefficient Plot for table 1

coefplot(table1_armed,intercept=F,title="Regression Coefficients: Pursuit of Armed Drones",newNames=c("defense"="Alliance","lngdpcap"="GDP/Capita","polity2_squared"="Polity (squared)","polity2"= "Polity","lnnterr5"= "Terrorism","terrdisputes"= "Territorial Disputes"))

##############################################
############ Regime Type Replications
##############################################

##Democrat-autocrat replication
armed_dem_aut<- glm(armedprogram~terrdisputes+lnnterr5+democrat+autocrat+lngdpcap+defense,family=binomial(link="probit"),data=drones)
robust.summary(armed_dem_aut,cowcc)


##Table A2 Replication
advreg <- glm(advanced~terrdisputes+lnnterr5+gwf_personal+gwf_party+gwf_military+gwf_monarchy+lngdpcap+defense,family=binomial(link="probit"),data=drones)
robust.summary(advreg,cowcc)

armed_regimetype <- glm(armedprogram~terrdisputes+lnnterr5+gwf_personal+gwf_party+gwf_military+gwf_monarchy+lngdpcap+defense,family=binomial(link="probit"),data=drones)
robust.summary(armed_regimetype,cowcc) 

##############################################
############ Replicating figure 3
##############################################

armed<- glm(armedprogram~terrdisputes+lnnterr5+poly(polity2,2)+lngdpcap+defense,family=binomial(link="probit"),data=origdrones)
#armed <- robustify(armed,cowcc)

newdata <- data.frame(terrdisputes=1.75,lnnterr5=2.11,polity2=c(1:21),polity2_squared=c(1:21)^2,lngdpcap=8.566,defense=0,lnhightech=mean(origdrones$lnhightech,na.rm=T)) # set to means

predicted <- predict(armed,newdata,type="response",se.fit=T) #predicted values

plot(predicted$fit,
     main="Democracy and Probability of Pursuit of Armed Drones",
     xlab="Polity Scores",
     ylab="Probability of Pursuing Armed Drones",
     xaxp=c(1,21,1),
     ylim=c(-.2,.8),
     type="n",
     pch=16)# plot
points(predicted$fit,pch=16)
abline(h=0,col="grey") #line at zero
axis(1,seq(1,21,1))
arrows(c(1:21),predicted$fit-1.96*predicted$se.fit,c(1:21),predicted$fit+1.96*predicted$se.fit,length=.0681,angle=90,code=3,col="brown") # add bars

### For Latex
png("drones_dem_rep.png")

####################################################################
####################################################################
########################## Extensions ##############################
####################################################################
####################################################################

######################################################################
######################################################################
########### Extension 1: V-Dem
#####################################################################
######################################################################

## Vdem instead of polity, using each indicator for democracy. I ran this vs model 2 (no high tech exports)

#for reference, here is original model: 
armed<- glm(armedprogram~terrdisputes+lnnterr5+poly(polity2,2,raw=T)+lngdpcap+defense,family=binomial(link="probit"),data=origdrones)
robust.summary(armed,cowcc)

###Polyarchy measure:

armed_polyarchy<- glm(armedprogram~terrdisputes+lnnterr5+poly(v2x_polyarchy,2,raw=T)+lngdpcap+defense,family=binomial(link="probit"),data=drones)

robust.summary(armed_polyarchy,cowcc)
polyarchy_robust <- robustify(armed_polyarchy,cowcc)

#linear term only
armed_polyarchy1<- glm(armedprogram~terrdisputes+lnnterr5+v2x_polyarchy+lngdpcap+defense,family=binomial(link="probit"),data=drones)
robust.summary(armed_polyarchy1,cowcc) #.58

##Accountability
armed_accountability<- glm(armedprogram~terrdisputes+lnnterr5+v2x_accountability+I(v2x_accountability^2)+lngdpcap+defense,family=binomial(link="probit"),data=drones)
accountability_robust <- robustify(armed_accountability,cowcc)

armed_accountability1<- glm(armedprogram~terrdisputes+lnnterr5+v2x_accountability+lngdpcap+defense,family=binomial(link="probit"),data=drones)
robust.summary(armed_accountability1,cowcc) #.85

##liberal democracy
armed_libdem<- glm(armedprogram~terrdisputes+lnnterr5+poly(v2x_libdem,2,raw=TRUE)+lngdpcap+defense,family=binomial(link="probit"),data=drones)

robust.summary(armed_libdem,cowcc)
libdem_robust <- robustify(armed_libdem,cowcc)

#linear
armed_libdem1<- glm(armedprogram~terrdisputes+lnnterr5+v2x_libdem+lngdpcap+defense,family=binomial(link="probit"),data=drones)
robust.summary(armed_libdem1,cowcc) #.558

##liberal
armed_lib<- glm(armedprogram~terrdisputes+lnnterr5+poly(v2x_liberal,2,raw=T)+lngdpcap+defense,family=binomial(link="probit"),data=drones)

robust.summary(armed_lib,cowcc)
lib_robust <- robustify(armed_lib,cowcc)

#linear
armed_lib1<- glm(armedprogram~terrdisputes+lnnterr5+v2x_liberal+lngdpcap+defense,family=binomial(link="probit"),data=drones)
robust.summary(armed_lib1,cowcc) #.86


##participatory democracy
armed_partipdem<- glm(armedprogram~terrdisputes+lnnterr5+poly(v2x_partipdem,2,raw=T)+lngdpcap+defense,family=binomial(link="probit"),data=drones)

robust.summary(armed_partipdem,cowcc)
partipdem_robust <- robustify(armed_partipdem,cowcc)

##delib democracy
armed_delibdem<- glm(armedprogram~terrdisputes+lnnterr5+poly(v2x_delibdem,2,raw=T)+lngdpcap+defense,family=binomial(link="probit"),data=drones)

robust.summary(armed_delibdem,cowcc)
delibdem_robust <- robustify(armed_delibdem,cowcc)

##egalitarian democracy
armed_egal<- glm(armedprogram~terrdisputes+lnnterr5+poly(v2x_egal,2,raw=T)+lngdpcap+defense,family=binomial(link="probit"),data=drones)

robust.summary(armed_egal,cowcc)
egal_robust <- robustify(armed_egal,cowcc)

## Table for poster
vdem_table <- rbind(coef(summary(polyarchy_robust))[c(4,5),4],
                    coef(summary(lib_robust))[c(4,5),4],
                    coef(summary(libdem_robust))[c(4,5),4],
                    coef(summary(partipdem_robust))[c(4,5),4],
                    coef(summary(delibdem_robust))[c(4,5),4],
                    coef(summary(egal_robust))[c(4,5),4],
                    coef(summary(accountability_robust))[c(4,5),4])

vdem_table <- as.data.frame(vdem_table)
vdem_names <- c("Polyarchy","Liberal","Liberal Democracy","Participatory Democracy","Deliberative Democracy","Egalitarian Democracy","Accountability")
vdem_table <- cbind(vdem_names,vdem_table)
vdem_table <- data.frame(vdem_names,round(vdem_table[,c(2,3)],3))
colnames(vdem_table) <- c("Index","p (linear)","p (quadratic)")

# for latex
xtable(vdem_table,caption="Alternative Measurement of Democracy: VDem")

######################################################################
######################################################################
########### Extension 2: Multiple Imputation
#####################################################################
######################################################################

toimpute <- with(origdrones, data.frame(cowcc,statenme,armedprogram,terrdisputes,lnnterr5,polity2,lngdpcap,defense))

missmap(toimpute) # for reference

imputed1 <- amelia(toimpute, m=20,idvars=c("cowcc","statenme"),noms=("armedprogram"),logs=c("lnnterr5", "lngdpcap"),p2s=0) 

imputedreg <- mi.eval(glm(armedprogram~terrdisputes+lnnterr5+poly(polity2,2,raw=T)+lngdpcap+defense,family=binomial(link="probit"),data=imputed1))

## The coefficient plot
coefplot(imputedreg,intercept=F,title="Regression Coefficients: Imputed Data",newNames=c("defense"="Alliance","lngdpcap"="GDP/Capita","poly(polity2, 2, raw = T)2"="Polity (squared)","poly(polity2, 2, raw = T)1"= "Polity","lnnterr5"= "Terrorism","terrdisputes"= "Territorial Disputes"))

# for Latex
stargazer(coef(summary(imputedreg)),title="Model Using Multiple Imputation")

######################################################################
######################################################################
########### Extension 3A: Network Visualization of Results
#####################################################################
######################################################################

# For reference, the data I used above in the imputation section (toimpute is the data I am interested in): 
toimpute <- with(origdrones, data.frame(cowcc,statenme,armedprogram,terrdisputes,lnnterr5,polity2,polity2_squared,lngdpcap,defense))
smaller <- toimpute[complete.cases(toimpute),]
armed<- glm(armedprogram~terrdisputes+lnnterr5+polity2+polity2_squared+lngdpcap+defense,family=binomial(link="probit"),data=smaller)

smaller$predicted <- fitted(armed)

# getting ready for edge list

names <- data.frame(as.character(smaller$statenme),as.character(smaller$statenme),smaller$predicted)

expanded1 <- expand.grid(names[,1],names[,2])
colnames(names) <- c("Country1","Country2","Fit")
colnames(expanded1) <- c("Country1","Country2")

expanded2 <- merge(expanded1,names[,c(1,3)],by="Country1",all.x=T)
colnames(expanded2) <- c("Country1","Country2","Fit1")
expanded3 <- merge(expanded2,names[,c(2,3)],by="Country2",all.x=T)
colnames(expanded3) <- c("Country1","Country2","Fit2","Fit1")

expanded <- subset(expanded3,Country1!=Country2)
expanded[3:4] <- lapply(expanded[3:4],function(x)as.numeric(as.character(x)))
expanded$edge <- ifelse(abs(expanded$Fit1-expanded$Fit2)<=.1,1,0) #normal fit
expanded$logfit1 <- log(expanded$Fit1)
expanded$logfit2 <- log(expanded$Fit2)
expanded$logedge <- ifelse(abs(expanded$logfit1-expanded$logfit2)<=.1,1,0) #log fit
expanded$logweight <- 1/abs(expanded$logfit1-expanded$logfit2)
expanded$weight <- 1/abs(expanded$Fit1-expanded$Fit2)

forigraph <- expanded[-c(3:9)] 
forigraph_log <- expanded[-c(3:8,10)] 

#making graph object
g <- graph.data.frame(forigraph,directed=F)
g_log <- graph.data.frame(forigraph_log,directed=F)

## adding vertex attributes

ix <- match(V(g)$name, smaller$statenme)
# match(V(g1)$name, smaller$statenme) # testing

V(g)$polity <- smaller$polity2[ix]
V(g)$terr <- smaller$lnnterr5[ix]
V(g)$gdp <- smaller$lngdpcap[ix]

### Same, for logged
ix <- match(V(g_log)$name, smaller$statenme)
# match(V(g1)$name, smaller$statenme) # testing

V(g_log)$polity <- smaller$polity2[ix]
V(g_log)$terr <- smaller$lnnterr5[ix]
V(g_log)$gdp <- smaller$lngdpcap[ix]

##########################
############# Colors for Democracies
#########################

a <- colorRampPalette(c("red","blue","yellow"))(21)


for(i in 1:21){
  V(g)[polity == i]$color <- a[i]
  
}

for(i in 1:21){
  V(g_log)[polity == i]$color <- a[i]
  
}

## Removing edge weights less than X
# log
e_log <- E(g_log)[E(g_log)$logweight<2]
g_log <- g_log-e_log

## for normal weight
# normal
e <- E(g)[E(g)$weight<10]
g <- g-e

#############################
########### Graph to submit
#############################

graphics.off()
####### Log one for paper
#########
dev.new()

set.seed(5812)
plot(g_log,vertex.size=6,vertex.label=NA) 
legend("topleft",c("Strong Democracy","Mixed Regime","Strong Autocracy"),fill=c("Yellow","Blue","Red"),cex=1.5,bty="n")
title("Network Visualization: Log Scale",cex.main=2)

jpeg("nw_log1.jpeg")

####### Linear Model
## This one for the paper
# cut edges less than 10
set.seed(5812)
plot(g,vertex.size=6,edge.width=.000000001*E(g)$weight,layout=layout.kamada.kawai,vertex.label=NA) 
title("Network Visualization of Results",cex.main=2)

legend("topleft",c("Strong Democracy","Mixed Regime","Strong Autocracy"),fill=c("Yellow","Blue","Red"),cex=1.3,bty="n")

## for paper
ggsave("nw_1.PNG") 

## This one for the poster
set.seed(5892)
plot(g,vertex.size=6,edge.width=.000000001*E(g)$weight,layout=layout.kamada.kawai,main="Networked Visualization of Results",cex=2,vertex.label.cex=.5,vertex.label.color="black",edge.color="lightgrey") 
legend("topleft",c("Strong Democracy","Mixed Regime","Strong Autocracy"),fill=c("Yellow","Blue","Red"),cex=.8,bty="n")

######################################################################
######################################################################
########### Extension 3B: Correlation Matrix
#####################################################################
######################################################################


## Getting data ready
new <- with(origdrones, data.frame(statenme,as.numeric(armedprogramC),as.numeric(armedprogramB),as.numeric(tactical),as.numeric(advanced),as.numeric(armedprogram),as.numeric(armedpossession),as.numeric(armedtactical)))
mat <- cor(t(new[2:8]))
smallmat <-cor(t(new[4:8])) 

## NAs to 0
mat <- ifelse(is.na(mat),0,mat)
smallmat <- ifelse(is.na(smallmat),0,mat)

## Negatives to 0
mat <- ifelse(mat<0,0,mat)
smallmat <- ifelse(smallmat<0,0,smallmat)

##making the igraph object. 
corgraph <- graph_from_adjacency_matrix(mat,mode="undirected",weighted=T)
smallcorgraph <- graph_from_adjacency_matrix(smallmat,mode="undirected",weighted=T)
names2 <- origdrones$statenme
V(corgraph)$name <- names2
V(smallcorgraph)$name <- names2

ix_2 <- match(V(corgraph)$name, origdrones$statenme)
# match(V(g1)$name, smaller$statenme) # testing
V(corgraph)$polity <- origdrones$polity2[ix_2]
V(corgraph)$terr <- origdrones$lnnterr5[ix_2]
V(corgraph)$gdp <- origdrones$lngdpcap[ix_2]

# removing self correlations
corgraph <- simplify(corgraph)
smallcorgraph <- simplify(smallcorgraph)

# Isolates
corgraph <- induced.subgraph(corgraph,
                             vids=which(igraph::degree(corgraph)>1))#get rid of isolates

smallcorgraph <- induced.subgraph(smallcorgraph,
                                  vids=which(igraph::degree(smallcorgraph)>.1))#get rid of

# community detection
walktrap <- cluster_walktrap(corgraph,weights = E(corgraph)$weight)
fast_greedy <- cluster_fast_greedy(corgraph)
infomap <- cluster_infomap(corgraph)
louvain <- cluster_louvain(corgraph)
spin <- cluster_spinglass(corgraph,weights=E(corgraph)$weight)
louvain_small <- cluster_louvain(smallcorgraph)

##Size based on strength of edge
V(corgraph)$size <- graph.strength(corgraph,weights=E(corgraph)$weight)/5


#############################
########### Graph to submit
#############################
graphics.off()

dev.new()
png("corgraph.png",res=500,width=9000,height=10000)
colors <- c("aquamarine","chartreuse","yellow")
set.seed(5892)
plot(corgraph,vertex.label.cex=.55,vertex.color=colors[louvain$membership],vertex.size=V(corgraph)$size,vertex.label.color="black",edge.width=E(corgraph)$weight) #GRAPH 1 - BY GROUPS
title("Similarity of Drone Programs: Louvain Clustering",cex=3)
dev.off()

################### Heatmap for paper
mat[is.na(mat)] <- 0
heatmap(mat)
title("Heatmap of Drone Program Correlations")

## heatmap w/ ggplot
melted_cormat <- melt(mat)
cor_nozero <- melted_cormat[melted_cormat$value>0,]
ggplot(melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile()


##################################
################################
######### Small Extensions
################################
##################################

# logit not probit
table1_logit<- glm(advanced~terrdisputes+lnnterr5+polity2+polity2_squared+lngdpcap+defense,family=binomial(link="logit"),data=drones)
robust.summary(table1_logit,cowcc) # close enough

# firth logit not probit
firth <- logistf(advanced~terrdisputes+lnnterr5+polity2+polity2_squared+lngdpcap+defense,data=drones)
summary(firth)

###########
######## unarmed drones by regime type
###########

origdrones$unarmed <- NULL
origdrones$unarmed <- ifelse((origdrones$tactical==1)&origdrones$armedprogram==0,1,0) #making the variable

## The regression:
unarmedreg <- glm(unarmed~terrdisputes+lnnterr5+poly(polity2,2,raw=T)+lngdpcap+defense,family=binomial(link="probit"),data=origdrones)
robustified <- robustify(unarmedreg,cowcc)
summary(robustified)

## results from robustified model
unarmed_robust_toprint <- signif(summary(robustified)$coefficients[,c(1,2,4)],2)
stargazer(unarmed_robust_toprint)

####### For Latex
library(stargazer)
stargazer(robust.summary(unarmedreg,cowcc))

coefplot(robustified,intercept=F,title="Regression Coefficients: Unarmed Drones",newNames=c("defense"="Alliance","lngdpcap"="GDP/Capita","poly(polity2, 2, raw = T)2"="Polity (squared)","poly(polity2, 2, raw = T)1"= "Polity","lnnterr5"= "Terrorism","terrdisputes"= "Territorial Disputes"))