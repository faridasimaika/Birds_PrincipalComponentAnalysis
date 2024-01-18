x<-read.csv("bird.csv",header=TRUE)
x<-x[,-c(1,12)]
sum(is.na(x))
x<-na.omit(x)
x=as.matrix(x); head(x); x=scale(x)
## Checking the correlation between every pair of variables
library(ggcorrplot)
ggcorrplot::ggcorrplot(cor(x))
correlat <- cor(x)
ggcorrplot(correlat,lab =TRUE,title='Correlation Matrix Heatmap',
           outline.col = "white",ggtheme=theme_minimal(),
           colors = c("#0C6291", "#FBFEF9", "#A63446"))
## Classical PCA
pc=princomp(x, cor=T); summary(pc,loadings=T)
plot(pc) # Scree plot;
v=pc$sd^2; sv=sum(v); cs=cumsum(v)/sv; m=length(cs[cs<0.95])
if(m>1) pairs(pc$scores[,1:m],pch=19)
#Outputs:
#pc$scores; pc$sd; pc$loadings
## Robust PCA
library(robustbase)
library(robustX)
b=mvBACON(x)
plot(b$dis,pch=19);abline(h=b$limit);
id=1:length(b$dis);out=id[b$dis>b$limit]
points(id[out],b$dis[out],pch=19,col="red")
d=diag(b$cov)^-0.5; D=diag(d); R=D%*%b$cov%*%D
xBasic=x[-out,]
pcr=princomp(xBasic,cor=T); summary(pcr,loadings=T)
plot(pcr)
