require(lpSolve)


# Read in budget simulator data

budsim <- read_excel("budsim.xlsx")
budsim<-budsim%>%mutate(revperconv = `Conv Value`/Conversions,projectedspend= `Current Daily Budget`*16)



maxprofit<-list()
#### Objective fn : CM = Rev/Conv * Conv - CPC* Clicks ##

 for (i in 1:nrow(budsim))
 {
 objective.in=c(budsim$revperconv[i],-budsim$CPC[i])
 const.mat=matrix(c(1,-budsim$CVR[i],0,1),nrow = 2,byrow = T)

 const1 =0
 const2=budsim$Clicks[i]
 const.rhs=c(const1,const2)

 const.dir=c("<=","<=")

 opt=lp(direction = "max",objective.in,const.mat1,const.dir,const.rhs)
 opt$solution
 maxprofit[i]<-opt$objval
 }
 
y<-maxprofit%>%map_df(as_tibble)

### Predicting CM based on projected spend #####

ggplot(data=budsim,aes(x=projectedspend,y=CM))+ 
  geom_point()+geom_line() + xlim(0,39000) + ylim(0,15000)

ggplot(data=model_data,aes(x=inv_spend,y=CM))+ 
  geom_point()+geom_line() 

model_data<-budsim%>%select(projectedspend,CM) 

# Polynomial Regression # CM = intercept + spend + spend ^2
model<-lm(CM~poly(projectedspend,2,raw=TRUE),data=model_data)
model_data$predictions<-predict(model,model_data)

#  Cubic Spline
knots <- quantile(model_data$projectedspend, p = c(0.25, 0.5, 0.75))
model1<-lm(CM~bs(projectedspend,knots=knots),data=model_data)
summary(model1)

knots <- quantile(model_data$projectedspend, p = c(0.25, 0.75))
model2<-lm(CM~bs(projectedspend,knots=knots),data=model_data)
summary(model2)

# Check predictions
model_data$predictions<-predict(model1,model_data)

############ Optimize


get_return <- function(x) {
  spend <- x
  cm_return <- predict(model,data.frame(projectedspend=spend))
  cm_return
}

objective_function <- function(params) { 
  x <- params[1]
  if(x <= 40000) { 
    return(-get_return(x))
  } else {
    return(0)
  }
}

#Initial parameter values
params<-c(1)
get_return(x)
objective_function(params)

#Optimize Spend

optimx(params, fn=objective_function)




# Case 1 : Assuming Usual Spend
objective.in=c(196.01,-11.22)

const.mat=matrix(c(1,-.1033,0,1),nrow = 2,byrow = T)

const1 = 0
# const2= 1714 *17500/10.21 (budget/CPC)
const2= 1556
const3 = 161
const.rhs=c(const1,const2,const3)
const.dir=c("<=","<=",">=")

opt=lp(direction = "max",objective.in,const.mat,const.dir,const.rhs)

maxprofit<-opt$objval
maxprofit<-currency(maxprofit)
opt$solution


objective.in=c(200.29,-10.21)

const.mat=matrix(c(1,-.099,0,1,1,0),nrow = 3,byrow = T)

const1 = 0
const2= 979
const3 = 50
const.rhs=c(const1,const2,const3)
const.dir=c("<=","<=","<=")

opt=lp(direction = "max",objective.in,const.mat,const.dir,const.rhs)

maxprofit<-opt$objval
maxprofit<-currency(maxprofit)