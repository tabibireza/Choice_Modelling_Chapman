library(dplyr)


# survey based 
cbc.df <- read.csv("http://goo.gl/5xQObB", colClasses = c(seat = "factor", price = "factor"))
summary(cbc.df)

# Inspecting Choice Data
xtabs(choice ~ price, data = cbc.df)
xtabs(choice ~ cargo  , data = cbc.df)


# fitting the model - without intercept 
library(mlogit)
cbc.mlogit <- mlogit.data(data=cbc.df, choice="choice", shape="long",
                          varying=3:6, alt.levels=paste("pos",1:3),
                           id.var="resp.id")
m1 <- mlogit(choice ~ 0 + seat + cargo + eng + price, data = cbc.mlogit)
summary(m1)

# test with intercept effect of position
m2 <- mlogit(choice ~ seat + cargo + eng + price, data = cbc.mlogit)
summary(m2)

lrtest(m1,m2)   # 0.71 > 0.05 both model are equals 

# have price as a single parameter 
m3 <- mlogit(choice ~ 0 + seat + cargo + eng + as.numeric(as.character(price)),
             data = cbc.mlogit)
summary(m3)


# interpreting the result 
# WTP
coef(m3)["cargo3ft"]/(-coef(m3)["as.numeric(as.character(price))"]/1000)

# Predicting shares
predict.mnl <- function(model, data) {
# Function for predicting shares from a multinomial logit model
# model: mlogit object returned by mlogit()
# data: a data frame containing the set of designs for which you want to
# predict shares. Same format as the data used to estimate model.
data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
utility <- data.model %*% model$coef
share <- exp(utility)/sum(exp(utility))
cbind(share, data)
}


attrib <- list(seat = c("6", "7", "8"),
               cargo = c("2ft", "3ft"),
               eng = c("gas", "hyb", "elec"),
               price = c("30", "35", "40"))

# creat designs 
new.data <- expand.grid(attrib)[c(8, 1, 3, 41, 49, 26), ]

# choose
predict.mnl(m3, new.data)


#Sensitivity Plots
sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
  # Function for creating data for a share-sensitivity chart
  # model: mlogit object returned by mlogit() function
  # attrib: list of vectors with attribute levels to be used in sensitivity
  # base.data: data frame containing baseline design of target product
  
data <- rbind(base.data, competitor.data)
base.share <- predict.mnl(model, data)[1,1]
share <- NULL
for (a in seq_along(attrib)) {
 for (i in attrib[[a]]) {
      data[1,] <- base.data
   data[1,a] <- i
   share <- c(share, predict.mnl(model, data)[1,1])
   }
  }

data.frame(level=unlist(attrib), share=share, increase=share-base.share)
}# competitor.data: data frame containing design of competitive set

base.data <- expand.grid(attrib)[c(8), ]
competitor.data <- expand.grid(attrib)[c(1, 3, 41, 49, 26), ]
tradeoff <- sensitivity.mnl(m1, attrib, base.data, competitor.data)


barplot(tradeoff$increase, horiz=FALSE, names.arg=tradeoff$level,
         ylab="Change in Share for Baseline Product")


# Planning the Sample Size for a Conjoint Study
library(mlogit)
small.cbc <- mlogit.data(data=cbc.df[1:(25*15*3),],choice="choice", shape="long",
                         varying=3:6, alt.levels=paste("pos", 1:3),
                         id.var="resp.id")

m4 <- mlogit(choice ~ 0 + seat + cargo + eng + price, data = small.cbc)
cbind(predict.mnl(m4,new.data), predict.mnl(m1,new.data))


# Adding Consumer Heterogeneity to Choice Models(383)

m1.rpar <- rep("n", length=length(m1$coef)) 
names(m1.rpar) <- names(m1$coef)
m1.rpar
 
m1.hier <- mlogit(choice ~ 0 + seat + eng + cargo + price,
                   data = cbc.mlogit,
                   panel=TRUE, rpar = m1.rpar, correlation = FALSE)
summary(m1.hier)

m2.hier <- update(m1.hier, correlation = TRUE)
summary(m2.hier)
cov2cor(cov.mlogit(m2.hier)) 


# Share Prediction for Heterogeneous Choice Models

predict.hier.mnl <- function(model, data, nresp=1000) {
   # Function for predicting shares of a hierarchical multinomial logit model
   # model: mlogit object returned by mlogit()
   # data: a data frame containing the set of designs for which you want to
   # predict shares. Same format at the data used to estimate model.
   # Note that this code assumes all model parameters are random
   data.model <- model.matrix(updacte(model$formula, 0 ~ .), data = data)[,-1]
   coef.Sigma <- cov.mlogit(model)
   coef.mu <- model$coef[1:dim(coef.Sigma)[1]]
   draws <- MASS::mvrnorm(n=nresp, coef.mu, coef.Sigma)
   shares <- matrix(NA, nrow=nresp, ncol=nrow(data))
   for (i in 1:nresp) {
     utility <- data.model%*%draws[i,]
     share = exp(utility)/sum(exp(utility))
     shares[i,] <- share
   }
  cbind(colMeans(shares), data)
}

predict.hier.mnl(m2.hier, data=new.data)
