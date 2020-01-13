# conjoint data entry and simulation 
attrib <- list(seat = c("6", "7", "8"),
               cargo = c("2ft", "3ft"),
               eng = c("gas", "hyb", "elec"),
               price = c("30", "35", "40"))



# create names for the coefficients , dropping the first level attribute
# concatenating the attribute and the level designation
coef.names <- NULL
for (a in seq_along(attrib)){
  coef.names <- c(coef.names,
                  paste(names(attrib)[a],attrib[[a]][-1], sep=""))
}
coef.names

# simulated data assume the average part worths
mu <- c(-1, -1, 0.5, -1, -2, -1, -2)
names(mu) <- coef.names
mu

# own unique part worth coefficients follow multivariate normal distribution
Sigma <- diag(c(0.3,1,0.1,0.3,1,0.2,0.3))
dimnames(Sigma) <- list(coef.names, coef.names)
Sigma["enghyb", "engelec"] <- Sigma["enghyb", "engelec"] <- 0.3

# each respondent’s part worth coefficients
set.seed(33040)
resp.id <- 1:200 # respondent ids
carpool <- sample(c("yes","no"), size = length(resp.id), replace = TRUE, prob=c(0.3,0.7))
library(MASS)
coefs <-mvrnorm(length(resp.id), mu=mu, Sigma = Sigma)
colnames(coefs) <- coef.names

# the part worths for respondents who use the minivan
coefs[carpool=="yes","seat8"] <- coefs[carpool=="yes","seat8"]+2
coefs[carpool=="yes","seat7"] <- coefs[carpool=="yes","seat7"]+1.5
head(cbind(carpool, coefs))

# ready to generate our survey questions
nques <- 15
nalt <- 3
profiles <- expand.grid(attrib)    # 3*2*3*3 = 54
nrow(profiles)
head(profiles)

# convert profiles to dummy coding
profiles.coded <- model.matrix(~ seat+ cargo+ eng+ price,data = profiles)[,-1]
head(profiles.coded)

# each respondent’s expected utility
cbc1.df <- data.frame(NULL)
for (i in seq_along(resp.id)) {
  profiles.i <- sample(1:nrow(profiles), size=nques*nalt)
  utility <- profiles.coded[profiles.i, ] %*% coefs[i, ]
  wide.util <- matrix(data=utility, ncol=nalt, byrow=TRUE)
  probs <- exp(wide.util) / rowSums(exp(wide.util))
  choice <- apply(probs, 1, function(x) sample(1:nalt, size=1, prob=x))
  choice <- rep(choice, each=nalt)==rep(1:nalt, nques)
  conjoint.i <- data.frame(resp.id=rep(i, nques),
                             ques = rep(1:nques, each=nalt),
                             alt = rep(1:nalt, nques),
                             carpool = rep(carpool[i], nques),
                             profiles[profiles.i, ],
                             choice = as.numeric(choice))
  cbc1.df <- rbind(cbc.df, conjoint.i)
}

# Tidy up, keeping only cbc.df and attrib
rm(a, i, resp.id, carpool, mu, Sigma, coefs, coef.names,
      conjoint.i, profiles, profiles.i, profiles.coded, utility,
     wide.util, probs, choice, nalt, nques)
}
