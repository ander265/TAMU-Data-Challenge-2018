rm(list = ls())
library(DPpackage)
library(MASS)

# Data import
library(readr)
taxi_real_data <- read_delim("C:/Users/seyoonlee/OneDrive - stat.tamu.edu/TAMU Study/Projects/taxi_project/training_data(final).txt", 
                             "\t", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)

# No missing data

data = as.matrix(taxi_real_data)

# Function
norm_vec <- function(x,y) {
  norm_vector = sqrt(sum((x-y)^2))
  
  if (norm_vector == Inf){
    norm_vector = 1e+308
  } else {
    norm_vector = norm_vector
  } 
  
  return(norm_vector)
  
}



# Data structure
T.weeks = dim(data)[2] # Total weeks for training data
I = dim(data)[1]
X = matrix(c(rep(1,T.weeks),c(1:T.weeks)) ,nrow = T.weeks, ncol = 2, byrow = F)


# Response: Y  = rate of average taxi revinue of the i-th side at the t-th week
# i = 1,2, ..., 9 (There are 9 sides)
# t = 1,2, ..., T.weeks (There are 4*52 weeks during 2013 ~ 2016)
# T.weeks = 210

one.vector = rep(1,I) # 9 dim 1 vector

# Y.vector.i = rate of average taxi revinue of the i-th side

Y.vector.i = function(i){
  G = as.numeric(data[i,])
  return(G)
}

plot(Y.vector.i(1),type = "l", ylim = c(0,570),ylab = "taxi fare per week", xlab = "weeks")
for (i in 2:I){
  lines(Y.vector.i(i),type = "l", col = i)
}
legend("topleft",legend = c("Central","FarNorth","FarSouthwest","North"
                            , "Northwest","South", "Southeast", "Southwest", "West")
                          ,col = c(1,2,3,4,5,6,7,8,9),lty = rep(1,9))
title("Taxi fares of nine sides from 2013 to 2016 (training test set)")
#Central	1
#FarNorth	2
#FarSouthwest	3
#North	4
#Northwest	5
#South	6
#Southeast	7
#Southwest	8
#West	9


# Make a room
S = 11000 # no of simulation
gamma.vec = matrix(rep(0,2*I*S), nrow = 2*I) # 2 dim vector to store intercept and slope for each sides
sigma.sq.c = matrix(rep(0,1*S), nrow = 1) # 1 dim vector
sigma.sq.alpha = matrix(rep(0,1*S), nrow = 1) # 1 dim vector
sigma.sq.beta = matrix(rep(0,1*S), nrow = 1) # 1 dim vector
alpha.c = matrix(rep(0,1*S), nrow = 1) # 1 dim vector
beta.c = matrix(rep(0,1*S), nrow = 1) # 1 dim vector
rita.c = matrix(rep(0,1*S), nrow = 1) # 1 dim vector
rita.alpha = matrix(rep(0,1*S), nrow = 1) # 1 dim vector
rita.beta = matrix(rep(0,1*S), nrow = 1) # 1 dim vector


# Initial Values
gamma.vec[,1] = matrix(seq(1,2*I, by =1), nrow = 2*I) # 2 dim vector to store intercept and slope for each sides
sigma.sq.c[1] = matrix(1, nrow = 1) # 1 dim vector
sigma.sq.alpha[1] = matrix(1, nrow = 1) # 1 dim vector
sigma.sq.beta[1] = matrix(1, nrow = 1) # 1 dim vector
alpha.c[1] = matrix(1, nrow = 1) # 1 dim vector
beta.c[1] = matrix(1, nrow = 1) # 1 dim vector
rita.c[1] = matrix(1, nrow = 1) # 1 dim vector
rita.alpha[1] = matrix(1, nrow = 1) # 1 dim vector
rita.beta[1] = matrix(1, nrow = 1) # 1 dim vector

# Gibbs sampler Algorithm
set.seed(1)
for (s in 1:(S-1)){
  
  # 2 - 1
  rita.c[s+1] = 1/rgamma(n=1, shape = 1, rate = 1+1/sigma.sq.c[s])
  
  # 2 - 2
  rita.alpha[s+1] = 1/rgamma(n=1, shape = 1, rate = 1+1/sigma.sq.alpha[s])
  
  # 2 - 3
  rita.beta[s+1] = 1/rgamma(n=1, shape = 1, rate = 1+1/sigma.sq.beta[s])
  
  # 2 - 4
  
  temp.vec = c()
  
  for (i in 1:I){
    
    g = 2*(i-1)+1
    
    temp.vec[i] = norm_vec(Y.vector.i(i), X%*%gamma.vec[g:(g+1),s])^2
    
  }
  
  sigma.sq.c[s+1] = 1/rgamma(n=1, shape = (I*T.weeks + 1)/2, rate = (1/2)*sum(temp.vec) + 1/rita.c[s+1])
  
  # 2 - 5
  
  alpha.vec = gamma.vec[seq(1,(2*I-1),by = 2),s]
  sigma.sq.alpha[s+1] = 1/rgamma(n=1, shape = (I + 1)/2, rate = (1/2)*norm_vec(alpha.vec, alpha.c[s]*one.vector)^2 + 1/rita.alpha[s+1] )
  
  # 2 - 6
  
  beta.vec = gamma.vec[seq(2,(2*I),by = 2),s]
  sigma.sq.beta[s+1] = 1/rgamma(n=1, shape = (I + 1)/2, rate = (1/2)*norm_vec(beta.vec, beta.c[s]*one.vector)^2 + 1/rita.beta[s+1] )
  
  # 2 - 7
  
  alpha.vec = gamma.vec[seq(1,(2*I-1),by = 2),s]
  alpha.c[s+1] = rnorm(n = 1, mean = mean(alpha.vec), sd = sqrt((sigma.sq.alpha[s+1])/I ) )
  
  # 2 - 8
  beta.vec = gamma.vec[seq(2,(2*I),by = 2),s]
  beta.c[s+1] = rnorm(n = 1, mean = mean(beta.vec), sd = sqrt((sigma.sq.beta[s+1])/I) )
  
  # 2 - 9
  
  H = (1/sigma.sq.c[s+1])*t(X)%*%X + diag(c(1/sigma.sq.alpha[s+1], 1/sigma.sq.beta[s+1]))
  Sig.matrix.gamma =solve(H) 
  
  for (i in 1:I){
    
    g = 2*(i-1)+1
    
    gamma.vec[g:(g+1),s+1] = mvrnorm(n = 1, 
                                     mu = Sig.matrix.gamma%*%((1/sigma.sq.c[s+1])*t(X)%*%Y.vector.i(i) + c(alpha.c[s+1]/sigma.sq.alpha[s+1], beta.c[s+1]/sigma.sq.beta[s+1]))
                                     , Sigma = Sig.matrix.gamma)
    
  }
  
  print(s)
  
}





#### Diaginose ####


burn = 1000
thin = 10


plot(as.numeric(gamma.vec[1,seq(burn,S,by = thin)]), type = "l")
acf(gamma.vec[i,seq(burn,s,by = thin)])


# Plug-in Plots
zic.sun = function(x, a, b) a + b*x

i = 1
g = 2*(i-1) + 1

a = mean(gamma.vec[g, seq(burn,S,by = thin)]) # Intercept
b = mean(gamma.vec[g+1,seq(burn,S,by = thin)]) # Slope
par(mfrow = c(1,1))
curve(zic.sun(x,a = a, b = b), xlim = c(0, max(X)), ylim = c(0, 570), ylab = "taxi fare per week", xlab = "weeks")
lines(c(X[,2]), Y.vector.i(i), lty = 2, type = "l")

for (i in 2:I){
  
  g = 2*(i-1) + 1
  
  a = mean(gamma.vec[g, seq(burn,S,by = thin)]) # Intercept
  b = mean(gamma.vec[g+1,seq(burn,S,by = thin)]) # Slope
  curve(zic.sun(x,a = a, b = b),add = T, col = i)
  lines(c(X[,2]), Y.vector.i(i), lty = 2, type = "l", col = i)
  
}

over.a = mean(alpha.c[seq(burn,S,by = thin)])
over.b = mean(beta.c[seq(burn,S,by = thin)])

curve(zic.sun(x,a = over.a, b = over.b), xlim = c(0, 210), add = T, col = "red", lwd = 5)
# curve(zic.sun(x,a = a, b = b), xlim = c(-15, 15), add = T, col = "red", lwd = 5)

title("Posterior Inference For The Nine Sides and Chicago")

legend("topleft",legend = c("Central","FarNorth","FarSouthwest","North"
                             , "Northwest","South", "Southeast", "Southwest", "West","Chicago")
       ,col = c(1,2,3,4,5,6,7,8,9,"red"),lty = rep(1,10)
       ,lwd = c(rep(1,9),5))















## Posterior inference ##



taxi.posterior.plot = function(Credible.interval = T){
  
  post.gamma.vec = as.matrix(gamma.vec[, seq(burn,S,by = thin)])
  post.alpha.c = as.matrix(alpha.c[seq(burn,S,by = thin)])
  post.beta.c = as.matrix(beta.c[seq(burn,S,by = thin)])
 
  if (Credible.interval == T){
   for (i in 1:I){
     
     if (i == 1){
       g = 2*(i-1) + 1
       temp.post.mean = c()
       temp.post.upper95 = c()
       temp.post.lower05 = c()
       
       for (t in 1:T.weeks){
         
         temp.sample = c()  
         
         for (s in 1:dim(post.gamma.vec)[2]){
           
           temp.sample[s] = post.gamma.vec[g,s] + post.gamma.vec[(g+1),s]*t
           
         }  
         
         temp.post.mean[t] = mean(temp.sample)
         temp.post.upper95[t] = quantile(temp.sample,probs = 0.95)
         temp.post.lower05[t] = quantile(temp.sample,probs = 0.05)
         
       }
       
       plot(temp.post.mean, type = "l",xlim = c(0, max(X)), ylim = c(0, 570), ylab = "taxi fare per week", xlab = "weeks")
       lines(temp.post.upper95, type = "l",xlim = c(0, max(X)), ylim = c(0, 570), col = "green")
       lines(temp.post.lower05, type = "l",xlim = c(0, max(X)), ylim = c(0, 570), col = "green")
       lines(c(X[,2]), Y.vector.i(i), lty = 2, type = "l")
     } else {
       g = 2*(i-1) + 1
       temp.post.mean = c()
       temp.post.upper95 = c()
       temp.post.lower05 = c()
       
       for (t in 1:T.weeks){
         
         temp.sample = c()  
         
         for (s in 1:dim(post.gamma.vec)[2]){
           
           temp.sample[s] = post.gamma.vec[g,s] + post.gamma.vec[(g+1),s]*t
           
         }  
         
         temp.post.mean[t] = mean(temp.sample)
         temp.post.upper95[t] = quantile(temp.sample,probs = 0.95)
         temp.post.lower05[t] = quantile(temp.sample,probs = 0.05)
         
       }
       
       lines(temp.post.mean, type = "l",xlim = c(0, max(X)), ylim = c(0, 570), col = i)
       lines(temp.post.upper95, type = "l",xlim = c(0, max(X)), ylim = c(0, 570), col = "green")
       lines(temp.post.lower05, type = "l",xlim = c(0, max(X)), ylim = c(0, 570), col = "green")
       lines(c(X[,2]), Y.vector.i(i), lty = 2, type = "l", col = i)
     }
     
   } 
 }else {
   for (i in 1:I){
     
     if (i == 1){
       g = 2*(i-1) + 1
       temp.post.mean = c()
       temp.post.upper95 = c()
       temp.post.lower05 = c()
       
       for (t in 1:T.weeks){
         
         temp.sample = c()  
         
         for (s in 1:dim(post.gamma.vec)[2]){
           
           temp.sample[s] = post.gamma.vec[g,s] + post.gamma.vec[(g+1),s]*t
           
         }  
         
         temp.post.mean[t] = mean(temp.sample)
         
         
       }
       
       plot(temp.post.mean, type = "l",xlim = c(0, max(X)), ylim = c(0, 570), ylab = "taxi fare per week", xlab = "weeks")
       lines(temp.post.upper95, type = "l",xlim = c(0, max(X)), ylim = c(0, 570), col = "green")
       lines(temp.post.lower05, type = "l",xlim = c(0, max(X)), ylim = c(0, 570), col = "green")
       lines(c(X[,2]), Y.vector.i(i), lty = 2, type = "l")
     } else {
       g = 2*(i-1) + 1
       temp.post.mean = c()
       temp.post.upper95 = c()
       temp.post.lower05 = c()
       
       for (t in 1:T.weeks){
         
         temp.sample = c()  
         
         for (s in 1:dim(post.gamma.vec)[2]){
           
           temp.sample[s] = post.gamma.vec[g,s] + post.gamma.vec[(g+1),s]*t
           
         }  
         
         temp.post.mean[t] = mean(temp.sample)
         temp.post.upper95[t] = quantile(temp.sample,probs = 0.95)
         temp.post.lower05[t] = quantile(temp.sample,probs = 0.05)
         
       }
       
       lines(temp.post.mean, type = "l",xlim = c(0, max(X)), ylim = c(0, 570), col = i)
       
       lines(c(X[,2]), Y.vector.i(i), lty = 2, type = "l", col = i)
     }
     
   }
 }
  
  #### Chicago Overall
  
  temp.post.mean = c()
  temp.post.upper95 = c()
  temp.post.lower05 = c()
  
  for (t in 1:T.weeks){
    
    temp.sample = c()  
    
    for (s in 1:dim(post.alpha.c)[2]){
      
      temp.sample[s] = post.alpha.c[s] + post.beta.c[s]*t
      
    }  
    
    temp.post.mean[t] = mean(temp.sample)
    temp.post.upper95[t] = quantile(temp.sample,probs = 0.95)
    temp.post.lower05[t] = quantile(temp.sample,probs = 0.05)
    
  }
  
  lines(temp.post.mean, type = "l",xlim = c(0, max(X)), ylim = c(0, 570), col = "red", lwd = 5)
  
  
  title("Posterior Inference For The Nine Sides and Chicago")
  
  legend("topleft",legend = c("Central","FarNorth","FarSouthwest","North"
                               , "Northwest","South", "Southeast", "Southwest", "West","Chicago")
         ,col = c(1,2,3,4,5,6,7,8,9,"red"),lty = rep(1,10)
         ,lwd = c(rep(1,9),5))

}

taxi.posterior.plot(Credible.interval = T)
taxi.posterior.plot(Credible.interval = F)




########################################################################################################
########################################################################################################


par(mfrow = c(2,3))
plot(c(alpha.c), type = "l", main = "traceplot of alpha.c")
plot(c(gamma.vec[1,]), type = "l", main = "traceplot of alpha.i=1")
plot(c(gamma.vec[3,]), type = "l", main = "traceplot of alpha.i=2")

plot(c(beta.c), type = "l",  main = "traceplot of beta.c")
plot(c(gamma.vec[2,]), type = "l", main  = "traceplot of beta.i=1")
plot(c(gamma.vec[4,]), type = "l", main  = "traceplot of beta.i=2")

acf(c(alpha.c))
acf(c(gamma.vec[1,]))
acf(c(gamma.vec[3,]))

acf(c(beta.c))
acf(c(gamma.vec[2,]))
acf(c(gamma.vec[4,]))


#################### Testing Data ########################
par(mfrow= c(1,1))
library(readr)
test_data <- read_delim("C:/Users/seyoonlee/OneDrive - stat.tamu.edu/TAMU Study/Projects/taxi_project/test_data.txt", 
                        "\t", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)
test_data = as.matrix(test_data)

full_data = cbind(data,test_data) # Training data + test data



Y.vector.i = function(i){
  G = as.numeric(full_data[i,])
  return(G)
}


plot(Y.vector.i(1),type = "l", ylim = c(0,570), ylab = "taxi fare per week", xlab = "weeks")
x = c(210:271, 271:210)
y = c(rep(-100,62),rep(700,62))
polygon(x, y,  col = "grey")
lines(Y.vector.i(1),type = "l", ylim = c(0,570), ylab = "taxi fare per week", xlab = "weeks")
for (i in 2:I){
  lines(Y.vector.i(i),type = "l", col = i)
}

for (i in 2:I){
  lines(Y.vector.i(i),type = "l", col = i)
}

legend("topleft",legend = c("Central","FarNorth","FarSouthwest","North"
                             , "Northwest","South", "Southeast", "Southwest", "West")
       ,col = c(1,2,3,4,5,6,7,8,9),lty = rep(1,9))
title("Taxi fares of nine sides from 2013 to 2017 (training & test set)")
#Central	1
#FarNorth	2
#FarSouthwest	3
#North	4
#Northwest	5
#South	6
#Southeast	7
#Southwest	8
#West	9







###########################################################################
############################## Prediction ################################
###########################################################################
#### We use plug-in predition method (which is not actually Bayesian way)
par(mfrow= c(1,1))
# Plug-in Prediction
zic.sun = function(x, a, b) a + b*x

i = 1
g = 2*(i-1) + 1

a = mean(gamma.vec[g, seq(burn,S,by = thin)]) # Intercept
b = mean(gamma.vec[g+1,seq(burn,S,by = thin)]) # Slope
par(mfrow = c(1,1))
curve(zic.sun(x,a = a, b = b), xlim = c(0, dim(full_data)[2]), ylim = c(0, 570), ylab = "taxi fare per week", xlab = "weeks")
lines(c(1:dim(full_data)[2]), Y.vector.i(i), lty = 2, type = "l")

x = c(210:271, 271:210)
y = c(rep(-100,62),rep(700,62))
polygon(x, y,  col = "grey")

curve(zic.sun(x,a = a, b = b), xlim = c(0, dim(full_data)[2]), ylim = c(0, 570), ylab = "taxi fare per week", xlab = "weeks", add = T)
lines(c(1:dim(full_data)[2]), Y.vector.i(i), lty = 2, type = "l")


for (i in 2:I){
  
  g = 2*(i-1) + 1
  
  a = mean(gamma.vec[g, seq(burn,S,by = thin)]) # Intercept
  b = mean(gamma.vec[g+1,seq(burn,S,by = thin)]) # Slope
  curve(zic.sun(x,a = a, b = b),add = T, col = i)
  lines(c(1:dim(full_data)[2]), Y.vector.i(i), lty = 2, type = "l", col = i)
  
}

over.a = mean(alpha.c[seq(burn,S,by = thin)])
over.b = mean(beta.c[seq(burn,S,by = thin)])

curve(zic.sun(x,a = over.a, b = over.b), xlim = c(0, dim(full_data)[2]), add = T, col = "red", lwd = 5)
# curve(zic.sun(x,a = a, b = b), xlim = c(-15, 15), add = T, col = "red", lwd = 5)

legend("topleft",legend = c("Central","FarNorth","FarSouthwest","North"
                            , "Northwest","South", "Southeast", "Southwest", "West","Chicago")
       ,col = c(1,2,3,4,5,6,7,8,9,"red"),lty = rep(1,10)
       ,lwd = c(rep(1,9),5))

# abline(v = 210, lwd = 2, col = "blue")

title("Prediction For The Nine Sides and Chicago")







################################################################################
################################################################################


## Data Exporation ###
#################### Full Data (Training + Test data) ########################
par(mfrow= c(1,1))
library(readr)
test_data <- read_delim("C:/Users/seyoonlee/OneDrive - stat.tamu.edu/TAMU Study/Projects/taxi_project/test_data.txt", 
                        "\t", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)
test_data = as.matrix(test_data)

full_data = cbind(data,test_data) # Training data + test data



Y.vector.i = function(i){
  G = as.numeric(full_data[i,])
  return(G)
}


plot(Y.vector.i(1),type = "l", ylim = c(0,570), ylab = "taxi fare per week", xlab = "weeks")
x = c(210:271, 271:210)
y = c(rep(-100,62),rep(700,62))
polygon(x, y,  col = "grey")
lines(Y.vector.i(1),type = "l", ylim = c(0,570), ylab = "taxi fare per week", xlab = "weeks")
for (i in 2:I){
  lines(Y.vector.i(i),type = "l", col = i)
}

for (i in 2:I){
  lines(Y.vector.i(i),type = "l", col = i)
}

legend("topleft",legend = c("Central","FarNorth","FarSouthwest","North"
                            , "Northwest","South", "Southeast", "Southwest", "West")
       ,col = c(1,2,3,4,5,6,7,8,9),lty = rep(1,9))
title("Taxi fares of nine sides from 2013 to 2017 (training & test set)")
#Central	1
#FarNorth	2
#FarSouthwest	3
#North	4
#Northwest	5
#South	6
#Southeast	7
#Southwest	8
#West	9


install.packages("ggplot2")

par(mfrow = c(3,3))
for (i in 1:9)
{
  hist(Y.vector.i(i),breaks = 20, main = i)  
  
}

for (i in 1:9){
  
  plot(density(x = Y.vector.i(i),bw = "nrd0", adjust = 1,kernel =  "gaussian"), main = i)  

}


for (i in 1:9)
{
  acf(Y.vector.i(i), main = i)  
}




