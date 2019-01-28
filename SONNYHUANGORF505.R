load(paste("/Users/sonnyhuang/Desktop/","Jan2019FinalData.r",sep=""))

#Using a parametric way to fit a distribution to the change of 
#inventory of stocks in a given day

##calculating the change of inventory of stock 1
change1 = array(1:1500)
for (i in 1:1500){
  change1[i] = INVENT_1[i+1] - INVENT_1[i]
  
}

plot(change1)
##calculating the change of inventory of stock 2
change7 = array(1:921)
for (i in 1:921){
  change7[i] = INVENT_7[i+1] - INVENT_7[i]
  
}
##calculating the change of inventory of stock 3
change79 = array(1:1899)
for (i in 1:1899){
  change79[i] = INVENT_79[i+1] - INVENT_79[i]
  
}
##checking the histogram of stock 1
hist(change1)
##plotting the quantiles of stock 1 against the quantiles of normal 
##distribution using a qq plot. 
qqnorm(change1)
abline(0,1)

##fitting a GPD to the upper and lower tails of the data for stock 1
count = 0 
for (i in 1:1500){
  if(change1[i] >= 0) {count = count + 1}
  
}
change1pos = array(1:913)
index = 0
for (i in 1:1500){
  if(change1[i] >= 0) {index = index + 1
  change1pos[index] = change1[i]}
  
}

count = 0 
for (i in 1:1500){
  if(change1[i] < 0) {count = count + 1}
  
}
change1neg = array(1:587)
index = 0
for (i in 1:1500){
  if(change1[i] < 0) {index = index + 1
  change1neg[index] = -change1[i]}
  
}
shape.plot(change1pos)
shape.plot(change1neg)
hist(change1pos)
change1 = as.numeric(change1)
change1est <- fit.gpd(change1,tail="two",lower = -600,upper=1000)
tailplot(change1est,tail="two")
change1est@upper.par.ests[2]
change1est@lower.par.ests[2]
mean1 = mean(change1)
var1 = var(change1)



hist(change7)
qqnorm(change7)
abline(0,1)


count = 0 
for (i in 1:921){
  if(change7[i] >= 0) {count = count + 1}
  
}
change7pos = array(1:310)
index = 0
for (i in 1:921){
  if(change7[i] >= 0) {index = index + 1
  change7pos[index] = change7[i]}
  
}


count = 0 
for (i in 1:921){
  if(change7[i] < 0) {count = count + 1}
  
}
change7neg = array(1:611)
index = 0
for (i in 1:921){
  if(change7[i] < 0) {index = index + 1
  change7neg[index] = -change7[i]}
  
}

shape.plot(change7pos)
shape.plot(change7neg)

change7 = as.numeric(change7)
change7est <- fit.gpd(change7,tail="two",lower = -1200,upper=800)
tailplot(change7est,tail="two")
change7est@upper.par.ests[2]
change7est@lower.par.ests[2]
mean7 = mean(change7)
var7 = var(change7)


hist(change79)
qqnorm(change79)
abline(0,1)


count = 0 
for (i in 1:1900){
  if(change79[i] >= 0) {count = count + 1}
  
}
change79pos = array(1:863)
index = 0
for (i in 1:1900){
  if(change79[i] >= 0) {index = index + 1
  change79pos[index] = change79[i]}
  
}
count = 0 
for (i in 1:1900){
  if(change79[i] < 0) {count = count + 1}
  
}
change79neg = array(1:1036)
index = 0
for (i in 1:1036){
  if(change79[i] < 0) {index = index + 1
  change79neg[index] = -change79[i]}
  
}
shape.plot(change79pos)
shape.plot(change79neg)


change79 = as.numeric(change79)
change79est <- fit.gpd(change79,tail="two",lower = -900,upper=1000)
tailplot(change7est,tail="two")
change79est@upper.par.ests[2]
change79est@lower.par.ests[2]
mean79 = mean(change79)
var79 = var(change79)

spaced1 = approx(TIMES_1,INVENT_1,n = 781)
spaced7 = approx(TIMES_7,INVENT_7,n = 781)
spaced79 = approx(TIMES_79,INVENT_79,n = 781)

I1 = array(1:780)
for (i in 1:780){
  I1[i] = spaced1$y[i+1] - spaced1$y[i]
  
}

I7 = array(1:780)
for (i in 1:780){
  I7[i] = spaced7$y[i+1] - spaced7$y[i]
  
}

I79 = array(1:780)
for (i in 1:780){
  I79[i] = spaced79$y[i+1] - spaced79$y[i]
  
}

hist(I1)
qqnorm(I1)
abline(0,1)


count = 0 
for (i in 1:780){
  if(I1[i] >= 0) {count = count + 1}
  
}
changeI1pos = array(1:383)
index = 0
for (i in 1:780){
  if(I1[i] >= 0) {index = index + 1
  changeI1pos[index] = I1[i]}
  
}
count = 0 
for (i in 1:780){
  if(I1[i] < 0) {count = count + 1}
  
}
changeI1neg = array(1:397)
index = 0
for (i in 1:780){
  if(I1[i] < 0) {index = index + 1
  changeI1neg[index] = -I1[i]}
  
}
shape.plot(changeI1pos)
shape.plot(changeI1neg)




I1 = as.numeric(I1)
I1est <- fit.gpd(I1,tail="two",lower = -1000,upper=4000)
tailplot(I1est,tail="two")
I1est@upper.par.ests[2]
I1est@lower.par.ests[2]
I1
meanI1 = mean(I1)
varI1 = var(I1)
meanI1 
varI1


hist(I7)
qqnorm(I7)
abline(0,1)

count = 0 
for (i in 1:780){
  if(I7[i] >= 0) {count = count + 1}
  
}
changeI7pos = array(1:302)
index = 0
for (i in 1:780){
  if(I7[i] >= 0) {index = index + 1
  changeI7pos[index] = I7[i]}
  
}
count = 0 
for (i in 1:780){
  if(I7[i] < 0) {count = count + 1}
  
}
changeI7neg = array(1:478)
index = 0
for (i in 1:780){
  if(I7[i] < 0) {index = index + 1
  changeI7neg[index] = -I7[i]}
  
}
shape.plot(changeI7pos)
shape.plot(changeI7neg)
I7 = as.numeric(I7)
I7est <- fit.gpd(I7,tail="two",lower = -1500,upper=900)
tailplot(I7est,tail="two")
I7est@upper.par.ests[2]
I7est@lower.par.ests[2]

meanI7 = mean(I7)
varI7 = var(I7)
meanI7 
varI7

hist(I79)
qqnorm(I79)
abline(0,1)

count = 0 
for (i in 1:780){
  if(I79[i] >= 0) {count = count + 1}
  
}
changeI79pos = array(1:390)
index = 0
for (i in 1:780){
  if(I79[i] >= 0) {index = index + 1
  changeI79pos[index] = I79[i]}
  
}
count = 0 
for (i in 1:780){
  if(I79[i] < 0) {count = count + 1}
  
}
changeI79neg = array(1:390)
index = 0
for (i in 1:780){
  if(I79[i] < 0) {index = index + 1
  changeI79neg[index] = -I79[i]}
  
}
shape.plot(changeI79pos)
shape.plot(changeI79neg)
I79 = as.numeric(I79)
I79est <- fit.gpd(I79,tail="two",lower = -3000,upper=2500)
tailplot(I79est,tail="two")
I79est@upper.par.ests[2]
I79est@lower.par.ests[2]

meanI79 = mean(I79)
varI79 = var(I79)
meanI79
varI79

number = 0
for (i in 1:780){
  if (-I1[i]>50000){
    number = number + 1
  }
}
number1 = 0
for (i in 1:780){
  if (I7[i]>7000){
    number1 = number1 + 1
  }
}

d1 = dpareto(7000,xi=0.4516846 )
d2 = dpareto(50000,xi=0.3621031)
q1 = qpareto(d1,xi=0.4516846)
q2 = qpareto(d2,xi=0.3621031)
ppareto(q1,xi=0.4516846)
ppareto(q2,xi=0.3621031)

X1_train = array(1:9722)
X2_train = array(1:9722)
T1_train = array(1:9722)
T2_train = array(1:9722)
for (k in 1:9722){
  X1_train[k] = x_train[k,1,1000]
  X2_train[k] = x_train[k,1,999]
  T1_train[k] = x_train[k,2,1000]
  T2_train[k] = x_train[k,2,999]
}

X1_test = array(1:2462)
X2_test = array(1:2462)
T1_test = array(1:2462)
T2_test = array(1:2462)
for (k in 1:2462){
  X1_test[k] = x_test[k,1,1000]
  X2_test[k] = x_test[k,1,999]
  T1_test[k] = x_test[k,2,1000]
  T2_test[k] = x_test[k,2,999]
}
lm1 <- lm(Z_train ~X1_train + X2_train + T1_train + T2_train)
plot(lm1$residuals)
Z_test_hat = array(1:2462)
for(i in 1:2462){
  Z_test_hat[i] = lm1$coefficients[1] + lm1$coefficients[2]*X1_test[i] + lm1$coefficients[3]*X2_test[i]
  +lm1$coefficients[4]*T1_test[i] + lm1$coefficients[5]*T2_test[i]
}
FOM1 = 0
for ( i in 1:2462){
  FOM1 = FOM1 + (Z_test_hat[i] - Z_test[i])^2
}
FOM1 = sqrt(FOM1/2462)
FOM1
Train = c()
Train = rbind(X1_train,X2_train,T1_train,T2_train)
Train1 = t(Train)
Test = rbind(X1_test,X2_test,T1_test,T2_test)
Test1 = t(Test)
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 400, activation = 'relu',input_shape = 4,bias_initializer = "zeros", kernel_initializer = "zeros") %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 20, activation = 'relu',bias_initializer = "zeros", kernel_initializer = "zeros") %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 1, activation = 'linear')

model %>% compile(
  loss = 'mean_squared_error',  optimizer = 'sgd',  metrics = c('mse','mae'))

nn = model %>% fit(Train1, Z_train, batch_size = 500, epochs = 20)

score <- model %>% evaluate(Test1, Z_test, batch_size = length(Z_test))
sqrt(score$mean_squared_error)
dim(Train1) <- c(9722, 4, 1, 1)
model <- keras_model_sequential()
model %>%
  layer_conv_2d(filters = 400, kernel_size = c(1, 2), activation = "relu",
                input_shape = c(4, 1, 1)) %>%
  layer_max_pooling_2d(pool_size = c(1, 1)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(1, 2), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(1, 1)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(1, 2), activation = "relu")

model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 1, activation = "linear")


model %>% compile(
  optimizer = "rmsprop",
  loss = "mean_squared_error",
  metrics = c('mse','mae')
)

model %>% fit(
  Train1, Z_train,
  epochs = 20, batch_size=500
)
dim(Test1) <- c(2462, 4, 1, 1)
score <- model %>% evaluate(Test1, Z_test, batch_size = length(Z_test))
sqrt(score$mean_squared_error)
