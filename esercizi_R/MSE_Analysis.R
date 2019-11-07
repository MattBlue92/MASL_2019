#esercizio slide: Introduction to MASL
#impostatione dei parametri del modello 
n = 1000
nsim = 15
sigma = 2
split = round(n*0.7)
b0 = -1 # intercetta del modello
b1 = 2 # coefficiente di reg per X1
b2 = 1 # coefficiente di reg l'interazione tra X1 e X2
#fine impostazione dei parametri

# generazione dei dati e del modello
set.seed(42)
X1 = rnorm(n, sd = 2)
X2 = rnorm(n, sd = 2)
eps = rnorm(n)
Y = -1+2*X1^3+(X1)^2*X2+eps
X = data.frame("Y"= Y, "X1" = X1, "X2" = X2 )
X = X[sample(nrow(X),nrow(X)),] #permuta i dati
# fine generazione

#split dei dati
training = data.frame("Y"=Y[seq(from = 1, to = split)], 
                      "X1"=X1[seq(from = 1, to = split)]
                      ,"X2"=X2[seq(from = 1, to = split)])
test = data.frame("Y"=Y[seq(from = split+1, to = length(X1))], 
                  "X1"= X1[seq(from = split+1, to = length(X1))]
                  ,"X2"=X2[seq(from = split+1, to = length(X1))])
#fine split data

#stima del modello con i dati di training e calcolo del MSE
mseTraining = c()
mseTest = c()
for (i in 1:nsim) {
  myMod = lm(Y~poly(X1, degree = i, raw = TRUE), data = training)
  mseTraining[i] = mean(myMod$residuals^2)
  mseTest[i] = mean((test$Y-predict.lm(myMod, test))^2)
}
#fine

#plotting 
plot(1:nsim, mseTraining, type="l", pch=19, col="red", lty= 2,
     xlab="Complessity model", ylab="MSE")
lines(1:nsim, mseTest, pch=19, col="blue", type="l", lty=2)
legend("topright", legend=c("Training Set", "Test Set"),
       col=c("red", "blue"), lty=c(2,2), cex=0.8, title = "MSE")

