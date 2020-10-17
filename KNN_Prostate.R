prc<-read.csv("Prostate_Cancer.csv",stringsAsFactors = F) #This command imports the required data set and saves it to the prc data frame.
#This command helps to convert every character vector to a factor wherever it makes sense.
str(prc) #We use this command to see whether the data is structured or not.
prc <- prc[-1]  #removes the first variable(id) from the data set.
table(prc$diagnosis_result)  # it helps us to get the number of patients
prc$diagnosis <- factor(prc$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))
round(prop.table(table(prc$diagnosis)) * 100, digits = 1)  
# it gives the result in the percentage form rounded of to 1 decimal place( and so it's digit = 1)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
prc_n <- as.data.frame(lapply(prc[2:9], normalize))
set.seed(21)
nrow<-length(prc[,1])
ncol<-length(prc[1,])
idx<-sample(nrow,size = 0.7*nrow,replace = F)
idx
prc_train <- prc_n[idx,]
prc_test <- prc_n[-idx,]
prc_train_labels <- prc[idx, 1]
prc_test_labels <- prc[-idx, 1]   

install.packages("class")
library(class)
install.packages("gmodels")
library(gmodels)

i=1
k.optm=1                     
for (i in 1:11){ 
  knn.mod <-  knn(train=prc_train, test=prc_test, cl=prc_train_labels, k=i)
  k.optm[i] <- 100 * sum(prc_test_labels== knn.mod)/NROW(prc_test_labels)
  k=i  
  cat(k,'=',k.optm[i],'\n') }   
plot(k.optm, type="b", xlab="K - Value",ylab="Accuracy")

prc_test_pred <- knn(train = prc_train, test = prc_test,cl = prc_train_labels, k=9)
CrossTable(x=prc_test_labels,y=prc_test_pred,expected=FALSE, prop.r=T, prop.c=T,
           prop.t=F, prop.chisq=F, chisq = FALSE, fisher=FALSE, mcnemar=FALSE,
           resid=FALSE, sresid=FALSE, asresid=FALSE,
           missing.include=FALSE)

#The test data consisted of 30 observations. Out of which 8 cases have been accurately predicted (TN->True Negatives) as 
#Benign (B) in nature which constitutes 26.67%. 
#Also, 20 out of 30 observations were accurately predicted (TP-> True Positives) as Malignant (M) in nature which constitutes 66.67%. 
#Thus a total of 20 out of 30 predictions where TP i.e, True Positive in nature.
#There are 0 cases of False Negatives (FN) i.e. no case was recorded which actually is malignant in nature but got predicted as benign. 
#The FNs pose a potential threat for the same reason and the main focus to increase the accuracy of the model is to reduce FNs.
#There were 2 cases of False Positives (FP) meaning 2 cases were actually benign in nature but got predicted as malignant.
#The total accuracy of the model is 93.33 %( (TN+TP)/30) which shows that there may be chances to improve the model performance.







