library(forecast)
library(fpp)
library(smooth)
library(readxl)
library(tseries)
cocacoala=read_xlsx('C:\\Users\\j seshadri reddy\\Downloads\\ASSIGMENTS\\forecasting\\CocaCola_Sales_Rawdata.xlsx')
View(cocacoala)
##
str(cocacoala)
summary(cocacoala)
sum(is.na(cocacoala))
amts=ts(cocacoala$Sales,frequency = 4,start = c(77))
amts
View(amts)
train=amts[1:38]
test=amts[39:42]
train=ts(train,frequency = 4)
test=ts(test,frequency = 4)
plot(amts)
hw_a=HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a
hw_a1=HoltWinters(train)
hw_a1
hw_a2=HoltWinters(train,alpha = T,beta = F,gamma = F)
hw_a2
hwa_pred=data.frame(predict(hw_a,n.ahead = 4))
hwa_pred
plot(forecast(hw_a,h=4))
hwa_mape=MAPE(hwa_pred$fit,test)*100
hwa_mape
hw_ab=HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)
hw_ab
hwab_pred=data.frame(predict(hw_ab,n.ahead = 4))
hwab_pred
plot(forecast(hw_ab,h=4))
hwab_mape=MAPE(hwab_pred$fit,test)*100
hwab_mape
hw_abg=HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg
hwabg_pred=data.frame(predict(hw_abg,n.ahead = 4))
hwabg_pred
plot(forecast(hw_abg,h=4))
hwabg_mape=MAPE(hwabg_pred$fit,test)*100
hwabg_mape
##
hw_na=HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred=data.frame(predict(hw_na,n.ahead = 4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape=MAPE(hwna_pred$fit,test)*100
hwna_mape
##
hw_nab=HoltWinters(train,gamma = F)
hw_nab
hwnab_pred=data.frame(predict(hw_nab,n.ahead = 4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape=MAPE(hwnab_pred$fit,test)*100
hwnab_mape

