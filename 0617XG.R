install.packages("RMySQL")
library(RMySQL)
library(DBI)
library(pool)

host1 <-"192.168.35.119" #村長那台電腦的IP
port1 <- 3306
dbname1 <- "dcard_db"
user1 <- "derek"
password1 <- "derek"

engine <- dbConnect(RMySQL::MySQL(),
                    host = host1,
                    port = port1,
                    dbname = dbname1,
                    user = user1,
                    password = password1
)

#dcard_mysql <- dbReadTable(engine, name = 'dcard_comment', fileEncoding = "ansi", header= FALSE)
View(b)

dbSendQuery(engine, 'set character set "Big5"'); #要先Send才能下一行的Get
b <- dbGetQuery(engine,'select * from dcard_content;') #GetQuery得到dataframe



#把time欄位轉成時段 上午下午晚上半夜
b$time = substr(b$time ,1,2)
b$time = as.character(b$time)
b1=which(b$time=='07' |b$time=='08'|b$time=='09'|b$time=='10'|b$time=='11'|b$time=='12' )
b[b1,'time']='上午' 
b2=which(b$time=='13' |b$time=='14'|b$time=='15'|b$time=='16'|b$time=='17'|b$time=='18' )
b[b2,'time']='下午' 
b3=which(b$time=='19' |b$time=='20'|b$time=='21'|b$time=='22'|b$time=='23'|b$time=='00' )
b[b3,'time']='晚上' 
b4=which(b$time=='01' |b$time=='02'|b$time=='03'|b$time=='04'|b$time=='05'|b$time=='06' )
b[b4,'time']='半夜' 

b$area = as.character(b$area)
ar0=which(b$area==0)
b[ar0,'area'] ='其他地區'
ar1=which(b$area==1)
b[ar1,'area'] ='北區'
ar2=which(b$area==2)
b[ar2,'area'] ='中區'
ar3=which(b$area==3)
b[ar3,'area'] ='南區'
ar4=which(b$area==4)
b[ar4,'area'] ='東區'
ar5=which(b$area==5)
b[ar5,'area'] ='離島區'

b$weekday = as.character(b$weekday)
we1=which(b$weekday==1)
b[we1,'weekday'] ='星期一'
we2=which(b$weekday==2)
b[we2,'weekday'] ='星期二'
we3=which(b$weekday==3)
b[we3,'weekday'] ='星期三'
we4=which(b$weekday==4)
b[we4,'weekday'] ='星期四'
we5=which(b$weekday==5)
b[we5,'weekday'] ='星期五'
we6=which(b$weekday==6)
b[we6,'weekday'] ='星期六'
we7=which(b$weekday==7)
b[we7,'weekday'] ='星期日'
b1 = b[ ,c("weekday","time","gender","area","content" ,"heart" )]

#等一下攤平
#-----先對各篇文章做斷詞 取得有效字元數---------------
library(jiebaR)
library(tm)
library(tmcn) 
library(jsonlite)
work_user<-worker(user="C:/Users/tun yu/Desktop/FO.csv", 
                  stop_word="C:/Users/tun yu/Desktop/stop2.csv")

xtext2 = lapply(b1$content,segment,jiebar=work_user)
b1$content2 = xtext2 
b1$content2 = as.character(b1$content2)
b1$content2 <-gsub("c","",b1$content2) #把c去掉
b1$content2 <-gsub('[(.*)]' , "" , b1$content2) #把括號()清掉
b1$content2 <-gsub("\"" , "",b1$content2) #把"去掉
b1$content2 <-gsub(',',"、",b1$content2) #把,換成、
b1$content2 <-gsub(" ","",b1$content2) #把空格去掉

b1$nchar1 =nchar(b1$content)  #原文字元數
b1$nchar2 =nchar(b1$content2) #有效字元數

#要開始攤平了
library(nnet)
b2<- as.data.frame(cbind(b1, 
                         class.ind(b1$weekday),
                         class.ind(b1$gender),
                         class.ind(b1$area)))
#攤平後 選取有效欄位
b3 = b2[ ,c(6,8:26)]
b3 = b3[ ,-c(10,11,12,20)]
colnames(b3)=c("heart","nchar1","nchar2","Mon","Tue",
               "Wed","Fri","Sat","Sun","F","M","Central",
               "North","others","East","Sorth")

#--------------XGBoost----------------------
data.y <- b3$heart
data.n <- b3[ ,-1]
data.y=theF(data.y)
data.y =as.factor(data.y)
set.seed(2)
#分訓練測試資料 sample函數 從1:nrow(data)中 隨機挑選80%
select <- sample(1:nrow(b3),nrow(b3)*0.8)

train_set.x <- data.n[select,]
train_set.y <- data.y[select] #訓練資料的y
test_set.x <- data.n[-select,]
test_set.y <- data.y[-select] #測試資料的y

#預處理結束-----------------------建模開始 ---------------
library(xgboost)

#要把原本的Dataframe先換成xgboost的矩陣格式
dtrain <- xgb.DMatrix(data = as.matrix(train_set.x), label =  as.matrix(train_set.y))
#測試資料只要X就好
dtest <- xgb.DMatrix(data = as.matrix(test_set.x)) 

#設定參數
xgb.params = list(
  #col的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
  colsample_bytree = 0.5,                    
  #row的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
  subsample = 0.5,                      
  booster = "gbtree",
  #樹的最大深度，越高表示模型可以長得越深，模型複雜度越高
  max_depth = 2,           
  #boosting會增加被分錯的數據權重，而此參數是讓權重不會增加的那麼快，因此越大會讓模型愈保守
  eta = 0.04,
  # 或用'mae'也可以
  eval_metric = "rmse",                      
  objective = "reg:linear",
  #越大，模型會越保守，相對的模型複雜度比較低
  gamma = 0)

#先用xgb.cv()函數 為了知道建多少棵樹最好(重點!!))
cv.model = xgb.cv(
  params = xgb.params, 
  data = dtrain, #丟訓練資料
  nfold = 5,     # 5-fold cv
  nrounds=2000,   # 各個樹總數下的模型 嘗試種200棵樹
  # 如果當nrounds <30時，就已經有過度擬合情況發生，那表示不用繼續調下去了，可以提早停止
  early_stopping_rounds = 30, 
  print_every_n = 200 # 每20个单位才显示一次结果，
)

tmp <- cv.model$evaluation_log #把log先存起來

plot(x=1:nrow(tmp), y= tmp$train_rmse_mean, col='red', xlab="nround", ylab="rmse", main="Avg.Performance in CV") 
points(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue') 
legend("topright", pch=1, col = c("red", "blue"), 
       legend = c("Train", "Validation") )

best.nrounds = cv.model$best_iteration 
best.nrounds #得到 建多少顆樹最好

#真正建立xgboost模型 用xgb.train()函數 
#裡面的nrounds參數=剛剛得到建幾棵樹最好
xgb.Model <- xgb.train(paras = xgb.params, 
                       data = dtrain, nrounds = best.nrounds) 

# 先把訓練資料丟回模型做預測
xgb.Prediction <- predict(xgb.Model, dtrain)


#轉換函數 算準確率前再換就好了
theF<-function(A){
  p1 = which(A<300)
  A[p1]=1
  p2 = which(A>=300 & A<1000)
  A[p2]=2
  p3 = which(A>=1000 & A<5000)
  A[p3]=3
  p4 = which(A>=5000)
  A[p4]=4
  return(A)
}
#進行轉換
xgb.Prediction=theF(xgb.Prediction)
train_set.y=theF(as.numeric(train_set.y))


accuracy.xgb <- sum(xgb.Prediction==train_set.y)/length(xgb.Prediction)
accuracy.xgb

table(xgb.Prediction, train_set.y)

# 把測試資料丟進去預測
xgb.Prediction <- predict(xgb.Model, dtest)
xgb.Prediction=theF(xgb.Prediction)
test_set.y=theF(as.numeric(test_set.y))


accuracy.xgb <- sum(xgb.Prediction==test_set.y)/length(xgb.Prediction)
accuracy.xgb


table(xgb.Prediction, test_set.y)
#-------------------開始找有巧克力的文章-----------
cha=grep('巧克力',b2$content)
cha1 = b2[cha,c(6,8:26)]
cha1 = cha1[ ,-c(10,11,12,20)]
colnames(cha1)=c("heart","nchar1","nchar2","Mon","Tue",
               "Wed","Fri","Sat","Sun","F","M","Central",
               "North","others","East","Sorth")

data.y <- cha1$heart
data.n <- cha1[ ,-1]

dtest <- xgb.DMatrix(data = as.matrix(data.n)) 
xgb.Prediction <- predict(xgb.Model, dtest)
mean(xgb.Prediction)

mean(data.y )
