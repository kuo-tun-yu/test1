library(RMySQL)
library(DBI)
library(pool)

host1 <-"192.168.35.119" #村長那台電腦的IP
port1 <- 3306
dbname1 <- "ptt_db"
user1 <- "derek"
password1 <- "derek"

engine <- dbConnect(RMySQL::MySQL(),
                    host = "192.168.35.119",
                    port = 3306,
                    dbname = "ptt_db",
                    user = "derek",
                    password = "derek"
)

#dcard_mysql <- dbReadTable(engine, name = 'dcard_comment', fileEncoding = "ansi", header= FALSE)

dbSendQuery(engine, 'set character set "Big5"'); #要先Send才能下一行的Get
c1 <- dbGetQuery(engine,'select * from ptt_content;') #GetQuery得到dataframe

re1 <- dbGetQuery(engine,'select * from ptt_comment;') #GetQuery得到dataframe

J1=merge(x = c1, y = re1, by = "url")
J2 =J1[ ,c(3,4,5,6,7,10,11,12,13,14,15,16)]
rm(c1,re1,J1)
J2$n_content= nchar(J2$content) 
J2$n_comment= nchar(J2$comment)
table(J2$n_comment)
J2$y1 =0

z1=which(J2$n_comment==0)
J2[z1,'y1']=1 #定好y了
rm(z1)
library(stringr)
J2$time =substr(J2$time ,1,2)

b1=which(J2$time=='07' |J2$time=='08'|J2$time=='09'|J2$time=='10'|J2$time=='11'|J2$time=='12' )
J2[b1,'time']= 1 
b2=which(J2$time=='13' |J2$time=='14'|J2$time=='15'|J2$time=='16'|J2$time=='17'|J2$time=='18' )
J2[b2,'time']= 2 
b3=which(J2$time=='19' |J2$time=='20'|J2$time=='21'|J2$time=='22'|J2$time=='23'|J2$time=='00' )
J2[b3,'time']= 3 
b4=which(J2$time=='01' |J2$time=='02'|J2$time=='03'|J2$time=='04'|J2$time=='05'|J2$time=='06' )
J2[b4,'time']= 4
rm(b1,b2,b3,b4)

J3 =J2[ ,c(1,2,3,4,5,6,9,10,11,13,15)]
J3$year = as.factor(J3$year)
J3$month = as.factor(J3$month)
J3$day = as.factor(J3$day)
J3$weekday = as.factor(J3$weekday)
J3$time = as.factor(J3$time)
J3$area = as.factor(J3$area)
#J3$y1 = as.factor(J3$y1)

#分訓練測試資料
library(caTools)
set.seed(100)
#設定Y    訓練測試比例
split = sample.split(J3$y1, SplitRatio = 0.8)
training_set = subset(J3, split == TRUE)
test_set = subset(J3, split == FALSE)
View(test_set)

#正規化 如果是分類數據不用這正規化
training_set[c(7, 8, 9,10)] = scale(training_set[c(7, 8, 9,10)])
test_set[c(7, 8, 9,10)] = scale(test_set[c(7, 8, 9,10)])
#----------------------------預處理結束------------------------------

library(h2o)
h2o.init(nthreads = -1) 

classifier1 = h2o.deeplearning(y = 'y1', #告訴h2o那些欄是X、Y
                              #X不給就會除了Y之外全部欄位都是X
                              training_frame = as.h2o(training_set),
                              #丟進去training_frame的資料要先轉乘h2o型態
                              
                              activation = 'Maxout',
                              hidden = c(6,5), 
                              epochs = 100, #epochs是設定要反覆修正幾次
                              train_samples_per_iteration = -2)

prob_pred = h2o.predict(classifier1, newdata = as.h2o(training_set[-11]))
class(prob_pred)
prob_pred #想看一下發現看不懂

a = as.vector(prob_pred)
View(a) #轉換一下就看到每筆測試資料的預測結果了(分類結果=1的機率)

y_pred = (prob_pred > 0.99) #用機率大於小於0.5區分 大於是預測結果是1
y_pred = as.vector(y_pred)
View(y_pred)

#用Confusion Matrix矩陣來看
cm = table(training_set[, 11], y_pred)
cm

#直接算正確率
accurate_rate = sum(training_set[, 11]==y_pred)/length(y_pred)
accurate_rate
#-------------------接下來換測試資料----------------------
prob_pred = h2o.predict(classifier1, 
                        newdata = as.h2o(test_set[-11]))

y_pred = (prob_pred > 0.98) #用機率大於小於0.5區分 大於是預測結果是1
y_pred = as.vector(y_pred)
View(y_pred)

#用Confusion Matrix矩陣來看
cm = table(test_set[, 11], y_pred)
cm

#直接算正確率
accurate_rate = sum(test_set[, 11]==y_pred)/length(y_pred)
accurate_rate
