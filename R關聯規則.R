library(arules)
dataset = read.csv("C:/Users/USER/Desktop/機器學習 A-Z (Machine Learning A-Z in Chinese)/第22章 先驗算法 （Apriori）/Apriori_R/Market_Basket_Optimisation.csv", header = FALSE)
dataset = read.transactions("C:/Users/USER/Desktop/機器學習 A-Z (Machine Learning A-Z in Chinese)/第22章 先驗算法 （Apriori）/Apriori_R/Market_Basket_Optimisation.csv", 
                            sep = ',', rm.duplicates = TRUE) #csv是數字分隔 rm.duplicates = T去除重複
#distribution of transactions with duplicates:1 5 表示其中有5筆資料有1個值有重複 

summary(dataset) #看報表
#119 columns (items) and a density of 0.03288973 代表1佔0.032其他都是0
#most frequent items: 是最常出現的
#4分位距看每列平均是3.914

#畫出出現頻率最高的前X個
itemFrequencyPlot(dataset, topN = 42)

#用apriori()函數建立關聯規則模型
rules = apriori(data = dataset, #丟進去的資料要轉成特定的格式
                parameter = list(support = 0.003, confidence = 0.2))
#設定support支持度(總次數太少的不行)、
#confidence信度(2個東西同時出現機率太小也不行)但信度太高也不好 會一直有閒雜人等混進來
#執行完注意資訊
rules #單看有發現幾個規則


#看1~10個關聯規則(未排序))
inspect(rules[1:10])

# 希望排序 所以用sort來針對lift提升度做排序
inspect(sort(rules, by = 'lift')[1:10])
