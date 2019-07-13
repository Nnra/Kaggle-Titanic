#加载相关的数据包
library(ggplot2)
library(ggthemes)
library(scales)
library(plyr)
library(dplyr)
library(mice)
library(randomForest)
library(party)
library(corrplot)

#读入数据并查看
train <- read.csv("/Users/yumei/Desktop/ml100day/d2l-zh/data/kaggle_titanic/train.csv")
test <- read.csv("/Users/yumei/Desktop/ml100day/d2l-zh/data/kaggle_titanic/test.csv")
str(train)
str(test)
data <- bind_rows(train,test)
summary(data)

#数据预处理
#第一步，查看缺失值
sapply(data,function(x) sum(is.na(x)))
sapply(data,function(x) sum(x == ""))

########查看2个空值的位置和相关信息
Embarked.na <- data$Embarked
which(Embarked.na %in% "")
data_62 <- data[data$PassengerId == 62,]
data_830 <- data[data$PassengerId == 830,]
data_62
data_830

########用ggplot2绘制Embarked,fare,pclass的箱线图
ggplot(data[!is.na(data$Embarked),],aes(x=Embarked, y=Fare, fill=factor(Pclass))) +
geom_boxplot() + 
  geom_hline(aes(yintercept=80), color='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) + theme_few() 

#######定义Embarked空白值为“C"
data$Embarked[c(62,830)] <- "C"
sapply(data,function(x) sum(x == ""))

########查看缺失值位置和相关信息
Fare.na <- is.na(data$Fare)
which(Fare.na %in% TRUE)
data_1044 <- data[data$PassengerId == 1044,]
data_1044

########用ggplot2绘制Embarked,fare,pclass的箱线图
ggplot(data[data$Embarked=='S' & data$Pclass == 3,],aes(x=Embarked, y=Fare, fill=factor(Pclass))) +
  geom_boxplot() + 
  geom_hline(aes(yintercept=80), color='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) + theme_few() 
########用中位数填充缺失值
b <- median(data$Fare[data$Embarked == 'S' & data$Pclass == 3],na.rm = T)
data$Fare[1044] <- 8.05
sapply(data,function(x) sum(is.na(x)))


########查看年龄缺失值相关信息
data[is.na(data$Age),]
########设置随机种子
set.seed(129)
########执行多重插补法并输出
ss <- c('PassengerId','Name','Ticket','Cabin','family','Surname','Survived')
mice_age <- mice(data[,!names(data) %in% ss],method = 'rf')
mice_output <- complete(mice_age)

########绘制年龄分布图
par(mfrow=c(1,2))
hist(data$Age,freq = F,main = 'Age:ORiginal Data',col='darkblue',ylim = c(0,0.04))
hist(mice_output$Age,freq = F,main = 'Age:MICE Output',col = 'skyblue',ylim = c(0,0.04))
########使用预测结果的年龄替换原始数据中的年龄
data$Age <- mice_output$Age

###（1）Pclass对存活率的影响
data$Survived <- factor(data$Survived)
ggplot(data = data[1:nrow(train),], mapping = aes(x = Pclass, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Pclass') + 
  ylab('Count') + 
  ggtitle('Different Pclass impact survived') + 
  scale_fill_manual(values=c("#FF0000", "#00FF00")) +
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

###（2）Name对存活率的影响
########提取姓名中的title
data$Title <- sapply(data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
data$Title <- sub(" ","",data$Title)
table(data$Title)                   ##查看Title的种类
###将数量较少的Title归类为Others，并重新定义一些称呼
Others <- c('Capt','Col','Don','Dona','Jonkheer','Lady','Major','Sir','the Countess')
data$Title[data$Title=='Mlle'] <- 'Miss'
data$Title[data$Title=='Mme'] <- 'Mrs'
data$Title[data$Title=='Ms'] <- 'Miss'
data$Title[data$Title %in% Others] <- 'Others'
table(data$Title)

###用ggplot2绘制不同Title乘客的遇难和存活数
ggplot(data = data[1:891,], mapping = aes(x = Title, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='stack') + 
  xlab('Title') + 
  ylab('Count') + 
  ggtitle('Different Title impact survivor') + 
  scale_fill_discrete(name="Survived", breaks=c(0, 1), labels=c("0", "1")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

###（3）Sex对存活率的影响
data$Sex <- as.factor(data$Sex)
ggplot(data = data[1:891,], mapping = aes(x = Sex, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Sex') + 
  ylab('Count') + 
  ggtitle('Different Sex impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

#####先看SibSp对存活率的影响
ggplot(data = data[1:891,], mapping = aes(x = SibSp, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  labs(title = "Different SibSp impact survivor", x = "Sibsp", y = "Count", fill = "Survived") + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) +
  scale_x_continuous(breaks = c(0:8)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
#####再看Parch对存活率的影响
ggplot(data = data[1:891,], mapping = aes(x = Parch, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  labs(title = "Different Parch impact survivor", x = "Parch", y = "Count", fill = "Survived") + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  scale_x_continuous(breaks = c(0:6)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

#####新变量FamilySize对存活率的影响FamilySize在2-4时，存活率最高
data$FamilySize <- data$SibSp + data$Parch + 1
data$FamilySize
ggplot(data = data[1:891,], mapping = aes(x = FamilySize, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('FamilySize') + 
  ylab('Count') + 
  ggtitle('Different FamilySize impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  scale_x_continuous(breaks = c(0:11)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

ggplot(data[!is.na(data$Survived),],aes(Age,color=Survived))+
  geom_line(aes(label=..count..), stat = 'bin', binwidth=5)  + 
  labs(title = "Different Age impact survivor", x = "Age", y = "Count", fill = "Survived")+
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

#####生成新变量Age_New并进行分析
data$Age_New[data$Age < 18] <- 'child'
data$Age_New[data$Age >= 18] <- 'adult'
data$Age_New
table(data$Age_New,data$Survived)
#####用ggplot2绘制成年人和儿童的存活情况
ggplot(data[!is.na(data$Survived),],aes(Age_New,fill=Survived))+
  geom_bar(stat = 'count',position = 'dodge')+
  ggtitle('Adult and Child  Impact Survivor')+
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

###（6）ticket对存活率的影响
ticket.count <- aggregate(data$Ticket, by = list(data$Ticket), function(x) sum(!is.na(x)))
ticket.count
table(ticket.count$x)

###（7）票价Fare对存活率的影响
ggplot(data = data[!is.na(data$Survived) ,], aes(x = Fare, color=Survived)) + 
  geom_line(aes(label=..count..), stat = 'bin', binwidth=10)  + 
  labs(title = "Different Fare impact survivor", x = "Fare", y = "Count", fill = "Survived")+
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

###（8）登船港口Embarked对存活率的影响
ggplot(data[1:891, ], mapping = aes(x = Embarked, y = ..count.., fill = Survived)) +
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Embarked') +
  ylab('Count') +
  ggtitle('Different Embarked impact survivor') +
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

#建模预测
###（1）建立模型
data$Pclass <- factor(data$Pclass)
data$Title <- factor(data$Title)
data$Sex <- factor(data$Sex)
data$Age_New <- factor(data$Age_New)
data$FamilySize <- factor(data$FamilySize)
data$Fare <- factor(data$Fare)
data$Embarked <- factor(data$Embarked)
train <- data[1:891,]
test <- data[892:1309,]
set.seed(102)
model <- cforest(Survived ~ Pclass + Title + Sex + Age_New + FamilySize + Fare + Embarked, data = train,controls = cforest_unbiased(ntree=2000,mtry=3))
                 
###（2）生成预测结果
prediction<-predict(model,test,OOB=TRUE,type = "response") 
output<-data.frame(PassengerId=test$PassengerId,Survived=prediction)
output
write.csv(output,file = "/Users/yumei/Desktop/Prediction1.csv",row.names = FALSE)

#model选用randomForest，也許可以用不同的建模方法
#Age區間分細一點
#Cabin缺失值太多，也許要考慮
#建模后对影响模型因子重要性进行分析
#也可以創造因子
#https://zhuanlan.zhihu.com/p/28802636