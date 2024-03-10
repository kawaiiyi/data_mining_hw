setwd('/Users/xiaotai/Desktop/01研究生/第二学期/01数据挖掘/HW1')
video <- read.csv('data.csv')
# 1、查看变量的总体情况
summary(video)
unique_count_zuozheid <- length(unique(video$作者编号))
date_counts <- table(video$发布日期)
print(date_counts)
install.packages("chron")
library(chron)
time_data <- times(video$发布时间)
summary(time_data)


#加载中文字体
# install.packages("showtext")
library(showtext)
showtext_auto(enable = TRUE)

#点赞的直方图
# 将点赞数转换为以万为单位
video$点赞数万 <- as.numeric(format(video$点赞数 / 10000, digits = 2))
# 计算直方图的各个柱子的高度和中点，但不实际绘制直方图
counts <- hist(video$点赞数万, plot = FALSE)
mids <- counts$mids
counts <- counts$counts
# 绘制直方图
hist(video$点赞数万, main = "点赞分布直方图", xlab = "点赞数(万)", col = "steelblue", border = "black", family = "SimHei", ylim = c(0, max(counts) * 1.2))
# 在每个柱子的顶部添加标注
text(x = mids, y = counts + max(counts) * 0.05, labels = counts, pos = 3, cex = 0.75, col = "black")

#利用数据集中的发布时间变量，提取小时变量，命名为hour。
video$hour <- strptime(video$发布时间, "%H:%M:%S")$hour
#使用折线图展示不同小时发布的短视频点赞数的差异，并进行适当解读；
likes_by_hour <- aggregate(video$点赞数万, by=list(hour=video$hour), FUN=sum)
names(likes_by_hour) <- c("hour", "likes_by_hour")
likes_by_hour$likes_by_hour_million <- format(likes_by_hour$likes_by_hour/100,digits=2)
likes_by_hour$likes_by_hour_million <- as.numeric(likes_by_hour$likes_by_hour_million) 
plot(likes_by_hour$hour,likes_by_hour$likes_by_hour_million, type="l", xlab="小时", ylab="点赞数(百万）", main="不同小时发布的短视频点赞数差异", col="black",xaxt="n",ylim = c(0, max(likes_by_hour$likes_by_hour_million) * 1.2))
axis(1, at=likes_by_hour$hour, labels=likes_by_hour$hour)
for (i in 1:nrow(likes_by_hour)) {
  label <- likes_by_hour$likes_by_hour_million[i]
  text(likes_by_hour$hour[i], likes_by_hour$likes_by_hour_million[i], labels=label, pos=3, col="black")
}

#使用分组箱线图对比不同类别的视频的点赞数差异
library(ggplot2)
ggplot(video, aes(x=类别, y=点赞数万, fill=类别)) +
  geom_boxplot() +
  theme_bw() +
  labs(title="不同类别视频的点赞数差异", x="类别", y="点赞数（万）") +
  theme(
    plot.title = element_text(hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# 首先提取非汽车类数据命名为video1。
video1 <- video[video$类别 != "汽车",]
# 再将视频发布时间分为6个时段：“20:00-次日5:59”、“6:00-8:59”、“9:00-11:59”、“12:00-13:59”、“14:00-16:59”、“17:00-19:59”，分别命名为“夜间”“早间”“上午”“中午”“下午”“傍晚”，将该变量命名为“视频发布时间”。
video1$视频发布时间 <- cut(video1$hour,
                     breaks= c(-Inf, 6, 9,12,14,17,20,Inf),
                     labels = c("夜间","早间","上午","中午","下午","傍晚","夜间"),
                     right = FALSE)
# 建立点赞数的双对数线性回归模型，因变量为对数点赞数，自变量包括：视频发布时段（基准组：夜间）、对数分享数、对数评论数、时长、类别（基准组：宠物）、标题字数
video1 <- video1[video1$点赞数 > 0 & video1$分享数 > 0 & video1$评论数 > 0, ]
video1$类别 <- factor(video1$类别)  # 将类别转换为因子类型
video1$类别 <- relevel(video1$类别, ref = "宠物")  # 将宠物设置为基准组
video1$视频发布时间 <- factor(video1$视频发布时间)  
video1$视频发布时间 <- relevel(video1$视频发布时间, ref = "夜间")  
lm_model <- lm(log(点赞数) ~ 视频发布时间 + log(分享数) + log(评论数) + 时长 + 类别 + 标题字数, data = video1)
summary(lm_model)
# 对第3问中建立的模型进行模型诊断
# 残差与拟合值的关系（检查同方差性和线性性）
plot(lm_model$fitted.values, resid(lm_model),
     xlab = "拟合值", ylab = "残差",
     main = "残差 vs 拟合值")
abline(h = 0, col = "red")
# 检查残差的正态性
qqnorm(resid(lm_model))
qqline(resid(lm_model))
# 检查多重共线性
install.packages("car")
library(car)
vif(lm_model)
# 自相关检验
install.packages("lmtest")
library(lmtest)
dwtest(lm_model)
