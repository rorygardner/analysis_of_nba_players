###data 200
###final project - nba

#wd
getwd()
setwd("/Users/rorygardner/OneDrive - James Madison University/data 200")

#packages
library(pacman)
p_load(ggplot2, dplyr, mosaic, readr, factoextra, corrplot, ggrepel)

#data
advanced <- read.csv("data/advanced.csv")
View(advanced)
nrow(advanced)

max(nba$season)
##quick cleaning=============================================================
nba1 <- subset(advanced, advanced$lg == "NBA")
nba <- nba1[nba1$season > 1990, -c(1, 5, 25, 26)]
nba <- nba[!is.na(nba$per),]
nba$tm <- as.factor(nba$tm)
nba$pos <- as.factor(nba$pos)

##cleaning===================================================================

#subset for only nba
nba <- subset(advanced, advanced$lg == "NBA")
View(nba)
nrow(nba)


#missing values
sum(is.na(nba1))
nrow(nba)

#see missing vals proportion first half vs last half (season)

#find 1/2 year (~1990)
is.numeric(nba$season)
min(nba$season) + ((max(nba$season)-min(nba$season))/2)

#na before and after
sum(is.na(nba1[nba1$season <= 1990,]))
sum(is.na(nba1[nba1$season > 1990,]))

#proportion
sum(is.na(nba[nba$season < 1990,]))/sum(is.na(nba))
sum(is.na(nba[nba$season >= 1990,]))/sum(is.na(nba))

### 99% before 1990 - work with only more recent than 1990
nba <- nba[nba$season >= 1990,]

#na by column
colSums(is.na(nba))
max(colSums(is.na(nba)))

#columns
colnames(nba)

#remove: seas_id, birth_year, lg
nba <- nba[, -c(1, 5, 9, 25, 26)]
colnames(nba)

###explore====================================================================

colnames(nba)

#per by seasons
ggplot(nba, aes(x = season, y = per)) + 
  geom_point()

#by team
ggplot(nba, aes(x = per, y = ws)) + 
  geom_point()

#usg by per
ggplot(nba, aes(x = usg_percent, y = per)) + 
  geom_point()

cor(usg_percent ~ per, data = nba)
lm.u1 <- lm(usg_percent ~ per, data = nba)
summary(lm.u1)

lm.u2 <- lm(usg_percent ~ per + ws, data = nba)
summary(lm.u2)

lm.u2 <- lm(usg_percent ~ per + ws, data = nba)
summary(lm.u2)

#use pc
usg.pc <- as.data.frame(nba.pca$x[, 1:3])
usg.pc$usg_percent <- nba.num$usg_percent

lm.u3 <- lm(usg_percent ~ PC1 + PC2 + PC3, data = usg.pc)
summary(lm.u3)

###pca=======================================================================

#numeric cols
colnames(nba)
sapply(nba, class)
nba.num <- na.omit(nba[, c(5, 6, 9:27)])
sum(is.na(nba.num))
colnames(nba.num)

nba.pca <- prcomp(nba.num, scale. = T)

scores <- nba.pca$x
loading <- nba.pca$rotation

#scree
fviz_eig(nba.pca, main ="Scree Plot for PCA of NBA players")

nba.eigval <- get_eigenvalue(nba.pca)

#Biplots

#contribution
fviz_pca_var(nba.pca, 
             col.var = "contrib", repel = T, 
             gradient.cols = c("darkolivegreen4", "steelblue", "darkblue"))
)

#cos2
fviz_pca_var(nba.pca, 
             col.var = "cos2", 
             gradient.cols = c("steelblue", "indianred", "goldenrod"))

#get pca variables
nba.var <- get_pca_var(nba.pca)

#corrplot of cos2 vals
corrplot(nba.var$cos2, 
         type = "lower", 
         tl.col = "black")

corrplot(cor(nba.num), type = 'lower', order = 'hclust',
         tl.col = 'black', tl.srt = 90)

#barplot - contrib of each variable

nba.bar1 <- fviz_contrib(nba.pca, choice = "var", 
                         axes = 1:2, 
                         fill = "darkseagreen", 
                         color = "darkseagreen4")
nba.bar1



###lm==================================================================
#nba2: dataframe for players before 1990
nba2 <- nba1[nba1$season <= 1990,]
nba2 <- nba2[rowSums(is.na(nba2[, -10])) == 0, ]

set.seed(987)
training.size <- floor(0.8*nrow(nba))
train.individuals <- sample(seq_len(nrow(nba)), size = training.size)

training.data <- nba[train.individuals, ]
testing.data <- nba[-train.individuals, ]

lm1 <- lm(data = training.data, per ~ bpm + ws_48)
summary(lm1)

testing.data$prediction <- round(predict(lm1, 
                                          newdata = testing.data), 2)

nba2$predper <- round(predict(lm1, 
                                 newdata = nba2), 2)

rmspe.nba <- sqrt(mean(testing.data$per - testing.data$prediction)^2)
rmspe.nba

p1.train_v_pred <- (
  ggplot() + 
    geom_point(data = nba, aes(x = bpm, y = per)) + 
    geom_point(data = nba2, aes(x = bpm, y = predper), 
               col = "red", size = 3) + 
    geom_smooth(data = nba, aes(x = bpm, y = per), 
                method = "lm", col = "deepskyblue3") + 
    geom_errorbar(data = nba2, aes(x = bpm, 
                                ymin = predper - rmspe.nba, 
                                ymax = predper + rmspe.nba)) + 
    labs(x = "BPM", y = "PER", 
         title = "NBA Training vs Predicted PER") + 
    annotate("text", 50, 14,label = "Training Data", color = "deepskyblue3") + 
    annotate("text", 50, -10, label = "<1990 Predictions", color = "red") + 
    theme_bw())
plot(p1.train_v_pred)

nba[nba$per == max(nba$per), "player"]

nba2[nba2$predper == max(nba2$predper), c("player", "season")]
max(nba2$predper)

#pred v observed 
p2.obs_v_pred <- (
  ggplot(data = testing.data, aes(x = per, y = prediction)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    geom_abline(slope = 1, intercept = 0) +
    labs(x = "Observed Values", y = "Predicted Values", 
         title = "Observed vs Predicted PER of Players After 1990", 
         caption = "Figure 2") +
    annotate("text", -20, -40, label = "1:1 line", size = 5) + 
    annotate("text", 58, 25, label = "observed vs predicted", 
             col = "blue", size = 4) + 
    theme_bw())
plot(p2.obs_v_pred)

#test normality of y
qqnorm(nba1$per)
qqline(nba1$per)


### positions==================================================================

#ws, per, usg, bpm,ts
###c, pf, pg, sf, sg
nba.perf <- subset(nba, pos %in% 
                     c("C", "PF", "PG", "SF", "SG"))
#ws
avg.ws <- aggregate(nba.perf, ws ~ pos, mean)
med.ws <- aggregate(nba, ws ~ pos, median)

ggplot(data.frame(avg.ws), aes(x = pos, y = ws)) + 
  geom_point(size = 2, color = "firebrick1") + 
  geom_line(aes(group = 1), 
            color = "black", lty = 3) + 
  labs(title = "Average Win Shares by Position", 
       x = "Position", 
       y = "Mean Win Shares") + 
  theme_bw()

#per
avg.per <- aggregate(nba.perf, per ~ pos, mean)

ggplot(data.frame(avg.per), aes(x = pos, y = per)) + 
  geom_point(size = 2, color = "dodgerblue") + 
  geom_line(aes(group = 1), 
            color = "black", lty = 3) + 
  labs(title = "Average PER by Position", 
       x = "Position", 
       y = "Mean PER") + 
  theme_bw()

#usg
avg.usg <- aggregate(nba.perf, usg_percent ~ pos, mean)

ggplot(data.frame(avg.usg), aes(x = pos, y = usg_percent)) + 
  geom_point(size = 2, color = "orange") + 
  geom_line(aes(group = 1), 
            color = "black", lty = 3) + 
  labs(title = "Average Usage Percent by Position", 
       x = "Position", 
       y = "Mean Usage Percent") + 
  theme_bw()

#bpm
avg.bpm <- aggregate(nba.perf, bpm ~ pos, mean)

ggplot(data.frame(avg.bpm), aes(x = pos, y = bpm)) + 
  geom_point(size = 2, color = "darkseagreen3") + 
  geom_line(aes(group = 1), 
            color = "black", lty = 3) + 
  labs(title = "Average Box Plus/Minus by Position", 
       x = "Position", 
       y = "Mean Box Plus/Minus") + 
  theme_bw()


#all together!
avg.ws <- aggregate(nba.perf, ws ~ pos, mean)
avg.per <- aggregate(nba.perf, per ~ pos, mean)
avg.usg <- aggregate(nba.perf, usg_percent ~ pos, mean)
avg.bpm <- aggregate(nba.perf, bpm ~ pos, mean)
pm.by.pos <- avg.ws
pm.by.pos$per <- avg.per$per
pm.by.pos$usg <- avg.usg$usg_percent
pm.by.pos$bpm <- avg.bpm$bpm
is.data.frame(pm.by.pos)
str(pm.by.pos)

#add ts%
avg.tsp <- aggregate(nba.perf, ts_percent ~ pos, mean)
pm.by.pos$tsp <- avg.tsp$ts_percent

View(pm.by.pos)

ggplot(data = pm.by.pos, aes(x = pos)) + 
  geom_point(aes(y = ws, color = "Win Shares"), 
             size = 4) + 
  geom_line(aes(y = ws, group = 1, color = "Win Shares"), 
            lty = 3) +
  geom_point(aes(y = per, color = "PER"), 
             size = 4) + 
  geom_line(aes(y = per, group = 1, color = "PER"), 
            lty = 3) +
  geom_point(aes(y = usg, color = "Usage Percent"), 
             size = 4) + 
  geom_line(aes(y = usg, group = 1, color = "Usage Percent"), 
            lty = 3) +
  geom_point(aes(y = bpm, color = "Box Plus/Minus"), 
             size = 4) + 
  geom_line(aes(y = bpm, group = 1, color = "Box Plus/Minus"), 
            lty = 3) +
  geom_point(aes(y = tsp, color = "True Shooting Percent"), 
             size = 4) + 
  geom_line(aes(y = tsp, group = 1, color = "True Shooting Percent"), 
            lty = 3) +
  scale_color_manual(values = 
                       c("Win Shares" = "firebrick1", 
                         "PER" = "dodgerblue", 
                         "Usage Percent" = "orange", 
                         "Box Plus/Minus" = "darkolivegreen4", 
                         "True Shooting Percent" = "purple")) + 
  labs(title = "Measures of Performance by Position", 
       x = "Position", 
       y = "Value", 
       color = "Performance Metric") + 
  theme_minimal()

ggplot(data = nba.perf,aes(x = pos)) + 
  geom_bar(fill = "mistyrose2", color = "maroon") + 
  labs(title = "Frequency of Players by Position", 
       x = "Position", 
       y = "Count") + 
  theme_minimal()
###==================================================================

nba[nba$pos == "SG-PG-SF", "player"]
sum(is.na(advanced))


"floralwhite"

"darkolivegreen3"

"pink"

"cornsilk"

"steelblue"

"darkseagreen"

"indianred"

"lavender"

"coral3"

"maroon"

"skyblue"

"mistyrose"

byplay <- aggregate(nba, per ~ as.factor(player), mean)
dper <- byplay[order(-byplay$per),]

View(dper)
