# Cesar F. Pardede
# Math 536
# Final
# 15 MAY 2020

# read in data and extract only columns we think are related to the problem
data = read.csv('F:/Documents/CSU Fullerton/Spring 2020/Math 536/Final/baseball (1).csv')[c('description', 'balls', 'strikes', 'plate_x', 'plate_z')]
# dim(data) == c(20528, 5)

# plot all points
plot(data$plate_x[data$description == 'ball'], data$plate_z[data$description == 'ball'], col=1, main='All Pitches', xlab='X-coordinate', ylab='Z-coordinate')
points(data$plate_x[data$description == 'called_strike'], data$plate_z[data$description == 'called_strike'], col=2)
legend('topleft', c('ball', 'strike'), fill = c(1,2))

# sort x and z values that resulted in a strike
x_sort_strike = sort(data$plate_x[data$description == 'called_strike'])
z_sort_strike = sort(data$plate_z[data$description == 'called_strike'])
xlen = length(x_sort_strike)
zlen = length(z_sort_strike)

# function to plot "percent zones"
zone_plotter <- function(x1, x2, y1, y2, color){
  lines(c(x1, x1), c(y1, y2), col=color, lwd=2)
  lines(c(x1, x2), c(y2, y2), col=color, lwd=2)
  lines(c(x2, x2), c(y2, y1), col=color, lwd=2)
  lines(c(x2, x1), c(y1, y1), col=color, lwd=2)
}

a1 = 0.99; a2 = 0.90
# 99%*99% of strikes
zone_plotter(x_sort_strike[(1-a1)/2*xlen], x_sort_strike[(1-(1-a1)/2)*xlen], z_sort_strike[(1-a1)/2*zlen], z_sort_strike[(1-(1-a1)/2)*zlen], 3)
# 90%*90% of strikes
zone_plotter(x_sort_strike[(1-a2)/2*xlen], x_sort_strike[(1-(1-a2)/2)*xlen], z_sort_strike[(1-a2)/2*zlen], z_sort_strike[(1-(1-a2)/2)*zlen], 4)
# 100% of strikes
zone_plotter(x_sort_strike[1], x_sort_strike[xlen], z_sort_strike[1], z_sort_strike[zlen], 5) 
legend('topright', c("1.0", as.character(a1), as.character(a2)), fill = c(5, 3, 4))

# Lets zoom in 
zoomed_data = data[which(data$plate_x > x_sort_strike[1] & data$plate_x < x_sort_strike[xlen] & data$plate_z > z_sort_strike[1] & data$plate_z < z_sort_strike[zlen]),]
plot(zoomed_data$plate_x[zoomed_data$description == 'ball'], zoomed_data$plate_z[zoomed_data$description == 'ball'], col=1, main='Zoomed-in Pitches', xlab='X-coordinate', ylab='Z-coordinate')
points(zoomed_data$plate_x[zoomed_data$description == 'called_strike'], zoomed_data$plate_z[zoomed_data$description == 'called_strike'], col=2)
zone_plotter(x_sort_strike[(1-a1)/2*xlen], x_sort_strike[(1-(1-a1)/2)*xlen], z_sort_strike[(1-a1)/2*zlen], z_sort_strike[(1-(1-a1)/2)*zlen], 3)
zone_plotter(x_sort_strike[(1-a2)/2*xlen], x_sort_strike[(1-(1-a2)/2)*xlen], z_sort_strike[(1-a2)/2*zlen], z_sort_strike[(1-(1-a2)/2)*zlen], 4)
zone_plotter(x_sort_strike[1], x_sort_strike[xlen], z_sort_strike[1], z_sort_strike[zlen], 5) 
legend('topleft', c('ball', 'strike'), fill = c(1,2))
legend('topright', c("1.0", as.character(a1), as.character(a2)), fill = c(5, 3, 4))

# Define fringe pitches as those pitches within the 100% and 90%*90% strike zone
fringe_pitches = zoomed_data[-which(zoomed_data$plate_x>x_sort_strike[(1-a2)/2*xlen] & zoomed_data$plate_x<x_sort_strike[(1-(1-a2)/2)*xlen] &
                                     zoomed_data$plate_z>z_sort_strike[(1-a2)/2*zlen] & zoomed_data$plate_z<z_sort_strike[(1-(1-a2)/2)*zlen]),]

fringe_strikes = fringe_pitches[fringe_pitches$description == 'called_strike',] 
fringe_balls = fringe_pitches[fringe_pitches$description == 'ball',]
plot(fringe_balls$plate_x, fringe_balls$plate_z, main='Fringe Pitches', xlab='X-coordinate', ylab='Z-coordinate')
points(fringe_strikes$plate_x, fringe_strikes$plate_z, col=2)
zone_plotter(x_sort_strike[(1-a2)/2*xlen], x_sort_strike[(1-(1-a2)/2)*xlen], z_sort_strike[(1-a2)/2*zlen], z_sort_strike[(1-(1-a2)/2)*zlen], 4)
zone_plotter(x_sort_strike[1], x_sort_strike[xlen], z_sort_strike[1], z_sort_strike[zlen], 5) 
legend('topleft', c('ball', 'strike'), fill = c(1,2))
legend('topright', c("1.0", as.character(a2)), fill = c(5,4))

# Now we do analysis on the fringe pitches only
# dim(fringe_pitches) == c(6176, 5) # -> 6176/20528 pitches fall into fringe category
# quick contingency tables faintly suggest strikes are more likely to be called when the count is lower
prop.table(table(fringe_pitches$description, fringe_pitches$balls), margin=2)
prop.table(table(fringe_pitches$description, fringe_pitches$balls+fringe_pitches$strikes), margin=2)
prop.table(table(fringe_pitches[fringe_pitches$balls==3,'description'], fringe_pitches[fringe_pitches$balls==3, 'balls']+fringe_pitches[fringe_pitches$balls==3, 'strikes']), margin=2)

# can we make a model?
# Lets do a random forest model - adaptive boosting takes too long, and 
# format data
desc = as.factor(fringe_pitches$description)
balls = as.numeric(fringe_pitches$balls)
strikes = as.numeric(fringe_pitches$strikes)
xplate = as.numeric(fringe_pitches$plate_x)
zplate = as.numeric(fringe_pitches$plate_z)
fringe_df = data.frame(balls, strikes, xplate, zplate, desc)

library(randomForest)
library(ROCR)
library(pROC)

# cross-validation of models
k = 5
d = dim(fringe_df)
d1 = floor(d[1]/k)*k
indexmat = matrix(sample(1:d1, d1), 5)
auc_rf = auc_lr = matrix(NA, k, 1)
for (i in 1:k){
  index = indexmat[i,]
  train = fringe_df[-index,]
  test = fringe_df[index,]
  
  model_rf = randomForest(desc~balls+strikes+xplate+zplate,
                          data = train, mtry=4, ntree=50, # not much improvement after 50 trees 
                          control=rpart.control(minsplit=2,cp=10e-2))
  model_lr = glm(desc~balls+strikes+xplate+zplate,
                 data = train, family='binomial')
  
  test_rf = predict(model_rf,type="prob", newdata=test)
  pred_rf = prediction(test_rf[,2], test$desc)
  # perf_rf = performance(pred_rf,'tpr', 'fpr') # plot ROC curve
  auc_rf[i] = performance(pred_rf,'auc')@y.values[[1]]
  
  pred_lr = predict.glm(model_lr, test, type = 'response')
  auc_lr[i] = roc(test$desc, pred_lr, plot = F, print.auc = T)$auc
}
# measure performance with auc of roc curves
auc_rf; auc_lr

# plot ROC curve
perf_rf = performance(pred_rf,'tpr', 'fpr') 
plot(perf_rf, col=2, main='ROC of Random Forest Model')
abline(0,1, lty=2)
auc_text = sprintf('AUC = %3.2f', auc_rf[i])
legend('bottomright', auc_text)

# predict probability of strike in 3 cases
# 1. Three balls (3 balls, any strikes)
# 2. Full count (3 balls, 2 strikes)
# 3. Other (<3 balls, any strikes)
three = predict(model_rf,type="prob", newdata=fringe_df[fringe_df$balls==3,])[,2]
full = predict(model_rf,type="prob", newdata=fringe_df[fringe_df$balls==3&fringe_df$strikes==2,])[,2]
other = predict(model_rf,type="prob", newdata=fringe_df[fringe_df$balls!=3,])[,2]

# generate densities using built-in kernel density estimation
three_d = density(three)
full_d = density(full)
other_d = density(other)
# create cdf from densities
three_cd = three_d
three_cd$y = cumsum(three_d$y)/max(cumsum(three_d$y))
full_cd = full_d
full_cd$y = cumsum(full_d$y)/max(cumsum(full_d$y))
other_cd = other_d
other_cd$y = cumsum(other_d$y)/max(cumsum(other_d$y))

# plot histograms and densities together
par(mfrow=c(1,3))
hist(full, probability = T, main='Full Count Probabilities')
lines(full_d, col=2, lwd=2)
hist(three, probability = T, main='3 Balls, Any Strikes Probabilities')
lines(three_d, col=3, lwd=2)
hist(other, probability = T, main='<3 Balls Probabilities')
lines(other_d, col=4, lwd=2)
par(mfrow=c(1,1))

# just densities for comparison
plot(other_d, col=4, main='Density Comparisons', xlab='Probability')
lines(full_d, col=2)
lines(three_d, col=3)
legend('topright', c('Full Count', '3 Balls', 'Other'), fill = c(2, 3, 4))

# cumulative densities
plot(other_cd,  col=4, main='Cumulative Density Comparisons', xlab='Probability')
lines(full_cd, col=2)
lines(three_cd, col=3)
legend('topleft', c('Full Count', '3 Balls', 'Other'), fill = c(2, 3, 4))

# plot empirical cumulative densities
plot(ecdf(three), col=3, main = 'CDF of Probability of "called_strike"', xlim=c(-0.1, 1.1))
par(new=T)
plot(ecdf(full), col=2, main = '', xlim=c(-0.1, 1.1))
par(new=T)
plot(ecdf(other), col=4, main = '', xlim=c(-0.1, 1.1))
legend('topleft', c('Full Count', '3 Balls', 'Other'), fill = c(2, 3, 4))

# 2-sample ks-test and p-val on data
ks.test(full, three)
ks.test(full, other)
ks.test(three, other)

# length(three)
# length(full)
# length(other)

# subsample
three_t = sample(three, 196)
other_t = sample(other, 196)
# 2-sample ks-test and p-val on subsamples
ks.test(full, three_t)
ks.test(full, other_t)
ks.test(three_t, other_t)

# plot empirical cumulative densities on subsamples
plot(ecdf(three_t), col=3, main = 'CDF of Probability of "called_strike" (subsampled)', xlim=c(-0.1, 1.1))
par(new=T)
plot(ecdf(full), col=2, main = '', xlim=c(-0.1, 1.1))
par(new=T)
plot(ecdf(other_t), col=4, main = '', xlim=c(-0.1, 1.1))
legend('topleft', c('Full Count', '3 Balls', 'Other'), fill = c(2, 3, 4))
