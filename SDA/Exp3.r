setwd('D:/21CE1256/Experiment 3')

dataset = Experiment
library(ggplot2)
library(caTools)

set.seed(123)
split = sample.split(dataset$Scores, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

regressor=lm(formula = Scores ~ Hours, data = training_set)

y_pred = predict(regressor, newdata = test_set)

ggplot() +
  geom_point(aes(x = training_set$Hours,
                 y = training_set$Scores),
                 colour = 'red') +
  geom_line(aes(x = training_set$Hours,
                y = predict(regressor, newdata = training_set)),
            colour = 'blue') + 
  ggtitle('Scores vs Experience (Training set)') + 
  xlab('Hours') + 
  ylab('Scores')

ggplot() +
  geom_point(aes(x = test_set$Hours,
                 y = test_set$Scores),
             colour = 'red') +
  geom_line(aes(x = training_set$Hours,
                y = predict(regressor, newdata = training_set)),
            colour = 'blue') + 
  ggtitle('Scores vs Experience (Test set)') + 
  xlab('Hours') + 
  ylab('Scores')

relation <- lm(dataset$Scores~dataset$Hours)
a <- data.frame(Hours = 10)
result <- predict(regressor, a)
print(result)
