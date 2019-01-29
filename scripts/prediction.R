
### The Model

# random seed 
set.seed(992)

# deploy your model
model_rf <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked, data = train.cl)

# Graph model error
plot(model_rf, ylim=c(0,0.36))


### Prediction

# Use the test data to make a prediction
prediction <- predict(model_rf, test.cl)

# Save the solution to a dataframe and write the solutuion into an excel file
solution <- data.frame(PassengerID = test.cl$PassengerId, Survived = prediction)
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)


