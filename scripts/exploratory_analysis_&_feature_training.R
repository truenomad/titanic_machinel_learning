### Exploratory Analysis & Feature Training 

##Age Vs Survival

# Create an age group column with Child & Adult

train.cl$Agegrp[train.cl$Age < 18] <- 'Child'
train.cl$Agegrp[train.cl$Age >= 18] <- 'Adult'

# Visualise the relationship between age group and Survival
ggplot(train.cl, aes(Agegrp,fill = Survived)) +
  geom_bar() +
  labs(title = "Age-groups Vs Suvival rate") +
  labs(xlab("Age-Group")) +
  labs(ylab("Count"))+
  facet_grid(.~Sex)

# Make a 2x2 table having Children as the reference group
tab.age <- table(train.cl$Agegrp, train$Survived)
tab.age
# Check out the OR to see the strength of the association between Age group and suvival rate.
epi.2by2(tab.age, method="cohort.count")

# Children 1.75 times more likely to survive than adults, our 95% Confidence Interval is from 1.20 to 2.54.


#### Sex Vs Survival 

ggplot(train.cl, aes(Sex,fill = Survived)) +
  geom_bar() +
  labs(title = "Gender Vs Suvival rate") +
  labs(xlab("Gender")) +
  labs(ylab("Count"))
# Female survival rate appears to be greater then male survival rate.

# Odds Ratio and Chi2 test to see the likelihood of surival rate based on sex
# First, make a 2x2 table having Females as the referene group.
tab.sex <- table(train$Sex, train$Survived)
tab2.sex <- cbind(tab.sex[,2], tab.sex[,1]) 
colnames(tab2.sex) <- c("Survived", "Not Survived")
tab2.sex

#Now do the OR and chi2 test
epi.2by2(tab2.sex, method="cohort.count")

# The odds of a female surviving is 12.4 times the odds of a male survivng and we can be 95% certain that this values lies between 8.9 & 17.1. From this we can say that there is a strong association between Gender and Survival.

#### Pclass vs Suvival

ggplot(train.cl, aes(Pclass,fill = Survived)) +
  geom_bar() +
  labs(title = "Ticket Class Vs Suvival rate") +
  labs(xlab("Ticket Class")) +
  labs(ylab("Count"))

# Those in the lower Pclasses have a higher survival rate than those in the lower Pclass.

# Family Size vs Survival
ggplot(train.cl, aes(x=Parch, fill=Survived)) +
  geom_histogram(stat = "count")+
  labs(title = "Family Size (Parents and Children) Vs Suvival rate") +
  labs(xlab("Family Size")) +
  labs(ylab("Count"))

#Suprisingly those with smaller family size have a lower survival rate compared to this bigger family size.

#### Family Size vs Suvival


ggplot(train.cl, aes(x=SibSp, fill=Survived)) +
  geom_histogram(stat = "count") +
  labs(title = "Family Size (Siblings and spouses) Vs Suvival rate") +
  labs(xlab("Family Size")) +
  labs(ylab("Count"))

# Since Pclass SibSp are similiar and have the same distribution, we can cobine them together.

family <- train.cl$SibSp + train.cl$Parch

ggplot(train.cl, aes(x=family, fill=Survived)) +
  geom_histogram(stat = "count") +
  labs(title = "Family Size Vs Suvival rate") +
  labs(xlab("Family Size")) +
  labs(ylab("Count"))        

# Those with little to none family members appear have a higher survival rate than those with bigger families.