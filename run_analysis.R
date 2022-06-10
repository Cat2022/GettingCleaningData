
# Reading in Test and Train -----------------------------------------------

#Test Data
TimeFreqDom<- read.table('test/X_test.txt')
Activity<- read.table('test/Y_test.txt')
Subject<- read.table('test/subject_test.txt') 

#Renaming test columns before merge

colnames(Subject)<- 'Subject'
colnames(Activity)<- 'Activity'

TestData<- cbind(Activity,Subject,TimeFreqDom)
rm(TimeFreqDom,Activity,Subject)

#Reading Train
TimeFreqDom<- read.table('train/X_train.txt')
Activity<- read.table('train/Y_train.txt')
Subject<- read.table('train/subject_train.txt') 

#Renaming train columns before merge

colnames(Subject)<- 'Subject'
colnames(Activity)<- 'Activity'

TrainData<- cbind(Activity,Subject,TimeFreqDom)
rm(TimeFreqDom,Activity,Subject)

TestTrain<- rbind(TestData, TrainData)
rm(TestData, TrainData)
# Cleaning Data -----------------------------------------------------------

# Creating a lookup for Activity

Activity<- read.table('activity_labels.txt')
LookupActivity <- Activity$V2
names(LookupActivity)<- Activity$V1

# Keeping only the mean and standard deviation

Features<- read.table('features.txt', fill = T)
colnames(TestTrain)[3:563]<- Features$V2
Features<- subset(Features, grepl('mean',Features$V2)|grepl('std', Features$V2) )
Features$V1<- Features$V1+2

TestTrain<- TestTrain[,c(1,2,Features$V1)]

TestTrain$Activity<- LookupActivity[TestTrain$Activity]
rm(LookupActivity,Features,Activity)


# Clean Data Set  ---------------------------------------------------------

#Finding all the combinations of the person and activity
PersonActivityCombo <- unique.data.frame(TestTrain[,c(1,2)])
PersonActivityCombo$CombinedKey<- paste(PersonActivityCombo$Activity, '&',PersonActivityCombo$Subject, sep = '')

#Creating an empty DF to put the means of each activity into 
PersonActivityCombo2<- data.frame(row.names = colnames(TestTrain[-c(1:2)]))

#Function that subsets the main df for each subject and activity combo then creates a new DF of means
#This DF is then merged with the master DF to contain the means for all of the person/activity combos
for(combo in PersonActivityCombo$CombinedKey){
  person<- unlist(strsplit(combo,split = '&'))[2]
  activity <- unlist(strsplit(combo,split = '&'))[1]
  x<- subset(TestTrain, TestTrain$Activity==activity & TestTrain$Subject==person)
  z<- as.data.frame(apply(x[-c(1:2)], MARGIN = 2, mean))
  colnames(z)<- combo
  PersonActivityCombo2<<- cbind(PersonActivityCombo2,z)
}

TidyNames<- as.data.frame(rownames(PersonActivityCombo2))
TidyNames$`rownames(PersonActivityCombo2)`<- sapply(TidyNames$`rownames(PersonActivityCombo2)`, function(x){
  FirstLetter<- substr(x,1,1)
  FirstLetter<- gsub('t', 'Time', FirstLetter)
  FirstLetter<- gsub('f','FrequencyDomainSignals', FirstLetter)
  Final<- paste(FirstLetter,substr(x,2,nchar(x)), sep = '')
  return(Final)
})

rownames(PersonActivityCombo2)<- TidyNames$`rownames(PersonActivityCombo2)`
