install.packages("ggplot2")
install.packages("lattice")
install.packages("car")
install.packages("randomForest")
install.packages("lmtest")
install.packages("kableExtra")
library(ggplot2)
library(lattice)
library(car)
library(randomForest)
library(lmtest)
library(kableExtra)

################################################################################
#Importing the datasets
################################################################################

exam_results<-read.csv("exam_results.csv")
ICA_results<-read.csv("STAT0003_ICA__results.csv")
moodle_data<-read.csv("Merged_Moodle_data_STAT0003.csv")
exercise_submissions<-read.csv("STAT0003_exericse_submission.csv")
attendance<-read.csv("attendance_STAT0003_2022.csv")

################################################################################
#Cleaning the datasets
################################################################################

########################################################
#1. Exam results
########################################################

exam_results$Student_id<-as.character(exam_results$Student_id) #Student ID is turned into characters
exam_results$STAT0003_exam<-as.numeric(exam_results$STAT0003_exam) #Exam results are turned into numeric

########################################################
#2. ICA results
########################################################

ICA_results$Student_id<-as.character(ICA_results$Student_id) #Student ID is turned into characters
ICA_results$STAT0003_ICA<-as.numeric(ICA_results$STAT0003_ICA) #ICA results are turned into numeric

########################################################
#3. Exercise submissions
########################################################

exercise_submissions$Student_id<-as.character(exercise_submissions$Student_id) #Student ID is turned into characters
exercise_submissions$Week_2<-as.factor(exercise_submissions$Week_2) #Week 2 is turned into labels
exercise_submissions$Week_3<-as.factor(exercise_submissions$Week_3) #Week 3 is turned into labels
exercise_submissions$Week_4<-as.factor(exercise_submissions$Week_4) #Week 4 is turned into labels
exercise_submissions$Week_5<-as.factor(exercise_submissions$Week_5) #Week 5 is turned into labels
exercise_submissions$Week_7<-as.factor(exercise_submissions$Week_7) #Week 7 is turned into labels
exercise_submissions$Week_8<-as.factor(exercise_submissions$Week_8) #Week 8 is turned into labels
exercise_submissions$count<-as.numeric(exercise_submissions$count) #Exercise submission count is turned into numeric

########################################################
#4. Moodle data
########################################################

unique(moodle_data$Action)
unique(moodle_data$Moodle_entry)
unique(moodle_data$Description)
unique(moodle_data$Interactions)

#Remove the lines which involve "discussion deleted", "post deleted", "subscription deleted" from Action 
#column as they are not meaningful data to indicate whether a student is productive
moodle_data<-moodle_data[!grepl("Post deleted",moodle_data$Action),]
moodle_data<-moodle_data[!grepl("Discussion deleted",moodle_data$Action),]
moodle_data<-moodle_data[!grepl("Subscription deleted",moodle_data$Action),]
moodle_data<-moodle_data[!grepl("URL: Module information",moodle_data$Moodle_entry),]
moodle_data<-moodle_data[!grepl("Code",moodle_data$Moodle_entry),]
moodle_data

#4.1 Created another two columns using date_time column - month and time of day

#Extract the time from the date_time column
moodle_data$Date_time <- as.POSIXct(moodle_data$Date_time, format="%d/%m/%y, %H:%M")
moodle_data$Time<-format(moodle_data$Date_time,"%H:%M") #Time is extracted from the date-time

#Cut off data from the 2022-05-04 onwards as it is not relevant to the current analysis.
#The exam date was 2022-05-04.
#Cut off data before 2022-01-08 as it is not relevant to the current analysis.
moodle_data<-moodle_data[moodle_data$Date_time<"2022-05-04",]
moodle_data<-moodle_data[moodle_data$Date_time>="2022-01-08",]

#Turn the time column into respective categories (Morning, Afternoon, Evening, Late Evening)
moodle_data$Time<-ifelse(moodle_data$Time>="05:00" & moodle_data$Time<="12:00","Morning",
                         ifelse(moodle_data$Time>="12:01" & moodle_data$Time<="17:00","Afternoon",
                                ifelse(moodle_data$Time>="17:01" & moodle_data$Time<="22:00","Evening","Late Evening")))
#Make the time column into a factor
moodle_data$Time<-as.factor(moodle_data$Time)




moodle_data$week<-ifelse(moodle_data$Date_time>="2022-01-08" & moodle_data$Date_time<"2022-01-10","Week 0",
             ifelse(moodle_data$Date_time>="2022-01-10" & moodle_data$Date_time<"2022-01-17","Week 1",
                    ifelse(moodle_data$Date_time>="2022-01-17" & moodle_data$Date_time<"2022-01-24","Week 2",
                           ifelse(moodle_data$Date_time>="2022-01-24" & moodle_data$Date_time<"2022-01-31","Week 3",
                                  ifelse(moodle_data$Date_time>="2022-01-31" & moodle_data$Date_time<"2022-02-07","Week 4",
                                         ifelse(moodle_data$Date_time>="2022-02-07" & moodle_data$Date_time<"2022-02-14","Week 5",
                                                ifelse(moodle_data$Date_time>="2022-02-14" & moodle_data$Date_time<"2022-02-21","Reading Week",
                                                       ifelse(moodle_data$Date_time>="2022-02-21" & moodle_data$Date_time<"2022-02-28","Week 6",
                                                              ifelse(moodle_data$Date_time>="2022-02-28" & moodle_data$Date_time<"2022-03-07","Week 7",
                                                                     ifelse(moodle_data$Date_time>="2022-03-07" & moodle_data$Date_time<"2022-03-14","Week 8",
                                                                            ifelse(moodle_data$Date_time>="2022-03-14" & moodle_data$Date_time<"2022-03-21","Week 9",
                                                                                   ifelse(moodle_data$Date_time>="2022-03-21" & moodle_data$Date_time<"2022-03-28","Week 10",
                                                                                          ifelse(moodle_data$Date_time>="2022-03-28" & moodle_data$Date_time<"2022-04-25","Easter Holidays",
                                                                                                 ifelse(moodle_data$Date_time>="2022-04-25" & moodle_data$Date_time<"2022-05-04","Exam Season","Other"))))))))))))))

# Create another column for the weeks in Easter holiday and exam season
moodle_data$Easter_exam_week<-ifelse(moodle_data$Date_time>="2022-03-28" & moodle_data$Date_time<"2022-04-04","Easter Week 1",
                                  ifelse(moodle_data$Date_time>="2022-04-04" & moodle_data$Date_time<"2022-04-11","Easter Week 2",
                                         ifelse(moodle_data$Date_time>="2022-04-11" & moodle_data$Date_time<"2022-04-18","Easter Week 3",
                                              ifelse(moodle_data$Date_time>="2022-04-18" & moodle_data$Date_time<"2022-04-25","Easter Week 4", 
                                                    ifelse(moodle_data$Date_time>="2022-04-25" & moodle_data$Date_time<"2022-05-02","Exam Season Week 1",
                                                            ifelse(moodle_data$Date_time>="2022-05-02" & moodle_data$Date_time<"2022-05-09","Exam Season Week 2","Other"))))))

#4.2 Created a column Interactions – this column categorizes the type of interactions 
#through either quiz, video, discussion forum, notes, code ,live session, URL module information and slides.
moodle_data$Interactions<-ifelse(grepl("quiz",moodle_data$Moodle_entry, ignore.case = TRUE),"Quiz",
                                 ifelse(grepl("Video",moodle_data$Moodle_entry, ignore.case = TRUE),"Video",
                                        ifelse(grepl("Discussion forum",moodle_data$Moodle_entry, ignore.case = TRUE),"Forum",
                                               ifelse(grepl("notes",moodle_data$Moodle_entry, ignore.case = TRUE),"Notes",
                                                      ifelse(grepl("code",moodle_data$Moodle_entry, ignore.case = TRUE),"Code",
                                                             ifelse(grepl("live session",moodle_data$Moodle_entry, ignore.case = TRUE),"Live Session", 
                                                                    ifelse(grepl("URL",moodle_data$Moodle_entry, ignore.case = TRUE),"URL", 
                                                                        ifelse(grepl("slides",moodle_data$Moodle_entry, ignore.case = TRUE),"Slides","Other"))))))))



moodle_data<-moodle_data[!grepl("Code",moodle_data$Interactions),]

########################################################
#5 Attendance
########################################################


attendance$Number.attended[attendance$Number.attended==0]<-NA #0 is turned into NA

attendance$Tutorial.group <- factor(attendance$Tutorial.group,
                                    levels = c(1:11),
                                    labels = c(1:11),
                                    ordered = TRUE) #Tutorial group is turned into a factor and categorised groups 1 to 11

attendance$Date <- as.Date(paste(attendance$Month, attendance$Day, "2022"), 
                           format="%b %d %Y")
attendance$week<-ifelse(attendance$Date>="2022-01-08" & attendance$Date<"2022-01-10","Week 0",
             ifelse(attendance$Date>="2022-01-10" & attendance$Date<"2022-01-17","Week 1",
                    ifelse(attendance$Date>="2022-01-17" & attendance$Date<"2022-01-24","Week 2",
                           ifelse(attendance$Date>="2022-01-24" & attendance$Date<"2022-01-31","Week 3",
                                  ifelse(attendance$Date>="2022-01-31" & attendance$Date<"2022-02-07","Week 4",
                                         ifelse(attendance$Date>="2022-02-07" & attendance$Date<"2022-02-14","Week 5",
                                                ifelse(attendance$Date>="2022-02-14" & attendance$Date<"2022-02-21","Reading Week",
                                                       ifelse(attendance$Date>="2022-02-21" & attendance$Date<"2022-02-28","Week 6",
                                                              ifelse(attendance$Date>="2022-02-28" & attendance$Date<"2022-03-07","Week 7",
                                                                     ifelse(attendance$Date>="2022-03-07" & attendance$Date<"2022-03-14","Week 8",
                                                                            ifelse(attendance$Date>="2022-03-14" & attendance$Date<"2022-03-21","Week 9",
                                                                                   ifelse(attendance$Date>="2022-03-21" & attendance$Date<"2022-03-28","Week 10",
                                                                                          ifelse(attendance$Date>="2022-03-28" & attendance$Date<"2022-04-25","Easter Holidays",
                                                                                                 ifelse(attendance$Date>="2022-04-25" & attendance$Date<"2022-05-04","Exam Season","Other"))))))))))))))

# Converted "week" column to factor to ensure chronological order
attendance$week <- factor(attendance$week, levels = c("Week 0", "Week 1", "Week 2", "Week 3", "Week 4", "Week 5", 
                                                      "Reading Week", "Week 6", "Week 7", "Week 8", "Week 9", 
                                                      "Week 10", "Easter Holidays", "Exam Season"))

#Remove the columns day and month as they are no longer needed
attendance$Day<-NULL
attendance$Month<-NULL

########################################################
#Combined the cleaned datasets (Exam results, ICA results, Exercise submissions)
########################################################

#Firstly, combined exam results and ICA results
combined_data_v1<-merge(exam_results,ICA_results,by="Student_id")
#Then combined the combined data with exercise submissions
combined_data<-merge(combined_data_v1,exercise_submissions,by="Student_id")


################################################################################
#7.EDA - Exploratory Data Analysis using my shiny app
################################################################################

#### Interactions 6.

#A summary statistic to show how number of interactions per students varied widely

interaction_count<-table(moodle_data$Student_id) #Count the number of interactions for each student
interaction_count<-data.frame(Student_id=names(interaction_count),count=interaction_count) #Create a dataframe
interaction_count$count.Var1<-NULL #Removed the column count.Var1 as it provided no use


#median interaction count
median(interaction_count$count)
#standard deviation of interaction count
sd(interaction_count$count)
#mean interaction count
mean(interaction_count$count)
#range of interaction count
range(interaction_count$count)
#quartiles of interaction count
quantile(interaction_count$count)

mean_value <- mean(interaction_count$count)

# Create ggplot boxplot with a mean line
boxplot_interaction <- ggplot(interaction_count, aes(x = "1", y = interaction_count$count)) +
  geom_boxplot(outlier.color = "black", fill = "white", color = "black") +  # Boxplot
  geom_hline(aes(yintercept = mean_value, color = "Mean"), linetype = "solid", size = 1) +  # Mean line
  scale_color_manual(name = "", values = c("Mean" = "red")) +  # Adding the legend
  labs(title = "Total number of interactions per student", 
       y = "Number of Interactions", x = "") +
  theme_minimal() +
  #Top-right corner legend
  theme(legend.position = c(0.9,0.9),  # Top-right corner
        legend.background = element_rect(color = "black", fill = NA),
        legend.title=element_blank())  
# Print the plot
print(boxplot_interaction)

#Percentage of each type of interaction
quiz_percentage<-nrow(moodle_data[moodle_data$Interactions=="Quiz",])/nrow(moodle_data)*100
video_percentage<-nrow(moodle_data[moodle_data$Interactions=="Video",])/nrow(moodle_data)*100
forum_percentage<-nrow(moodle_data[moodle_data$Interactions=="Forum",])/nrow(moodle_data)*100
notes_percentage<-nrow(moodle_data[moodle_data$Interactions=="Notes",])/nrow(moodle_data)*100
code_percentage<-nrow(moodle_data[moodle_data$Interactions=="Code",])/nrow(moodle_data)*100
live_session_percentage<-nrow(moodle_data[moodle_data$Interactions=="Live Session",])/nrow(moodle_data)*100
slides_percentage<-nrow(moodle_data[moodle_data$Interactions=="Slides",])/nrow(moodle_data)*100


#Create a new column which counts the number of Ys
exercise_submissions$Y_count<-rowSums(exercise_submissions[,2:7]=="Y")
boxplot(combined_data$STAT0003_exam~combined_data$count,main="Exam Results vs Number of Ys",xlab="Number of Ys",ylab="Exam Results (%)")

head(moodle_data)

#Create 5 scatterplots where we each barplot represents the students having 5/5 Ys, 4/5 Ys, 3/5 Ys, 2/5 Ys, 1/5 Ys. The Y axis is the exam data from 0 to 100% and exlcude NA values
#Use the count column as it represnts the number of Ys and exam results as the Y axis
scatterplot_ES<-ggplot(data=combined_data,aes(x=count,y=STAT0003_exam))+geom_point()+labs(x="Number of Ys",y="Exam Results (%)",title="Exam Results vs Number of Ys")
print(scatterplot_ES)
#a lot more studetns get higher than 60% in the exam when they have 5/5 Ys compared to 1/5 Ys
scatterplot2_ES<-ggplot(data=combined_data,aes(x=count,y=STAT0003_ICA))+geom_point()+labs(x="Number of Ys",y="ICA Results (%)",title="ICA Results vs Number of Ys")
print(scatterplot2_ES)

# Ensure all possible counts (0 to 6) are represented in the data
combined_data$count <- factor(combined_data$count, levels = 0:6)

# Create the dotplot
Exam_vs_Ys<-ggplot(combined_data, aes(x = factor(count, levels = 0:6), y = STAT0003_exam)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) + 
  labs(
    title = "Exam Results (%) vs Number of Ys",
    x = "Number of Ys",
    y = "Exam Results (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
print(Exam_vs_Ys)

ICA_vs_Ys<-ggplot(combined_data, aes(x = factor(count, levels = 0:6), y = STAT0003_ICA)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) + 
  labs(
    title = "ICA Results (%) vs Number of Ys",
    x = "Number of Ys",
    y = "ICA Results (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
print(ICA_vs_Ys)

#Find the number of studnets with 6 submissions
nrow(exercise_submissions[exercise_submissions$count==6,])
nrow(exercise_submissions[exercise_submissions$count==5,])

#Scatterplot between exam and ICA results?
ggplot(combined_data,aes(x=STAT0003_ICA, y=STAT0003_exam))+geom_point()+labs(x="ICA Results (%)",y="Exam Results (%)",title="Exam Results vs ICA Results")

#correlation between exam and ICA results
cor(combined_data$STAT0003_ICA,combined_data$STAT0003_exam, use = "complete.obs")
#They are highly correlated so figure out the model to predict exam results based on ICA results

# Ensure Student_id is consistent
moodle_data$Student_id <- as.character(moodle_data$Student_id)

# Define weeks and their corresponding lecture dates
week_dates <- data.frame(
  Week = c("Week 5","Week 6", "Week 7", "Week 8", "Week 9", "Week 10"),
  Start_Date = as.Date(c("2022-01-31", "2022-02-07", "2022-02-21", "2022-02-28", "2022-03-07", "2022-03-14")),
  Lecture_Date = as.Date(c("2022-02-07", "2022-02-21", "2022-02-28", "2022-03-07","2022-03-14", "2022-03-21"))
)

# Initialize a list to store results for each week
weekly_video_data <- list()

for (i in 1:nrow(week_dates)) {
  # Filter Moodle data for the current week
  current_week_data <- subset(
    moodle_data,
    Date_time >= as.POSIXct(week_dates$Start_Date[i]) &
      Date_time < as.POSIXct(week_dates$Lecture_Date[i]) &
      Interactions == "Video" &
      Action == "Course activity completion updated"
  )
  
  # Count videos watched per student
  week_videos <- aggregate(Date_time ~ Student_id, data = current_week_data, length)
  colnames(week_videos) <- c("Student_id", paste0("Videos_Watched__Pre_Lecture_", week_dates$Week[i]))
  
  # Store results in the list
  weekly_video_data[[i]] <- week_videos
}

# Merge weekly results into a single dataset
final_video_data <- Reduce(function(x, y) merge(x, y, by = "Student_id", all = TRUE), weekly_video_data)

# Replace NA values with 0 (indicating no videos watched for those weeks)
final_video_data[is.na(final_video_data)] <- 0

# Add a column for the total number of videos watched across all weeks
final_video_data$Total_Videos_Watched <- rowSums(final_video_data[ , -1])

# Display the final dataset
head(final_video_data)

#mean of exam scores
mean(combined_data$STAT0003_exam,na.rm=TRUE)

#Proportion of videos watched in that respective week rather than before lectures!!!!!!!
#week 1
week1_list<-unique(moodle_data[moodle_data$week=="Week 1" & moodle_data$Interactions=="Video","Moodle_entry"])
week1_list
#week 2 but remove what ever is the week1_list
week2_list<-unique(moodle_data[moodle_data$week=="Week 2" & moodle_data$Interactions=="Video" & !moodle_data$Moodle_entry %in% week1_list,"Moodle_entry"])
week2_list
#week 3 but remove what ever is the week1_list and week2_list
week3_list<-unique(moodle_data[moodle_data$week=="Week 3" & moodle_data$Interactions=="Video" & !moodle_data$Moodle_entry %in% week1_list & !moodle_data$Moodle_entry %in% week2_list,"Moodle_entry"])
week3_list
#week 4 but remove what ever is the week1_list, week2_list and week3_list
week4_list<-unique(moodle_data[moodle_data$week=="Week 4" & moodle_data$Interactions=="Video" & !moodle_data$Moodle_entry %in% week1_list & !moodle_data$Moodle_entry %in% week2_list & !moodle_data$Moodle_entry %in% week3_list,"Moodle_entry"])
week4_list
#week 5 but remove what ever is the week1_list, week2_list, week3_list and week4_list but is also before the 22-02-07
week5_list<-unique(moodle_data[moodle_data$week=="Week 5" & moodle_data$Interactions=="Video" & !moodle_data$Moodle_entry %in% week1_list & !moodle_data$Moodle_entry %in% week2_list & !moodle_data$Moodle_entry %in% week3_list & !moodle_data$Moodle_entry %in% week4_list,"Moodle_entry"])
week5_list
#week 6 but remove what ever is the week1_list, week2_list, week3_list, week4_list and week5_list
week6_list<-unique(moodle_data[moodle_data$week=="Week 6" & moodle_data$Interactions=="Video" & !moodle_data$Moodle_entry %in% week1_list & !moodle_data$Moodle_entry %in% week2_list & !moodle_data$Moodle_entry %in% week3_list & !moodle_data$Moodle_entry %in% week4_list & !moodle_data$Moodle_entry %in% week5_list,"Moodle_entry"])
week6_list
#week 7 but remove what ever is the week1_list, week2_list, week3_list, week4_list, week5_list and week6_list
week7_list<-unique(moodle_data[moodle_data$week=="Week 7" & moodle_data$Interactions=="Video" & !moodle_data$Moodle_entry %in% week1_list & !moodle_data$Moodle_entry %in% week2_list & !moodle_data$Moodle_entry %in% week3_list & !moodle_data$Moodle_entry %in% week4_list & !moodle_data$Moodle_entry %in% week5_list & !moodle_data$Moodle_entry %in% week6_list,"Moodle_entry"])
week7_list
#week 8 but remove what ever is the week1_list, week2_list, week3_list, week4_list, week5_list, week6_list and week7_list
week8_list<-unique(moodle_data[moodle_data$week=="Week 8" & moodle_data$Interactions=="Video" & !moodle_data$Moodle_entry %in% week1_list & !moodle_data$Moodle_entry %in% week2_list & !moodle_data$Moodle_entry %in% week3_list & !moodle_data$Moodle_entry %in% week4_list & !moodle_data$Moodle_entry %in% week5_list & !moodle_data$Moodle_entry %in% week6_list & !moodle_data$Moodle_entry %in% week7_list,"Moodle_entry"])
week8_list
#week 9 but remove what ever is the week1_list, week2_list, week3_list, week4_list, week5_list, week6_list, week7_list and week8_list
week9_list<-unique(moodle_data[moodle_data$week=="Week 9" & moodle_data$Interactions=="Video" & !moodle_data$Moodle_entry %in% week1_list & !moodle_data$Moodle_entry %in% week2_list & !moodle_data$Moodle_entry %in% week3_list & !moodle_data$Moodle_entry %in% week4_list & !moodle_data$Moodle_entry %in% week5_list & !moodle_data$Moodle_entry %in% week6_list & !moodle_data$Moodle_entry %in% week7_list & !moodle_data$Moodle_entry %in% week8_list,"Moodle_entry"])
week9_list
#week 10 but remove what ever is the week1_list, week2_list, week3_list, week4_list, week5_list, week6_list, week7_list, week8_list and week9_list
week10_list<-unique(moodle_data[moodle_data$week=="Week 10" & moodle_data$Interactions=="Video" & !moodle_data$Moodle_entry %in% week1_list & !moodle_data$Moodle_entry %in% week2_list & !moodle_data$Moodle_entry %in% week3_list & !moodle_data$Moodle_entry %in% week4_list & !moodle_data$Moodle_entry %in% week5_list & !moodle_data$Moodle_entry %in% week6_list & !moodle_data$Moodle_entry %in% week7_list & !moodle_data$Moodle_entry %in% week8_list & !moodle_data$Moodle_entry %in% week9_list,"Moodle_entry"])
week10_list

#Combine the interaction_count table and combined_data
combined_data3<-merge(combined_data,interaction_count,by="Student_id", all=TRUE)

#Find the total number of interactions with discussion forums created and merge the dataset with combined data
discussion_forums<-moodle_data[moodle_data$Interactions=="Forum",]
discussion_forums<-table(discussion_forums$Student_id)
discussion_forums<-data.frame(Student_id=names(discussion_forums),count=discussion_forums)
discussion_forums$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(discussion_forums)[names(discussion_forums) == "count.Freq"] <- "Discussion_Forum_Interactions"
combined_data4<-merge(combined_data3,discussion_forums,by="Student_id", all=TRUE)

#Find the total number of interactions with "quiz attempt summary reviewed" and "quiz attempt reviewed" and merge the dataset with combined data
quiz_reviewed<-moodle_data[moodle_data$Action=="Quiz attempt reviewed",]
quiz_reviewed<-table(quiz_reviewed$Student_id)
quiz_reviewed<-data.frame(Student_id=names(quiz_reviewed),count=quiz_reviewed)
quiz_reviewed$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(quiz_reviewed)[names(quiz_reviewed) == "count.Freq"] <- "Quiz_Review_Interactions"
combined_data5<-merge(combined_data4,quiz_reviewed,by="Student_id", all=TRUE)

#Find the total number of interactions with "quiz attempt submitted" merge the dataset with combined data
quiz_submitted<-moodle_data[moodle_data$Action=="Quiz attempt submitted",]
quiz_submitted<-table(quiz_submitted$Student_id)
quiz_submitted<-data.frame(Student_id=names(quiz_submitted),count=quiz_submitted)
quiz_submitted$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(quiz_submitted)[names(quiz_submitted) == "count.Freq"] <- "Quiz_Submitted_Interactions"
combined_data5<-merge(combined_data5,quiz_submitted,by="Student_id", all=TRUE)

quiz_attempt_summary_viewed<-moodle_data[moodle_data$Action=="Quiz attempt summary viewed",]
quiz_attempt_summary_viewed<-table(quiz_attempt_summary_viewed$Student_id)
quiz_attempt_summary_viewed<-data.frame(Student_id=names(quiz_attempt_summary_viewed),count=quiz_attempt_summary_viewed)
quiz_attempt_summary_viewed$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
#turn NA values to 0
quiz_attempt_summary_viewed[is.na(quiz_attempt_summary_viewed)]<-0
names(quiz_attempt_summary_viewed)[names(quiz_attempt_summary_viewed) == "count.Freq"] <- "Quiz_Attempt_Summary_Viewed"
quiz_attempt_summary_viewed
combined_data6<-merge(combined_data5,quiz_attempt_summary_viewed,by="Student_id", all=TRUE)

#Find the total number of interactions with "videos"and merge the dataset with combined_data5
videos<-moodle_data[moodle_data$Interactions=="Video",]
videos<-table(videos$Student_id)
videos<-data.frame(Student_id=names(videos),count=videos)
videos$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(videos)[names(videos) == "count.Freq"] <- "Video_Interactions"
combined_data7<-merge(combined_data6,videos,by="Student_id", all=TRUE)

#Find the total number of interactions with "Quiz: 1.1 Mathematics refresher quiz" and merge the dataset with combined_data7
maths_quiz<-moodle_data[moodle_data$Moodle_entry=="Quiz: 1.1 Mathematics refresher quiz",]
maths_quiz<-table(maths_quiz$Student_id)
maths_quiz<-data.frame(Student_id=names(maths_quiz),count=maths_quiz)
maths_quiz$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(maths_quiz)[names(maths_quiz) == "count.Freq"] <- "Maths_Quiz_Interactions"
combined_data8<-merge(combined_data7,maths_quiz,by="Student_id", all=TRUE)

#Find the proportion of interactions in the afternoon, evening, late evening and morning and  and merge the dataset with combined_data8
Morning<-moodle_data[moodle_data$Time=="Morning",]
Morning<-table(Morning$Student_id)
Morning<-data.frame(Student_id=names(Morning),count=Morning)
Morning$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(Morning)[names(Morning) == "count.Freq"] <- "Morning_Interactions"
combined_data9<-merge(combined_data8,Morning,by="Student_id", all=TRUE)

Afternoon<-moodle_data[moodle_data$Time=="Afternoon",]
Afternoon<-table(Afternoon$Student_id)
Afternoon<-data.frame(Student_id=names(Afternoon),count=Afternoon)
Afternoon$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(Afternoon)[names(Afternoon) == "count.Freq"] <- "Afternoon_Interactions"
combined_data10<-merge(combined_data9,Afternoon,by="Student_id", all=TRUE)

Evening<-moodle_data[moodle_data$Time=="Evening",]
Evening<-table(Evening$Student_id)
Evening<-data.frame(Student_id=names(Evening),count=Evening)
Evening$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(Evening)[names(Evening) == "count.Freq"] <- "Evening_Interactions"
combined_data11<-merge(combined_data10,Evening,by="Student_id", all=TRUE)

Late_evening<-moodle_data[moodle_data$Time=="Late Evening",]
Late_evening<-table(Late_evening$Student_id)
Late_evening<-data.frame(Student_id=names(Late_evening),count=Late_evening)
Late_evening$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(Late_evening)[names(Late_evening) == "count.Freq"] <- "Late_Evening_Interactions"
combined_data12<-merge(combined_data11,Late_evening,by="Student_id", all=TRUE)

#Find the number of interactions in "Reading week" and merge the dataset with combined_data12
Reading_week<-moodle_data[moodle_data$week=="Reading Week",]
Reading_week<-table(Reading_week$Student_id)
Reading_week<-data.frame(Student_id=names(Reading_week),count=Reading_week)
Reading_week$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(Reading_week)[names(Reading_week) == "count.Freq"] <- "Reading_Week_Interactions"
combined_data13<-merge(combined_data12,Reading_week,by="Student_id", all=TRUE)

#Find the number of interactions for the videos in the week_1 list within only that week 1 and merge the dataset with combined_data13
week1_videos<-moodle_data[moodle_data$Moodle_entry %in% week1_list & moodle_data$Interactions=="Video" & moodle_data$week=="Week 1",]
week1_videos<-table(week1_videos$Student_id)
week1_videos<-data.frame(Student_id=names(week1_videos),count=week1_videos)
week1_videos$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(week1_videos)[names(week1_videos) == "count.Freq"] <- "Week1_Video_Interactions"
combined_data14<-merge(combined_data13,week1_videos,by="Student_id", all=TRUE)

week2_videos<-moodle_data[moodle_data$Moodle_entry %in% week2_list & moodle_data$Interactions=="Video" & moodle_data$week=="Week 2",]
week2_videos<-table(week2_videos$Student_id)
week2_videos<-data.frame(Student_id=names(week2_videos),count=week2_videos)
week2_videos$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(week2_videos)[names(week2_videos) == "count.Freq"] <- "Week2_Video_Interactions"
combined_data15<-merge(combined_data14,week2_videos,by="Student_id", all=TRUE)

week3_videos<-moodle_data[moodle_data$Moodle_entry %in% week3_list & moodle_data$Interactions=="Video" & moodle_data$week=="Week 3",]
week3_videos<-table(week3_videos$Student_id)
week3_videos<-data.frame(Student_id=names(week3_videos),count=week3_videos)
week3_videos$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(week3_videos)[names(week3_videos) == "count.Freq"] <- "Week3_Video_Interactions"
combined_data16<-merge(combined_data15,week3_videos,by="Student_id", all=TRUE)

week4_videos<-moodle_data[moodle_data$Moodle_entry %in% week4_list & moodle_data$Interactions=="Video" & moodle_data$week=="Week 4",]
week4_videos<-table(week4_videos$Student_id)
week4_videos<-data.frame(Student_id=names(week4_videos),count=week4_videos)
week4_videos$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(week4_videos)[names(week4_videos) == "count.Freq"] <- "Week4_Video_Interactions"
combined_data17<-merge(combined_data16,week4_videos,by="Student_id", all=TRUE)

week5_videos<-moodle_data[moodle_data$Moodle_entry %in% week5_list & moodle_data$Interactions=="Video" & moodle_data$week=="Week 5",]
week5_videos<-table(week5_videos$Student_id)
week5_videos<-data.frame(Student_id=names(week5_videos),count=week5_videos)
week5_videos$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(week5_videos)[names(week5_videos) == "count.Freq"] <- "Week5_Video_Interactions"
combined_data18<-merge(combined_data17,week5_videos,by="Student_id", all=TRUE)

week6_videos<-moodle_data[moodle_data$Moodle_entry %in% week6_list & moodle_data$Interactions=="Video" & moodle_data$week=="Week 6",]
week6_videos<-table(week6_videos$Student_id)
week6_videos<-data.frame(Student_id=names(week6_videos),count=week6_videos)
week6_videos$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(week6_videos)[names(week6_videos) == "count.Freq"] <- "Week6_Video_Interactions"
combined_data19<-merge(combined_data18,week6_videos,by="Student_id", all=TRUE)

week7_videos<-moodle_data[moodle_data$Moodle_entry %in% week7_list & moodle_data$Interactions=="Video" & moodle_data$week=="Week 7",]
week7_videos<-table(week7_videos$Student_id)
week7_videos<-data.frame(Student_id=names(week7_videos),count=week7_videos)
week7_videos$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(week7_videos)[names(week7_videos) == "count.Freq"] <- "Week7_Video_Interactions"
combined_data20<-merge(combined_data19,week7_videos,by="Student_id", all=TRUE)

week8_videos<-moodle_data[moodle_data$Moodle_entry %in% week8_list & moodle_data$Interactions=="Video" & moodle_data$week=="Week 8",]
week8_videos<-table(week8_videos$Student_id)
week8_videos<-data.frame(Student_id=names(week8_videos),count=week8_videos)
week8_videos$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(week8_videos)[names(week8_videos) == "count.Freq"] <- "Week8_Video_Interactions"
combined_data21<-merge(combined_data20,week8_videos,by="Student_id", all=TRUE)

week9_videos<-moodle_data[moodle_data$Moodle_entry %in% week9_list & moodle_data$Interactions=="Video" & moodle_data$week=="Week 9",]
week9_videos<-table(week9_videos$Student_id)
week9_videos<-data.frame(Student_id=names(week9_videos),count=week9_videos)
week9_videos$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(week9_videos)[names(week9_videos) == "count.Freq"] <- "Week9_Video_Interactions"
combined_data22<-merge(combined_data21,week9_videos,by="Student_id", all=TRUE)

week10_videos<-moodle_data[moodle_data$Moodle_entry %in% week10_list & moodle_data$Interactions=="Video" & moodle_data$week=="Week 10",]
week10_videos<-table(week10_videos$Student_id)
week10_videos<-data.frame(Student_id=names(week10_videos),count=week10_videos)
week10_videos$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(week10_videos)[names(week10_videos) == "count.Freq"] <- "Week10_Video_Interactions"
combined_data23<-merge(combined_data22,week10_videos,by="Student_id", all=TRUE)

#Find the total number of interactions before reading week so from before and including week 5 and merge with the combined_data23
before_reading_week<-moodle_data[moodle_data$week %in% c("Week 0 ", "Week 1","Week 2","Week 3","Week 4","Week 5"),]
before_reading_week<-table(before_reading_week$Student_id)
before_reading_week<-data.frame(Student_id=names(before_reading_week),count=before_reading_week)
before_reading_week$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(before_reading_week)[names(before_reading_week) == "count.Freq"] <- "Before_Reading_Week_Interactions"
combined_data23<-merge(combined_data23,before_reading_week,by="Student_id", all=TRUE)

#Find the total number of interactions after reading week so from week 6 and to week 10 and merge with the combined_data23
after_reading_week<-moodle_data[moodle_data$week %in% c("Week 6","Week 7","Week 8","Week 9","Week 10"),]
after_reading_week<-table(after_reading_week$Student_id)
after_reading_week<-data.frame(Student_id=names(after_reading_week),count=after_reading_week)
after_reading_week$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(after_reading_week)[names(after_reading_week) == "count.Freq"] <- "After_Reading_Week_Interactions"
combined_data23<-merge(combined_data23,after_reading_week,by="Student_id", all=TRUE)

# Filter moodle_data to only include videos from week1_list, week2_list, week3_list, week4_list, week5_list, week6_list, week7_list, week8_list, week9_list, week10_list
week1_videos_watched <- moodle_data[moodle_data$Moodle_entry %in% week1_list & 
                                      moodle_data$Interactions == "Video", ]
# Count the number of unique videos watched by each student
week1_video_score <- aggregate(Moodle_entry ~ Student_id, data = week1_videos_watched, function(x) length(unique(x)))
# Rename column to indicate it's a score out of 12
colnames(week1_video_score) <- c("Student_id", "Week1_Video_Score")
combined_data23<-merge(combined_data23,week1_video_score,by="Student_id", all=TRUE)

week2_videos_watched <- moodle_data[moodle_data$Moodle_entry %in% week2_list & 
                                      moodle_data$Interactions == "Video", ]
# Count the number of unique videos watched by each student
week2_video_score <- aggregate(Moodle_entry ~ Student_id, data = week2_videos_watched, function(x) length(unique(x)))
# Rename column to indicate it's a score out of 12
colnames(week2_video_score) <- c("Student_id", "Week2_Video_Score")
combined_data23<-merge(combined_data23,week2_video_score,by="Student_id", all=TRUE)

week3_videos_watched <- moodle_data[moodle_data$Moodle_entry %in% week3_list & 
                                      moodle_data$Interactions == "Video", ]
# Count the number of unique videos watched by each student
week3_video_score <- aggregate(Moodle_entry ~ Student_id, data = week3_videos_watched, function(x) length(unique(x)))
# Rename column to indicate it's a score out of 12
colnames(week3_video_score) <- c("Student_id", "Week3_Video_Score")
combined_data23<-merge(combined_data23,week3_video_score,by="Student_id", all=TRUE)

week4_videos_watched <- moodle_data[moodle_data$Moodle_entry %in% week4_list & 
                                      moodle_data$Interactions == "Video", ]
# Count the number of unique videos watched by each student
week4_video_score <- aggregate(Moodle_entry ~ Student_id, data = week4_videos_watched, function(x) length(unique(x)))
# Rename column to indicate it's a score out of 12
colnames(week4_video_score) <- c("Student_id", "Week4_Video_Score")
combined_data23<-merge(combined_data23,week4_video_score,by="Student_id", all=TRUE)

week5_videos_watched <- moodle_data[moodle_data$Moodle_entry %in% week5_list & 
                                      moodle_data$Interactions == "Video", ]
# Count the number of unique videos watched by each student
week5_video_score <- aggregate(Moodle_entry ~ Student_id, data = week5_videos_watched, function(x) length(unique(x)))
# Rename column to indicate it's a score out of 12
colnames(week5_video_score) <- c("Student_id", "Week5_Video_Score")
combined_data23<-merge(combined_data23,week5_video_score,by="Student_id", all=TRUE)

week6_videos_watched <- moodle_data[moodle_data$Moodle_entry %in% week6_list & 
                                      moodle_data$Interactions == "Video", ]
# Count the number of unique videos watched by each student
week6_video_score <- aggregate(Moodle_entry ~ Student_id, data = week6_videos_watched, function(x) length(unique(x)))
# Rename column to indicate it's a score out of 12
colnames(week6_video_score) <- c("Student_id", "Week6_Video_Score")
combined_data23<-merge(combined_data23,week6_video_score,by="Student_id", all=TRUE)

week7_videos_watched <- moodle_data[moodle_data$Moodle_entry %in% week7_list & 
                                      moodle_data$Interactions == "Video", ]
# Count the number of unique videos watched by each student
week7_video_score <- aggregate(Moodle_entry ~ Student_id, data = week7_videos_watched, function(x) length(unique(x)))
# Rename column to indicate it's a score out of 12
colnames(week7_video_score) <- c("Student_id", "Week7_Video_Score")
combined_data23<-merge(combined_data23,week7_video_score,by="Student_id", all=TRUE)

week8_videos_watched <- moodle_data[moodle_data$Moodle_entry %in% week8_list & 
                                      moodle_data$Interactions == "Video", ]
# Count the number of unique videos watched by each student
week8_video_score <- aggregate(Moodle_entry ~ Student_id, data = week8_videos_watched, function(x) length(unique(x)))
# Rename column to indicate it's a score out of 12
colnames(week8_video_score) <- c("Student_id", "Week8_Video_Score")
combined_data23<-merge(combined_data23,week8_video_score,by="Student_id", all=TRUE)

week9_videos_watched <- moodle_data[moodle_data$Moodle_entry %in% week9_list & 
                                      moodle_data$Interactions == "Video", ]
# Count the number of unique videos watched by each student
week9_video_score <- aggregate(Moodle_entry ~ Student_id, data = week9_videos_watched, function(x) length(unique(x)))
# Rename column to indicate it's a score out of 12
colnames(week9_video_score) <- c("Student_id", "Week9_Video_Score")
combined_data23<-merge(combined_data23,week9_video_score,by="Student_id", all=TRUE)

week10_videos_watched <- moodle_data[moodle_data$Moodle_entry %in% week10_list & 
                                      moodle_data$Interactions == "Video", ]
# Count the number of unique videos watched by each student
week10_video_score <- aggregate(Moodle_entry ~ Student_id, data = week10_videos_watched, function(x) length(unique(x)))
# Rename column to indicate it's a score out of 12
colnames(week10_video_score) <- c("Student_id", "Week10_Video_Score")
combined_data23<-merge(combined_data23,week10_video_score,by="Student_id", all=TRUE)

#Find the total number of interactions with discussion forums created and merge the dataset with combined data
discussion_forums_created<-moodle_data[moodle_data$Action=="Discussion created",]
discussion_forums_created<-table(discussion_forums_created$Student_id)
discussion_forums_created<-data.frame(Student_id=names(discussion_forums_created),count=discussion_forums_created)
discussion_forums_created$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(discussion_forums_created)[names(discussion_forums_created) == "count.Freq"] <- "Discussion_Forum_Created"
combined_data23<-merge(combined_data23,discussion_forums_created,by="Student_id", all=TRUE)

#Find the number of interactions in Exam Season and merge with the combined_data23
exam_season<-moodle_data[moodle_data$week %in% "Exam Season",]
exam_season<-table(exam_season$Student_id)
exam_season<-data.frame(Student_id=names(exam_season),count=exam_season)
exam_season$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(exam_season)[names(exam_season) == "count.Freq"] <- "Exam_Season_Interactions"
combined_data23<-merge(combined_data23,exam_season,by="Student_id", all=TRUE)

#Find the number of interactions in Easter and merge with the combined_data23
easter_holidays<-moodle_data[moodle_data$week %in% "Easter Holidays",]
easter_holidays<-table(easter_holidays$Student_id)
easter_holidays<-data.frame(Student_id=names(easter_holidays),count=easter_holidays)
easter_holidays$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(easter_holidays)[names(easter_holidays) == "count.Freq"] <- "Easter_Holidays_Interactions"
combined_data23<-merge(combined_data23,easter_holidays,by="Student_id", all=TRUE)

#Find the number of interactions before week 8 and after week 8 and merge with the combined_data23
before_week8<-moodle_data[moodle_data$week %in% c("Week 0 ", "Week 1","Week 2","Week 3","Week 4","Week 5","Week 6","Week 7"),]
before_week8<-table(before_week8$Student_id)
before_week8<-data.frame(Student_id=names(before_week8),count=before_week8)
before_week8$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(before_week8)[names(before_week8) == "count.Freq"] <- "Before_Week8_Interactions"
combined_data23<-merge(combined_data23,before_week8,by="Student_id", all=TRUE)

after_week8<-moodle_data[moodle_data$week %in% c("Week 9","Week 10", "Easter Holidays", "Exam Season"),]
after_week8<-table(after_week8$Student_id)
after_week8<-data.frame(Student_id=names(after_week8),count=after_week8)
after_week8$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(after_week8)[names(after_week8) == "count.Freq"] <- "After_Week8_Interactions"
combined_data23<-merge(combined_data23,after_week8,by="Student_id", all=TRUE)

#Find the number of interactions between week 6 and week 8 inclusive and merge with the combined_data23
week6_week8<-moodle_data[moodle_data$week %in% c("Week 6","Week 7","Week 8"),]
week6_week8<-table(week6_week8$Student_id)
week6_week8<-data.frame(Student_id=names(week6_week8),count=week6_week8)
week6_week8$count.Var1<-NULL #Removed the column count.Var1 as it provided no use
names(week6_week8)[names(week6_week8) == "count.Freq"] <- "Week6_Week8_Interactions"
combined_data23<-merge(combined_data23,week6_week8,by="Student_id", all=TRUE)

combined_data23$Total_Video_Score <- rowSums(combined_data23[, c("Week1_Video_Score", "Week2_Video_Score", 
                                                                 "Week3_Video_Score", "Week4_Video_Score", 
                                                                 "Week5_Video_Score", "Week6_Video_Score", 
                                                                 "Week7_Video_Score", "Week8_Video_Score" 
                                                                )],na.rm = TRUE)

#Convert all NA values in the columns Discussion_Forum_Interactions,Quiz_Review_Interactions,Quiz_Attempt_Summary_Viewed,Maths_Quiz_Interactions,Afternoon_Interactions,Evening_Interactions to 0
combined_data23$Discussion_Forum_Interactions[is.na(combined_data23$Discussion_Forum_Interactions)]<-0
combined_data23$Quiz_Review_Interactions[is.na(combined_data23$Quiz_Review_Interactions)]<-0
combined_data23$Quiz_Attempt_Summary_Viewed[is.na(combined_data23$Quiz_Attempt_Summary_Viewed)]<-0
combined_data23$Maths_Quiz_Interactions[is.na(combined_data23$Maths_Quiz_Interactions)]<-0
combined_data23$Morning_Interactions[is.na(combined_data23$Morning_Interactions)]<-0
combined_data23$Afternoon_Interactions[is.na(combined_data23$Afternoon_Interactions)]<-0
combined_data23$Evening_Interactions[is.na(combined_data23$Evening_Interactions)]<-0
combined_data23$Late_Evening_Interactions[is.na(combined_data23$Late_Evening_Interactions)]<-0
combined_data23$Reading_Week_Interactions[is.na(combined_data23$Reading_Week_Interactions)]<-0
combined_data23$Week1_Video_Interactions[is.na(combined_data23$Week1_Video_Interactions)]<-0
combined_data23$Week2_Video_Interactions[is.na(combined_data23$Week2_Video_Interactions)]<-0
combined_data23$Week3_Video_Interactions[is.na(combined_data23$Week3_Video_Interactions)]<-0
combined_data23$Week4_Video_Interactions[is.na(combined_data23$Week4_Video_Interactions)]<-0
combined_data23$Week5_Video_Interactions[is.na(combined_data23$Week5_Video_Interactions)]<-0
combined_data23$Week6_Video_Interactions[is.na(combined_data23$Week6_Video_Interactions)]<-0
combined_data23$Week7_Video_Interactions[is.na(combined_data23$Week7_Video_Interactions)]<-0
combined_data23$Week8_Video_Interactions[is.na(combined_data23$Week8_Video_Interactions)]<-0
combined_data23$Week9_Video_Interactions[is.na(combined_data23$Week9_Video_Interactions)]<-0
combined_data23$Week10_Video_Interactions[is.na(combined_data23$Week10_Video_Interactions)]<-0
combined_data23$Before_Reading_Week_Interactions[is.na(combined_data23$Before_Reading_Week_Interactions)]<-0
combined_data23$After_Reading_Week_Interactions[is.na(combined_data23$After_Reading_Week_Interactions)]<-0
combined_data23$Week1_Video_Score[is.na(combined_data23$Week1_Video_Score)]<-0
combined_data23$Week2_Video_Score[is.na(combined_data23$Week2_Video_Score)]<-0
combined_data23$Week3_Video_Score[is.na(combined_data23$Week3_Video_Score)]<-0
combined_data23$Week4_Video_Score[is.na(combined_data23$Week4_Video_Score)]<-0
combined_data23$Week5_Video_Score[is.na(combined_data23$Week5_Video_Score)]<-0
combined_data23$Week6_Video_Score[is.na(combined_data23$Week6_Video_Score)]<-0
combined_data23$Week7_Video_Score[is.na(combined_data23$Week7_Video_Score)]<-0
combined_data23$Week8_Video_Score[is.na(combined_data23$Week8_Video_Score)]<-0
combined_data23$Week9_Video_Score[is.na(combined_data23$Week9_Video_Score)]<-0
combined_data23$Week10_Video_Score[is.na(combined_data23$Week10_Video_Score)]<-0
combined_data23$Discussion_Forum_Created[is.na(combined_data23$Discussion_Forum_Created)]<-0
combined_data23$Exam_Season_Interactions[is.na(combined_data23$Exam_Season_Interactions)]<-0
combined_data23$Easter_Holidays_Interactions[is.na(combined_data23$Easter_Holidays_Interactions)]<-0
combined_data23$Before_Week8_Interactions[is.na(combined_data23$Before_Week8_Interactions)]<-0
combined_data23$After_Week8_Interactions[is.na(combined_data23$After_Week8_Interactions)]<-0
combined_data23$Week6_Week8_Interactions[is.na(combined_data23$Week6_Week8_Interactions)]<-0

#Finding the proportion of interactions during exam season
combined_data23$Exam_Season_Proportion<-combined_data23$Exam_Season_Interactions/combined_data23$count.Freq

#Plot of proportion of interactions during exam season against exam results
plot(combined_data23$Exam_Season_Proportion,combined_data23$STAT0003_exam)
#Conclude - studying in exam season has no effect on exam results

#Finding the proportion of interactions during easter holidays
combined_data23$Easter_Holidays_Proportion<-combined_data23$Easter_Holidays_Interactions/combined_data23$count.Freq

#Plot of proportion of interactions during easter holidays against exam results
plot(combined_data23$Easter_Holidays_Proportion,combined_data23$STAT0003_exam)

#change count to numeric
combined_data23$count<-as.numeric(combined_data23$count)

#Proportion of Afternoon engagements compared to morning , evening and late evenings
combined_data23$Afternoon_Proportion<-combined_data23$Afternoon_Interactions/(combined_data23$Morning_Interactions+combined_data23$Evening_Interactions+combined_data23$Late_Evening_Interactions+combined_data23$Afternoon_Interactions)

#Proportion of Morning engagements compared to afternoon, evening and late evenings
combined_data23$Morning_Proportion<-combined_data23$Morning_Interactions/(combined_data23$Morning_Interactions+combined_data23$Evening_Interactions+combined_data23$Late_Evening_Interactions+combined_data23$Afternoon_Interactions)

#Create a covariate that adds the video scores from week 1 to 5 and merge to combined_data23
combined_data23$Week1_to_week5_video_scores<-combined_data23$Week1_Video_Score+combined_data23$Week2_Video_Score+combined_data23$Week3_Video_Score+combined_data23$Week4_Video_Score+combined_data23$Week5_Video_Score

#Create a covariate that adds the video scores from week 6 to 8 and merge to combined_data23
combined_data23$Week6_to_week8_video_scores<-combined_data23$Week6_Video_Score+combined_data23$Week7_Video_Score+combined_data23$Week8_Video_Score

#Investigate students with students who don't perform well in the ICA but do well in the exam
plot(combined_data23$STAT0003_ICA,combined_data23$STAT0003_exam)

# Define thresholds
low_ICA_threshold <- 70  # Below this is "not performing well"
high_exam_threshold <- 60  # Above this is "doing well"

# Filter students who have low ICA scores but high exam scores
high_exam_low_ICA <- combined_data23[combined_data23$STAT0003_ICA < low_ICA_threshold & 
                                       combined_data23$STAT0003_exam > high_exam_threshold, ]

# Display these students
print(high_exam_low_ICA)

# Compare interactions before and after ICA
high_exam_low_ICA$Before_ICA_Interactions <- high_exam_low_ICA$Week1_Video_Score + 
  high_exam_low_ICA$Week2_Video_Score +
  high_exam_low_ICA$Week3_Video_Score +
  high_exam_low_ICA$Week4_Video_Score

high_exam_low_ICA$After_ICA_Interactions <- high_exam_low_ICA$Week5_Video_Score +
  high_exam_low_ICA$Week6_Video_Score +
  high_exam_low_ICA$Week7_Video_Score +
  high_exam_low_ICA$Week8_Video_Score

# Check if students increased interactions after the ICA
high_exam_low_ICA$Interaction_Change <- high_exam_low_ICA$After_ICA_Interactions - high_exam_low_ICA$Before_ICA_Interactions
print(high_exam_low_ICA[, c("Student_id", "Before_ICA_Interactions", "After_ICA_Interactions", "Interaction_Change")])

ggplot(combined_data23, aes(x = STAT0003_ICA, y = STAT0003_exam)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(data = high_exam_low_ICA, aes(x = STAT0003_ICA, y = STAT0003_exam), color = "blue", size = 3) +
  labs(title = "ICA vs Exam Scores", x = "ICA Score (%)", y = "Exam Score (%)") +
  theme_minimal()
#Blue dots highlights the students who improved in the exams compared to ICA so they achieved below the ICA threshold and achieved higher than the exam threshold

t.test(high_exam_low_ICA$Before_ICA_Interactions, high_exam_low_ICA$After_ICA_Interactions, paired = TRUE)
#p-value =0.003328<0.05 so there is a significant difference in the interactions before and after ICA
#Even though it is a small minority, it is a good indicator if students don't perform too well in the ICA but do well in the exam is to look at the interactions before and after the ICA

model_1<- lm(STAT0003_exam ~ count + STAT0003_ICA +
                    Exam_Season_Proportion + Easter_Holidays_Proportion + Morning_Proportion+  Afternoon_Proportion +
                    Quiz_Review_Interactions + Quiz_Submitted_Interactions + Maths_Quiz_Interactions +
                    Discussion_Forum_Interactions+Discussion_Forum_Created + Reading_Week_Interactions +
                    Week1_Video_Score + Week2_Video_Score + Week3_Video_Score + Week4_Video_Score + 
                    Week5_Video_Score + Week6_Video_Score + Week7_Video_Score + Week8_Video_Score + 
                    Week9_Video_Score + Week10_Video_Score +
                    Before_Reading_Week_Interactions + After_Reading_Week_Interactions
                    ,
                  data = combined_data23)
#After reading week has no impact on the exam results so it is not included in model 2 and before_Reading week is more 
#significant than after_reading_week interactions
summary(model_1) #R-SQUARED - 0.6226
deviance(model_1) #Deviance - 7077.291

model_2 <- lm(STAT0003_exam ~ count + STAT0003_ICA +
                     Exam_Season_Proportion + Easter_Holidays_Proportion + Afternoon_Proportion +
                     Quiz_Review_Interactions + Quiz_Submitted_Interactions + Maths_Quiz_Interactions +
                     Discussion_Forum_Interactions+Discussion_Forum_Created + Reading_Week_Interactions +
                     Week1_Video_Score + Week2_Video_Score + Week3_Video_Score + Week4_Video_Score + 
                     Week5_Video_Score + Week6_to_week8_video_scores + 
                     Week9_Video_Score + Week10_Video_Score +
                     Before_Reading_Week_Interactions
                     ,
                   data = combined_data23)
summary(model_2) #R-SQUARED - 0.6173
deviance(model_2) #7177.298

#F test betweeen model 1 and 2
anova(model_1, model_2,test="F") #p-value = 0.7629

model_3 <- lm(STAT0003_exam ~ count + STAT0003_ICA +
                Exam_Season_Proportion + Easter_Holidays_Proportion + Afternoon_Proportion + Morning_Proportion +
                Quiz_Review_Interactions + Quiz_Submitted_Interactions + Maths_Quiz_Interactions +
                Discussion_Forum_Interactions+Discussion_Forum_Created + Reading_Week_Interactions +
                Week1_Video_Score + Week2_Video_Score + Week3_Video_Score + Week4_Video_Score + 
                Week5_Video_Score + Week6_to_week8_video_scores + 
                Before_Reading_Week_Interactions, 
              data = combined_data23)
#week 9 and week 10 video interactions removed as it had to significant impact and also as after reading week has no impact on the exam results so 
# started to remove videos after ICA also attendance was really low that week
summary(model_3) #R-SQUARED - 0.6094
deviance(model_3) #7326.272

#F test between model 2 and 3
anova(model_2, model_3,test="F") #p-value = 0.1989

model_4 <- lm(STAT0003_exam ~ count + STAT0003_ICA +
                     Exam_Season_Proportion + Easter_Holidays_Proportion + Afternoon_Proportion + Morning_Proportion +
                     Quiz_Review_Interactions + Quiz_Submitted_Interactions +
                     Discussion_Forum_Interactions+Discussion_Forum_Created + Reading_Week_Interactions +
                     Week1_Video_Score + Week2_Video_Score + Week3_Video_Score + Week4_Video_Score + 
                     Week5_Video_Score + Week6_to_week8_video_scores + 
                     Week9_Video_Score +
                     Before_Reading_Week_Interactions ,
                   data = combined_data23)
#removed maths refresher quiz as it had no impact 
summary(model_4) #R-SQUARED - 0.6163
deviance(model_4) #7196.415

#F test betweeen model 3 and 4
anova(model_3, model_4,test="F") # p-value=0.09022

model_5 <- lm(STAT0003_exam ~ count + STAT0003_ICA +
             Easter_Holidays_Proportion + Afternoon_Proportion + Morning_Proportion +
                     Quiz_Review_Interactions + Quiz_Submitted_Interactions +
                     Discussion_Forum_Interactions+Discussion_Forum_Created + Reading_Week_Interactions +
                     Week1_Video_Score + Week2_Video_Score + Week3_Video_Score + Week4_Video_Score + 
                     Week5_Video_Score + Week6_to_week8_video_scores + 
                     Before_Reading_Week_Interactions,
                   data = combined_data23)
#removed exam season proportion showing learning the content during the term is much more benefical than studying during 
#the exam season. Also last minute cramming doesn't help with exam results
summary(model_5) #R-SQUARED - 0.6081
deviance(model_5) #Deviance - 7350.496

vif(model_5)

#F test betweeen model 4 and 5
anova(model_4, model_5,test="F") #p-value =0.2368

model_6 <- lm(STAT0003_exam ~ count + STAT0003_ICA +
                     Easter_Holidays_Proportion + Afternoon_Proportion + Morning_Proportion +
                     Quiz_Review_Interactions* Quiz_Submitted_Interactions +
                     Discussion_Forum_Interactions+Discussion_Forum_Created + Reading_Week_Interactions +
                     Week1_Video_Score + Week2_Video_Score + Week3_Video_Score + Week4_Video_Score + 
                     Week5_Video_Score + Week6_to_week8_video_scores +
                     Before_Reading_Week_Interactions ,data = combined_data23)
#Interaction terms with quiz review interactions and quiz submitted interactions introduced
summary(model_6) #R-SQUARED - 0.6089
deviance(model_6) #7334.785

vif(model_6)

#F test betweeen model 5 and 6
anova(model_5, model_6,test="F") #p-value = 0.5889

model_7 <- lm(STAT0003_exam ~ count + STAT0003_ICA + 
                     Easter_Holidays_Proportion + Afternoon_Proportion  +
                     Quiz_Review_Interactions* Quiz_Submitted_Interactions +
                     Discussion_Forum_Interactions+Discussion_Forum_Created + Reading_Week_Interactions +
                     Week1_to_week5_video_scores + Week6_to_week8_video_scores +  
                     Before_Reading_Week_Interactions 
                   ,
                   data = combined_data23)
#Put the videos into two categories , week 1 to 5 and 6 to 8. Also removed morning proportion
summary(model_7) #R-SQUARED - 0.6016
deviance(model_7) #Deviance - 7472.426

vif(model_7)

#F test betweeen model 6 and 7
anova(model_6, model_7,test="F") #p-value = 0.7652

model_8 <- lm(STAT0003_exam ~ count + STAT0003_ICA +
                     Afternoon_Proportion +
                     Quiz_Review_Interactions*Quiz_Submitted_Interactions +
                     Discussion_Forum_Interactions+Discussion_Forum_Created + Reading_Week_Interactions +
                     Week1_to_week5_video_scores + Week6_to_week8_video_scores +  
                     Before_Reading_Week_Interactions,
                   data = combined_data23)
#removed easter holiday proportion as it had no impact on the exam results. So studying during the term has a much greater effect than outside of the term
summary(model_8) #R-SQUARED - 0.6007
deviance(model_8) #Deviance - 7489.096
vif(model_8)
#F test betweeen model 7 and 8
anova(model_7, model_8,test="F") #p-value = 0.57



model_9 <- lm(STAT0003_exam ~ count + STAT0003_ICA + Afternoon_Proportion + 
                Discussion_Forum_Interactions*Discussion_Forum_Created + 
                Reading_Week_Interactions  +
                Week1_to_week5_video_scores + 
                Week6_to_week8_video_scores +
                Before_Reading_Week_Interactions, data = combined_data23)
#Removed quizzes as they have don't have a significant impact on exam results
summary(model_9) #R-SQUARED - 0.6118
deviance(model_9) #Deviance - 8662.954
#Use this as my current final model
vif(model_9)

#anova(model_8, model_9,test="F") #p-value = 0.57

model_final <- lm(STAT0003_exam ~ count + STAT0003_ICA + Afternoon_Proportion + 
                    Discussion_Forum_Created*Discussion_Forum_Interactions+ 
                    Before_Reading_Week_Interactions +
                    Reading_Week_Interactions  + Week6_to_week8_video_scores , data = combined_data23)
summary(model_final) #R-SQUARED - 0.6118
deviance(model_final) #Deviance - 8663
#Removed week1_to_week5_video_scores as it had no impact on the exam results
vif(model_final)

anova(model_final, model_9) # p-value = 0.9582

#Create diagnostic plots for model_final 
plot(model_final, which = 1)  # Residuals vs Fitted
#What is the name of the red line in the plot?
#The red line is the lowess line which is a locally weighted scatterplot smoothing line. It is used to show the trend in the data.
plot(model_final, which = 2)  # Normal Q-Q Plot
plot(model_final, which = 3)  # Scale-Location
plot(model_final, which = 4)  # Residuals vs Leverage
plot(model_final, which = 5)  # Cook's Distance
plot(model_final, which = 6)  # Residuals vs Fitted

standardised_residuals<-rstandard(model_final)
residuals<-resid(model_final)

#Create a ggplot for checking homoscedasticity with a straight black line at x=0
Homoscedasticity<-ggplot(model_final, aes(x = model_final$fitted.values, y = standardised_residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  labs(title = "Standardised Residuals vs Fitted values", x = "Fitted value", y = "Standardised residual") +
  theme_minimal()
print(Homoscedasticity)

#Create a ggplot for checking normaility with a normal Q-Q plot and outine observations 18,82 and 105 by writing the numbers on the plot
Normality<-ggplot(model_final, aes(sample = standardised_residuals)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantile", y = "Standardised Residual") +
  theme_minimal()
print(Normality)

#Create a ggplot for checking linearity with a residuals vs fitted plot with a legend for the loess line
Linearity <- ggplot(model_final, aes(x = model_final$fitted.values, y = standardised_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(color = "Loess line"), method = "loess", se = FALSE, lwd = 0.9) +  # Use aes(color = "Loess line")
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  scale_color_manual(name = "", values = c("Loess line" = "red")) +  # Now linked properly
  labs(title = "Standardised Residuals vs Fitted values", x = "Fitted value", y = "Standardised Residual") +
  theme_minimal() +
  theme(legend.position = c(0.915,0.95),
  legend.background = element_rect(color = "black", fill = NA),
  legend.title=element_blank())  # Adds a box around the legend
print(Linearity)

#ANOVA test between model_10 and model_final for droppping Week1_to_week5_video_Scores
anova(model_9, model_final, test="F")

#AIC and BIC values for model_1 and model_final
AIC(model_1) #1089.816.097
AIC(model_final) #1161.165 - Lower
BIC(model_1) #1169.112
BIC(model_final) #1195.528 - Lower

summary(model_1) # R-squared - 0.6344 , Adjusted R-squared - 0.573, RSE - 7.554
summary(model_final) # R-squared - 0.6118 , Adjusted R-squared - 0.5897, RSE - 7.405
#what do VIF values over 5 indicate?

vif(model_1)

#Finding the AIC and BIC of models
AIC(model_1) #1089.816
AIC(model_final) #1161.165
BIC(model_1) #1169.112
BIC(model_final) #1195.528

#Find the variance of model_final
var(model_final$fitted.values)

#Create a ggplot between ICA results and Exam results
ICA_vs_Exam<-ggplot(combined_data23, aes(x = STAT0003_ICA, y = STAT0003_exam)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Exam results (%) vs ICA results", x = "ICA Result (%)", y = "Exam Result (%)") +
  theme_minimal()

#F-test between model_final and model with only 

model_final <- lm(STAT0003_exam ~ count + STAT0003_ICA + Afternoon_Proportion + 
                    Discussion_Forum_Created*Discussion_Forum_Interactions+ 
                    Before_Reading_Week_Interactions +
                    Reading_Week_Interactions  + Week6_to_week8_video_scores , data = combined_data23)

model_15 <- lm(STAT0003_exam ~ STAT0003_ICA, data=combined_data23)

anova(model_final, model_15, test="F") 

#pearson correlation between ICA and Exam results
cor(combined_data23$STAT0003_ICA,combined_data23$STAT0003_exam, use = "complete.obs", method="pearson")
cor(combined_data$STAT0003_ICA,combined_data$STAT0003_exam, use = "complete.obs")

#Durbin Watson test for model_final
durbinWatsonTest(model_final)

dwtest(model_final,alternative = "two.sided")
dwtest(model_final,alternative = "greater")
dwtest(model_final,alternative = "less")

# Extract residuals
residuals_final <- residuals(model_final)
standardised_residuals<-rstandard(model_final)

# Ensure x and y have the same length
x_resid <- standardised_residuals[-length(standardised_residuals)]  # Remove last element
y_resid <- standardised_residuals[-1]  # Remove first element

# Create dataframe with equal-length vectors
serial_corr_data <- data.frame(x_resid, y_resid)

# Plot with corrected data
Serial_Correlation <- ggplot(serial_corr_data, aes(x = x_resid, y = y_resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  labs(
    title = expression("i"^{th} ~ "standardised residuals against the (i-1)"^{th} ~ "standardised residuals"),
    x = expression("(i-1)"^{th} ~ "standardised residual"),
    y = expression("i"^{th} ~ "standardised residual")) +
  theme_minimal()

# Print plot
print(Serial_Correlation)


#Random forest model

combined_data23_clean <- na.omit(combined_data23)
Random_Forest_model <- randomForest(STAT0003_exam ~ count + STAT0003_ICA + Afternoon_Proportion + 
                           Discussion_Forum_Created*Discussion_Forum_Interactions+ 
                           Before_Reading_Week_Interactions +
                           Reading_Week_Interactions  + Week6_to_week8_video_scores, data = combined_data23_clean, importance = TRUE)

Importance<- importance(Random_Forest_model)
print(Importance)
VarImPlot_1<- varImpPlot(Random_Forest_model)
print(VarImPlot_1)






