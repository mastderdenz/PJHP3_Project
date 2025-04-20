# Sourcing the project code as the data is cleaned and merged in the project code
source("Stat0035_Project_Code.R")

#Intall packages and using their respective libraries
install.packages("shiny")
install.packages("latex2exp")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("kableExtra")
install.packages("car")
install.packages("randomForest")
install.packages("knitr")
install.packages("lmtest")
library(shiny)
library(latex2exp)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(car)
library(randomForest)
library(knitr)
library(lmtest)

################################################################################
# ShinyApp
################################################################################

ui <- fluidPage(
  titlePanel("Comprehensive Analysis of Student Interactions and Exam Results"),
  sidebarLayout(
    sidebarPanel(
      selectInput("interaction", "Select the Type of Moodle Interaction to Filter:",
                  choices = c("All", "Quiz", "Video", "Forum", "Notes", 
                              "Live Session", "Slides")),
      conditionalPanel(
        condition = "input.interaction == 'Quiz'",
        selectInput("quiz", "Select Quiz:", 
                    choices = c("All", unique(moodle_data$Moodle_entry[moodle_data$Interactions == "Quiz"])))
      ),
      conditionalPanel(
        condition = "input.interaction == 'Video'",
        selectInput("video", "Select Video:", 
                    choices = c("All", unique(moodle_data$Moodle_entry[moodle_data$Interactions == "Video"])))
      ),
      conditionalPanel(
        condition = "input.interaction != 'All'",
        selectInput("sub_interaction", "Sub-Category of each type of Interaction:",
                    choices = c("All", "Course module viewed", "Course activity completion updated", 
                                "Zip archive of folder downloaded", "Quiz attempt reviewed",
                                "Quiz attempt started", "Quiz attempt submitted", 
                                "Quiz attempt summary viewed", "Discussion viewed", 
                                "Discussion created", "Some content has been posted", 
                                "Subscription created", "Post created", "Post updated"))),
      selectInput("week", "Select Week/Period:",
                  choices = c("All", "Exam Season","Easter Holidays","Week 10",
                              "Week 9","Week 8","Week 7","Week 6","Reading Week",
                              "Week 5","Week 4","Week 3","Week 2","Week 1","Week 0")),
      selectInput("student", "Student ID:",
                  choices = c("All", unique(moodle_data$Student_id))),
      selectInput("tutorial_group", "Tutorial Group:",
                  choices = c("All", "1","2","3","4","5","6","7","8","9","10","11")),
      selectInput("covariate", "Select a Covariate to Compare Against Exam Scores:",
                  choices = c("count", "STAT0003_ICA", "Afternoon_Proportion", 
                              "Discussion_Forum_Created", "Discussion_Forum_Interactions", 
                              "Before_Reading_Week_Interactions", "Reading_Week_Interactions", 
                              "Week6_to_week8_video_scores")),
      checkboxInput("add_regression", "Add Regression Line", value = TRUE),
      downloadButton("download_data", "Download Filtered Data"),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Statistics", verbatimTextOutput("summary_stats"),dataTableOutput("filtered_summary_table")),
        tabPanel("Scatterplot Analysis of Interactions", plotOutput("interaction_exam_plot")),
        navbarMenu("Interaction Trends",
                   tabPanel("Overall Interaction Trends", plotOutput("weekly_trend_plot")),
                   tabPanel("Easter & Exam Weekly Trends", plotOutput("easter_exam_weekly_trend_plot"))),
        tabPanel("Weekly Tutorial Attendance", plotOutput("weekly_attendance_plot")),
        # New Split for Predictive Model Section
        navbarMenu("Predictive Model",
                   tabPanel("Model Summary", verbatimTextOutput("model_summary")),
                   tabPanel("Predicted Scores", dataTableOutput("predicted_scores")),
                   tabPanel("Model Evaluation", verbatimTextOutput("model_evaluation")),
                    tabPanel("Predicted vs Actual Plot", plotOutput("predicted_vs_actual_plot"))),
        
        # New Tab for Covariate Comparisons
        tabPanel("Scatterplot of Covariate vs Exam Results", plotOutput("covariate_vs_exam_plot"))
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data <- moodle_data
    if (input$interaction != "All") {
      data <- data[data$Interactions == input$interaction, ]
    }
    if (input$interaction == "Quiz" && !is.null(input$quiz) && input$quiz != "All") {
      data <- data[data$Moodle_entry == input$quiz, ]
    }
      if (input$interaction == "Video" && !is.null(input$video) && input$video != "All") {
        data <- data[data$Moodle_entry == input$video, ]
    }
    if (input$week != "All") {
      data <- data[data$week == input$week, ]
    }
    if (input$student != "All") {
      data <- data[data$Student_id == input$student, ]
    }
    if (input$sub_interaction != "All") {
      data <- data[data$Action == input$sub_interaction, ]
    }
    data
  })
  
  output$summary_stats <- renderPrint({
    data <- filtered_data()
    
    total_interactions <- nrow(data)
    avg_interactions <- mean(table(data$Student_id))
    top_interactions <- sort(table(data$Interactions), decreasing = TRUE)[1:5]
    merged_exam_data <- merge(data, exam_results, by = "Student_id")
    avg_exam_score <- if (nrow(merged_exam_data) > 0) mean(merged_exam_data$STAT0003_exam, na.rm = TRUE) else NA
    num_students <- length(unique(data$Student_id))
    
    time_distribution <- table(data$Time)
    morning_interactions <- sum(data$Time == "Morning")
    afternoon_interactions <- sum(data$Time == "Afternoon")
    evening_interactions <- sum(data$Time == "Evening")
    late_evening_interactions <- sum(data$Time == "Late Evening")
    
    merged_data <- merge(data.frame(Student_id = names(table(data$Student_id)), count = as.numeric(table(data$Student_id))), exam_results, by = "Student_id")
    correlation <- if (nrow(merged_data) > 1) cor(merged_data$count, merged_data$STAT0003_exam, use = "complete.obs") else NA
    
    cat("Summary Statistics:\n")
    cat("Total Number of Interactions:", total_interactions, "\n")
    cat("Average Number of Interactions per Student:", avg_interactions, "\n")
    cat("Top Interaction Types:\n")
    print(top_interactions)
    cat("Average Exam Score:", avg_exam_score, "\n")
    cat("Number of Students:", num_students, "\n")
    cat("Correlation between Interactions and Exam Scores:", correlation, "\n")
    cat("Interactions by Time of Day:\n")
    cat("Morning:", morning_interactions, "\n")
    cat("Afternoon:", afternoon_interactions, "\n")
    cat("Evening:", evening_interactions, "\n")
    cat("Late Evening:", late_evening_interactions, "\n")
  })
  
  output$interaction_exam_plot <- renderPlot({
    data <- filtered_data()
    interaction_data <- table(data$Student_id)
    interaction_data <- data.frame(Student_id = names(interaction_data), count = as.numeric(interaction_data))
    interaction_data <- merge(interaction_data, exam_results, by = "Student_id")
    if (!is.null(input$sub_interaction) && input$sub_interaction != "All") {
      axis_label <- paste("Number of",input$interaction, "Interactions (",input$sub_interaction,")")
      plot_title <- paste("Exam Results (%) vs Number of",input$interaction, "Interactions (",input$sub_interaction,")")
    } else {
      axis_label <- paste("Number of", input$interaction, "Interactions")
      plot_title <- paste("Exam Results (%) vs Number of", input$interaction, "Interactions")
    }
    p <- ggplot(interaction_data, aes(x = count, y = STAT0003_exam)) +
      geom_point() +
      labs(x = axis_label, y = "Exam Results (%)", title = plot_title)
    if (input$add_regression) {
      p <- p + geom_smooth(method = "lm", se = FALSE, colour = "red")
    }
    p
    })
  
  output$easter_exam_weekly_trend_plot <- renderPlot({
    data <- filtered_data()
    if (input$interaction == "All") {
      analysis_data <- moodle_data
    } else {
      analysis_data <- moodle_data[moodle_data$Interactions == input$interaction, ]}
    data <- moodle_data[analysis_data$Easter_exam_week %in% c("Easter Week 1", "Easter Week 2",
                                                            "Easter Week 3", 
                                                            "Easter Week 4", 
                                                            "Exam Season Week 1", 
                                                            "Exam Season Week 2"), ]
    data$Easter_exam_week <- factor(data$Easter_exam_week, 
                                    levels = c("Easter Week 1", "Easter Week 2",
                                               "Easter Week 3", 
                                               "Easter Week 4", 
                                               "Exam Season Week 1", 
                                               "Exam Season Week 2"))
    week_counts <- as.data.frame(table(data$Easter_exam_week))
    colnames(week_counts) <- c("Week", "Count")
    ggplot(week_counts, aes(x = Week, y = Count)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      geom_text(aes(label = Count), vjust = -0.5, color = "black", size = 4.5) +
      labs(title = "Weekly Interactions During Easter and Exam Season",
           x = "Week",
           y = "Number of Interactions") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$weekly_trend_plot <- renderPlot({
    data <- filtered_data()
    week_order <- c("Week 0", "Week 1", "Week 2", "Week 3", "Week 4", "Week 5", "Reading Week",
                    "Week 6", "Week 7", "Week 8", "Week 9", "Week 10"
                    , "Easter Holidays", "Exam Season")
    data$week <- factor(data$week, levels = week_order, ordered = TRUE)
    weekly_data <- aggregate(Student_id ~ week, data, length)
    ggplot(weekly_data, aes(x = week, y = Student_id)) +
      geom_bar(stat = "identity") +
      stat_summary(fun = sum, geom = "text", aes(label = round(..y.., 2)), vjust = -0.5, color = "red")+
      labs(x = "Week", y = "Number of Interactions", title = "Weekly Interaction Trends") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$weekly_attendance_plot <- renderPlot({
    filtered_attendance <- attendance
    if (input$tutorial_group != "All") {
      filtered_attendance <- filtered_attendance[filtered_attendance$Tutorial.group == input$tutorial_group, ]
    }
    filtered_attendance$Tutorial.group <- factor(filtered_attendance$Tutorial.group, 
                                                 levels = c(3,4,5,6,7,9,10,11),
                                                 labels = c(3,4,5,6,7,9,10,11))
    filtered_attendance <- filtered_attendance[!is.na(filtered_attendance$Number.attended) &
                                                 !is.na(filtered_attendance$Class.size) &
                                                 !is.na(filtered_attendance$week),]
    filtered_attendance$ProportionAttended <- filtered_attendance$Number.attended / filtered_attendance$Class.size
    attendance_prop_summary <- aggregate(ProportionAttended ~ week + Tutorial.group, data = filtered_attendance, mean)
    ggplot(attendance_prop_summary, aes(x = week, y = ProportionAttended, fill = Tutorial.group)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Week", y = "Proportion (%) of Students Attending", 
           title = "Weekly Tutorial Attendance by Group", 
           fill = "Tutorial Group") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("filtered_data", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    })
  
  output$covariate_vs_exam_plot <- renderPlot({
    ggplot(model_data, aes_string(x = input$covariate, y = "STAT0003_exam")) +
      geom_point() +
      labs(x = input$covariate, y = "Exam Results (%)", title = paste("Exam Results vs", input$covariate)) +
      theme_minimal() +
      if (input$add_regression) geom_smooth(method = "lm", se = FALSE, colour = "red")
})
  #Add, display summary and predict exam scores of model_final below
  
  model_data<-combined_data23
  model_final <- reactive({lm(STAT0003_exam ~ count + STAT0003_ICA + Afternoon_Proportion + 
                                Discussion_Forum_Created*Discussion_Forum_Interactions+ 
                                Before_Reading_Week_Interactions +
                                Reading_Week_Interactions  + Week6_to_week8_video_scores , data = combined_data23)
  })
  
  # Display the summary of the final model
  output$model_summary <- renderPrint({
    summary(model_final())
  })
  
  # Predict exam scores based on the final model
  output$predicted_scores <- renderDataTable({
    model <- model_final()
    
    
    # Add predictions to dataset 
    model_data$Predicted_Scores <- predict(model, newdata = model_data)
    
    #Round predicted scores to whole numbers
    model_data$Predicted_Scores <- round(predict(model, newdata = model_data), 0)
    
    model_data$mse <- (model_data$STAT0003_exam - model_data$Predicted_Scores)^2
    
    model_data$RPD <- abs(model_data$STAT0003_exam - model_data$Predicted_Scores) / 
      ((model_data$STAT0003_exam + model_data$Predicted_Scores) / 2) * 100
    model_data$RPD<-round(model_data$RPD,2)
    
    #Now create a dataset from model_data which only includes student id, STAT0003_exam, STAT0003_ICA , predicted_scores and MSE
    model_data <- model_data[, c("Student_id", "STAT0003_exam", "STAT0003_ICA", "Predicted_Scores", "RPD")]
    
    return(model_data)
  })
  
  output$model_evaluation <- renderPrint({
    model <- model_final()
    model_data <- combined_data23
    model_data$Predicted_Scores <- predict(model, newdata = model_data)
    model_data$mse <- (model_data$STAT0003_exam - model_data$Predicted_Scores)^2
    mse <- mean(model_data$mse, na.rm = TRUE)
    rmse <- sqrt(mse)
    mae <- mean(abs(model_data$STAT0003_exam - model_data$Predicted_Scores), na.rm = TRUE)
    mape <- mean(abs(model_data$STAT0003_exam - model_data$Predicted_Scores) / model_data$STAT0003_exam * 100, na.rm = TRUE)
    RPD <- abs(model_data$STAT0003_exam - model_data$Predicted_Scores) / 
      ((model_data$STAT0003_exam + model_data$Predicted_Scores) / 2) * 100
    cat("Mean Squared Error:", round(mse,2), "\n")
    cat("Root Mean Squared Error:", round(rmse,2), "\n")
    cat("Mean Absolute Error:", round(mae,2), "\n")
    cat("Mean Absolute Percentage Error:", round(mape,2),"%\n")
    cat("Relative Percentage Difference:", round(mean(RPD, na.rm = TRUE),2),"%\n")
  }) 
  output$predicted_vs_actual_plot <- renderPlot({
    model <- model_final()
    model_data$Predicted_Scores <- predict(model, newdata = model_data)
    ggplot(model_data, aes(x = STAT0003_exam, y = Predicted_Scores)) +
      geom_point(alpha = 0.7, color = "darkblue") +
      geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "red") +
      labs(x = "Actual Exam Score (%)", y = "Predicted Exam Score (%)",
           title = "Predicted vs Actual Exam Results") +
      theme_minimal()
  })
}

#Run the shiny app here
shinyApp(ui = ui, server = server)


