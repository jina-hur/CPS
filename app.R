library(shiny)
library(DT)
library(ggplot2)
library(tidyverse)
library(shinydashboard)

#set workding directory to where you downloaded your csv file
#setwd()

data = read.csv("/Users/jinyounghur/shiny/Washoe County Student SEL Survey.csv")[, 2:44]
rownames(data) <- data[, 1]

# rowMeans for 8 components of SEL
overall <- rowMeans(data[, 4:43])
concept <-rowMeans(data[, 4:7])
emotion <- rowMeans(data[, 8:13])
social <- rowMeans(data[, 14:18])
regluation <- rowMeans(data[, 19:22])
goal <- rowMeans(data[, 23:26])
schoolwork <- rowMeans(data[, 27:32])
relation <- rowMeans(data[, 33:38])
responsible <- rowMeans(data[, 39:43])

#individual score data frame
score <- data.frame("Grade"=data$Grade,
                    "Teacher Name"=data$Teacher.Name,
                    "Student Name"=data$Student.Name,
                    overall,
                    concept,
                    emotion,
                    social,
                    regluation,
                    goal,
                    schoolwork,
                    relation,
                    responsible)

score[, 4:12] <- round(score[, 4:12],1)
score <- score[order(score$Grade), ]


#facet
# grade distribution ggplot
ggplot(score, aes(x=overall))+
  geom_histogram(binwidth = 0.2)+
  facet_wrap(. ~ Grade)

# score_2 with class_identity (Grade + Teacher.Name)
class_identity <- paste(score[, 1], score[, 2])
score_2 <- add_column(score, class_identity, .before = 1)
score_2 <- score_2[c(1, 4:13)]

#class distribution ggplot
ggplot(score_2, aes(x=overall))+
  geom_histogram(binwidth = 0.2)+
  facet_wrap(. ~ class_identity)

ggplot(score_2, aes(x=overall))+
  geom_density(fill="lightblue")+
  geom_vline(aes(xintercept=mean(overall)),
             color = "blue", linetype="dashed", size = 0.5) + 
  facet_wrap(. ~ class_identity)

ggplot(score_2, aes(x=overall))+
  geom_histogram(aes(y=..density..), color="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(overall)),
             color = "blue", linetype="dashed", size = 0.5) +
  facet_wrap(. ~ class_identity)

# ggplot(score_2, aes(x=overall, fill=Grade)) +
#   geom_density()

# school level: histogram & density olot
hist(score$overall)
d <- density(score$overall)
plot(d)

ggplot(score, aes(x=overall))+
  geom_histogram(aes(y=..density..), color="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(overall)),
             color = "blue", linetype="dashed", size = 0.5)



#class score data frame
grade_mean <- score %>%
  group_by(Grade) %>%
  summarize(overall = mean(overall),
            concept = mean(concept),
            emotion = mean(emotion),
            social = mean(social),
            regluation = mean(regluation),
            goal = mean(goal),
            schoolwork = mean(schoolwork),
            relation = mean(relation),
            responsible = mean(responsible))
grade_mean <- data.frame(grade_mean[, 1], round(grade_mean[, 2:10], 1))

grade_median <- score %>%
  group_by(Grade) %>%
  summarize(overall = median(overall),
            concept = median(concept),
            emotion = median(emotion),
            social = median(social),
            regluation = median(regluation),
            goal = median(goal),
            schoolwork = median(schoolwork),
            relation = median(relation),
            responsible = median(responsible))
grade_median <- data.frame(grade_median[, 1], round(grade_median[, 2:10], 1))

grade_sd <- score %>%
  group_by(Grade) %>%
  summarize(overall = sd(overall),
            concept = sd(concept),
            emotion = sd(emotion),
            social = sd(social),
            regluation = sd(regluation),
            goal = sd(goal),
            schoolwork = sd(schoolwork),
            relation = sd(relation),
            responsible = sd(responsible))
grade_sd <- data.frame(grade_sd[, 1], round(grade_sd[, 2:10], 1))

#class data frame
class_mean <- score %>%
  group_by(Grade, Teacher.Name) %>%
  summarize(overall = mean(overall),
            concept = mean(concept),
            emotion = mean(emotion),
            social = mean(social),
            regluation = mean(regluation),
            goal = mean(goal),
            schoolwork = mean(schoolwork),
            relation = mean(relation),
            responsible = mean(responsible))
class_mean<- data.frame(class_mean[, 1:2], round(class_mean[, 3:11], 1))

class_median <- score %>%
  group_by(Grade, Teacher.Name) %>%
  summarize(overall = median(overall),
            concept = median(concept),
            emotion = median(emotion),
            social = median(social),
            regluation = median(regluation),
            goal = median(goal),
            schoolwork = median(schoolwork),
            relation = median(relation),
            responsible = median(responsible))
class_median<- data.frame(class_median[, 1:2], round(class_median[, 3:11], 1))

class_sd <- score %>%
  group_by(Grade, Teacher.Name) %>%
  summarize(overall = sd(overall),
            concept = sd(concept),
            emotion = sd(emotion),
            social = sd(social),
            regluation = sd(regluation),
            goal = sd(goal),
            schoolwork = sd(schoolwork),
            relation = sd(relation),
            responsible = sd(responsible))
class_sd<- data.frame(class_sd[, 1:2], round(class_sd[, 3:11], 1))


# Define UI ----

#sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Individual Assessment", icon = icon("th"), tabName = "assessment")
  )
) 

#fluidrow  
fluidrow <- 
  fluidRow(
    column(4,
           selectInput("gra",
                       "Grade:",
                       c("All",
                         unique(as.character(score$Grade))))
    ),
    column(4,
           selectInput("tea",
                       "Teacher Name:",
                       c("All",
                         unique(as.character(score$Teacher.Name))))
    ),
    column(4,
           selectInput("stu",
                       "Student Name:",
                       c("All",
                         unique(as.character(score$Student.Name))))
    )
  )
  
#

ui <- dashboardPage(
  dashboardHeader(title = "WCSD Social and Emotional Competency Assessment",
                  titleWidth = 500),
  
  # Create a new Row in the UI for selectInputs
  sidebar,
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              h2("Home tab content")
      ),
      
      tabItem(tabName = "assessment",
              h2("Individual Assessment"),
              fluidrow,
              # Create a new row for the table.
              DT::dataTableOutput("table"),
              br(),
              a(strong("Note:")),
              a("0) overall = composite score of SEL competency;"), 
              a("1) concept = self-awareness of strengths and weaknesses;"),
              a("2) emotion = self-awareness of emotions;"),
              a("3) social = social awareness;"),
              a("4) regulation = self-management of emotions;"),
              a("5) goal = self-management of goals;"),
              a("6) schoolwork = self-management of school work;"),
              a("7) relation = relationship skills;"),
              a("8) responsible = responsible decision-making")
      )
    )
  ))



# Define server logic ----

server <- function(input, output, session) {
  # Filter data based on selections

  output$table <- DT::renderDataTable(DT::datatable({
    #score <- score
    if (input$gra != "All") {
      score <- score[score$Grade == input$gra,]
    }
    if (input$tea != "All") {
      score <- score[score$Teacher.Name == input$tea,]
    }
    if (input$stu != "All") {
      score <- score[score$Student.Name == input$stu,]
    }
    score
  }, options = list(lengthMenu = list(c(10,20,50,100), c('10','20','50','All'))),
  rownames = FALSE,
  ))
}

# Run the app ----
shinyApp(ui = ui, server = server)
