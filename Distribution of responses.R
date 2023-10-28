# Load the necessary packages
my_packages <-c("tidyverse","likert","ggthemes","psych",
                "GPArotation","ordinal","MASS","rcompanion","brant","DescTools")
lapply(my_packages, require, character.only = TRUE)
survey <- read.csv("dataset/CleanSurveyData.csv", stringsAsFactors = TRUE)
socio <- survey[,c(2:11,73,74)]


#-------------------------------------------------------------------------------
# EXPLORING VARIABLES DISTRIBUTION
# count the category of each college
college <- count(socio, college)
# create the bar plot
ggplot(college,aes(x = college, y = n)) +
  geom_bar( stat = "identity", fill = c(rep("grey",7),"#99020f","grey","#0047AB")) +
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4) +
  labs(title = "Distribution of Response from each college ",
       subtitle = "The college of physical sciences had the highest recorded responses.",
       x = "College", y = "number of responses") +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# count the category of each department
department <- count(socio,department)
ggplot(department,aes(department, y = n)) +
  geom_col(fill = c(rep("grey",9),"#0047AB",rep("grey",22),"#99020f","#0047AB","grey")) +
geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4) +
  labs(title = "Distribution of Response from each department",
       subtitle = "The Statistics department recorded 33 responses,
       whereas the Vet Medicine and Biochemistry departments recorded only one response each.",
       x = "deprtment", y = "number of responses") +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# count the category of each level
level <- count(socio,level)
# make the plot
ggplot(level,aes(level, y = n)) +
  geom_col(fill = c("grey","grey","#99020f","grey","grey","#0047AB")) +
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4) +
  labs(title = "Distribution of Response from each level",
       subtitle = "The highest number of respondents were from the 300 level,
       while only one student from the 600 level took the survey.",
       x = "deprtment", y = "number of responses") +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )


# count the category of each gender
gender <- count(socio,gender)
#make the plot
ggplot(gender,aes(gender, y = n)) +
  geom_col(fill = c("#0047AB","#99020f")) +
  geom_text(aes(label = n), vjust = 10, color = "white", size = 4) +
  labs(title = "Distribution of Response from each gender",
       subtitle = "The highest number of respondents were from the male students,
       while only 104 females took the survey.",
       x = "deprtment", y = "number of responses") +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# count the category of each stress
stress <- count(socio,stress)
#make the plot
ggplot(stress,aes(stress, y = n)) +
  geom_col(fill = c("grey","grey" ,"#0047AB","grey","#99020f")) +
  geom_text(aes(label = n), vjust = 10, color = "white", size = 4) +
  labs(title = "Distribution of Response from each stress level",
       subtitle = "",
       x = "stress level", y = "number of responses") +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# count the category of each age group
age <- count(socio,age)
#make the plot
ggplot(age,aes(age, y = n)) +
  geom_col(fill = c("#99020f","grey","#0047AB")) +
  geom_text(aes(label = n), vjust = -.5, color = "#99020f", size = 4) +
  labs(title = "Distribution of Response from each age group",
       subtitle = "",
       x = "age group", y = "number of responses") +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

#-------------------------------------------------------------------------------
# Amount of stress expirienced by students
stress_scores <- c("above 300","250 - 299","200 - 249","150 - 199","below - 149")

Stress <- data.frame(stress =stress$stress,scores = stress_scores, count = stress$n)

Stress$scores <-factor(Stress$scores, ordered = is.ordered(Stress$scores))

#make the plot
ggplot(Stress,aes(Stress,x = scores, y = count, fill = stress)) +
  geom_col() +
  geom_text(aes(label = count), vjust = 10, color = "white", size = 4) +
  labs(title = "Distribution of Response from each stress score category",
       subtitle = "59 students stress score fall between 300 and ",
       x = "stress scores", y = "number of students") +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )



