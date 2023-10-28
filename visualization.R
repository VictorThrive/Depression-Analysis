# Load the necessary packages
my_packages <-c("tidyverse","likert","ggthemes","psych","gtable","gtExtra","skimr",
                "GPArotation","ordinal","MASS","rcompanion","brant","DescTools")
lapply(my_packages, require, character.only = TRUE)
survey <- read.csv("dataset/cleandf.csv", stringsAsFactors = TRUE)
View(survey)
str(survey)
survey$level <- factor(survey$level, levels = unique(c(100,200,300,400,500,600)))
socio <- survey[,c(2:11,73,74)]


#-------------------------------------------------------------------------------
# EXPLORING VARIABLES DISTRIBUTION
# count the category of each college


# create the bar plot
coll <-count(socio,college)
p1 <-ggplot(coll,aes(x = reorder(college, -n), y = n)) +
  geom_bar( stat = "identity", fill = c(rep("grey",7),"#99020f","grey","#0047AB")) +
  geom_text(aes(label = n), vjust = -0.5, color = "#99020f" , size = 5) +
  labs(
       subtitle ="college of Physical had 36% response",
       x = "College", y = "number of responses")
  ptheme <-theme(
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

  p1+ptheme

  # Measuring the stress level of student

  lvl <-count(survey,stress_level)
  stl <-ggplot(lvl,aes(x = reorder(stress_level, -n), y = n)) +
    geom_bar( stat = "identity", fill = c("#0047ab",rep("#666666",3),"#99020f")) +
    geom_text(aes(label = n), vjust = -0.5, color = "#99020f" , size = 5) +
    labs(
      subtitle ="51% of the student suffers Major Depresion",
      x = "College", y = "number of students") + ptheme


# count the category of each department
department <- count(socio,department)
p2<-ggplot(department,aes(reorder(department, n), y = n)) +
  geom_col(fill = c(rep("grey",9),"#0047AB",rep("grey",22),"#99020f","#0047AB","grey")) +
geom_text(aes(label = n), hjust = -0.5, color = "black", size = 3) +
  labs(title = "",
       subtitle = " statistics department had 14% response",
       x = "deprtment", y = "number of responses")
p2+ptheme +coord_flip()


# count the category of each level
level <- count(socio,level)
# make the plot
p3<-ggplot(level,aes(level, y = n)) +
  geom_col(fill = c("grey","grey","#99020f","grey","grey","#0047AB")) +
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4) +
  labs(title = "Distribution of Response from each study level",
       subtitle = "The highest number of responce were recorded from the 300 level,
       wheras 600 level recorded the lowest",
       x = "level of study", y = "number of students")
p3+ptheme

# count the category of each gender
gender <- count(socio,gender)
#make the plot
p4<-ggplot(gender,aes(gender, y = n)) +
  geom_col(fill = c("#0047AB","#99020f")) +
  geom_text(aes(label = n), vjust = 10, color = "white", size = 4) +
  labs(title = "Distribution of Response from each gender",
       subtitle = "The male students had the highest number of response.",
       x = "gender", y = "number of responses")
p4+ptheme

# count the category of each stress
stress <- count(socio,stress)
#make the plot
p5<-ggplot(stress,aes(stress, y = n)) +
  geom_col(fill = c("grey","grey" ,"#0047AB","grey","#99020f")) +
  geom_text(aes(label = n), vjust = 10, color = "white", size = 4) +
  labs(title = "Distribution of Response from each stress level",
       subtitle = "Most of the students suffers Very little stress",
       x = "stress level", y = "number of responses")
p5+ptheme

# count the category of each age group
age <- count(socio,age)
#make the plot
p6 <-ggplot(age,aes(age, y = n)) +
  geom_col(fill = c("#99020f","grey","#0047AB")) +
  geom_text(aes(label = n), vjust = -.5, color = "#99020f", size = 4) +
  labs(title = "Distribution of Response from each age group",
       subtitle = "The highest number of response fall within 18 - 23 age group",
       x = "age group", y = "number of responses")
p6+ptheme

#-------------------------------------------------------------------------------
# Amount of stress experienced by students
stress_scores <- c("above 300","250 - 299","200 - 249","150 - 199","below - 149")

Stress <- data.frame(stress =stress$stress,scores = stress_scores, count = stress$n)

Stress$scores <-factor(Stress$scores, ordered = is.ordered(Stress$scores))

#make the plot
p7 <-ggplot(Stress,aes(Stress,x = scores, y = count, fill = stress)) +
  geom_col() +
  geom_text(aes(label = count), vjust = 10, color = "white", size = 4) +
  labs(title = "Student Stress Status",
       subtitle = "Most students stress score falls below 145",
       x = "stress scores", y = "number of students")
p7+ptheme


stressors <- my_data[,6:8]

print(stressors)




# summarize the data
summary_table <- table(stressor_)
# Convert the summary table to percentages
percent_table <- round(summary_table / sum(summary_table) * 100, 1)
# Combine the tables into a data frame
summary_data <- data.frame(summary_table, percent_table)
# Rename the columns
colnames(summary_data) <- c("Count", "Percentage","Count", "Percentage")
# Print the summary data
print(summary_data)
