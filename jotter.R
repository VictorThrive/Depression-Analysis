# Example long format data
data_long <- data.frame(
  id = c(1, 2, 3, 4),
  question = rep(c("Q1", "Q2", "Q3"), each = 4),
  response = c("Agree", "Strongly Agree", "Disagree", "Neutral",
               "Disagree", "Strongly Disagree", "Neutral", "Neutral",
               "Agree", "Agree", "Disagree", "Agree"))

# Convert long format data to likert format
data_likert <- data_long %>%
  pivot_wider(names_from = question, values_from = response)

# View the resulting likert data
data_likert

# Output:
# # A tibble: 4 x 3
#      id Q1              Q2              Q3
 # 1     1 Agree           Strongly Agree Disagreeoghb 9o
# 2     2 Strongly Agree Strongly Disa~ Neutral
# 3     3 Disagree        Neutral         Agree
# 4     4 Neutral         Neutral         Agree








bf <- data.frame(Name = c("John", "Mary", "Bob"),
                 Age = c(25, 30, 35),
                 City = c("New York", "Los Angeles", "Chicago"))

bf_gathered <- gather(bf, key = "Name", value = "City")







library(likert)

# create example data
d <- data.frame(
  statement = c("Statement 1", "Statement 2", "Statement 3", "Statement 4"),
  strongly.disagree = c(5, 3, 2, 1),
  disagree = c(10, 8, 6, 2),
  neutral = c(20, 15, 10, 5),
  agree = c(15, 20, 25, 15),
  strongly.agree = c(5, 15, 20, 25)
)

# convert data to likert format
likert_df <- likert(df[, -1], n.levels = 5, labels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

# plot 100% stacked bar chart
plot(likert_df, type = "percent", centered = FALSE, text.size = 3)



# Load the tidyverse package
library(tidyverse)

# Create the data as a tibble
data <- tibble(
  f1 = c("Agree", "Agree", "Agree", "Agree", "Disagree", "Strongly Agree"),
  f2 = c("Not Applicable", "Strongly Agree", "Strongly Agree", "Strongly Agree", "Strongly Agree", "Strongly Agree"),
  f3 = c("Strongly Agree", "Strongly Agree", "Not Applicable", "Strongly Agree", "Neutral", "Not Applicable")
)

# Convert the data to long format
data_long <- data %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")

# Summarize the data
summary_table <- table(data_long$Response)

# Convert the summary table to percentages
percent_table <- round(summary_table / sum(summary_table) * 100, 1)

# Combine the tables into a data frame
summary_data <- data.frame(summary_table, percent_table)

# Rename the columns
colnames(summary_data) <- c("Count", "Percentage")

# Print the summary data
print(summary_data)

# create the likert list
likert_list <- likert(summary = dt)

# examine its structure
str(likert_list)






# Load the necessary packages
library(tidyverse)
library(likert)

# Create the data as a tibble
data <- tibble(
  f1 = c("Agree", "Agree", "Agree", "Agree", "Disagree", "Strongly Agree"),
  f2 = c("Not Applicable", "Strongly Agree", "Strongly Agree", "Strongly Agree", "Strongly Agree", "Strongly Agree"),
  f3 = c("Strongly Agree", "Strongly Agree", "Not Applicable", "Strongly Agree", "Neutral", "Not Applicable")
)

# Convert the data to long format
data_long <- data %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")

# Create the likert plot
likert_data <- likert(my_data[,6]) %>%
  plot(type = "percent", plot.percents = TRUE, plot.percent.low = FALSE,
       plot.percent.high = FALSE, plot.percent.neutral = FALSE,
       centered = FALSE, plot.legend = TRUE, plot.grid = TRUE,
       plot.mean = FALSE)

# Customize the plot
likert_data + ggtitle("Responses to Survey Questions") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 12),
        legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 12))







# Load the necessary packages
library(tidyverse)
library(likert)

# Create the data as a tibble
data <- tibble(
  f1 = c("Agree", "Agree", "Agree", "Agree", "Disagree", "Strongly Agree"),
  f2 = c("Not Applicable", "Strongly Agree", "Strongly Agree", "Strongly Agree", "Strongly Agree", "Strongly Agree"),
  f3 = c("Strongly Agree", "Strongly Agree", "Not Applicable", "Strongly Agree", "Neutral", "Not Applicable")
)

# Check the levels of each column
levels(data$f1)
levels(data$f2)
levels(data$f3)

# Modify the data to ensure that each column has the same set of levels
data$f1 <- factor(data$f1, levels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))
data$f2 <- factor(data$f2, levels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))
data$f3 <- factor(data$f3, levels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))

# Convert the data to long format
data_long <- data %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")

# Create the likert plot
likert_data <- likert(data_long$Response) %>%
  plot(type = "percent", plot.percents = TRUE, plot.percent.low = FALSE,
       plot.percent.high = FALSE, plot.percent.neutral = FALSE,
       centered = FALSE, plot.legend = TRUE, plot.grid = TRUE,
       plot.mean = FALSE)

# Customize the plot
likert_data + ggtitle("Responses to Survey Questions") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 12),
        legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 12))
stressors_ <- likert(stressors)

# Create the likert plot
likert_data <- likert(stressors[,1]) %>%
  plot(type = "percent", plot.percents = TRUE, plot.percent.low = FALSE,
       plot.percent.high = FALSE, plot.percent.neutral = FALSE,
       centered = FALSE, plot.legend = TRUE, plot.grid = TRUE,
       plot.mean = FALSE)


# Define custom colors for each stress score category
custom_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF")
# Make the plot
ggplot(Stress, aes(x = scores, y = count, fill = stress)) +
  geom_col(color = "black") +
  geom_text(aes(label = count), vjust = 10, color = "white", size = 4) +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  labs(title = "Distribution of Response from each stress score category",
       subtitle = "59 students' stress scores fall between 300 and",
       x = "Stress Scores", y = "Number of Students") +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
