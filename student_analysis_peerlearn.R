library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

#Simulating administrative data
set.seed(123)
admin_data <- data.frame(
  student_id = 1001:1050,
  gender = sample(c("Male", "Female", "Non-binary"), 50, replace = TRUE),
  gpa = round(runif(50, 2.0, 4.0), 2),
  credits_completed = sample(0:120, 50, replace = TRUE),
  persisted = sample(c(1, 0), 50, replace = TRUE, prob = c(0.7, 0.3))
)

#Simulating PeerLearn survey data
survey_data <- data.frame(
  student_id = sample(1001:1050, 45),
  peer_feedback_score = round(runif(45, 1, 5), 1),
  peer_reviews_given = sample(0:5, 45, replace = TRUE),
  engagement_level = sample(c("High", "Medium", "Low"), 45, replace = TRUE)
)

#Merging datasets
merged_data <- left_join(admin_data, survey_data, by = "student_id")

#Clean & reshape
merged_data <- merged_data %>%
  mutate(peer_feedback_score = ifelse(is.na(peer_feedback_score) & peer_reviews_given == 0, 0, peer_feedback_score))

merged_data$engagement_level[is.na(merged_data$engagement_level)] <- "Unknown"

#Basic Analysis
persistence_summary <- merged_data %>%
  group_by(engagement_level) %>%
  summarise(
    count = n(),
    avg_gpa = mean(gpa, na.rm = TRUE),
    persistence_rate = mean(persisted, na.rm = TRUE)
  )

print(persistence_summary)

#Visualise
ggplot(merged_data, aes(x = gpa, fill = factor(persisted))) +
  geom_histogram(bins = 10, position = "dodge") +
  labs(title = "GPA Distribution by Persistence", x = "GPA", fill = "Persisted (1 = Yes)") +
  theme_minimal()

ggplot(merged_data, aes(x = engagement_level, y = peer_feedback_score, fill = engagement_level)) +
  geom_boxplot() +
  labs(title = "Peer Feedback Score by Engagement Level", y = "Feedback Score", x = "Engagement Level") +
  theme_minimal()

#Exporting the data
write_csv(merged_data, "cleaned_student_peerlearn_data.csv")