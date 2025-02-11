library(readr)
library(tidyverse)


# Loading Data and Cleaning -----------------------------------------------


#Loading Data (Delhi) (&Deleting the last column)
data <- read_csv("fes.csv")

data_delhi <- data %>%
  filter(City == "Delhi")

data_delhi <- data_delhi[, -ncol(data_delhi)]



# Converting to Likert Scale
data_delhi <- read.csv("data_delhi.csv", stringsAsFactors = FALSE)


likert_mapping <- c("Strongly Disagree" = 1, 
                    "Disagree" = 2, 
                    "Agree" = 3, 
                    "Strongly Agree" = 4, 
                    "na" = NA)


likert_columns <- names(data_delhi)[5:length(data_delhi)]  # Assuming first 4 columns are non-Likert


data_delhi[likert_columns] <- lapply(data_delhi[likert_columns], function(x) likert_mapping[x])

data_delhi[is.na(data_delhi)] <- 0


# Meta Analysis ------------------------------------------------


# Define column groups based on the categories
leadership_support <- c("My.PM.consistently.shares.helpful.and.timely.feedback.on.my.development.as.a.teacher.and.leader.with.me",
                        "My.PM.helps.me.identify.opportunities.within.and.beyond.TFI.that.will.aid.my.development.as.a.leader",
                        "My.PM.is.an.effective.mentor.to.me",
                        "There.is.a.person.s..at.Teach.For.India.that.shares.timely.feedback.with.me.on.my.classroom.s.progress")

resources_ecosystem <- c("There.is.a.sufficient.ecosystem.for.consistent.and.timely.support.in.my.city.that.I.can.access.for.challenges.I.face.while.working.with.the.community.in.which.my.placement.school.is.located",
                         "There.is.a.sufficient.ecosystem.for.consistent.and.timely.support.in.my.city.that.I.can.access.for.the.challenges.I.face.while.working.in.my.placement.school")

career_growth <- c("I.have.access.to.helpful.and.timely.support..resources.and.networks.via.Teach.For.India.to.find.a.suitable.post.Fellowship.opportunity",
                   "I.find.the.Career.options.available.to.me.post.Fellowship.exciting")

organizational_engagement <- c("I.feel.my.work..when.worthy..is.aptly.recognised.and.praised.at.Teach.For.India",
                               "In.my.experience..my.views.are.taken.into.account.and.acted.upon.at.Teach.for.India",
                               "I.feel.comfortable.in.approaching.Senior.Management.with.any.concerns..questions.or.ideas.that.I.have")

safety_work_env <- c("I.feel.safe.working.in.my.placement.school",
                     "I.feel.safe.in.the.community.in.which.in.my.placement.school.is.located")

# Compute mean scores for each category
category_scores <- data.frame(
  Category = c("Leadership & Support", "Resources & Ecosystem Support", 
               "Career Growth & Post-Fellowship Opportunities", 
               "Organizational Engagement & Morale", "Safety & Work Environment"),
  Average_Score = c(
    mean(colMeans(data_delhi[, leadership_support], na.rm = TRUE)),
    mean(colMeans(data_delhi[, resources_ecosystem], na.rm = TRUE)),
    mean(colMeans(data_delhi[, career_growth], na.rm = TRUE)),
    mean(colMeans(data_delhi[, organizational_engagement], na.rm = TRUE)),
    mean(colMeans(data_delhi[, safety_work_env], na.rm = TRUE))
  )
)

# Sort categories by lowest scores (biggest challenges)
category_scores <- category_scores %>% arrange(Average_Score)

# Print the results
print(category_scores)

#Graphs

# Create the bar plot with gradient fill
ggplot(category_scores, aes(x = reorder(Category, Average_Score), y = Average_Score, fill = Average_Score)) +
  geom_bar(stat = "identity", width = 0.6) +
  coord_flip() +  # Horizontal bar chart for better readability
  labs(
    title = "Average Scores by Category - Delhi Fellows",
    x = "Category",
    y = "Average Score"
  ) +
  scale_fill_gradient(
    low = "#ADD8E6",  # Light blue
    #mid = "#576CB9,
    high = "#00008B"  # Dark blue
  ) +
  theme_minimal() +
  theme(legend.position = "none")



# Main Issue Analysis -----------------------------------------------------

# Leadership & Support
leadership_support_df <- data.frame(
  Question = c(
    "My PM consistently shares helpful and timely feedback on my development as a teacher and leader with me",
    "My PM helps me identify opportunities within and beyond TFI that will aid my development as a leader",
    "My PM is an effective mentor to me",
    "There is a person(s) at Teach For India that shares timely feedback with me on my classroom's progress"
  ),
  Score = colMeans(data_delhi[, leadership_support], na.rm = TRUE),
  Category = "Leadership & Support"
)

# Resources & Ecosystem Support
resources_ecosystem_df <- data.frame(
  Question = c(
    "There is a sufficient ecosystem for consistent and timely support in my city that I can access for challenges I face while working with the community in which my placement school is located",
    "There is a sufficient ecosystem for consistent and timely support in my city that I can access for the challenges I face while working in my placement school"
  ),
  Score = colMeans(data_delhi[, resources_ecosystem], na.rm = TRUE),
  Category = "Resources & Ecosystem Support"
)

# Career Growth & Post-Fellowship Opportunities
career_growth_df <- data.frame(
  Question = c(
    "I have access to helpful and timely support, resources and networks via Teach For India to find a suitable post-Fellowship opportunity",
    "I find the Career options available to me post Fellowship exciting"
  ),
  Score = colMeans(data_delhi[, career_growth], na.rm = TRUE),
  Category = "Career Growth & Post-Fellowship Opportunities"
)

# Organizational Engagement & Morale
organizational_engagement_df <- data.frame(
  Question = c(
    "I feel my work, when worthy, is aptly recognised and praised at Teach For India",
    "In my experience, my views are taken into account and acted upon at Teach for India",
    "I feel comfortable in approaching Senior Management with any concerns, questions or ideas that I have"
  ),
  Score = colMeans(data_delhi[, organizational_engagement], na.rm = TRUE),
  Category = "Organizational Engagement & Morale"
)

# Safety & Work Environment
safety_work_env_df <- data.frame(
  Question = c(
    "I feel safe working in my placement school",
    "I feel safe in the community in which in my placement school is located"
  ),
  Score = colMeans(data_delhi[, safety_work_env], na.rm = TRUE),
  Category = "Safety & Work Environment"
)

# Combine all into one data frame
fes_scores_df <- rbind(
  leadership_support_df,
  resources_ecosystem_df,
  career_growth_df,
  organizational_engagement_df,
  safety_work_env_df
)



# Career Growth & Post-Fellowship Oppoetunities
# short labels for the questions
career_growth_plot_df$Short_Question <- c(
  "Access to Career Support",
  "Exciting Career Options"
)

# custom colors
custom_colors <- c("#3498db", "#e74c3c")  # TFI-themed distinct colors

# bar chart
ggplot(career_growth_plot_df, aes(x = reorder(Short_Question, Score), y = Score, fill = Short_Question)) +
  geom_col(show.legend = FALSE) +  # Hide legend since colors are intuitive
  coord_flip() +  # Flip for better readability
  scale_fill_manual(values = custom_colors) +  # Use distinct colors
  labs(
    title = "Career Growth & Post-Fellowship Opportunities",
    x = "Question",
    y = "Average Score"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12, face = "bold")
  )

# Organisational Engagement & Morale
# Shorten question labels
organizational_engagement_df$Short_Question <- c(
  "Work Recognition",
  "Views Considered",
  "Approach Senior Mgmt"
)

#  custom colors
org_custom_colors <- c("#3498db", "#e74c3c", "#9b59b6")  # Green, Orange, Purple

# bar chart
ggplot(organizational_engagement_df, aes(x = reorder(Short_Question, Score), y = Score, fill = Short_Question)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = org_custom_colors) +
  labs(
    title = "Organizational Engagement & Morale",
    x = "Question",
    y = "Average Score"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12, face = "bold")
  )


# Resource and Ecosystem Support
# Shorten question labels
leadership_support_df$Short_Question <- c(
  "Community Support",
  "School Support"
)

# Define custom colors
resources_custom_colors <- c("#3498db", "#e74c3c")

# Create bar chart
ggplot(leadership_support_df, aes(x = reorder(Short_Question, Score), y = Score, fill = Short_Question)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = resources_custom_colors) +
  labs(
    title = "Resources & Ecosystem Support",
    x = "Question",
    y = "Average Score"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12, face = "bold")
  )








