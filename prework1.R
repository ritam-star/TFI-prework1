#libraries
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


likert_columns <- names(data_delhi)[5:length(data_delhi)]  # bar first 4 columns


data_delhi[likert_columns] <- lapply(data_delhi[likert_columns], function(x) likert_mapping[x])

data_delhi[is.na(data_delhi)] <- 0


# Meta Analysis ------------------------------------------------


# categorizing columns
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

# mean scores for each category (bit redundant)
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

# sorting categories by lowest scores (biggest challenges)
category_scores <- category_scores %>% arrange(Average_Score)

# testing code
print(category_scores)

#Graphs

# bar plot with gradient fill
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

# all into one data frame (redundant?)
fes_scores_df <- rbind(
  leadership_support_df,
  resources_ecosystem_df,
  career_growth_df,
  organizational_engagement_df,
  safety_work_env_df
)


#Graphs
# Career Growth & Post-Fellowship opportunities
# shortening labels for the questions
career_growth_plot_df$Short_Question <- c(
  "Access to Career Support",
  "Exciting Career Options"
)

# custom colors (for easy do overs)
custom_colors <- c("#3498db", "#e74c3c")

# bar chart
ggplot(career_growth_plot_df, aes(x = reorder(Short_Question, Score), y = Score, fill = Short_Question)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
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
# Shortening question labels
organizational_engagement_df$Short_Question <- c(
  "Work Recognition",
  "Views Considered",
  "Approach Senior Mgmt"
)

#  custom colors
org_custom_colors <- c("#3498db", "#e74c3c", "#9b59b6") 

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
resources_ecosystem_df$Short_Question <- c(
  "Community Support",
  "School Support"
)

# Define custom colors
resources_custom_colors <- c("#3498db", "#e74c3c")

# Create bar chart
ggplot(resources_ecosystem_df, aes(x = reorder(Short_Question, Score), y = Score, fill = Short_Question)) +
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


# Beautified Graphs -------------------------------------------------------

# Career Growth & Post-Fellowship opportunities
ggplot(career_growth_plot_df, aes(x = reorder(Short_Question, Score), y = Score, fill = Score)) +
  geom_col(show.legend = FALSE, width = 0.6, color = "black", size = 0.3) +
  geom_text(aes(label = round(Score, 2)), hjust = -0.2, size = 5, fontface = "bold", color = "black") +
  coord_flip() +
  scale_fill_gradient(low = "#D55E00", high = "#0072B2") +  # Gradient color
  labs(
    title = "📈 Career Growth & Post-Fellowship Opportunities",
    x = NULL,
    y = "Average Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "grey92", color = NA),  # Overall Grey Background
    panel.background = element_rect(fill = "grey80", color = NA),  # Grey Axis Area
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18, color = "#222222", margin = margin(b = 10)),
    axis.text.y = element_text(face = "bold", size = 12, color = "#444444"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Resources & Ecosystem Support 
ggplot(resources_ecosystem_df, aes(x = reorder(Short_Question, Score), y = Score, fill = Score)) +
  geom_col(show.legend = FALSE, width = 0.6, color = "black", size = 0.3) +
  geom_text(aes(label = round(Score, 2)), hjust = -0.2, size = 5, fontface = "bold", color = "black") +
  coord_flip() +
  scale_fill_gradient(low = "#D55E00", high = "#0072B2") +  # Gradient color
  labs(
    title = "🌍 Resources & Ecosystem Support",
    x = NULL,
    y = "Average Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "grey92", color = NA),  # Overall Grey Background
    panel.background = element_rect(fill = "grey80", color = NA),  # Grey Axis Area
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18, color = "#222222", margin = margin(b = 10)),
    axis.text.y = element_text(face = "bold", size = 12, color = "#444444"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Organizational Engagement & Morale
ggplot(organizational_engagement_df, aes(x = reorder(Short_Question, Score), y = Score, fill = Score)) +
  geom_col(show.legend = FALSE, width = 0.6, color = "black", size = 0.3) +
  geom_text(aes(label = round(Score, 2)), hjust = -0.2, size = 5, fontface = "bold", color = "black") +
  coord_flip() +
  scale_fill_gradient(low = "#D55E00", high = "#0072B2") +  # Gradient color
  labs(
    title = "🏢 Organizational Engagement & Morale",
    x = NULL,
    y = "Average Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "grey92", color = NA),  # Overall Grey Background
    panel.background = element_rect(fill = "grey80", color = NA),  # Grey Axis Area
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18, color = "#222222", margin = margin(b = 10)),
    axis.text.y = element_text(face = "bold", size = 12, color = "#444444"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )




