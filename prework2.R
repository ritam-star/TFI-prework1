# libraries
library(readxl)
library(tidyverse)
library(janitor)
library(scales)


#loading data
  #sheet 1
survey_data <- read_excel("dsr.xlsx", sheet = 1)

survey_questions <- read_excel("dsr.xlsx", sheet = 2)


                                                                 #DATA CLEANING#

survey_data[survey_data == ""] <- NA #replaced blanks w/ NA

survey_data <- janitor::clean_names(survey_data) #standardized col names 


#renaming cols 
survey_data <- survey_data %>%
  rename(
    candidate_id = candidate_id_number,
    reason_decline = factor_that_led_to_declining_the_offer,
    reason_alternative = instead_of_the_fellowship_ill_be_doing,
    recruitment_phone_call = how_did_chatting_with_a_recruiter_over_phone_call_impact_your_motivation_to_join_the_fellowship,
    recruitment_email = how_did_exchanging_emails_with_the_recruitment_team_impact_your_motivation_to_join_the_fellowship,
    recruitment_event = how_did_attending_any_of_the_recruitment_events_impact_your_motivation_to_join_the_fellowship,
    recruitment_group_interview = how_did_meeting_during_the_group_interview_stage_impact_your_motivation_to_join_the_fellowship,
    recruitment_webinar = how_did_attending_online_webinar_recruitment_events_impact_your_motivation_to_join_the_fellowship
  )


                                                                      #ANALYSIS#
# decline reasons 
reason_decline_counts <- survey_data %>%
  select(contains("reason_decline")) %>%
  pivot_longer(cols = everything(), names_to = "Reason", values_to = "Response") %>%
  filter(!is.na(Response)) %>%
  count(Response, sort = TRUE)

# checking results
print(reason_decline_counts) #checks out

#VISUALIZATION

# calculating top 5 reasons
top_5_decline_reasons <- reason_decline_counts %>%
  top_n(5, n) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

# plotting pie chart
ggplot(top_5_decline_reasons, aes(x = "", y = n, fill = Response)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Convert to pie chart
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5), size = 5) + 
    title = "Top 5 Reasons for Declining the Offer",
    x = NULL,
    y = NULL,
    fill = "Reason"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    #plot.background = element_rect(fill = "grey92", color = NA), #removed here, looks bad
    panel.background = element_rect(fill = "grey80", color = NA),  #grey box around circle
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18, color = "#222222", margin = margin(b = 10)),
    axis.text = element_blank(),
    axis.ticks = element_blank(),  
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10)
  )



# decline alternative reasons 
reason_alternative_counts <- survey_data %>%
  select(contains("reason_alternative")) %>%
  pivot_longer(cols = everything(), names_to = "Reason", values_to = "Response") %>%
  filter(!is.na(Response)) %>%
  count(Response, sort = TRUE)

# checking results
print(reason_alternative_counts) #checks out

#VISUALIZATION

# calculating top 5 reasons
top_5_alternative_reasons <- reason_alternative_counts %>%
  top_n(5, n) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

# plotting pie chart
ggplot(top_5_alternative_reasons, aes(x = "", y = n, fill = Response)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Convert to pie chart
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5), size = 5) + 
  labs(
    title = "Top 5 Alternatives Instead of Fellowship",
    x = NULL,
    y = NULL,
    fill = "Reason"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    #plot.background = element_rect(fill = "grey92", color = NA), #removed here, looks bad
    panel.background = element_rect(fill = "grey80", color = NA),  #grey box around circle
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18, color = "#222222", margin = margin(b = 10)),
    axis.text = element_blank(),
    axis.ticks = element_blank(),  
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10)
  )






#recruitment analysis
# summary statistics for recruitment event effectiveness
# function to analyze a specific recruitment column
analyze_recruitment <- function(column_name) {
  survey_data %>%
    filter(!is.na(.data[[column_name]])) %>%  #removing blank responses
    count(.data[[column_name]], sort = TRUE) %>%
    rename(Response = .data[[column_name]], Count = n)
}

# iterating function over the 5 variables
phone_call_analysis <- analyze_recruitment("recruitment_phone_call")
email_analysis <- analyze_recruitment("recruitment_email")
event_analysis <- analyze_recruitment("recruitment_event")
group_interview_analysis <- analyze_recruitment("recruitment_group_interview")
webinar_analysis <- analyze_recruitment("recruitment_webinar")

# checking results 
print(phone_call_analysis)
print(email_analysis)
print(event_analysis)
print(group_interview_analysis)
print(webinar_analysis) #checks out
 

#graphs
plot_recruitment <- function(data, title) {
  ggplot(data, aes(x = reorder(Response, Count), y = Count, fill = Count)) +
    geom_col(show.legend = FALSE, width = 0.6, color = "black", linewidth = 0.3) + 
    geom_text(aes(label = Count), hjust = -0.2, size = 5, fontface = "bold", color = "black") +  
    coord_flip() +
    scale_fill_gradient(low = "#D55E00", high = "#0072B2") + 
    labs(
      title = title,
      x = NULL,
      y = "Response Count"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "grey92", color = NA),  
      panel.background = element_rect(fill = "grey80", color = NA),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18, color = "#222222", margin = margin(b = 10)),
      axis.text.y = element_text(face = "bold", size = 12, color = "#444444"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}


# plotting bar charts
plot_recruitment(phone_call_analysis, "Impact of Phone Calls")
plot_recruitment(email_analysis, "Impact of Emails")
plot_recruitment(event_analysis, "Impact of Recruitment Events")
plot_recruitment(group_interview_analysis, "Impact of Group Interviews")
plot_recruitment(webinar_analysis, "Impact of Webinars")























































