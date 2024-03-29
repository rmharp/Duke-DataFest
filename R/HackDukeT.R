#' ---
#' title: "HackDuke"
#' output: html_document
#' date: "`r Sys.Date()`"
#' ---
#' 
## ----setup, include=FALSE---------------------------------------------------------------------------------
if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)) {install.packages("tidyverse"); library(tidyverse)}
if(!require(rvest)) {install.packages("rvest"); library(rvest)}
if(!require(RSelenium)) {install.packages("RSelenium"); library(RSelenium)}
if(!require(wdman)) {install.packages("wdman"); library(wdman)}
if(!require(netstat)) {install.packages("netstat"); library(netstat)}
if(!require(xml2)) {install.packages("xml2"); library(xml2)}
if(!require(webdriver)) {install.packages("webdriver"); library(webdriver)}
if(!require(purrr)) {install.packages("purrr"); library(purrr)}
if (!require(here)) {install.packages("here"); library(here)}
if (!require(dotenv)) {install.packages("dotenv"); library(dotenv)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}

#' 
## ----data-------------------------------------------------------------------------------------------------
#data <- read.csv("./data/responses.csv")
#data_responses <- read.csv("./data/responses.csv")
data_responses <- read.csv("./Data Files/responses.csv")
#data_mediaviews <- read.csv("./data/media_views.csv")
data_mediaviews <- read.csv("./Data Files/media_views.csv")
data_pageviews <- read.csv("./Data Files/page_views.csv")

#' 
## ---------------------------------------------------------------------------------------------------------
### Informed EDA

sample_rows_with_non_na <- apply(data_responses[, 29:40], 1, function(x) any(!is.na(x)))
sample_filtered_data <- data_responses[sample_rows_with_non_na, ]
lrn_type_counts <- table(data_responses$lrn_type)
# Convert the table to a dataframe for ggplot2
lrn_type_counts_data_pageviews <- as.data.frame(lrn_type_counts)

# Rename the columns for clarity
names(lrn_type_counts_data_pageviews) <- c("lrn_type", "count")

# Calculate the total count
total_count <- sum(lrn_type_counts_data_pageviews$count)

# Add a percentage column
lrn_type_counts_data_pageviews <- lrn_type_counts_data_pageviews %>%
  mutate(percentage = (count / total_count) * 100)

# Create the bar chart showing percentages
ggplot(lrn_type_counts_data_pageviews, aes(x = lrn_type, y = percentage, fill = lrn_type)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue", show.legend = FALSE) + # Add border color and fill
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), # Adjust text size and angle for readability
        axis.title = element_text(size = 14), # Adjust axis title size
        title = element_text(size = 16), # Adjust plot title size
        panel.grid.major.x = element_blank(), # Remove x major grid lines for cleaner look
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.background = element_rect(fill = "white", colour = "black"), # Background color
        plot.background = element_rect(fill = "white", color = NA)) + # Plot background color
  labs(title = "Percentage of Each Unique Learning Type",
       x = "Learning Type",
       y = "Percentage (%)")

# Calculate the average points earned for each lrn_type
avg_points_by_lrn_type <- data_responses %>%
  group_by(lrn_type) %>%
  summarise(avg_points_earned = mean(points_earned, na.rm = TRUE))

# Create the bar plot
ggplot(avg_points_by_lrn_type, aes(x = lrn_type, y = avg_points_earned, fill = lrn_type)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  scale_fill_viridis_d() + # A professional color scale from the viridis package
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) + # Center the title for a professional look
  labs(title = "Average Points Earned by Learning Type",
       x = "Learning Type",
       y = "Average Points Earned")

# Calculate the average points earned for each lrn_type
avg_points_by_book <- data_responses %>%
  group_by(book) %>%
  summarise(avg_points_earned = mean(points_earned, na.rm = TRUE))

ggplot(avg_points_by_book, aes(x = book, y = avg_points_earned, fill = book)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  scale_fill_viridis_d(begin = 0.3, end = 0.9, option = "C") + # A professional and accessible color scale
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) + # Hide the legend title
  labs(title = "Average Points Earned by Book",
       x = "Book",
       y = "Average Points Earned")

data_responses$combined_column <- paste(data_responses$chapter_number, data_responses$section_number, sep = ".")

avg_points_by_combined <- data_responses %>%
  group_by(combined_column) %>%
  summarise(avg_points_earned = mean(points_earned, na.rm = TRUE))

ggplot(avg_points_by_combined, aes(x = combined_column, y = avg_points_earned, fill = combined_column)) +
  geom_col() +
  scale_fill_viridis_d(begin = 0.3, end = 0.9, option = "C") + # Use a professional color scale
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8), # Adjust text for readability
        axis.title.x = element_blank(), # Since labels are self-explanatory
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") + # Hide legend for clarity
  labs(title = "Average Points Earned by Chapter.Section",
       y = "Average Points Earned")

#' 
## ---------------------------------------------------------------------------------------------------------
### Uninformed EDA

plot_avg_points_by_column <- function(data, column_name) {
  # First, ensure the column is treated as a factor for the plotting
  data <- data %>%
    mutate(!!sym(column_name) := as.factor(!!sym(column_name)))
  
  # Then, group by and summarise
  avg_points <- data %>%
    group_by(!!sym(column_name)) %>%
    summarise(avg_points_earned = mean(points_earned, na.rm = TRUE), .groups = 'drop')
  
  # Determine if the original column (before conversion) is numeric
  is_numeric <- is.numeric(data[[column_name]])
  
  # Plotting
  p <- ggplot(avg_points, aes(x = !!sym(column_name), y = avg_points_earned, fill = !!sym(column_name))) +
    geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = paste("Average Points Earned by", column_name),
         x = column_name,
         y = "Average Points Earned")
  
  # Apply appropriate color scale based on data type
  if (is_numeric) {
    p <- p + scale_fill_viridis_c()
  } else {
    p <- p + scale_fill_viridis_d()
  }
  
  print(p)
}

# Exclude multiple columns when iterating for plots
columns_to_exclude <- c("points_earned", "page", "points_possible", "dt_submitted", "completes_page", "lrn_response_id", "lrn_dt_started", "lrn_dt_saved", "lrn_status", "lrn_option_0", "lrn_option_1", "lrn_option_2", "lrn_option_3", "lrn_option_4", "lrn_option_5", "lrn_option_6", "lrn_option_7", "lrn_option_8", "lrn_option_9", "lrn_option_10", "lrn_option_11", "response", "prompt", "user_agent")
columns_of_interest <- setdiff(names(data_responses), columns_to_exclude)

for(column_name in columns_of_interest) {
  plot_avg_points_by_column(data_responses, column_name)
}

#' 
## ---------------------------------------------------------------------------------------------------------
filtered <- subset(data_pageviews, !was_complete | !engaged)
engagement_by_chapter_and_institution <- filtered %>%
  filter(!is.na(engaged)) %>%
  group_by(book, chapter, institution_id) %>%
  summarise(mean_engaged = mean(engaged, na.rm = TRUE), median_engaged = median(engaged, na.rm = TRUE))

engagement_by_chapter_and_institution <- engagement_by_chapter_and_institution %>%
  arrange(median_engaged)

y_axis_limits <- quantile(engagement_by_chapter_and_institution$median_engaged, c(0.05, 0.95))

plot1 <- ggplot(engagement_by_chapter_and_institution, aes(x = chapter, y = median_engaged, color = factor(institution_id))) +
  geom_point() +
  facet_wrap(~book) +
  labs(title = "Engagement by Chapter and Institution", x = "Chapter", y = "Median Engaged") +
  scale_color_discrete(name = "Institution ID") +
  theme_minimal() +
  coord_cartesian(ylim = y_axis_limits)

plot2 <- ggplot(engagement_by_chapter_and_institution, aes(x = chapter, y = median_engaged, group = institution_id, color = factor(institution_id))) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~book) +
  labs(title = "Engagement by Chapter and Institution", x = "Chapter", y = "Median Engaged") +
  scale_color_discrete(name = "Institution ID") +
  theme_minimal()

institution_id_filtered_4 <- engagement_by_chapter_and_institution %>%
  filter(institution_id %in% c("d2e6c885-36f4-48b9-988b-42eef1f8ed9d", 
                               "364da48a-e0b2-4507-bc31-e7761fe 16e95", 
                               "94a809a9-a0ef-4c47-8d96-3a5ad76f674b",
                               "04157183-8665-400a-925d-3bbb70ffe45e"))

institution_id_filtered_3_1 <- engagement_by_chapter_and_institution %>%
  filter(institution_id %in% c("f17495c5-e105-492d-878a-07a03ea3f805", 
                               "fc5f1b1b-2aeb-4e09-93fc-06fdac0d8030", 
                               "94a809a9-a0ef-4c47-8d96-3a5ad76f674b"))



institution_id_filtered_3_2 <- engagement_by_chapter_and_institution %>%
  filter(institution_id %in% c("292cff87-3c74-4e94-8622-233afb0427dd", 
                               "2f830a93-5a14-4aff-a6e8-c7d2562e2007", 
                               "c699dd97-e5a4-49ce-9718-877a81b1d475"))

plot3 <- ggplot(institution_id_filtered_4, aes(x = chapter, y = median_engaged, group = institution_id, color = factor(institution_id))) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~book) +
  labs(title = "Engagement by Chapter and Institution", x = "Chapter", y = "Median Engaged") +
  scale_color_discrete(name = "Institution ID") +
  theme_minimal()

plot4 <- ggplot(institution_id_filtered_3_1, aes(x = chapter, y = median_engaged, group = institution_id, color = factor(institution_id))) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~book) +
  labs(title = "Engagement by Chapter and Institution", x = "Chapter", y = "Median Engaged") +
  scale_color_discrete(name = "Institution ID") +
  theme_minimal()

plot5 <- ggplot(institution_id_filtered_3_2, aes(x = chapter, y = median_engaged, group = institution_id, color = factor(institution_id))) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~book) +
  labs(title = "Engagement by Chapter and Institution", x = "Chapter", y = "Median Engaged") +
  scale_color_discrete(name = "Institution ID") +
  theme_minimal()

plot1
plot2
plot3
plot4
plot5

ggsave("plot.png", plot = plot5, width = 15, height = 10)
system("open plot.png")

#' 
## ---------------------------------------------------------------------------------------------------------
sample_data_checkpoints <- read_csv("./Data Files/checkpoints.csv")

#' 
## ---------------------------------------------------------------------------------------------------------
### Pulse Rating vs. Average Score

# Step 1: Calculate the average pulse rating for each construct within each book chapter
avg_pulse_rating_construct <- sample_data_checkpoints %>%
  filter(!is.na(book), !is.na(chapter_number), !is.na(response) & response != "" & !is.na(as.numeric(response))) %>%
  group_by(book, chapter_number, construct) %>%
  summarise(avg_pulse_rating = mean(as.numeric(response), na.rm = TRUE))

avg_pulse_rating <- sample_data_checkpoints %>%
  mutate(response = as.numeric(response)) %>%
  filter(!is.na(book) & !is.na(chapter_number) & !is.na(response)) %>%
  group_by(book, chapter_number) %>%
  summarise(avg_pulse_rating = mean(response, na.rm = TRUE), .groups = 'drop')

# Step 2: Calculate the average score for each book 
avg_score <- sample_data_responses %>%
  filter(!is.na(book), !is.na(chapter_number), !is.na(points_earned)) %>%
  group_by(book, chapter_number) %>%
  summarise(avg_score = mean(points_earned, na.rm = TRUE))

merged_data <- inner_join(avg_pulse_rating, avg_score, by = c("book", "chapter_number"))
merged_data_by_construct <- inner_join(avg_pulse_rating_construct, avg_score, by = c("book", "chapter_number"))

# Step 4: Plotting
ggplot(merged_data, aes(x = avg_pulse_rating, y = avg_score, color = book)) +
  geom_point() +
  labs(
    title = "Average Score by Chapter vs. Pulse Rating Overall",
    x = "Average Pulse Rating",
    y = "Average Score by Chapter"
  ) +
  theme_minimal()

ggplot(merged_data_by_construct, aes(x = avg_pulse_rating, y = avg_score, color = book)) +
  geom_point() +
  facet_wrap(~construct) +
  labs(
    title = "Average Score by Chapter vs. Pulse Rating by Construct",
    x = "Average Pulse Rating",
    y = "Average Score by Chapter"
  ) +
  theme_minimal()

## ---------------------------------------------------------------------------------------------------------
#| label: tidy-page-views

mean_section_engagements <- data_pageviews |>
  filter(!is.na(book), !is.na(chapter_number), !is.na(section_number), !is.na(engaged)) %>%
  group_by(book, chapter_number, section_number) |>
    summarize(mean_engagement = mean(engaged, na.rm = TRUE))

mean_section_scores <- data_responses %>%
  filter(!is.na(book), !is.na(chapter_number), !is.na(section_number), !is.na(points_earned)) %>%
  group_by(book, chapter_number, section_number) %>%
  summarise(mean_score = mean(points_earned, na.rm = TRUE))

merged_data <- inner_join(mean_section_engagements, mean_section_scores, by = c("book", "chapter_number", "section_number"))  %>% filter(mean_engagement < 1000000)

plot <- ggplot(merged_data, aes(x = mean_engagement, y = mean_score, color = book)) +
  geom_point() +
  facet_wrap(~ book, ncol = 2) +
  labs(x = "Mean Engagement", y = "Mean Score", title = "Density Graph of Mean Score vs. Mean Engagement, Facet Wrapped by Book") +
  theme(legend.position = "bottom",
        legend.justification = "right",
        legend.box = "horizontal",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

plot

table(merged_data)
mean_by_book <- merged_data %>%
  group_by(book) %>%
  summarize(mean_points = mean(mean_score, na.rm = TRUE), median_score = median(mean_score, na.rm = TRUE))
mean_by_book


#' ```
#' 
## ---------------------------------------------------------------------------------------------------------
data_mediaviews$combined_column <- paste(data_mediaviews$chapter_number, data_mediaviews$section_number, sep = ".")
sorted_mediaviews <- data_mediaviews[order(data_mediaviews$combined_column), ]
combined_counts <- table(sorted_mediaviews$combined_column)
combined_counts

