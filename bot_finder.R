# Script to pull data from Adobe Analytics and compare to past data to see if
# an apparent bot has emerged

# Load libraries
library(tidyverse)
library(RSiteCatalyst)

# Load the theme
source("./scripts/themes_ref.R")

# Set the end date to be the most recent Saturday
# end_date <- (Sys.Date() - as.POSIXlt(Sys.Date())$wday - 1) %>%
#   as.character()
end_date <- "2017-05-13"

# Set the start date to be six weeks before the end date
start_date <- as.character(as.Date(end_date) - 41) 

trend_metrics = c("visits", "orders", "revenue")
trend_elements = c("operatingsystem","browser")

# Get authentication credentials
adobe_key = Sys.getenv("ADOBE_API_USERNAME_MSC")
adobe_secret = Sys.getenv("ADOBE_API_SECRET_MSC")
rsid = Sys.getenv("ADOBE_RSID_MSC")

# Authenticate
SCAuth(adobe_key, adobe_secret)

# Vector of segment IDs to apply to data when pulling. The two below are
# `No Punchouts or Bots` and `Last Touch Channel = Direct`
segment_ids <- c("537ce3d0e4b083ab6f5eccbd","s300000270_58571c53e4b0374f4d0c19d0")
# segment_ids <- ""

# Get the data
trended_data <- QueueTrended(rsid,
                           date.from = start_date,
                           date.to = end_date,
                           metrics = trend_metrics,
                           elements = trend_elements,
                           date.granularity = "week",
                           segment.id = segment_ids,
                           top = 100
)

# Convert the datetime from POSIXlt to Date. Has it always come back as POSIXlt?
trended_data$datetime <- as.Date(trended_data$datetime)

# If almost no orders have been placed, then it's not a probable bot
no_orders_data <- trended_data %>%
  filter(orders == 0)

# At this point, all we want is the visits data (plus the dimensions). Note the use
# of standard evaluation here (select_()) -- that's because we want to be able to try
# out different combinations of elements/dimensions, and that's needed to pass in 
# a vector of elements -- using .dots.
visits_data <- no_orders_data %>%
  select_("datetime", .dots = trend_elements, "visits")
  
# We're going to again use standard evaluation (note the group_by_() and .dots) to group by all
# of the elements specified earlier, plus the metric (we're in long format, remember? So,
# that's the name of the metric). With that grouping, we then calculate the median absolute
# deviation (MAD) and then calculate a lower (min) and upper (max) bound for whether a 
# value is outside the expected range -- is an outlier.
find_min_max <- visits_data %>%
  group_by_(.dots = trend_elements) %>%
  summarise(min = median(visits) - mad(visits), max = median(visits) + mad(visits))

# Here's where we'll actually do the comparison of each value to the respective median PLUS 
# the MAD (we only care about spikes -- not dips) to ID which rows look to be a positive outlier. 
# There will be some super-low-volume combinations that pop up as outliers, so set a threshold for those.
find_deviations <- visits_data %>%
  left_join(find_min_max) %>%
  mutate(deviation = ifelse(visits > max, TRUE, FALSE)) %>%
  filter(deviation == TRUE & visits > 150)

# Join this back to the full data set so we have all of the data and can see the spike.
# First, join just to get all the data, then join again to get TRUE/FALSE on deviations.
# Then, mash together the dimensions into a single value
final_data <- find_deviations %>%
  select_(.dots = trend_elements) %>% 
  left_join(visits_data) %>% 
  left_join(find_deviations) %>% 
  select(-min, -max) %>% 
  mutate(deviation = ifelse(is.na(deviation), NA, visits)) %>% 
  unite_("combined_dims", trend_elements, sep = " > ")

# Do a little sorting -- ultimately, want to visualize from highest to lowest traffic
sort_order <- final_data %>% 
  group_by(combined_dims) %>% 
  summarise(total_visits = sum(visits)) %>% 
  arrange(-total_visits)

# Convert combined_dims to a factor so we get the order we want
final_data$combined_dims <- factor(final_data$combined_dims,
                                   levels = sort_order$combined_dims)

# Plot it!
final_plot <- ggplot(final_data, mapping = aes(x=datetime, y=visits)) +
  geom_bar(stat = "identity", fill = "gray70") +
  geom_bar(stat = "identity", mapping = aes(y=deviation), fill = "darkred") +
  geom_text(mapping = aes(x = median(unique(visits_data$datetime)), 
                          y = max(final_data$visits) * 1.1, label = combined_dims)) +
  facet_grid(combined_dims ~ .) +
  default_theme

final_plot


# # Plot visits
# 
# # Filter to just use the visits_percent rows
# visits_data <- long_data %>%
#   filter(metric == "visits_percent") %>%
#   filter(value >= .01)
# 
# # Get the order and apply it as a factor so the charts will be readable
# os_browser_order <- visits_data %>%
#   group_by(os_browser) %>%
#   summarise(total_percent = sum(value)) %>%
#   arrange(total_percent) %>%
#   select(os_browser)
# 
# visits_data$os_browser <- factor(visits_data$os_browser,
#                                  levels = os_browser_order$os_browser)
# 
# visits_plot <- ggplot(visits_data, mapping = aes(x=datetime, y=os_browser)) +
#   geom_tile(mapping = aes(fill=value)) +
#   geom_text(mapping = aes(label=sprintf("%1.2f%%", 100*value))) +
#   scale_fill_gradient(low = "white", high = "lightblue") +
#   default_theme
# 
# 
# 
# # Get the totals for each metric for each week. We'll use this to
# # calculate "% of total," which is more normalized
# weekly_totals <- trended_data %>%
#   group_by(datetime) %>%
#   summarise(total_visits = sum(visits), total_orders = sum(orders), total_revenue = sum(revenue))
# 
# # Bring that back into trended_data and then calculate %s
# augmented_trends <- left_join(trended_data, weekly_totals) %>%
#   mutate(visits_percent = visits / total_visits, orders_percent = orders / total_orders,
#          revenue_percent = revenue / total_revenue) %>%
#   select(-segment.id, -segment.name)
# 
# 
# # Clean up the data and convert to long format
# long_data <- augmented_trends %>%
#   gather(metric, value, -datetime, -operatingsystem, -browser) %>%
#   mutate(os_browser = paste0(operatingsystem,": ",browser))
# 
# visits_plot <- ggplot(visits_data, mapping = aes(x=os_browser, y=value)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   facet_grid(. ~ datetime) +
#   default_theme
# 
# 
# visits_plot <- ggplot(visits_data, mapping = aes(x=datetime, y=value)) +
#   geom_line() +
#   facet_grid(operatingsystem ~ browser)
# 
# visits_plot
# 
