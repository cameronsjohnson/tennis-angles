################################################################################
# Title: 2019 Australian Open Final
# Date: November 2023
# Analyst: Cam Johnson
################################################################################

################################################################################
# Set up
################################################################################

library(tidyverse)
library(hexbin) # for heatmap 
library(reshape2) # for melt function
library(knitr) # for kable function
library(gganimate) # to animate tennis rally

# Colours
hard <- "#336699"
grass <- "#339966"
clay <- "#993300"
ball <- "#c6ed2c"

################################################################################
# Import and manipulatedata
################################################################################

# Import shot data as events, points and rallies
events <- read_csv("Data/events.csv")
points <- read.csv("Data/points.csv")
rallies <- read.csv("Data/rallies.csv")

# Change the coordinates in each table 

# Change the coordinates in the events table
events_transformed <- events %>% 
  rename(hitter_x = hitter_y,
         hitter_y = hitter_x,
         receiver_x = receiver_y,
         receiver_y = receiver_x)

# Change the coordinates in the points table
points_transformed <- points %>% 
  rename(x = y,
         y = x)

# Mutate points table for joining to events_transformed for winners data
points_transformed_winners <- points_transformed %>%
  select(winner, reason, rallyid, strokes) %>%
  mutate(strokeid = strokes) %>%
  select(-strokes)

complete_events <- events_transformed %>%
  left_join(points_transformed_winners, by = c("rallyid", "strokeid")) %>%
  mutate(winner = replace_na(winner, "Non-Winner")) %>%
  mutate(reason = replace_na(reason, "Non-Winner"))


################################################################################
# Plot tennis court
################################################################################

# Coordinates of the baseline + side line 
# Save the coordinates as a data frame as "base_side_line"
base_side_line <- data.frame(
  x = c(0, 0, 23.77, 23.77, 0),
  y = c(0, 10.97, 10.97, 0, 0)
)
# Coordinates of the service line and center line 
# Save the coordinates as a data frame as "serves_center_line"
serves_center_line <- data.frame(
  x = c(5.585, 5.585, 5.585, 18.385, 18.385, 18.385),
  y = c(1.37, 9.6, 5.485, 5.485, 1.37, 9.6)
)

# Plot the tennis court 
# Save the tennis court with name 'court'
court <- ggplot() +
  geom_path(data = base_side_line, aes(x = x, y = y)) +
  geom_path(data = serves_center_line, aes(x = x, y = y)) +
  geom_path(aes(x = c(23.77, 0), y = c(1.37, 1.37))) + # include lower singles lines
  geom_path(aes(x = c(23.77, 0), y = c(9.6, 9.6))) + # include upper singles lines
  geom_path(aes(x = c(11.985, 11.985), y = c(0, 10.97)), lty = 2) + # include dotted net line
  ylim(c(-1, 11.97)) + xlim(c(-4, 27.77)) + # zoom out 
  theme_void()

court

court_colour <- ggplot() +
  
  # Adding rectangles for service boxes
  geom_rect(aes(xmin = 0, xmax = 11.985, ymin = 1.37, ymax = 5.485), fill = "#336699") +
  geom_rect(aes(xmin = 11.985, xmax = 23.77, ymin = 1.37, ymax = 5.485), fill = "#336699") +
  geom_rect(aes(xmin = 0, xmax = 11.985, ymin = 5.485, ymax = 9.6), fill = "#336699") +
  geom_rect(aes(xmin = 11.985, xmax = 23.77, ymin = 5.485, ymax = 9.6), fill = "#336699") +
  geom_rect(aes(xmin = 0, xmax = 23.77, ymin = 0, ymax = 1.37), fill = "#336699") +
  geom_rect(aes(xmin = 0, xmax = 23.77, ymin = 9.6, ymax = 10.97), fill = "#336699") +
  
  # Drawing the baseline and sidelines
  geom_path(data = base_side_line, aes(x = x, y = y), color = "white", size = 1) +
  # Drawing the service and center lines
  geom_path(data = serves_center_line, aes(x = x, y = y), color = "white", size = 1) +
  # Adding other lines
  geom_path(aes(x = c(23.77, 0), y = c(1.37, 1.37)), color = "white", size = 1) + 
  geom_path(aes(x = c(23.77, 0), y = c(9.6, 9.6)), color = "white", size = 1) + 
  geom_path(aes(x = c(11.985, 11.985), y = c(0, 10.97)), lty = 2, color = "white", size = 1) +
  
  # Adjusting the plot limits and theme
  ylim(c(-3, 11.97)) + xlim(c(-4, 27.77)) + 
  theme_void() +
  theme(panel.background = element_rect(fill = "#339966"))

court_colour

court_colour_angles <- court_colour +
  geom_path(aes(x = c(-4, 11.985), y = c(5.485,5.485)), lty = 2, color = "white", size = 1) + # 0 degrees i.e. centre line
  geom_path(aes(x = c(-4, 11.985), y = c(4.185,5.485)), lty = 2, color = "white", size = 1) + # 5 degrees
  geom_path(aes(x = c(-4, 11.985), y = c(2.786,5.485)), lty = 2, color = "white", size = 1) + # 10 degrees
  geom_path(aes(x = c(-4, 11.985), y = c(1.387,5.485)), lty = 2, color = "white", size = 1) + # 15 degrees
  geom_path(aes(x = c(-4, 11.985), y = c(-0.012,5.485)), lty = 2, color = "white", size = 1) + # 20 degrees
  geom_path(aes(x = c(-4, 11.985), y = c(-1.411,5.485)), lty = 2, color = "white", size = 1) + # 25 degrees
  geom_path(aes(x = c(-4, 11.985), y = c(-2.810,5.485)), lty = 2, color = "white", size = 1)  # 30 degrees

court_colour_angles_annotated <- court_colour_angles +
  annotate("text", x = -3, y = 4.8, label = "5\u00B0", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = -3, y = 3.6, label = "10\u00B0", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = -3, y = 2.3, label = "15\u00B0", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = -3, y = 1.0, label = "20\u00B0", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = -3, y = -0.4, label = "25\u00B0", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = -3, y = -1.7, label = "30\u00B0", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = -0, y = -1.7, label = ">31\u00B0", size = 5, color = "white", fontface = "bold")


################################################################################
# Court colour + shades for lengths depending on winner probability

court_colour_depth <- ggplot() +
  
  # Adding rectangles for service boxes
  geom_rect(aes(xmin = 8.785, xmax = 11.985, ymin = 1.37, ymax = 9.6), fill = "#daf0ff") +
  geom_rect(aes(xmin = 5.585, xmax = 8.785, ymin = 1.37, ymax = 9.6), fill = "#336699") +
  geom_rect(aes(xmin = 2.792, xmax = 5.585, ymin = 1.37, ymax = 9.6), fill = "#45b6fe") +
  geom_rect(aes(xmin = 0, xmax = 2.792, ymin = 1.37, ymax = 9.6), fill = "#8fd3fe") +
  geom_rect(aes(xmin = -4, xmax = 0, ymin = -1, ymax = 11.97), fill = "#daf0ff") +
  geom_rect(aes(xmin = 11.985, xmax = 15.185, ymin = 1.37, ymax = 9.6), fill = "#daf0ff") +
  geom_rect(aes(xmin = 15.185, xmax = 18.385, ymin = 1.37, ymax = 9.6), fill = "#336699") +
  geom_rect(aes(xmin = 18.385, xmax = 21.177, ymin = 1.37, ymax = 9.6), fill = "#45b6fe") +
  geom_rect(aes(xmin = 21.177, xmax = 23.77, ymin = 1.37, ymax = 9.6), fill = "#8fd3fe") +
  geom_rect(aes(xmin = 23.77, xmax = 27.77, ymin = -1, ymax = 11.97), fill = "#daf0ff") +
  
  # Adding rectangles for remainder of tennis court
  # geom_rect(aes(xmin = 11.985, xmax = 23.77, ymin = 1.37, ymax = 5.485), fill = "#336699") +
  # geom_rect(aes(xmin = 11.985, xmax = 23.77, ymin = 5.485, ymax = 9.6), fill = "#336699") +
  geom_rect(aes(xmin = 0, xmax = 23.77, ymin = 0, ymax = 1.37), fill = "#336699") +
  geom_rect(aes(xmin = 0, xmax = 23.77, ymin = 9.6, ymax = 10.97), fill = "#336699") +
  geom_rect(aes(xmin = 0, xmax = 23.77, ymin = -1, ymax = 0), fill = "#339966") +
  geom_rect(aes(xmin = 0, xmax = 23.77, ymin = 10.97, ymax = 11.97), fill = "#339966") +
  # geom_rect(aes(xmin = 23.77, xmax = 27.77, ymin = -1, ymax = 11.97), fill = "#339966") +
  
  # Drawing the baseline and sidelines
  geom_path(data = base_side_line, aes(x = x, y = y), color = "white", size = 1) +
  # Drawing the service and center lines
  geom_path(data = serves_center_line, aes(x = x, y = y), color = "white", size = 1) +
  # Adding other lines
  geom_path(aes(x = c(23.77, 0), y = c(1.37, 1.37)), color = "white", size = 1) + 
  geom_path(aes(x = c(23.77, 0), y = c(9.6, 9.6)), color = "white", size = 1) + 
  geom_path(aes(x = c(11.985, 11.985), y = c(0, 10.97)), lty = 2, color = "white", size = 1) +
  
  # Adjusting the plot limits and theme
  theme_void()
  
court_colour_depth

court_colour_depth_annotated <- court_colour_depth +
  annotate("text", x = 10.385, y = 7.542, label = "0%", size = 5, color = "black", fontface = "bold") +
  annotate("text", x = 10.385, y = 3.428, label = "0%", size = 5, color = "black", fontface = "bold") +
  annotate("text", x = 7.185, y = 7.542, label = "45%", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = 7.185, y = 3.428, label = "45%", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = 4.188, y = 5.485, label = "25%", size = 7, color = "black", fontface = "bold") +
  annotate("text", x = 1.396, y = 5.485, label = "7%", size = 7, color = "black", fontface = "bold") +
  annotate("text", x = -2, y = 5.485, label = "3%", size = 7, color = "black", fontface = "bold") +
  annotate("text", x = 13.585, y = 7.542, label = "0%", size = 5, color = "black", fontface = "bold") +
  annotate("text", x = 13.585, y = 3.428, label = "0%", size = 5, color = "black", fontface = "bold") +
  annotate("text", x = 16.785, y = 7.542, label = "45%", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = 16.785, y = 3.428, label = "45%", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = 19.782, y = 5.485, label = "25%", size = 7, color = "black", fontface = "bold") +
  annotate("text", x = 22.574, y = 5.485, label = "7%", size = 7, color = "black", fontface = "bold") +
  annotate("text", x = 25.97, y = 5.485, label = "3%", size = 7, color = "black", fontface = "bold")
  # annotate("text", x = 11.985, y = 11.47, label = "KILLER ZONES: Probability of Hitting a Winner", size = 7, color = "black", fontface = "bold")


court_colour_depth_annotated

court_colour_depth_annotated <- court_colour_depth +
  annotate("text", x = 10.385, y = 7.542, label = "0%", size = 5, color = "black", fontface = "bold") +
  annotate("text", x = 10.385, y = 3.428, label = "0%", size = 5, color = "black", fontface = "bold") +
  annotate("text", x = 7.185, y = 7.542, label = "45%", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = 7.185, y = 3.428, label = "45%", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = 4.188, y = 5.485, label = "25%", size = 7, color = "black", fontface = "bold") +
  annotate("text", x = 1.396, y = 5.485, label = "7%", size = 7, color = "black", fontface = "bold") +
  annotate("text", x = -2, y = 5.485, label = "3%", size = 7, color = "black", fontface = "bold")
  # annotate("text", x = 11.985, y = 11.47, label = "KILLER ZONES: Probability of Hitting a Winner", size = 7, color = "black", fontface = "bold")


court_colour_depth_annotated

court_colour_depth_annotated2 <- court_colour_depth_annotated +
  geom_path(aes(x = c(-4, 11.985), y = c(1.387,5.485)), lty = 2, color = "white", size = 1) + # 15 degrees
  geom_path(aes(x = c(-4, 11.985), y = c(-1.411,5.485)), lty = 2, color = "white", size = 1) + # 25 degrees
  annotate("text", x = -3, y = 2.3, label = "15\u00B0", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = -3, y = -0.4, label = "25\u00B0", size = 5, color = "white", fontface = "bold")

court_colour_depth_annotated2

court_colour_goldilocks <- court_colour %>%
  

################################################################################
# Plot hitter position
################################################################################

court_colour_angles_annotated + # overlay the shot location on the court plot
  geom_point(data = events_transformed, # using the events_transformed table
             # retrieve the shot location from the x axis and y axis data 
             # colour code the shots by the player
             aes(x = hitter_x, y = hitter_y, color = hitter) 
  ) +
  scale_color_manual(values = c(ball, ball)) +
  theme(legend.position = "none", # Position the legends at the bottom
        plot.title = element_blank(
        )) + #Assign the size 
  ggtitle("") + #Give the plot a title
  annotate("text", x = -3, y = 4.8, label = "5\u00B0", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = -3, y = 3.6, label = "10\u00B0", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = -3, y = 2.3, label = "15\u00B0", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = -3, y = 1.0, label = "20\u00B0", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = -3, y = -0.4, label = "25\u00B0", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = -3, y = -1.7, label = "30\u00B0", size = 5, color = "white", fontface = "bold") +
  annotate("text", x = -0, y = -1.7, label = ">31\u00B0", size = 5, color = "white", fontface = "bold")


################################################################################
# Plot Djokovic position
################################################################################

#Get shots only hit by Djokovic
djokovic <- events_transformed %>%
  subset(hitter == "Djokovic")

# Plot using the geom_point function
court +
  geom_point(data = djokovic, # Use "djokovic" table
             aes(x = hitter_x, y = hitter_y, colour = hitter)) +
  
  theme(legend.position = "bottom", # Legend at the bottom 
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Djokovic Shot Position") # Give the plot a title 

# Shot heatmap 
court + 
  geom_hex(data = djokovic, 
           bins = 35, 
           aes(x = hitter_x, y = hitter_y), 
           alpha = 0.65) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Djokovic shot position") + 
  coord_equal()

# Shot category plot
court + 
  geom_point(data = djokovic, 
             aes(x = hitter_x, y = hitter_y, colour = stroke)) +
  ggtitle("Djokovic shot type")

# Shot type plot
court + 
  geom_point(data = djokovic, 
             aes(x = hitter_x, y = hitter_y, colour = type)) +
  ggtitle("Djokovic shot type")


################################################################################
# Plot Nadal position
################################################################################

#Get shots only hit by Nadal
nadal <- events_transformed %>%
  subset(hitter == "Nadal")

# Plot using the geom_point function
court_colour +
  geom_point(data = nadal, # Use "nadal" table
             aes(x = hitter_x, y = hitter_y, colour = hitter)) +
  
  theme(legend.position = "bottom", # Legend at the bottom 
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Nadal Shot Position") # Give the plot a title 

#Shot heatmap 
court + 
  geom_hex(data = nadal, 
           bins = 35, 
           aes(x = hitter_x, y = hitter_y), 
           alpha = 0.65) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Nadal shot position") + 
  coord_equal()

# Shot category plot
court + 
  geom_point(data = nadal, 
             aes(x = hitter_x, y = hitter_y, colour = stroke)) +
  ggtitle("Nadal shot type")

# Shot type plot
court + 
  geom_point(data = nadal, 
             aes(x = hitter_x, y = hitter_y, colour = type)) +
  ggtitle("Nadal shot type")


################################################################################
# Plot/animate rallies
################################################################################

# x coordinates of the hitter of every rallyid 
ball_x <- events_transformed %>%
  melt(id.vars = c("rallyid", "strokeid", "hitter", "receiver"),
       measure.vars = c("hitter_x")
  ) %>% 
  rename(x_kind = variable,
         x = value)

# y coordinates of the hitter of every rallyid
ball_y <- events_transformed %>% 
  melt(id.vars = c("rallyid", "strokeid", "hitter", "receiver"),
       measure.vars = c("hitter_y")
  ) %>% 
  rename(y_kind = variable,
         y = value)

# Bind the ball_x and ball_y by the rally id and stroke id 
ball_position <- inner_join(ball_x, ball_y[, c("rallyid", "strokeid", "y")], by = c("rallyid", "strokeid"))

# Return a table of which includes ball_position and the rally ids
kable(head(arrange(ball_position, rallyid)))


#Rally id: 100
rally100 <- ball_position %>% 
  filter(rallyid == 100) 

#Plot the rally on the court 
#name it rallie100_line for later
rally100_line <- court_colour + 
  #Use the geom_point() function to plot the shot hit from
  geom_point(data = rally100, aes(x = x, y = y), colour = "red") +
  #Use the geom_path() function to plot the trajectory of the shot
  geom_path(data = rally100, aes(x = x, y = y), colour = "red")


#Animate the rally 
rallie100_animate <- rally100_line +     
  transition_reveal(strokeid)
rallie100_animate


################################################################################
# Table of winners & reasons
################################################################################

winner_nadal <- points_transformed %>%
  filter(winner == "Nadal") %>%
  select(reason)

table(winner_nadal)

winner_djokovic <- points_transformed %>%
  filter(winner == "Djokovic") %>%
  select(reason)

table(winner_djokovic)

################################################################################
# Plot positions of winners
################################################################################

winners_position <- complete_events %>%
  filter(reason == "winner")

# all shots
court_colour +
  geom_point(data = complete_events,
             aes(x = hitter_x, y = hitter_y, colour = hitter)) +
  # geom_point(data = winner_position_nadal,
  #            aes(x = x, y = y, colour = "Winner")) +
  theme(legend.position = "bottom", # Legend at the bottom 
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Nadal Shot Position") # Give the plot a title 

# winner shots
court_colour +
  geom_point(data = winners_position,
             aes(x = hitter_x, y = hitter_y, colour = hitter)) +
  # geom_point(data = winner_position_nadal,
  #            aes(x = x, y = y, colour = "Winner")) +
  theme(legend.position = "bottom", # Legend at the bottom 
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Nadal Shot Position") # Give the plot a title 

table(winners_position$winner)


################################################################################
# Hitter angles
################################################################################

# Calculating angles from hitter_x and hitter_y positions
complete_events$hitter_x_fromcentre <- abs(11.985 - complete_events$hitter_x)   # hitter x position from centre court
complete_events$hitter_y_fromcentre <- abs(5.485 - complete_events$hitter_y)   # hitter y position from centre court
complete_events$hitter_angle_radians <- atan(complete_events$hitter_y_fromcentre/complete_events$hitter_x_fromcentre)
complete_events$hitter_angle_degrees <- complete_events$hitter_angle_radians*(180/pi)

# Mean angle of all shots
complete_events %>%
  summarise(mean = mean(hitter_angle_degrees))

# Mean angle of winner shots
complete_events %>%
  filter(reason == "winner") %>%
  summarise(mean = mean(hitter_angle_degrees))

# Histogram of frequency of hitter angles
ggplot(complete_events, aes(x = hitter_angle_degrees)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  ggtitle("Hitter angle from centre line") +
  xlab("Hitter Angle from Centre Line (degrees)") +
  ylab("Frequency")

# Histogram of frequency of hitter angles capped at 31 degrees
complete_events$capped_hitter_angle <- ifelse(complete_events$hitter_angle_degrees > 31, 31, complete_events$hitter_angle_degrees)

ggplot(complete_events, aes(x = capped_hitter_angle)) +
  geom_histogram(binwidth = 1, fill = ball, color = "black") +
  ggtitle("Hitter angle from centre line") +
  xlab("Hitter Angle from Centre Line (degrees)") +
  ylab("Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text = element_text(size = 12)
  ) 

# Grouping by hitter angles, in 2 degree increments
# Define custom breaks for the intervals
breaks2 <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, Inf)

# Define labels for the intervals
breaks2_labels <- c(
  "2 degrees",
  "4 degrees",
  "6 degrees",
  "8 degrees",
  "10 degrees",
  "12 degrees",
  "14 degrees",
  "16 degrees",
  "18 degrees",
  "20 degrees",
  "22 degrees",
  "24 degrees",
  "26 degrees",
  "28 degrees",
  "30 degrees",
  "32 degrees",
  "34 degrees",
  "30+ degrees"
)

# Use the cut function to create a new column with interval labels hitter angle
complete_events$hitter_angle_degrees_grouped2 <- cut(complete_events$hitter_angle_degrees, breaks = breaks2, labels = breaks2_labels, right = FALSE)

complete_events <- complete_events %>%
  mutate(reason2 = ifelse(reason == "winner", "winner", "non-winner"))


# Subset hitter_angle_degrees_grouped and reason columns, complete groupings and carry out calculations
complete_events_angles <- complete_events %>%
  select(hitter_angle_degrees_grouped2, reason2) %>%
  group_by(hitter_angle_degrees_grouped2) %>%
  summarise(count = n(),
            winners = sum(reason2 == "winner"),
            winner_probability = winners/count)


ggplot(complete_events_angles, aes(x = hitter_angle_degrees_grouped2, y = winner_probability)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Probability of Being a Winner by Hitter Angle",
       x = "Hitter Angle Degrees Grouped 2 Degrees",
       y = "Winner Probability")

# Grouping by hitter angles, in 3 degree increments
# Define custom breaks for the intervals
breaks3 <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, Inf)

# Define labels for the intervals
breaks3_labels <- c(
  "3 degrees",
  "6 degrees",
  "9 degrees",
  "12 degrees",
  "15 degrees",
  "18 degrees",
  "21 degrees",
  "24 degrees",
  "27 degrees",
  "30 degrees",
  "33 degrees",
  "36 degrees",
  "37+ degrees"
)

# Use the cut function to create a new column with interval labels hitter angle
complete_events$hitter_angle_degrees_grouped3 <- cut(complete_events$hitter_angle_degrees, breaks = breaks3, labels = breaks3_labels, right = FALSE)

complete_events <- complete_events %>%
  mutate(reason2 = ifelse(reason == "winner", "winner", "non-winner"))


# Subset hitter_angle_degrees_grouped and reason columns, complete groupings and carry out calculations
complete_events_angles <- complete_events %>%
  select(hitter_angle_degrees_grouped3, reason2) %>%
  group_by(hitter_angle_degrees_grouped3) %>%
  summarise(count = n(),
            winners = sum(reason2 == "winner"),
            winner_probability = winners/count)


ggplot(complete_events_angles, aes(x = hitter_angle_degrees_grouped3, y = winner_probability)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Probability of Being a Winner by Hitter Angle",
       x = "Hitter Angle Degrees Grouped 3 Degrees",
       y = "Winner Probability")


# Grouping by hitter angles, in 5 degree increments
# Define custom breaks for the intervals
breaks5 <- c(0, 5, 10, 15, 20, 25, 30, Inf)

# Define labels for the intervals
breaks5_labels <- c(
  "5",
  "10",
  "15",
  "20",
  "25",
  "30",
  "31+"
)

# Use the cut function to create a new column with interval labels hitter angle
complete_events$hitter_angle_degrees_grouped5 <- cut(complete_events$hitter_angle_degrees, breaks = breaks5, labels = breaks5_labels, right = FALSE)

complete_events <- complete_events %>%
  mutate(reason2 = ifelse(reason == "winner", "winner", "non-winner"))


# Subset hitter_angle_degrees_grouped and reason columns, complete groupings and carry out calculations
complete_events_angles <- complete_events %>%
  select(hitter_angle_degrees_grouped5, reason2) %>%
  group_by(hitter_angle_degrees_grouped5) %>%
  summarise(count = n(),
            winners = sum(reason2 == "winner"),
            winner_probability = winners/count)


ggplot(complete_events_angles, aes(x = hitter_angle_degrees_grouped5, y = winner_probability*100)) +
  geom_bar(stat = "identity", fill = ball, color = "black") +
  theme_minimal() +
  labs(title = "Probability of Being a Winner by Hitter Angle",
       x = "Hitter Angle from Centre Line (degrees)",
       y = "Winner Probability (%)") +
  theme(
    plot.title = element_blank(),
    axis.text = element_text(size = 12))


################################################################################
### Hitter depth
################################################################################

# Transfer all hitter shots to one quarter of court (as angles will be same from each quarter i.e. mirror images)

net_threshold <- 11.985
centre_threshold <- 5.485

complete_events_transformed <- complete_events %>%
  mutate(hitter_x_transformed = ifelse(hitter_x > net_threshold, 2 * net_threshold - hitter_x, hitter_x),
         hitter_y_transformed = ifelse(hitter_y > centre_threshold, 2* centre_threshold - hitter_y, hitter_y)
  )

# Plot all shots by winner/non-winner
court_colour_depth +
  geom_point(data = complete_events_transformed,
             aes(x = hitter_x_transformed, y = hitter_y_transformed, colour = reason2)) +
  theme(legend.position = "bottom", # Legend at the bottom 
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Winner Shot Position") # Give the plot a title 

# Average x coordinate (i.e. average position of hitter in x direction)
x_average <- mean(complete_events_transformed$hitter_x_transformed)
x_average

# Grouping by x coordinate, dividing court into 4 sections and an additional one beyond the baseline
# Define custom breaks for the intervals
breaks_x <- c(-Inf, 0, 2.792, 5.585, 8.377, Inf)

# Define labels for the intervals
breaks_x_labels <- c(
  "Beyond baseline",
  "Baseline quarter",
  "Baseline middle quarter",
  "Net middle quarter",
  "Net quarter"
)

# Use the cut function to create a new column with interval labels hitter angle
complete_events_transformed$hitter_x_transformed_grouped <- cut(complete_events_transformed$hitter_x_transformed, breaks = breaks_x, labels = breaks_x_labels, right = FALSE)

# Subset hitter_x_transformed_grouped and reason columns, complete groupings and carry out calculations
complete_events_transformed_x <- complete_events_transformed %>%
  select(hitter_x_transformed_grouped, reason2) %>%
  group_by(hitter_x_transformed_grouped) %>%
  summarise(count = n(),
            winners = sum(reason2 == "winner"),
            winner_probability = winners/count)


ggplot(complete_events_transformed_x, aes(x = hitter_x_transformed_grouped, y = winner_probability*100)) +
  geom_bar(stat = "identity", fill = ball, color = "black") +
  theme_minimal() +
  labs(title = "Probability of Being a Winner by Hitter Depth",
       x = "Hitter Depth from Net",
       y = "Winner Probability (%)") +
  theme(
    plot.title = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14))
