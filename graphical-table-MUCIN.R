############### Graphical Table
library(dplyr)
library(tidyr)
library(readxl)


# Your original code
summary_MUC <- all.counts %>%
  filter(grepl("MUC", EXPERIMENT, ignore.case = TRUE)) %>%  
  group_by(wormID, SEX, STRUCTURE) %>%
  summarize(
    mean_ratio_MUC = mean(ratio_MUCIN, na.rm = TRUE)
  )

# Pivot the table
summary_pivoted_MUC <- summary_MUC %>%
  pivot_wider(names_from = STRUCTURE, values_from = mean_ratio_MUC)

# Print the pivoted table
print(summary_pivoted_MUC)

# Write the summary to a CSV file
write.csv(summary_pivoted_MUC, "/Users/rribeiro/_hcr-counts/summary_MUC.csv", row.names = FALSE)



#### Graphical table plot
library(ggplot2)

# Define colors for the graphical table
colors <- c("white" = "white", "grey" = "grey", "peru" = "peru")

# Define custom order for 'SEX' column
sex_order <- c("MALE", "FEMALE", "UNKNOWN")

# Create a function to assign colors based on values
assign_color <- function(value) {
  ifelse(is.na(value), "white", ifelse(value == 0, "grey", "yellow2"))
}

# Apply the color function to each cell in the table
summary_pivoted_colors <- summary_pivoted_MUC
summary_pivoted_colors[, -c(1, 2)] <- apply(summary_pivoted_colors[, -c(1, 2)], 2, assign_color)

# Convert the data to long format for plotting
summary_pivoted_long <- tidyr::gather(summary_pivoted_colors, key = "structure", value = "color", -wormID, -SEX)

# Arrange the data by 'SEX' in custom order
summary_pivoted_long <- summary_pivoted_long[order(factor(summary_pivoted_long$SEX, levels = sex_order)), ]

# Plot the graphical table with legend
ggplot(summary_pivoted_long, aes(x = structure, y = wormID, fill = color)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = colors, labels = c("Absent", "Present", "Not found")) +
  labs(x = "Structure", y = "Worm ID", title = "Graphical Table of Mean Ratio MUCIN") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~SEX, scales = "free_y") +
  guides(fill = guide_legend(title = "Color"))
