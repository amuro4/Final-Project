# Load required libraries
library(palmerpenguins)  # Dataset
library(dplyr)           # Data manipulation
library(ggplot2)         # Data visualization

# Load and inspect the dataset
data("penguins")  # Load the dataset

# View the structure of the dataset
str(penguins)     # Inspect data types and structure
summary(penguins) # Generate summary statistics

# Check for missing values
colSums(is.na(penguins))  # Count missing values for each column

# Initial EDA: Univariate analysis
# Visualize distribution of numerical variables
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Body Mass", x = "Body Mass (g)", y = "Count") +
  theme_minimal()

# Visualize species count
ggplot(penguins, aes(x = species, fill = species)) +
  geom_bar() +
  labs(title = "Species Distribution", x = "Species", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Initial EDA: Bivariate analysis
# Scatter plot of bill length vs bill depth by species
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Bill Length vs Bill Depth by Species",
       x = "Bill Length (mm)", y = "Bill Depth (mm)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

# Pairwise correlation (numerical variables only)
numeric_vars <- penguins %>%
  select(where(is.numeric)) %>%
  na.omit()

cor(numeric_vars)





# Custom function to calculate the mode
calculate_mode <- function(x) {
  x <- na.omit(x)  # Remove NA values
  uniq_x <- unique(x)  # Get unique values
  uniq_x[which.max(tabulate(match(x, uniq_x)))]  # Return the most frequent value
}

# Handle missing values for numerical variables
# Replace missing values with median grouped by species
# Justification: The median is a robust measure of central tendency that is not affected by outliers.
# Grouping by species ensures that species-specific variability is preserved in the imputation process.
penguins <- penguins %>%
  group_by(species) %>%
  mutate(
    bill_length_mm = ifelse(is.na(bill_length_mm), median(bill_length_mm, na.rm = TRUE), bill_length_mm),
    bill_depth_mm = ifelse(is.na(bill_depth_mm), median(bill_depth_mm, na.rm = TRUE), bill_depth_mm),
    flipper_length_mm = ifelse(is.na(flipper_length_mm), median(flipper_length_mm, na.rm = TRUE), flipper_length_mm),
    body_mass_g = ifelse(is.na(body_mass_g), median(body_mass_g, na.rm = TRUE), body_mass_g)
  ) %>%
  ungroup()

# Handle missing values for categorical variables
# Replace missing values in 'sex' with mode grouped by species
# Justification: The mode is the most frequently occurring value in a categorical variable.
# Grouping by species ensures that the imputed values align with the characteristics of each species,
# thereby preserving meaningful patterns in the data.
penguins <- penguins %>%
  group_by(species) %>%
  mutate(
    sex = ifelse(is.na(sex), as.character(calculate_mode(sex)), as.character(sex))
  ) %>%
  ungroup()

# Verify missing values are handled
colSums(is.na(penguins))









# Enhanced Scatter Plot: Bill Length vs. Bill Depth by Species with regression lines
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Bill Length vs Bill Depth by Species (With Regression Lines)",
       x = "Bill Length (mm)", y = "Bill Depth (mm)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

# Box Plot: Body Mass by Island
ggplot(penguins, aes(x = island, y = body_mass_g, fill = island)) +
  geom_boxplot() +
  labs(title = "Body Mass Distribution by Island",
       x = "Island", y = "Body Mass (g)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Faceted Histogram: Flipper Length by Species and Sex
ggplot(penguins, aes(x = flipper_length_mm, fill = sex)) +
  geom_histogram(binwidth = 5, alpha = 0.7, color = "black", position = "dodge") +
  facet_wrap(~species) +
  labs(title = "Flipper Length Distribution by Species and Sex",
       x = "Flipper Length (mm)", y = "Count") +
  theme_minimal()







# Subset numerical variables
penguins_numeric <- penguins %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

# Verify the subset
head(penguins_numeric)
summary(penguins_numeric)

# Normalize the numerical dataset
penguins_scaled <- scale(penguins_numeric)

# Check the scaled data
head(penguins_scaled)



# Save the cleaned dataset
write.csv(penguins, "cleaned_penguins.csv", row.names = FALSE)

# Save the scaled dataset
write.csv(as.data.frame(penguins_scaled), "scaled_penguins.csv", row.names = FALSE)




# Perform PCA on the scaled dataset
pca_result <- prcomp(penguins_scaled, center = TRUE, scale. = TRUE)

# Print PCA summary to inspect variance explained
summary(pca_result)

# Extract variance explained by each component
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
explained_variance_cumsum <- cumsum(explained_variance)

# Display variance explained
explained_variance
explained_variance_cumsum




library(ggplot2)

# Scree Plot: Variance Explained
scree_plot <- data.frame(
  Principal_Component = paste0("PC", 1:length(explained_variance)),
  Variance_Explained = explained_variance
)

ggplot(scree_plot, aes(x = Principal_Component, y = Variance_Explained)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(group = 1), color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Scree Plot: Variance Explained by Principal Components",
    x = "Principal Components",
    y = "Variance Explained"
  ) +
  theme_minimal()

# Biplot: PCA Loadings and Scores
library(ggbiplot)

# Generate biplot for first two principal components
ggbiplot(pca_result,
         obs.scale = 1,
         var.scale = 1,
         groups = penguins$species,
         ellipse = TRUE,
         circle = TRUE) +
  labs(title = "PCA Biplot: First Two Principal Components") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

