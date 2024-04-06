library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)


df <- read.csv("data/inforce-data-cleaned.csv", header = TRUE)

df_COD <- df %>% 
  filter(!is.na(Cause.of.Death)) %>% 
  group_by(Cause.of.Death) %>%
  summarise(n = n())


# Map cause of death values according to ICD-10
cod_dict <- list(
  "A00-B99" = "Certain infectious and parasitic diseases",
  "C00-D48" = "Neoplasms",
  "D50-D89" = "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
  "E00-E88" = "Endocrine, nutritional and metabolic diseases",
  "F01-F99" = "Mental and behavioural disorders",
  "G00-G98" = "Diseases of the nervous system",
  "I00-I99" = "Diseases of the circulatory system ",
  "J00-J98" = "Diseases of the respiratory system",
  "K00-K92" = "Diseases of the digestive system",
  "L00-L98" = "Diseases of the skin and subcutaneous tissue",
  "M00-M99" = "Diseases of the musculoskeletal system and connective tissue",
  "N00-N98" = "Diseases of the genitourinary system",
  "O00-O99" = "Pregnancy, childbirth and the puerperium",
  "Q00-Q99" = "Congenital malformations, deformations and chromosomal abnormalities",
  "R00-R99" = "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
  "V01-Y89" = "External causes of morbidity and mortality",
  "unknown" = "unknown"
)


# Convert list to data frame
mapping_df <- do.call(rbind, cod_dict)

# Print the data frame
# Print the fancier table
html_table <- kable(mapping_df, "html") %>%
  kable_styling(full_width = FALSE)

# Save the HTML output to a file
writeLines(html_table, "cause_of_death_mapping.html")


################ Map disease names codes and plot

df_COD$COD_named <- sapply(df_COD$Cause.of.Death, function(x) cod_dict[[x]])


df_agg_named <- aggregate(n ~ ifelse(n >= 600, as.character(COD_named), "Other"), 
                          data = df_COD, 
                          FUN = sum) %>% 
  arrange(n)
colnames(df_agg_named) <- c("Cause.of.Death", "count")


df_agg_named$Cause.of.Death <- reorder(df_agg_named$Cause.of.Death, -df_agg_named$count)


# # Reorder the levels of COD based on counts
# df_agg$Cause.of.Death <- factor(df_agg$Cause.of.Death, levels = df_agg$Cause.of.Death[order(df_agg$count, decreasing = TRUE)])
################# PIE Chart
pie_chart <- ggplot(df_agg_named, aes(x = "", y = count, fill = Cause.of.Death)) +
  geom_bar(stat = "identity", width = 1) +
  # geom_text(aes(label = paste0(round(count / sum(count) * 100), "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +  # You can change the color palette
  theme(legend.position = "right") +
  labs(title = 'Proportions of Death Causes of Lumaria Inforce Policyholders')


# Print the pie chart
print(pie_chart)


# Bar-chart --------------------------------------------------------------

# Create the bar chart for COUNT
bar_chart <- ggplot(df_agg_named, aes(x = Cause.of.Death, y = count, fill = Cause.of.Death)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  labs(x = "Cause of Death", y = "Count") +  # Label axes
  scale_fill_brewer(palette = "Set3") +  # You can change the color palette
  coord_flip()  # Flip the coordinates for horizontal bars

# Print the bar chart
print(bar_chart)


# Bar chart for proportions
# Calculate proportion
df_agg_named_prop <- transform(df_agg_named, proportion = count / sum(count))

# Reorder the levels of Cause.of.Death based on proportion
df_agg_named_prop$Cause.of.Death <- reorder(df_agg_named_prop$Cause.of.Death, -df_agg_named_prop$proportion)

# Create the bar chart
bar_chart <- ggplot(df_agg_named_prop, aes(x = Cause.of.Death, y = proportion, fill = Cause.of.Death)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(proportion * 100), "%"), y = proportion), vjust = -0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels for better readability
  labs(x = "Cause of Death", y = "Proportion", title = 'Proportions of Death Causes of Lumaria Inforce Policyholders') +  # Label axes
  scale_fill_brewer(palette = "Set3")

# Print the bar chart
print(bar_chart)

### Check which categories are part of others
df_COD %>% 
  filter(n < 600) %>% 
  select(COD_named)

