# Code used during Dissertation----

# Libraries ----
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)


# Pie Charts ----

custom_colors <- c("green", "dodgerblue", "blue", "deepskyblue",
                        "skyblue")
                        

            
LIB_data$Component <- factor(composition_data$Component, levels = custom_order)

ggplot(data = LIB_data, aes(x = "", y = Mass_Distribution, fill = reorder(Component, - Mass_Distribution))) + 
  geom_bar(stat = "identity", colour = "black") + 
  coord_polar("y")+
  theme_void() +
  scale_fill_manual(values = custom_colors)


 


# ---- LIB Composition -------------------------------------------------------

LIB_data <- data.frame(Component = c("Battery Housing", "Graphite Anode", "NMC Cathode", "Electrolyte",
                                            "Other Cell Components"),
                              Mass_Distribution = c(0.302, 0.2792, 0.228, 0.125, 0.0726))
  



# ---- IMPACT ASSESSMENT GRAPHS ----------------------------------------------


# Create dataframe

# Get Impact Assessment Data

impactAdata <- read.csv("LCIA_Data.csv", header = TRUE)


# Reshape the data for plotting
data_long <- tidyr::gather(data, key = "Category", value = "Value", -Battery_Phase)


# Create a stacked bar chart

ggplot(data_long, aes(x = Battery_Phase, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(
       x = "Battery Phase",
       y = "Environmental Impact Points (pts)") +
  scale_fill_manual(values = c("Human_Health" = "#7eb0d5", "Ecosystem_Health" = "#b2e061", "Resource_Consumption" = "#fd7f6f"),
                    name = "Endpoint categories") +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 20, hjust = 1, color = "black", size = 12),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.ticks.length  = unit(0.2, "cm"),
        axis.title.x = element_text(size = 16),  # Adjust x-axis title size
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.key.size = unit(0.6, "cm"),
        legend.position = c(0.9, 0.75))+
  scale_y_continuous(breaks = seq(0, 10, by = 1))

legend.position = c(0.9, 0.75)
  


# ---- IMPACT CATEGORY ANALYSIS ------------------------------------------

impactdata <- read.csv("LCA_Data.csv", header = TRUE)
getwd()

impact_long <- tidyr::gather(impactdata, key = "Impact_category", value = "Value", -)


# Reorder Impact_category factor levels
data$Impact_category <- factor(data$Impact_category, levels = c("Climate change Human Health", "Ozone depletion", "Human toxicity", "Photochemical oxidant formation",
                                                                "Particulate matter formation", "Ionising radiation", "Climate change Ecosystems", "Terrestrial acidification",
                                                                "Freshwater eutrophication", "Terrestrial ecotoxicity", "Freshwater ecotoxicity", "Marine ecotoxicity",
                                                                "Agricultural land occupation", "Urban land occupation", "Natural land transformation", "Metal depletion",
                                                                "Fossil depletion"))

# Create a stacked bar graph
ggplot(data, aes(x = Impact_category, y = Value, fill = Component)) +
  geom_bar(stat = "identity") +
  labs(x = "ReCiPe Midpoint Impact Category",
       y = "Environmental Impact Points (pts)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = c(0.6, 0.8)) +
  scale_fill_manual(values = rainbow(length(unique(data$Component)))) +
  scale_y_continuous(limits = c(0,3), breaks = seq(0, 3, by = 0.2)) +
  guides(fill = guide_legend(title = "Life Cycle Component", nrow = 4, keywidth = unit(0.2, "in"), keyheight = unit(0.2, "in")))





# ---- VRFB IMPACT CATEGORY ANALYSIS -----------------------------

VRFBdata <- read.csv("LCIA_Data_VRFB.csv", header = TRUE)
# Create a data frame with the provided data

# Reshape the data for plotting
VRFB_long <- gather(VRFBdata, key = "Component", value = "Environmental_Impact_Points", -Impact_category)

desired_order <- c("Climate change Human Health", "Ozone depletion", "Human toxicity", "Photochemical oxidant formation", 
                   "Particulate matter formation", "Ionising radiation", "Climate change Ecosystems", "Terrestrial acidification", 
                   "Freshwater eutrophication", "Terrestrial ecotoxicity", "Freshwater ecotoxicity", "Marine ecotoxicity", 
                   "Agricultural land occupation", "Urban land occupation", "Natural land transformation", "Metal depletion", 
                   "Fossil depletion")

VRFB_long$Impact_category <- factor(VRFB_long$Impact_category, levels = desired_order)

# Create a stacked bar chart
ggplot(VRFB_long, aes(x = Impact_category, y = Environmental_Impact_Points, fill = Component)) +
  geom_bar(stat = "identity") +
  labs(x = "ReCiPe Midpoint Impact Category",
       y = "Environmental Impact Points (pts)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = c(0.6, 0.8)) +
  scale_fill_manual(values = rainbow(length(unique(data_long$Component)))) +
  scale_y_continuous(limits = c(0,3), breaks = seq(0, 3, by = 0.2)) +
  guides(fill = guide_legend(title = "Life Cycle Component", nrow = 4, keywidth = unit(0.2, "in"), keyheight = unit(0.2, "in")))
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") 
  
  scale_y_continuous(limits = c(0, 3))
  scale_fill_manual(values = rainbow(length(unique(data_long$Component))))


  
  # ---- CLIMATE CHANGE ---------------------------------------------------
 
  
  # Calculate the total for LIB
  lib_total <- sum(lib_data$Anode, lib_data$Battery_Housing, lib_data$NMC_Cathode, lib_data$Other_Components,
                   lib_data$Electrolyte, lib_data$Heat, lib_data$Transport, lib_data$Electricity)
  
  # Calculate the total for VRFB
  vrfb_total <- sum(vrfb_data$Electrolyte, vrfb_data$Electrolyte_Tank, vrfb_data$Periphery, vrfb_data$Bipolar_Plate,
                    vrfb_data$Current_collector, vrfb_data$Stack_Frame, vrfb_data$Cell_Frame, vrfb_data$Electrode,
                    vrfb_data$Membrane, vrfb_data$Transport)
  
  # Divide each component by the total and multiply by 100 to get the percentage
  lib_data_percentage <- lib_data[, -1] / lib_total * 100
  vrfb_data_percentage <- vrfb_data[, -1] / vrfb_total * 100
  
  # Add the Battery column back
  lib_data_percentage$Battery <- "LIB"
  vrfb_data_percentage$Battery <- "VRFB"
  
  # Reshape the data for plotting (for LIB)
  lib_data_long <- pivot_longer(lib_data_percentage, -Battery, names_to = "Component", values_to = "Value")
  
  # Reshape the data for plotting (for VRFB)
  vrfb_data_long <- pivot_longer(vrfb_data_percentage, -Battery, names_to = "Component", values_to = "Value")
  
  # Combine the data
  combined_data <- rbind(lib_data_long, vrfb_data_long)
  
  # Create a custom color palette for LIB components
  lib_colors <- c("blue", "turquoise", "purple", "darkblue", "purple", "pink", "#e377c2", "green", "#bcbd22")
  
  # Create a custom color palette for VRFB components
  vrfb_colors <- c("red", "orange", "red", "orange", "yellow", "darkgreen", "brown", "green", "turquoise", "darkblue")
  
  # Create the horizontal stacked bar chart
  ggplot(combined_data, aes(x = Value, y = Battery, fill = Component)) +
    geom_bar(stat = "identity", width = 0.2) +
    scale_fill_manual(values = c(vrfb_colors, lib_colors)) +
    labs(
         x = "Percentage (%)",
         y = "") +
    theme_classic()+
    theme(legend.position = "top", legend.title = element_blank(),
          axis.text.x = element_text(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_line(color = "black"),
                  axis.text.y = element_text(colour = "black")) +
    guides(fill = guide_legend(nrow = 4, title.position = "top", title.hjust = 0.5))
  
  
  
  
  
  
  # Reorder the levels of the "Battery" factor
  combined_data$Battery <- factor(combined_data$Battery, levels = c("LIB", "VRFB"))
  
  # Plot the horizontal stacked bar chart
  ggplot(combined_data, aes(x = Value, y = Battery, fill = Component)) +
    geom_bar(stat = "identity", width = 0.2) +
    scale_fill_manual(values = c(lib_colors, vrfb_colors)) +
    labs(
      x = "Percentage (%)",
      y = "Battery"
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.text.x = element_text(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text.y = element_text(colour = "black")
    ) +
    guides(fill = guide_legend(nrow = 4, title.position = "top", title.hjust = 0.5))
  
  
  # ---- FULL LIFE CYCLE -----------------------------------------





LCA_long <- gather(LCA_, group, value, "Climate_Change", "Human_Toxicity", "Particulate_Matter", "Terrestrial_Ecotoxicity", "Fossil_Depletion", "Metal_Depletion") %>%
  arrange(factor(Life_Cycle_Stage, levels = c("Production", "Use", "EoL"))) %>% 
  mutate(Life_Cycle_Stage = factor(Life_Cycle_Stage, levels = c("Production", "Use", "EoL")),
         group = factor(group, levels = c("Climate_Change", "Human_Toxicity", "Particulate_Matter", "Terrestrial_Ecotoxicity", "Fossil_Depletion", "Metal_Depletion")))


ggplot(LCA_long, aes(fill=Life_Cycle_Stage, y=value, x=group)) + 
         geom_bar(position="stack", stat="identity") +
  labs(
    x = "Impact Category",
    y= "Environmental Impact Score (pts)"
    )+
  geom_hline(yintercept = 0, color = "black", size = 0.2, linetype = "dashed") +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12),
        axis.title.x = element_text(size = 16),  # Adjust x-axis title size
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16)) +
  scale_y_continuous(limits = c(-2,8), breaks = seq(-2, 8, by = 2))+
  scale_fill_manual(values = c("Production" = "#FF4040", "Use" = "#1874CD", "EoL" = "#ADFF2F"))



# VRFB

VRFB_long <- gather(VRFB, group, value, "Climate_Change", "Human_Toxicity", "Particulate_Matter", "Terrestrial_Ecotoxicity", "Fossil_Depletion", "Metal_Depletion") %>%
  arrange(factor(Life_Cycle_Stage, levels = c("Production", "Use", "EoL"))) %>% 
  mutate(Life_Cycle_Stage = factor(Life_Cycle_Stage, levels = c("Production", "Use", "EoL")),
         group = factor(group, levels = c("Climate_Change", "Human_Toxicity", "Particulate_Matter", "Terrestrial_Ecotoxicity", "Fossil_Depletion", "Metal_Depletion")))


ggplot(VRFB_long, aes(fill=Life_Cycle_Stage, y=value, x=group)) + 
  geom_bar(position="stack", stat="identity") +
  labs(
    x = "Impact Category",
    y= "Environmental Impact Score (pts)"
  )+
  geom_hline(yintercept = 0, color = "black", size = 0.1, linetype = "dashed") +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12),
        axis.title.x = element_text(size = 16),  # Adjust x-axis title size
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16)) +
  scale_y_continuous(limits = c(-2,7.8), breaks = seq(-2, 8, by = 1)) +
  scale_fill_manual(values = c("Production" = "#FF4040", "Use" = "#1874CD", "EoL" = "#ADFF2F"))





# ---- REDOING THE MIDPOINT IMPACT CATEGORY PER BATTERY COMPONENT SO THAT
#      ONLY THE SIGNIFICANT CATEGORIES ARE PRESENTED

midpoint <- read.csv("midpoint_Data.csv", header = TRUE)


midpoint$Impact_category <- factor(midpoint$Impact_category, levels = c("Climate change", "Human toxicity",
                                                                "Particulate matter formation",
                                                                "Terrestrial ecotoxicity",
                                                                 "Metal depletion",
                                                                "Fossil depletion"))

# Create a stacked bar graph
ggplot(midpoint, aes(x = Impact_category, y = Value, fill = Component)) +
  geom_bar(stat = "identity") +
  labs(x = "ReCiPe Midpoint Impact Category",
       y = "Environmental Impact Points (pts)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = c(0.6, 0.8)) +
  #scale_fill_manual(values = rainbow(length(unique(data$Component)))) +
  scale_fill_manual(values = c("red", "orange", "#00FA9A", "#00CD00", "#00FFFF", "blue", "purple", "pink")) +
  scale_y_continuous(limits = c(0,3), breaks = seq(0, 3, by = 0.2)) +
  guides(fill = guide_legend(title = "Life Cycle Component", nrow = 4, keywidth = unit(0.2, "in"), keyheight = unit(0.2, "in")))

# ---- VRFB----------
#     Changes

VRFBmidpoint <- read.csv("VRFBmid_Data.csv", header = TRUE)


# Reshape the data for plotting
VRFB_long <- gather(VRFBmidpoint, key = "Component", value = "Environmental_Impact_Points", -Impact_category)

desired_order <- c("Climate change", "Human toxicity", 
                   "Particulate matter formation", 
                   "Terrestrial ecotoxicity", 
                   "Metal depletion", 
                   "Fossil depletion")

VRFB_long$Impact_category <- factor(VRFB_long$Impact_category, levels = desired_order)

# Create a stacked bar chart
ggplot(VRFB_long, aes(x = Impact_category, y = Environmental_Impact_Points, fill = Component)) +
  geom_bar(stat = "identity") +
  labs(x = "ReCiPe Midpoint Impact Category",
       y = "Environmental Impact Points (pts)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = c(0.6, 0.8)) +
  #scale_fill_manual(values = rainbow(length(unique(data_long$Component)))) +
  scale_fill_manual(values = c("red", "orange","yellow", "#00FA9A", "#00FFFF","#00CD00", "#87CEEB","blue", "purple", "pink")) +
  scale_y_continuous(limits = c(0,3), breaks = seq(0, 3, by = 0.2)) +
  guides(fill = guide_legend(title = "Life Cycle Component", nrow = 4, keywidth = unit(0.2, "in"), keyheight = unit(0.2, "in")))
  
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") 

scale_y_continuous(limits = c(0, 3))
scale_fill_manual(values = rainbow(length(unique(data_long$Component))))


# ---- SENSITIVITY ANALYSIS ------------------------------------------------------------
# ---- ENERGY DENSITY ------------------------------------------------------------------


SA_Density <- read.csv("Sensitivity_Analysis.csv", header = TRUE)

# Reshape the data for ggplot
density_long <- tidyr::gather(SA_Density, Category, Value, -Energy_Density)

# Define the order of the categories
density_long$Category <- factor(density_long$Category, levels = c("Ecosystem_Health", "Human_Health", "Resource_Consumption"))


# Create the stacked bar plot
ggplot(density_long, aes(x = Energy_Density, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Energy Density (Wh/kg)",
    y = "Environmental Impact Points"
  ) +
  scale_fill_manual(values = c("#b2e061", "#7eb0d5", "#fd7f6f"),
                    name = "Endpoint Categories",
                    labels = c("Ecosystem Health", "Human Health", "Resource Consumption")) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 20, hjust = 1, color = "black", size = 12),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.ticks.length  = unit(0.2, "cm"),
    axis.title.x = element_text(size = 16),  # Adjust x-axis title size
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.key.size = unit(0.6, "cm"),
    legend.position = c(0.9, 0.9)
  )



#Line Graph for SA Density
# Separate LIB and VRFB data
lib_data <- filter(SA_Density, battery == "LIB")
vrfb_data <- filter(SA_Density, battery == "VRFB")


# Create the line graph for LIB
ggplot(lib_data_long, aes(x = total_impact, y = Energy_Density)) +
  geom_line() +
  scale_y_continuous(c, breaks = c(16, 20, 26, 160, 200, 260), labels = c(16, 20, 26, 160, 200, 260)) +
  scale_x_continuous(limits = c(2.5,4), breaks = seq(2.5, 4, by = 0.5)) +
  labs(
    x = "Total Environmental Impact Points",
    y = "Energy Density (Wh/kg)"
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 20, hjust = 1, color = "black", size = 12),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  ) +
  geom_line(data = vrfb_data_long, aes(x = total_impact, y = Energy_Density), color = "red") +
  scale_color_manual(values = c("blue", "red"), labels = c("LIB", "VRFB")) +
  labs(color = "Battery Type")

ggplot() +
  geom_point(data = lib_data, aes(x = Density, y = Point), color = "blue") +
  geom_point(data = vrfb_data, aes(x = Density, y = Point), color = "red") +
  geom_line(data = lib_data, aes(x = Density, y = Point, group = 1), color = "blue") +
  geom_line(data = vrfb_data, aes(x = Density, y = Point, group = 1), color = "red") +
  labs(
    x = "Energy Density (Wh/kg)",
    y = "Environmental Impact Points"
  ) +
  scale_x_continuous(limits = c(0, 280), breaks = seq(0,280, by = 40)) +
  scale_y_continuous(limits = c(1,4), breaks = seq(1, 4, by = 0.5))+
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 20, hjust = 1, color = "black", size = 12),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.ticks.length  = unit(0.2, "cm"),
        axis.title.x = element_text(size = 16),  # Adjust x-axis title size
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.key.size = unit(0.6, "cm"),
        legend.position = c(0.9, 0.9)) +
  scale_color_manual(name = "Battery", values = c("blue", "red"), labels = c("LIB", "VRFB"))



# Define the data
rate <- c(73, 80, 96)
impact_score <- c(3.15, 2.53, 1.15)

# Perform Pearson correlation coefficient test
correlation_test <- cor.test(rate, impact_score, method = "pearson")

# Print the results
print(correlation_test)




# ---- Recycling rate --------------

recycling <- read.csv("RecyclingSA_Data.csv", header = TRUE)

# Reshape the data for ggplot
recycling_long <- tidyr::gather(recycling, Category, Value, -Recycling_Rate)

# Define the order of the categories
recycling_long$Category <- factor(recycling_long$Category, levels = c("Ecosystem_Health", "Human_Health", "Resource_Consumption"))


recycling_long$Recycling_Rate <- factor(recycling_long$Recycling_Rate, levels = c("50%", "73%", "95%", "", "73%.", "80%", "96%"))

# Create the stacked bar plot
ggplot(recycling_long, aes(x = Recycling_Rate, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Recycling Rate (%)",
    y = "Environmental Impact Points"
  ) +
  scale_fill_manual(values = c("#b2e061", "#7eb0d5", "#fd7f6f"),
                    name = "Endpoint Categories",
                    labels = c("Ecosystem Health", "Human Health", "Resource Consumption")) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 20, hjust = 1, color = "black", size = 12),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.ticks.length  = unit(0.2, "cm"),
    axis.title.x = element_text(size = 16),  # Adjust x-axis title size
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.key.size = unit(0.6, "cm"),
    legend.position = c(0.9, 0.9)
  ) +
  scale_y_continuous(limits = c(0,4), breaks = seq(0, 4, by = 1))


#---- RECYCLING SENSITIVITY ANALYSIS BUT AS A LINE GRAPH--------------------------------------------------------------------

library(ggplot2)

# Create the data frame
data <- data.frame(
  Rec_rate = c(50, 73, 95, 73, 80, 96),
  Point = c(3.79, 3.03, 2.21, 3.15, 2.54, 1.15),
  battery = c("LIB", "LIB", "LIB", "VRFB", "VRFB", "VRFB")
)

# Separate LIB and VRFB data
lib_data <- filter(data, battery == "LIB")
vrfb_data <- filter(data, battery == "VRFB")

# Create the plot
ggplot() +
  geom_point(data = lib_data, aes(x = Rec_rate, y = Point), color = "blue") +
  geom_point(data = vrfb_data, aes(x = Rec_rate, y = Point), color = "red") +
  geom_line(data = lib_data, aes(x = Rec_rate, y = Point, group = 1), color = "blue") +
  geom_line(data = vrfb_data, aes(x = Rec_rate, y = Point, group = 1), color = "red") +
  labs(
    x = "Recycling Rate (%)",
    y = "Environmental Impact Points"
  ) +
  scale_x_continuous(limits = c(50,100), breaks = seq(50, 100, by = 10)) +
  scale_y_continuous(limits = c(1,4), breaks = seq(1, 4, by = 0.5))+
  theme_classic() +
  theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
axis.line = element_line(color = "black"),
axis.text.x = element_text(angle = 20, hjust = 1, color = "black", size = 12),
axis.text.y = element_text(colour = "black", size = 12),
axis.ticks.length  = unit(0.2, "cm"),
axis.title.x = element_text(size = 16),  # Adjust x-axis title size
axis.title.y = element_text(size = 16),
legend.text = element_text(size = 12),
legend.title = element_text(size = 16),
legend.key.size = unit(0.6, "cm"),
legend.position = c(0.9, 0.9)) +
  scale_color_manual(name = "Battery", values = c("blue", "red"), labels = c("LIB", "VRFB"))






#---- Re-use rate Sensitivity Analysis --------------------

# Define the data
rate <- c(0, 20, 80)
impact_score <- c(3.15, 3.10, 2.29)

# Perform Pearson correlation coefficient test
correlation_test <- cor.test(rate, impact_score, method = "pearson")

# Print the results
print(correlation_test)

reuse <- read.csv("ReuseSA_Data.csv", header = TRUE)

# Reshape the data for ggplot
reuse_long <- tidyr::gather(reuse, Category, Value, -Reuse_Rate)

# Define the order of the categories
reuse_long$Category <- factor(reuse_long$Category, levels = c("Ecosystem_Health", "Human_Health", "Resource_Consumption"))

reuse_long$Reuse_Rate <- factor(reuse_long$Reuse_Rate, levels = c("0%", "30%", "50%", "", "0%.", "20%", "80%"))

# Create the stacked bar plot
ggplot(reuse_long, aes(x = Reuse_Rate, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Re-use Rate (%)",
    y = "Environmental Impact Points"
  ) +
  scale_fill_manual(values = c("#b2e061", "#7eb0d5", "#fd7f6f"),
                    name = "Endpoint Categories",
                    labels = c("Ecosystem Health", "Human Health", "Resource Consumption")) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 20, hjust = 1, color = "black", size = 12),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.ticks.length  = unit(0.2, "cm"),
    axis.title.x = element_text(size = 16),  # Adjust x-axis title size
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.key.size = unit(0.6, "cm"),
    legend.position = c(0.9, 0.9)
  ) +
  scale_y_continuous(limits = c(0,4), breaks = seq(0, 4, by = 1))




# Re-use Line Graph

data <- data.frame(
  Reu_rate = c(0, 30, 50, 0, 20, 80),
  Point = c(3.4, 2.7, 2.47, 3.15, 3.09, 2.29),
  battery = c("LIB", "LIB", "LIB", "VRFB", "VRFB", "VRFB")
)

# Separate LIB and VRFB data
lib_data <- filter(data, battery == "LIB")
vrfb_data <- filter(data, battery == "VRFB")

# Create the plot
ggplot() +
  geom_point(data = lib_data, aes(x = Reu_rate, y = Point), color = "blue") +
  geom_point(data = vrfb_data, aes(x = Reu_rate, y = Point), color = "red") +
  geom_line(data = lib_data, aes(x = Reu_rate, y = Point, group = 1), color = "blue") +
  geom_line(data = vrfb_data, aes(x = Reu_rate, y = Point, group = 1), color = "red") +
  labs(
    x = "Material Re-use Rate (%)",
    y = "Environmental Impact Points"
  ) +
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(limits = c(1,4), breaks = seq(1, 4, by = 0.5))+
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 20, hjust = 1, color = "black", size = 12),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.ticks.length  = unit(0.2, "cm"),
        axis.title.x = element_text(size = 16),  # Adjust x-axis title size
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.key.size = unit(0.6, "cm"),
        legend.position = c(0.9, 0.9)) +
  scale_color_manual(name = "Battery", values = c("blue", "red"), labels = c("LIB", "VRFB"))



