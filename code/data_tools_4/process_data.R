
if(!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readxl)

# Download the dataset from aei.org
dir.create("data/4_full_inventory", showWarnings = FALSE)
download.file("https://www.aei.org/wp-content/uploads/2019/01/Full-Inventory-Data.xlsx",
              destfile = "data/4_full_inventory/Full-Inventory-Data.xlsx")

# Read the data
full_inventory_raw <- read_excel("data/4_full_inventory/Full-Inventory-Data.xlsx")

