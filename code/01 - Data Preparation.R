# Step 01 - Import Survey and Prep Survey Data
# Alec Stashevsky
# November 18, 2021


# Setup -------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(readxl)

path.in <- "C:/Users/Alec/Documents/GAP Research/GAP Curriculum Survey/Data/Survey Results from Climate Change Curriculum Survey.xlsx"

# Import ------------------------------------------------------------------
sheet.list <- readxl::excel_sheets(path.in)

# Initialize list to hold raw data
raw.total <- read_excel(path = path.in, sheet = sheet.list[1], range = "A2:AU106")
raw.pre <- read_excel(path = path.in, sheet = sheet.list[2], range = "A2:AU91") #issue with this one
raw.post <- read_excel(path = path.in, sheet = sheet.list[3], range = "A2:AU80")


# Preparation -------------------------------------------------------------

# Fix data types
raw.total$`Notation for more than 1 stage noted` <- as.numeric(raw.total$`Notation for more than 1 stage noted`)

raw.pre$`Notation for more than 1 stage noted` <- as.numeric(raw.pre$`Notation for more than 1 stage noted`)

raw.post$`Notation for more than 1 stage noted` <- as.numeric(raw.post$`Notation for more than 1 stage noted`)
