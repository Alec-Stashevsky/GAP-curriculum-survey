# Step 01 - Import Survey and Prep Survey Data
# Alec Stashevsky
# November 18, 2021


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)

path.in <- "C:/Users/Alec/Documents/GAP Research/GAP Curriculum Survey/Data/Survey Results from Climate Change Curriculum Survey.xlsx"

path.out <- "C:/Users/Alec/Documents/GAP Research/GAP Curriculum Survey/Output/"

# Import ------------------------------------------------------------------
sheet.list <- readxl::excel_sheets(path.in)

# Initialize list to hold raw data
raw.total <- read_excel(path = path.in, sheet = sheet.list[1], range = "A2:AU106") %>% setDT()
raw.pre <- read_excel(path = path.in, sheet = sheet.list[2], range = "A2:AU91") %>% setDT() #issue with this one
raw.post <- read_excel(path = path.in, sheet = sheet.list[3], range = "A2:AU80") %>% setDT()


# Preparation -------------------------------------------------------------

# Fix data types
raw.total$`Notation for more than 1 stage noted` <- as.numeric(raw.total$`Notation for more than 1 stage noted`)
raw.pre$`Notation for more than 1 stage noted` <- as.numeric(raw.pre$`Notation for more than 1 stage noted`)
raw.post$`Notation for more than 1 stage noted` <- as.numeric(raw.post$`Notation for more than 1 stage noted`)

# Remove any free response questions
# raw.post[,
#   .SD,
#   .SDcols = !grep("Free Response:", colnames(raw.post), ignore.case = TRUE)
#   ]

# Filter to numeric columns
post <- raw.post[, .SD, .SDcols = is.numeric]

post[, `:=`(
  `Record ID` = NULL,
  `Completed Information Sheet` = NULL,
  `Academic Stage` = NULL,
  `Notation for more than 1 stage noted` = NULL,
  `Completed Demographics` = NULL,
  `Completed Pre-survey` = NULL,
  `Completed vidoe module` = NULL,
  `Completed post-survey` = NULL
)]

post.colnames <- data.table(colnames(post))

# Check Assumptions -------------------------------------------------------

# Test for normality
normality.tests <- data.table(
  tibble::rownames_to_column(
    data.frame(
      t(
        sapply(post, shapiro.test)
        )
      ),
    "Variable"
    )
)

# Filter for significant values
normality.tests[, p.value := as.numeric(p.value)][p.value <= 0.05]
# Every column is significantly different from normal distribution, therefore a t-test is not appropriate. We will use non-parametric methods.


# Descriptive Stats -------------------------------------------------------

# Pairwise correlations
cormat.tables<- lapply(
  Hmisc::rcorr(as.matrix(post)), data.table
)

# Shape of response distributions
post.summary <- data.table(psych::describe(post, quant = c(0.01, 0.25, 0.75, 0.99)))
post.summary[, vars := post.colnames$V1]


# Need to rename to summarize
new.names <- c(
  "D1", "D2", "D3", "D4", "D5",
  "L1",
  "Q1",
  "L2", "L3",
  "MC1-1", "MC1-2", "MC1-3", "MC1-4", "MC1-5", "MC1-6", "MC1-7", "MC1-8",
  "R1", "R2", "R3", "R4", "R5", "R6", "R7",
  "MC2-1", "MC2-2", "MC2-3", "MC2-4", "MC2-5", "MC2-6"
  )

setnames(post, new.names)

# Initialization
count.list <- vector(mode = "list", length = length(colnames(post)))
colnames.key <- data.table("Names" = post.colnames, "Code" = new.names)
j <- 1

# Get counts and proportions for each column
for (col in colnames(post)) {

  # Extract counts and props
  count.list[[j]] <- post[, .("N" = as.numeric(.N)), by = c(col)][,
    "Prop" := N/sum(N)]

  # Order by response values
  setorderv(count.list[[j]], col)

  # Set to original names (need to do this to call in loop)
  setnames(count.list[[j]], old = col, new = colnames.key[j]$Names.V1)

  # Increment index
  j <- j + 1

}

# Set list names for writing to excel
names(count.list) <- new.names

# Initialize
Value <- count.list[[1]]$`Academic stage consolidated`
N <- count.list[[1]]$N
Prop <- count.list[[1]]$Prop

for (i in 2:length(count.list)) {

  Value <- c(Value, count.list[[i]][, c(1)])
  N <- c(N, count.list[[i]][, c(2)])
  Prop <- c(Prop, count.list[[i]][, c(3)])

}

count.table <- data.table(
  "Question" = rep(post.colnames$V1, sapply(count.list, nrow)),
  "Value" = unlist(Value),
  "N" = unlist(N),
  "Prop" = unlist(Prop)
  )

# Export ------------------------------------------------------------------
export.list = list(
  "Response Distribution" = post.summary,
  "Counts and Proportions" = count.table,
  "Correlation" = cormat.tables$r,
  "Correlation Counts" = cormat.tables$n,
  "Correlation P-value" = cormat.tables$P,
  "Normality Tests" = normality.tests
)

write.xlsx(
  export.list,
  paste0(path.out, "GAP Survey - Descriptive Statistics.xlsx"),
  overwrite = TRUE
  )

saveRDS(post, paste0(path.out, "post_survey_clean.RDs"))
saveRDS(colnames.key, paste0(path.out, "question_column_key.RDs"))
