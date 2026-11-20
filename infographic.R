# infographic.R
#
# Question: Was UC Berkeley biased against women in its 1973 graduate
#           admissions?
# Data:     UCBAdmissions, built into R (the six largest departments).
#
# Work in progress: this version computes and prints the numbers.
# The infographic and the command-line argument are added in later steps.
#
# Run from the project folder with:
#   Rscript infographic.R

suppressPackageStartupMessages(library(tidyverse))

# ---- 1. Data ----------------------------------------------------------------
# UCBAdmissions is a 3-dimensional table (Admit x Gender x Dept).
# as_tibble() turns it into one row per combination, with the count in `n`.
admissions <- as_tibble(UCBAdmissions) |>
  rename(admit = Admit, gender = Gender, dept = Dept) |>
  mutate(gender = if_else(gender == "Male", "Men", "Women"))

# ---- 2. Analysis ------------------------------------------------------------

# 2a. The headline: admission rate for men and women, all departments pooled.
overall <- admissions |>
  group_by(gender) |>
  summarise(
    applicants = sum(n),
    admitted   = sum(n[admit == "Admitted"]),
    .groups    = "drop"
  ) |>
  mutate(rate = admitted / applicants)

# 2b. The same rate, but separately for each department.
by_dept <- admissions |>
  group_by(dept, gender) |>
  summarise(
    applicants = sum(n),
    admitted   = sum(n[admit == "Admitted"]),
    .groups    = "drop"
  ) |>
  mutate(rate = admitted / applicants)

# 2c. Men and women side by side: who has the higher rate in each department?
dept_gap <- by_dept |>
  select(dept, gender, rate) |>
  pivot_wider(names_from = gender, values_from = rate) |>
  mutate(women_ahead = Women > Men)

n_depts       <- nrow(dept_gap)
n_women_ahead <- sum(dept_gap$women_ahead)

# 2d. How hard is each department to get into (men and women pooled)?
#     A department is "selective" here if it admits fewer than half.
dept_difficulty <- admissions |>
  group_by(dept) |>
  summarise(
    dept_rate = sum(n[admit == "Admitted"]) / sum(n),
    .groups   = "drop"
  ) |>
  mutate(selective = dept_rate < 0.5)

# 2e. Where did men and women apply? Share of each gender's applications
#     that went to each department.
applications <- by_dept |>
  group_by(gender) |>
  mutate(share = applicants / sum(applicants)) |>
  ungroup() |>
  left_join(dept_difficulty, by = "dept")

# 2f. Share of each gender's applications that went to a selective department.
to_selective <- applications |>
  group_by(gender) |>
  summarise(share_selective = sum(share[selective]), .groups = "drop")

# ---- 3. Print the numbers ---------------------------------------------------
# Printed with sprintf() so the output is identical on every machine.
pct <- function(x) sprintf("%.1f%%", 100 * x)

cat("\nOverall admission rate\n")
overall |>
  mutate(rate = pct(rate)) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\nAdmission rate by department\n")
dept_gap |>
  left_join(dept_difficulty, by = "dept") |>
  mutate(Men = pct(Men), Women = pct(Women), dept_rate = pct(dept_rate)) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat(sprintf(
  "\nWomen had the higher admission rate in %d of %d departments.\n",
  n_women_ahead, n_depts
))

cat("\nShare of applications sent to a selective department\n")
to_selective |>
  mutate(share_selective = pct(share_selective)) |>
  as.data.frame() |>
  print(row.names = FALSE)