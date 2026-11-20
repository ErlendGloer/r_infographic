
# infographic.R
#
# Was UC Berkeley biased against women in its 1973 graduate admissions?
# This script answers the question with an infographic built from
# UCBAdmissions, a dataset that ships with R.
#
# Usage:
#   Rscript infographic.R <colour scheme>
#
# Argument:
#   <colour scheme>  Required. Picks the two colours used for men and women,
#                    taken from one of R's built-in palettes. One of:
#                    r4, okabe-ito, dark2, set1, ggplot2
#
# Output:
#   berkeley_admissions_<colour scheme>.png, written to the current folder.
#   The numbers behind the infographic are printed to the terminal.
#
# The image committed to this repository, berkeley_admissions_r4.png, was
# produced with exactly this command, run from the project folder:
#   Rscript infographic.R r4
#
# Needs R 4.1 or later and the tidyverse. README.md has more detail.

# ---- 0. Command-line argument -----------------------------------------------
# Checked first, so that a wrong call fails at once with a helpful message.

# Each scheme is a pair of colours (men, women) picked from a palette that is
# built into R. palette.colors() is base R; palette.pals() lists the palettes.
schemes <- list(
  "r4"        = palette.colors(palette = "R4")[c(4, 2)],         # blue, pink
  "okabe-ito" = palette.colors(palette = "Okabe-Ito")[c(6, 7)],  # blue, red
  "dark2"     = palette.colors(palette = "Dark 2")[c(1, 2)],     # green, orange
  "set1"      = palette.colors(palette = "Set 1")[c(2, 1)],      # blue, red
  "ggplot2"   = palette.colors(palette = "ggplot2")[c(5, 2)]     # teal, salmon
)

usage <- paste0(
  "\n  Usage:   Rscript infographic.R <colour scheme>",
  "\n  Schemes: ", paste(names(schemes), collapse = ", "),
  "\n  Example: Rscript infographic.R r4"
)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("No colour scheme given.", usage, call. = FALSE)
}
if (length(args) > 1) {
  stop("Expected one argument but got ", length(args), ".", usage,
       call. = FALSE)
}

scheme <- tolower(args[1])

if (!scheme %in% names(schemes)) {
  stop("'", args[1], "' is not a colour scheme this script knows.", usage,
       call. = FALSE)
}

gender_cols <- c(Men = schemes[[scheme]][[1]], Women = schemes[[scheme]][[2]])
out_file    <- paste0("berkeley_admissions_", scheme, ".png")

# ---- 1. Packages, settings and data -----------------------------------------
suppressPackageStartupMessages(library(tidyverse))

text_size <- 11   # base text size in points; all other sizes follow from it

# Small helpers for turning numbers into text, and for breaking long
# sentences into lines of the same length on every machine.
pct  <- function(x) sprintf("%.1f%%", 100 * x)                # 0.445 -> "44.5%"
pct0 <- function(x) sprintf("%.0f%%", 100 * x)                # 0.445 -> "45%"
num  <- function(x) formatC(x, format = "d", big.mark = ",")  # 2691  -> "2,691"
wrap <- function(x) str_wrap(x, width = 80)

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
#     `gap` is the women's rate minus the men's rate.
dept_wide <- by_dept |>
  select(dept, gender, applicants, rate) |>
  pivot_wider(names_from = gender, values_from = c(applicants, rate)) |>
  mutate(
    gap         = rate_Women - rate_Men,
    women_ahead = gap > 0
  )

n_depts       <- nrow(dept_wide)
n_women_ahead <- sum(dept_wide$women_ahead)

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

# 2f. Share of each gender's applications that went to the easier and to the
#     selective departments.
by_difficulty <- applications |>
  group_by(selective, gender) |>
  summarise(share = sum(share), .groups = "drop")

# ---- 3. Print the numbers ---------------------------------------------------
# Printed with sprintf() so the output is identical on every machine.

cat("\nOverall admission rate\n")
overall |>
  mutate(rate = pct(rate)) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\nApplicants (n) and admission rate by department\n")
dept_wide |>
  left_join(dept_difficulty, by = "dept") |>
  transmute(
    dept,
    men_n      = applicants_Men,
    men_rate   = pct(rate_Men),
    women_n    = applicants_Women,
    women_rate = pct(rate_Women),
    women_ahead,
    dept_rate  = pct(dept_rate),
    selective
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat(sprintf(
  "\nWomen had the higher admission rate in %d of %d departments.\n",
  n_women_ahead, n_depts
))

cat("\nShare of applications by department difficulty\n")
by_difficulty |>
  mutate(share = pct(share)) |>
  pivot_wider(names_from = gender, values_from = share) |>
  as.data.frame() |>
  print(row.names = FALSE)

# ---- 4. Infographic ---------------------------------------------------------

# 4a. Colours for everything except the two genders, and a shared theme.
col_surface <- "#fcfcfb"  # page background
col_ink     <- "#0b0b0b"  # main text
col_ink2    <- "#52514e"  # secondary text
col_muted   <- "#898781"  # small print
col_axis    <- "#c3c2b7"  # baselines and brackets

# geom_text() measures size in mm, themes in points; .pt converts between them.
txt <- function(rel_size = 1) rel_size * text_size / .pt

theme_panel <- theme_minimal(base_size = text_size) +
  theme(
    plot.background      = element_rect(fill = col_surface, colour = NA),
    plot.margin          = margin(8, 28, 8, 28),
    plot.title.position  = "plot",
    plot.title           = element_text(face = "bold", size = rel(1.25),
                                        colour = col_ink),
    plot.subtitle        = element_text(colour = col_ink2, lineheight = 1.15,
                                        margin = margin(t = 2, b = 8)),
    panel.grid           = element_blank(),
    axis.title           = element_blank(),
    axis.text.x          = element_text(colour = col_ink, lineheight = 1.1),
    axis.text.y          = element_blank(),
    legend.position      = "top",
    legend.justification = "left",
    legend.title         = element_blank(),
    legend.margin        = margin(0, 0, 0, 0),
    legend.key.size      = unit(0.9, "lines")
  )

# A panel that holds only text (used for the header and the conclusion).
text_panel <- function(title, subtitle, caption = NULL, title_size = 1.25) {
  ggplot() +
    labs(title = title, subtitle = subtitle, caption = caption) +
    theme_void(base_size = text_size) +
    theme(
      plot.background       = element_rect(fill = col_surface, colour = NA),
      plot.margin           = margin(8, 28, 8, 28),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", size = rel(title_size),
                                   colour = col_ink, hjust = 0),
      plot.subtitle = element_text(colour = col_ink2, lineheight = 1.15,
                                   hjust = 0, margin = margin(t = 6)),
      plot.caption  = element_text(colour = col_muted, hjust = 0,
                                   lineheight = 1.15, margin = margin(t = 10))
    )
}

# Men are always drawn before women: left bar, then right bar.
gender_levels <- c("Men", "Women")
dodge <- position_dodge(width = 0.72)
men   <- filter(overall, gender == "Men")
women <- filter(overall, gender == "Women")

# 4b. Header.
p_header <- text_panel(
  title = "Was Berkeley biased against women?",
  subtitle = sprintf(
    "Graduate admissions at the University of California, Berkeley, in 1973:\n%s applicants to the six largest departments.",
    num(sum(overall$applicants))
  ),
  title_size = 2.1
)

# 4c. Panel 1: overall admission rate, one horizontal meter per gender.
#     In a horizontal chart ggplot puts the first level at the bottom,
#     so the levels are reversed here to get men on top.
overall_plot <- overall |>
  mutate(
    gender = factor(gender, levels = rev(gender_levels)),
    detail = sprintf("%s of %s applicants admitted",
                     num(admitted), num(applicants))
  )

p_overall <- ggplot(overall_plot, aes(y = gender, fill = gender)) +
  geom_col(aes(x = 1), alpha = 0.2, width = 0.4) +   # the track: 100%
  geom_col(aes(x = rate), width = 0.4) +             # the fill: admitted
  geom_text(aes(x = 1.03, label = pct(rate)), hjust = 0, fontface = "bold",
            size = txt(1.8), colour = col_ink) +
  geom_text(aes(x = 0, label = detail), hjust = 0, vjust = 1,
            position = position_nudge(y = -0.3),
            size = txt(0.85), colour = col_muted) +
  scale_fill_manual(values = gender_cols, guide = "none") +
  scale_x_continuous(limits = c(0, 1.25), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.75, 0.45))) +
  labs(
    title = "1. At first glance: yes",
    subtitle = wrap(sprintf(
      "Across the six departments together, %s of the men who applied were admitted, but only %s of the women. That is a gap of %.1f percentage points.",
      pct(men$rate), pct(women$rate), 100 * (men$rate - women$rate)
    ))
  ) +
  theme_panel +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold", size = rel(1.1),
                               colour = col_ink)
  )

# 4d. Panel 2: admission rate per department, men and women side by side.
#     The rate is written above each bar and the number of applicants (n)
#     below it.
dept_plot <- by_dept |>
  mutate(gender = factor(gender, levels = gender_levels))

p_dept <- ggplot(dept_plot, aes(x = dept, y = rate, fill = gender)) +
  geom_col(position = dodge, width = 0.64) +
  geom_hline(yintercept = 0, colour = col_axis, linewidth = 0.4) +
  geom_text(aes(label = pct0(rate)), position = dodge, vjust = -0.6,
            size = txt(0.85), colour = col_ink2) +
  geom_text(aes(y = -0.07, label = applicants), position = dodge,
            size = txt(0.8), colour = col_muted) +
  annotate("text", x = 0.58, y = -0.07, label = "n =", hjust = 1,
           size = txt(0.8), colour = col_muted) +
  scale_fill_manual(values = gender_cols) +
  scale_x_discrete(labels = function(d) paste("Dept", d)) +
  scale_y_continuous(limits = c(-0.12, 0.95), expand = c(0, 0)) +
  labs(
    title = "2. Department by department: no",
    subtitle = wrap(sprintf(
      "Women had the higher admission rate in %d of the %d departments. Above each bar is the share of applicants admitted; below it, n is the number who applied.",
      n_women_ahead, n_depts
    ))
  ) +
  theme_panel

# 4e. Panel 3: where men and women applied, with the same layout as panel 2
#     so that the departments line up. A bracket over each group of
#     departments gives the share of men and women who applied there.
apps_plot <- applications |>
  mutate(gender = factor(gender, levels = gender_levels))

dept_labels <- dept_difficulty |>
  mutate(label = sprintf("Dept %s\nadmits %s", dept, pct0(dept_rate)))

# One row per group: where it starts and ends on the x axis (departments are
# at x = 1, 2, 3, ...) and the text of its note.
group_notes <- by_difficulty |>
  pivot_wider(names_from = gender, values_from = share) |>
  left_join(
    dept_difficulty |>
      mutate(position = row_number()) |>
      group_by(selective) |>
      summarise(x_from = min(position) - 0.4, x_to = max(position) + 0.4,
                .groups = "drop"),
    by = "selective"
  ) |>
  mutate(
    heading = if_else(selective, "Selective departments",
                      "Easier departments"),
    detail  = sprintf("Chosen by %s of men\nand %s of women",
                      pct(Men), pct(Women))
  )

p_apps <- ggplot(apps_plot, aes(x = dept, y = share, fill = gender)) +
  geom_col(position = dodge, width = 0.64) +
  geom_hline(yintercept = 0, colour = col_axis, linewidth = 0.4) +
  geom_text(aes(label = pct0(share)), position = dodge, vjust = -0.6,
            size = txt(0.85), colour = col_ink2) +
  geom_segment(data = group_notes,
               aes(x = x_from, xend = x_to, y = 0.43, yend = 0.43),
               inherit.aes = FALSE, colour = col_axis, linewidth = 0.4) +
  geom_text(data = group_notes, aes(x = x_from, y = 0.45, label = detail),
            inherit.aes = FALSE, hjust = 0, vjust = 0, lineheight = 1.1,
            size = txt(0.9), colour = col_ink2) +
  geom_text(data = group_notes, aes(x = x_from, y = 0.565, label = heading),
            inherit.aes = FALSE, hjust = 0, vjust = 0, fontface = "bold",
            size = txt(0.9), colour = col_ink) +
  scale_fill_manual(values = gender_cols) +
  scale_x_discrete(labels = setNames(dept_labels$label, dept_labels$dept)) +
  scale_y_continuous(limits = c(0, 0.63), expand = c(0, 0)) +
  labs(
    title = "3. Why: women applied where it was hardest to get in",
    subtitle = wrap(
      "Share of each gender's applications that went to each department. Selective departments are those that admit fewer than half of their applicants."
    )
  ) +
  theme_panel

# 4f. Conclusion and source.
p_answer <- text_panel(
  title = "The answer: not in the admission decisions",
  subtitle = wrap(sprintf(
    "Within departments women were admitted about as often as men, and in %d of %d more often. The overall gap appears because most women applied to departments that turned away most applicants, men and women alike. A pattern that reverses when groups are pooled is known as Simpson's paradox.",
    n_women_ahead, n_depts
  )),
  caption = "Data: UCBAdmissions, built into R. Source: Bickel, Hammel and O'Connell (1975),\nScience 187, 398-403. Made with R and the tidyverse."
)

# 4g. Put the panels on one page. The page is an empty plot whose y axis
#     runs from 0 to 12 (inches); each panel is placed in a horizontal band.
#
#     ggplotGrob() turns a plot into a drawable object, and to measure text
#     it needs an open graphics device. pdf(NULL) opens one that writes no
#     file; without it, Rscript would leave a stray Rplots.pdf behind.
page_width  <- 8
page_height <- 12

pdf(NULL)
page <- ggplot() +
  annotation_custom(ggplotGrob(p_header),  ymin = 10.8, ymax = 12) +
  annotation_custom(ggplotGrob(p_overall), ymin = 8.65, ymax = 10.8) +
  annotation_custom(ggplotGrob(p_dept),    ymin = 5.45, ymax = 8.65) +
  annotation_custom(ggplotGrob(p_apps),    ymin = 1.85, ymax = 5.45) +
  annotation_custom(ggplotGrob(p_answer),  ymin = 0,    ymax = 1.85) +
  scale_x_continuous(limits = c(0, page_width), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, page_height), expand = c(0, 0)) +
  theme_void() +
  theme(plot.background = element_rect(fill = col_surface, colour = NA))
invisible(dev.off())

ggsave(out_file, page, width = page_width, height = page_height, dpi = 200)

cat(sprintf("\nInfographic written to %s\n", out_file))
