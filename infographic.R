
# infographic.R
#
# Question: Was UC Berkeley biased against women in its 1973 graduate
#           admissions?
# Data:     UCBAdmissions, built into R (the six largest departments).
#
# Work in progress: this version prints the numbers and writes the infographic
# with fixed settings. The command-line argument is added in the next step.
#
# Run from the project folder with:
#   Rscript infographic.R

suppressPackageStartupMessages(library(tidyverse))

# ---- 0. Settings ------------------------------------------------------------
out_file  <- "berkeley_admissions.png"  # where the infographic is written
text_size <- 11                         # base text size in points

# Small helpers for turning numbers into text, and for breaking long
# sentences into lines of the same length on every machine.
pct   <- function(x) sprintf("%.1f%%", 100 * x)             # 0.445 -> "44.5%"
num   <- function(x) formatC(x, format = "d", big.mark = ",") # 2691  -> "2,691"
wrap  <- function(x) str_wrap(x, width = 80)

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

# 4a. Colours and a shared theme.
#     One colour per gender, used the same way in every panel.
gender_cols <- c(Men = "#2a78d6", Women = "#eb6834")
col_surface <- "#fcfcfb"  # page background
col_ink     <- "#0b0b0b"  # main text
col_ink2    <- "#52514e"  # secondary text
col_muted   <- "#898781"  # small print
col_grid    <- "#e1e0d9"  # gridlines
col_axis    <- "#c3c2b7"  # connector lines

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
    panel.grid.minor     = element_blank(),
    panel.grid.major.y   = element_blank(),
    panel.grid.major.x   = element_line(colour = col_grid, linewidth = 0.3),
    axis.title           = element_blank(),
    axis.text.x          = element_text(colour = col_muted),
    axis.text.y          = element_text(colour = col_ink),
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

# In horizontal charts ggplot draws the first factor level at the bottom,
# so "Women" goes first to get men on top.
gender_levels <- c("Women", "Men")
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

# 4c. Panel 1: overall admission rate, one meter per gender.
overall_plot <- overall |>
  mutate(
    gender = factor(gender, levels = gender_levels),
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
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_blank(),
    axis.text.y        = element_text(face = "bold", size = rel(1.1))
  )

# 4d. Panel 2: admission rate per department, with the number of applicants
#     (n) written next to each dot. The label of the lower rate goes to the
#     left of its dot and the label of the higher rate to the right, so the
#     two can never overlap.
dept_plot <- by_dept |>
  group_by(dept) |>
  mutate(is_lower = rank(rate, ties.method = "first") == 1) |>
  ungroup() |>
  mutate(
    gender      = factor(gender, levels = gender_levels),
    n_label     = paste("n =", applicants),
    label_x     = rate + if_else(is_lower, -0.025, 0.025),
    label_hjust = if_else(is_lower, 1, 0)
  )

gap_plot <- dept_wide |>
  mutate(gap_label = sprintf("%s +%.1f", if_else(women_ahead, "Women", "Men"),
                             100 * abs(gap)))

p_dept <- ggplot(dept_plot, aes(x = rate, y = dept)) +
  geom_line(aes(group = dept), colour = col_axis, linewidth = 0.8) +
  geom_point(aes(fill = gender), shape = 21, colour = col_surface,
             stroke = 1, size = 4) +
  geom_text(aes(x = label_x, label = n_label, hjust = label_hjust),
            size = txt(0.8), colour = col_muted) +
  geom_text(data = gap_plot, aes(x = 1.3, label = gap_label), hjust = 1,
            size = txt(0.9), colour = col_ink2) +
  annotate("text", x = 1.3, y = Inf, label = "Higher rate (% points)",
           hjust = 1, vjust = -0.6, size = txt(0.8), colour = col_muted) +
  scale_fill_manual(values = gender_cols, breaks = c("Men", "Women")) +
  scale_x_continuous(limits = c(-0.13, 1.3), expand = c(0, 0),
                     breaks = seq(0, 1, 0.25),
                     labels = function(x) paste0(100 * x, "%")) +
  scale_y_discrete(limits = rev, labels = function(d) paste("Dept", d)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "2. Department by department: no",
    subtitle = wrap(sprintf(
      "Women had the higher admission rate in %d of %d departments. Each dot is the share of applicants admitted, and n is the number of applicants behind it.",
      n_women_ahead, n_depts
    ))
  ) +
  theme_panel

# 4e. Panel 3: where men and women applied. The departments are split into
#     the easier and the selective ones, and a note beside each group gives
#     the share of men's and women's applications that went there.
apps_plot <- applications |>
  mutate(
    gender     = factor(gender, levels = gender_levels),
    dept_label = sprintf("Dept %s (admits %.0f%%)", dept, 100 * dept_rate)
  )

group_notes <- by_difficulty |>
  pivot_wider(names_from = gender, values_from = share) |>
  left_join(
    summarise(group_by(dept_difficulty, selective), depts = n(),
              .groups = "drop"),
    by = "selective"
  ) |>
  mutate(
    heading = if_else(selective, "Selective departments",
                      "Easier departments"),
    detail  = sprintf("%s of men applied here,\n%s of women",
                      pct(Men), pct(Women)),
    y       = (depts + 1) / 2   # the vertical middle of the group
  )

p_apps <- ggplot(apps_plot, aes(x = share, y = dept_label, fill = gender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.68) +
  geom_text(aes(label = sprintf("%.0f%%", 100 * share)),
            position = position_dodge(width = 0.8), hjust = -0.2,
            size = txt(0.8), colour = col_ink2) +
  geom_text(data = group_notes, aes(x = 0.42, y = y, label = heading),
            inherit.aes = FALSE, hjust = 0, vjust = -0.5, fontface = "bold",
            size = txt(0.9), colour = col_ink) +
  geom_text(data = group_notes, aes(x = 0.42, y = y, label = detail),
            inherit.aes = FALSE, hjust = 0, vjust = 1.1, lineheight = 1.1,
            size = txt(0.9), colour = col_ink2) +
  facet_grid(rows = vars(selective), scales = "free_y", space = "free_y") +
  scale_fill_manual(values = gender_cols, breaks = c("Men", "Women")) +
  # The axis runs well past the longest bar to leave room for the notes.
  scale_x_continuous(limits = c(0, 0.7), expand = c(0, 0)) +
  scale_y_discrete(limits = rev) +
  coord_cartesian(clip = "off") +
  labs(
    title = "3. Why: women applied where it was hardest to get in",
    subtitle = wrap(
      "Share of each gender's applications that went to each department. Selective departments are those that admit fewer than half of their applicants."
    )
  ) +
  theme_panel +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_blank(),
    strip.text         = element_blank(),
    panel.spacing      = unit(1.2, "lines")
  )

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
  annotation_custom(ggplotGrob(p_header),  ymin = 10.75, ymax = 12) +
  annotation_custom(ggplotGrob(p_overall), ymin = 8.5,   ymax = 10.75) +
  annotation_custom(ggplotGrob(p_dept),    ymin = 5.2,   ymax = 8.5) +
  annotation_custom(ggplotGrob(p_apps),    ymin = 1.9,   ymax = 5.2) +
  annotation_custom(ggplotGrob(p_answer),  ymin = 0,     ymax = 1.9) +
  scale_x_continuous(limits = c(0, page_width), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, page_height), expand = c(0, 0)) +
  theme_void() +
  theme(plot.background = element_rect(fill = col_surface, colour = NA))
invisible(dev.off())

ggsave(out_file, page, width = page_width, height = page_height, dpi = 200)

cat(sprintf("\nInfographic written to %s\n", out_file))
