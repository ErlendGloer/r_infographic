Was Berkeley biased against women?

A reproducible analysis in R. One script, infographic.R, takes a dataset that ships with R, computes the numbers and writes an infographic that answers the question.

Infographic: was Berkeley biased against women?

The question and the answer

In 1973, 44.5% of the men who applied to the six largest graduate departments at UC Berkeley were admitted, but only 30.4% of the women. Was that bias against women?

Not in the admission decisions. Department by department, women had the higher admission rate in 4 of the 6 departments. The overall gap appears because women mostly applied to departments that turned away most applicants: 92.8% of women's applications went to departments that admit fewer than half, against 48.5% of men's. A pattern that reverses when groups are pooled like this is known as Simpson's paradox.

The data are UCBAdmissions, built into R. They come from Bickel, Hammel and O'Connell (1975), "Sex bias in graduate admissions: data from Berkeley", Science 187, 398-403.

Reproduce it

You need R 4.1 or later and the tidyverse package (install.packages("tidyverse")). Nothing else is used.

git clone https://github.com/ErlendGloer/r_infographic.git
cd r_infographic
Rscript infographic.R r4

The last line is the exact command that produced the committed image, berkeley_admissions_r4.png. It runs without any interactive steps, prints the numbers to the terminal and writes the image to the current folder, replacing the committed one with your own.

What you should get

The same numbers. The terminal output should match this exactly:


Overall admission rate
 gender applicants admitted  rate
    Men       2691     1198 44.5%
  Women       1835      557 30.4%

Applicants (n) and admission rate by department
 dept men_n men_rate women_n women_rate women_ahead dept_rate selective
    A   825    62.1%     108      82.4%        TRUE     64.4%     FALSE
    B   560    63.0%      25      68.0%        TRUE     63.2%     FALSE
    C   325    36.9%     593      34.1%       FALSE     35.1%      TRUE
    D   417    33.1%     375      34.9%        TRUE     34.0%      TRUE
    E   191    27.7%     393      23.9%       FALSE     25.2%      TRUE
    F   373     5.9%     341       7.0%        TRUE      6.4%      TRUE

Women had the higher admission rate in 4 of 6 departments.

Share of applications by department difficulty
 selective   Men Women
     FALSE 51.5%  7.2%
      TRUE 48.5% 92.8%

Infographic written to berkeley_admissions_r4.png

The same picture. Your berkeley_admissions_r4.png should show the same layout, bars, numbers and colours as the image at the top of this page. Fonts and graphics devices differ between machines, so small differences in the lettering and its spacing are expected.

To compare, open your image next to the one shown on GitHub. git status tells you whether your file differs at all from the committed one, and git restore berkeley_admissions_r4.png brings the committed one back.

The argument: a colour scheme

The script takes exactly one argument, the colour scheme. It decides the two colours used for men and women. Every scheme is a pair of colours from one of the palettes built into R, which palette.pals() lists and palette.colors() returns.

Argument	R palette	Men	Women
r4	R4	blue	pink
okabe-ito	Okabe-Ito	blue	red
dark2	Dark 2	green	orange
set1	Set 1	blue	red
ggplot2	ggplot2	teal	salmon

The output file is named after the scheme, so Rscript infographic.R dark2 writes berkeley_admissions_dark2.png and leaves the committed image alone.

If the argument is missing, is not one of the names above, or there is more than one, the script stops and says what it expects:

$ Rscript infographic.R purple
Error: 'purple' is not a colour scheme this script knows.
  Usage:   Rscript infographic.R <colour scheme>
  Schemes: r4, okabe-ito, dark2, set1, ggplot2
  Example: Rscript infographic.R r4
Execution halted
Files
File	What it is
infographic.R	The whole analysis, from data to image
berkeley_admissions_r4.png	The infographic, made with the command above
r_infographic.Rproj	The RStudio project file