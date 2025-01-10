## -----------------------------------------------------------------------------
library(dplyr)
library(labelled)
library(saros.base)
ex_survey |>
   mutate(b_1 = labelled::to_labelled(b_1)) |>
   count(b_1) # If your categorical variables look like this, you need to convert them

# This is how they ought to look
ex_survey |>
   count(b_1)


## -----------------------------------------------------------------------------
# Optionally convert all unordered to ordered factors if they are mostly that
my_data <- 
   ex_survey |> 
   mutate(across(where(~is.factor(.x)), ~factor(.x, ordered=TRUE)))


## -----------------------------------------------------------------------------
ex_survey |>
   look_for("^[abdep]_", details=TRUE)
# Alternatively
library(purrr)
ex_survey |>
   select(where(~is.factor(.x))) |>
   map(~levels(.x))


## -----------------------------------------------------------------------------
data <-
   ex_survey |>
   mutate(across(matches("p_"),
      ~factor(.x, levels=c(
            "Strongly disagree",
            "Somewhat disagree",
            "Somewhat agree",
            "Strongly agree"),
         ordered=TRUE))) |> # FALSE if nominal
   count(p_3)


## -----------------------------------------------------------------------------
library(forcats)
original_data <- ex_survey
modified_data <-
   original_data |>
   mutate(x1_sex = fct_rev(x1_sex)) |>
   copy_labels_from(from = original_data)


