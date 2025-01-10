## -----------------------------------------------------------------------------
#| echo: false
setwd(tempdir())


## -----------------------------------------------------------------------------
saros.base::copy_folder_contents_to_dir(
  from, 
  to = getwd(),
  only_copy_folders = FALSE)


## -----------------------------------------------------------------------------
library(saros.base)
saros.base::download_zip_to_folder(out_path = fs::path(getwd(), "Saros")) # Set to your project folder's Saros-folder (keep absolute folder path as short as possible)


## -----------------------------------------------------------------------------
# Modify these to your liking
project_path <- 
  fs::path(getwd())
project_initials <- "SMILE"
saros_folder_name <- "Saros"
project_structure_yaml_path <- "project_folder_structure.yaml"

# Se function help for more details
saros.base::initialize_saros_project(
  path = project_path, 
  structure_path = project_structure_yaml_path, 
  replacement_list = c("prosjekt_initialer" = project_initials),
  numbering_prefix = "global_max",
  numbering_inheritance = TRUE,
  word_separator = NULL,
  numbering_name_separator = " ",
  numbering_parent_child_separator = NULL,
  case = "asis",
  count_existing_folders = FALSE,
  r_files_out_path = fs::path(project_path, paste0(saros_folder_name, project_initials), "01_script", "script_templates"), 
  create = FALSE) # SET create = TRUE when you are satisfied with the folder structure


## -----------------------------------------------------------------------------
quarto_yaml <- yaml::read_yaml(file.path("02_resources", "YAML", "_quarto.yaml"))

quarto_yaml$website$title <- "PROJECT TITLE"
quarto_yaml$website$`site-url` <- "WEBSITE URL"
quarto_yaml$project_no <- "INTERNAL PROJECT NUMBER" # Usually not needed
quarto_yaml$funder <- "FUNDER"
quarto_yaml$funder_address <- "FUNDER'S ADDRESS"
yaml::write_yaml(quarto_yaml, file = file.path("02_resources", "YAML", "_quarto.yaml"))

report_yaml <- yaml::read_yaml(file.path("02_resources", "YAML", "_report_header.yaml"))
report_yaml$signer_1 <- "REPORT SIGNER 1"
report_yaml$signer_1_title <- "REPORT SIGNER 1 JOB TITLE"
report_yaml$signer_1 <- "REPORT SIGNER 2"
report_yaml$signer_1_title <- "REPORT SIGNER 2 JOB TITLE"
report_yaml$report_type <- "rapport" # Usually not needed
yaml::write_yaml(report_yaml, file = file.path("02_resources", "YAML", "_report_header.yaml"))


