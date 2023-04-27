# Use target pipeline
targets::use_targets()

usethis::use_r("clean_data")
usethis::use_r("string_replacements")
usethis::use_r("basic_stat")
usethis::use_directory("paper")
usethis::use_file("paper")
file.create(here::here("paper", "main_text.Rmd"))

