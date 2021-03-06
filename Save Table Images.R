library(tidyverse)
library(gtsummary)
library(gt)

# import data
translations <- readxl::read_excel("translations.xlsx")

# function that takes the language and ID, and returns translation
translate <- function(language, id) {
  translations %>%
    filter(id == .env$id) %>%
    pull(language)
}

df_results <-
  tibble(
    # list all languages to create figures for
    language = names(translations) %>% setdiff("id"),
    # transform the underlying data into that language
    data = map(
      language,
      ~trial %>%
        select(trt, age, grade) %>%
        mutate(
          trt = case_when(
            trt == "Drug A" ~ translate(.x, "Drug A"),
            trt == "Drug B" ~ translate(.x, "Drug B")
          )
        )
    ),
    # create the tbl_summary object and save it
    tbl_summary = map2(
      language, data,
      function(language, data) {
        # SET THE LANGUAGE THEME
        theme_gtsummary_language(language)
        tbl <-
          # build gtsummary table
          tbl_summary(
            data,
            by = trt,
            label = list(age = translate(language, "Age"),
                         grade = translate(language, "Grade"))
          ) %>%
          add_p() %>%
          # convert to gt and add the language title and flag
          as_gt() %>%
          tab_header(
            title = html(
              paste0("<strong>",
                     translate(language, "language_en"), ", ",
                     translate(language, "language"),
                     "</strong>"),
              web_image(
                url = translate(language, "flag_url"),
                height = px(19)
              )
            )
          ) %>%
          # set common widths so the GIF looks cute!
          cols_width(
            vars(label) ~ px(170),
            starts_with("stat_") ~ px(230),
            vars(p.value) ~ px(110),
            everything() ~ px(110)
          )
        reset_gtsummary_theme()

        # write the table to disk
        gtsave(tbl, filename = glue::glue("tbl_summary-{language}.png"))
        tbl
      }
    )
  )

# make the gif at https://ezgif.com/maker
