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
    language = names(translations) %>% setdiff("id"),
    data = map(
      language,
      ~trial %>%
        select(trt, age, grade) %>%
        mutate(
          trt = case_when(
            trt == "Drug A" ~ translate(.x, "Drug A"),
            trt == "Drug B" ~ translate(.x, "Drug B")
          ),
          grade = case_when(
            grade == "I" ~ translate(.x, "I"),
            grade == "II" ~ translate(.x, "II"),
            grade == "III" ~ translate(.x, "III"),
          ) %>% factor(levels = filter(translations, id %in% c("I", "II", "III"))[[.x]])
        )
    ),
    tbl_summary = map2(
      language, data,
      function(language, data) {
        theme_gtsummary_language(language)
        tbl <- tbl_summary(
          data,
          by = trt,
          label = list(age = translate(language, "Age"),
                       grade = translate(language, "Grade"))
        ) %>%
          add_p() %>%
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
          cols_width(
            vars(label) ~ px(170),
            starts_with("stat_") ~ px(230),
            vars(p.value) ~ px(110),
            everything() ~ px(110)
          )
        reset_gtsummary_theme()

        gtsave(tbl, filename = glue::glue("tbl_summary-{language}.png"))
        tbl
      }
    )
  )

df_results$tbl_summary[[1]]

# make the gif at https://ezgif.com/maker
