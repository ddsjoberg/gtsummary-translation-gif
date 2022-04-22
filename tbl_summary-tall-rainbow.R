library(tidyverse)
library(gtsummary)
library(gt)

# import data
translations <- readxl::read_excel("translations.xlsx")
all_languages <- setdiff(names(translations), "id") %>% sort()
rainow_hex <- c("#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663")

# function that takes the language and ID, and returns translation
translate <- function(language, id) {
  translations %>%
    filter(id == .env$id) %>%
    pull(language)
}



create_tall_table <- function(language) {
  theme_gtsummary_language(language)
  tbl <-
    trial %>%
    select(trt, age, grade) %>%
    mutate(
      trt = case_when(
        trt == "Drug A" ~ translate(language, "Drug A"),
        trt == "Drug B" ~ translate(language, "Drug B")
      )
    ) %>%
    tbl_summary(by = trt) %>%
    add_p() %>%
    as_gt() %>%
    # set common widths so the GIF looks cute!
    cols_width(
      c(label) ~ px(170),
      starts_with("stat_") ~ px(230),
      c(p.value) ~ px(110),
      everything() ~ px(110)
    )

  for (i in seq_along(all_languages)) {
    tbl <-
      tbl %>%
      gt::tab_spanner(
        id = all_languages[i],
        label = html(
          paste0("<strong>",
                 translate(all_languages[i], "language_en"), ", ",
                 translate(all_languages[i], "language"),
                 "</strong>"),
          web_image(
            url = translate(all_languages[i], "flag_url"),
            height = px(19)
          )
        ),
        columns = everything(),
        level = i
      )

    if (language == all_languages[i]) {
      tbl <-
        tab_style(
          tbl,
          style = cell_fill(color = rainow_hex[(i %% length(rainow_hex)) + 1L], alpha = 0.6),
          locations = cells_column_spanners(spanners = all_languages[i])
        )
    }
  }

  reset_gtsummary_theme()

  # write the table to disk
  gtsave(tbl, filename = glue::glue("tbl_summary-tall-rainbow-{language}.png"))
  tbl
}

create_tall_table("se")



df_results <-
  tibble(
    # list all languages to create figures for
    language = all_languages
  ) %>%
  rowwise() %>%
  mutate(
    gt_tbl = create_tall_table(language) %>% list()
  )



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
