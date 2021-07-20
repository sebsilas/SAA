

MST_instructions <- function() {
  list(
    psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$p(psychTestR::i18n("test_instructions_1.1")),
                                shiny::tags$p(psychTestR::i18n("test_instructions_1.2"))),
                    button_text = psychTestR::i18n("Next")
    ),

    psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$p(psychTestR::i18n("test_instructions_2.1")),
                                       shiny::tags$p(psychTestR::i18n("test_instructions_2.2"))),
                    button_text = psychTestR::i18n("Next")

    )
  )
}

