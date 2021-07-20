MST_intro <- function() {
  list(
    psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("title")),
                                           shiny::tags$img(src = 'custom-assets/img/intro.png', height = 100, width = 100),
                                           shiny::tags$p(psychTestR::i18n("welcome_message"),
                                  musicassessr::musicassessr_js_scripts(api_url = api_url,
                                                                        bucket_name = bucket_name,
                                                                        bucket_region = bucket_region,
                                                                        identity_pool_id = identity_pool_id,
                                                                        destination_bucket = destination_bucket)
                               )
    ),
    button_text = psychTestR::i18n("Next")
    ),

    psychTestR::NAFC_page(label = "headphones_and_microphone_check",
              choices = c(psychTestR::i18n("Yes"), psychTestR::i18n("No")),
              prompt = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("requirements")),
                           shiny::tags$ul(class = "roman",
                                   shiny::tags$li(psychTestR::i18n("requirements_1")),
                                   shiny::tags$li(psychTestR::i18n("requirements_2")),
                                   shiny::tags$li(psychTestR::i18n("requirements_3")),
                                   shiny::tags$ul(class = "square",
                                           shiny::tags$li(psychTestR::i18n("requirements_4"), shiny::tags$em(psychTestR::i18n("requirements_5"))),
                                           shiny::tags$li(psychTestR::i18n("requirements_6"))
                                   )),
                           shiny::tags$p(psychTestR::i18n("requirements_final"))),
              on_complete = musicassessr::have_requirements
    ),

    # volume calibration
    musicassessr::test_headphones_page()
  )

}

