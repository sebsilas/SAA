MST_example_protocol <- function() {

  c(

    psychTestR::one_button_page(body = shiny::tags$p(psychTestR::i18n("test_instructions_3")),
                                button_text = psychTestR::i18n("Next")),

    musicassessr::play_melody_until_satisfied_loop(melody = examples[['1']],
                                     var_name = "melody",
                                     max_goes = 3,
                                     page_type = "record_audio_page",
                                     page_text = psychTestR::i18n("main_trial_message"),
                                     get_answer = musicassessr::get_answer_null),

    musicassessr::play_melody_until_satisfied_loop(melody = examples[['2']],
                                     var_name = "melody",
                                     max_goes = 3,
                                     page_text = psychTestR::i18n("main_trial_message"),
                                     page_type = "record_audio_page",
                                     get_answer = musicassessr::get_answer_null),

    psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$p(psychTestR::i18n("ready_for_test")),
                                        shiny::tags$p(psychTestR::i18n("end_setup_message"))
    ), button_text = psychTestR::i18n("Next"))

  )

}
