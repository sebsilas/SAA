# usage
# MST(num_items = 10L)
# 2do.. script imports should happen at musicassessr level and enable.cors below etc.
# items_characteristics_sampler_block should allow corpus as argument..

# constants

api_url <- "https://255uxe6ajl.execute-api.us-east-1.amazonaws.com/api"
bucket_name <- "shinny-app-source-41630"
bucket_region <- "us-east-1"
identity_pool_id <- "us-east-1:feecdf7e-cdf6-416f-94d0-a6de428c8c6b"
destination_bucket <- "shinny-app-destination-41630"

examples <- list("1" = "62,64,65,67,64,60,62",
                 "2" = "60,59,58,60,57,55")

# dictionary
dict_df <- readxl::read_excel("data-raw/MST_dict.xlsx")
dict <- psychTestR::i18n_dict$new(dict_df)

.onLoad <- function(...) {
  shiny::addResourcePath(
    prefix = "custom-assets", # custom prefix that will be used to reference your directory
    directoryPath = system.file("www", package = "MST") # path to resource in your package
  )
  shiny::addResourcePath(
    prefix = "item_banks", # custom prefix that will be used to reference your directory
    directoryPath = system.file("item_banks", package = "itembankr") # path to resource in your package
  )
}

#shiny::addResourcePath("stimuli", "/Users/sebsilas/musicassessr_showcase/www/stimuli")

# shiny::addResourcePath("item_banks",
#                        system.file("data-raw/Berkowitz/berkowitz_midi_rhythmic_100bpm",
#                                    package = 'musicassessr',
#                                    mustWork = TRUE))

# musicassessr::present_stimuli(stimuli = 'item_banks/Berkowitz/berkowitz_midi_rhythmic_100bpm',
#                               stimuli_type = "midi_file",
#                               display_modality = "auditory",
#                               page_title = "Play Midi File"
# )


MST <- function(num_items = list("long_tones" = 6L,
                              "arrhythmic" = 10L,
                              "rhythmic" = 10L), item_bank = itembankr::Berkowitz) {

  timeline <- psychTestR::new_timeline(
    psychTestR::join(


      # introduction, same for all users
      MST_intro(),


     musicassessr::microphone_calibration_page(body = shiny::tags$div(shiny::tags$p(psychTestR::i18n("microphone_calibration_message_1")),
                                                      shiny::tags$p(psychTestR::i18n("microphone_calibration_message_2"))),
                                 button_text = psychTestR::i18n("microphone_calibration_button")),

      # musicassessr::get_instrument_range_pages("record_audio_page"),
     musicassessr::fake_range(),
     # sample melodies based on range

    musicassessr::sample_from_user_range(num_items$long_tones),

    musicassessr::build_multi_play_long_tone_record_audio_pages(no_pages = num_items$long_tones, page_type = "record_audio_page"),


     psychTestR::code_block(function(state, ...) {

       span <- psychTestR::get_global("span", state)

       # sample arrhythmic
       arrythmic_item_bank_subset <- itembankr::subset_item_bank(item_bank = item_bank("main"), span_max = span)
       arrhythmic_sample <- musicassessr::item_sampler(arrythmic_item_bank_subset, num_items$arrhythmic)
       psychTestR::set_global("arrhythmic_melody", arrhythmic_sample, state)

       # sample rhythmic
       rhythmic_item_bank_subset <- itembankr::subset_item_bank(item_bank = item_bank("phrases"), span_max = span)
       rhythmic_sample <- musicassessr::item_sampler(rhythmic_item_bank_subset, num_items$rhythmic)
       psychTestR::set_global("rhythmic_melody", rhythmic_sample, state)

     }),

      # instructions
      MST_instructions(),
      #
      # example protocol
      MST_example_protocol(),


      # arrhythmic
      musicassessr::build_multi_page_play_melody_until_satisfied_loop(
                  n_items = num_items$arrhythmic,
                  var_name = "arrhythmic_melody",
                  page_type = "record_audio_page",
                  max_goes = 3,
                  page_text = psychTestR::i18n("main_trial_message"),
                  get_answer = musicassessr::get_answer_store_async,
                  rel_to_abs_mel_function = musicassessr::rel_to_abs_mel_mean_centred,
                  arrhythmic = TRUE
                  ),

    psychTestR::one_button_page("Now you will hear melodies with rhythms. Please try and sing the melodies with the correct rhythm."),

     # rhythmic
     musicassessr::build_multi_page_play_melody_until_satisfied_loop(
       stimuli_type = "midi_notes",
       var_name = "rhythmic_melody",
       n_items = num_items$rhythmic,
       page_text = psychTestR::i18n("main_trial_message"),
       get_answer = musicassessr::get_answer_store_async,
       rel_to_abs_mel_function = musicassessr::rel_to_abs_mel_mean_centred
       ),


      psychTestR::NAFC_page(label = "how_long_PBE",
                prompt = "Approximately how long have you been playing by ear?",
                choices = c("Not at all", "Under 1 year", "1-2 years", "2-4 years", "4-10 years", "10+ years"),
                on_complete = musicassessr::on_complete_recode_async_penultimate
      ),

      psychTestR::NAFC_page(label = "how_good_PBE",
                prompt = "How good would you rate yourself at playing by ear?",
                choices = c("Not Good", "Quite Good", "Very Good", "Expert"),
                on_complete = musicassessr::on_complete_recode_async_penultimate
      ),

      psychTestR::NAFC_page(label = "interested_PBE_improve",
                prompt = "Are you interested in improving your playing by ear skills?",
                choices = c("Yes", "No")
      ),

      psychTestR::elt_save_results_to_disk(complete = FALSE),

     psychTestR::final_page("Thank you! You have completed the Melody Singing Task.")

    ),
    dict = dict
  )

  # run the test
  psychTestR::make_test(
    elts = timeline,
    opt = psychTestR::test_options(title = "Playing By Ear Study",
                       admin_password = "demo",
                       display = psychTestR::display_options(
                         left_margin = 1L,
                         right_margin = 1L,
                         css = system.file('www/css/style.css', package = "MST")
                         ),
                       languages = c("de", "en")
    )#, custom_admin_panel = musicassessr::aws_admin_panel
    )
}


#itembankr::Berkowitz("phrases")

# MST(num_items = list("long_tones" = 3L,
#                      "arrhythmic" = 5L,
#                      "rhythmic" = 5L
# ))

