MST <- function(aws_credentials,
                num_items = list("long_tones" = 6L,
                              "arrhythmic" = 10L,
                              "rhythmic" = 10L),
                item_bank = itembankr::Berkowitz,
                demographics = TRUE,
                demo = FALSE,
                feedback = FALSE,
                admin_password = "demo",
                SNR_test = TRUE,
                get_range = TRUE
                ) {

  if(demo) warning('Running MST in demo mode!')

  timeline <- psychTestR::join(
    psychTestR::new_timeline(
    psychTestR::join(

  psychTestR::module("MST",
    # introduction, same for all users
    MST_intro(aws_credentials, demo, SNR_test, get_range),

    # long tone trials
    musicassessr::long_tone_trials(num_items$long_tones, num_examples = 2, feedback = feedback),

    # arrhythmic
    musicassessr::arrhythmic_melody_trials(item_bank = item_bank,
                                           num_items = num_items$arrhythmic,
                                           num_examples = 2,
                                           feedback = feedback),

    # rhythmic
    musicassessr::rhythmic_melody_trials(item_bank = item_bank,
                                           num_items = num_items$rhythmic,
                                           num_examples = 2,
                                           feedback = feedback),

    psychTestR::elt_save_results_to_disk(complete = FALSE)

      )
    ),
    dict = MST_dict
  ),
    psyquest::GMS(subscales = c("Musical Training", "Singing Abilities")),
      musicassessr::deploy_demographics(demographics),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
      psychTestR::final_page("You have completed the Melody Singing Task!")
  )


  # run the test
  psychTestR::make_test(
    elts = timeline,
    opt = psychTestR::test_options(title = "Melody Singing Task",
                       admin_password = admin_password,
                       display = psychTestR::display_options(
                         left_margin = 1L,
                         right_margin = 1L,
                         css = system.file('www/css/style.css', package = "musicassessr")
                         ),
                       languages = c("en")
    )#, custom_admin_panel = musicassessr::aws_admin_panel
    )
}
