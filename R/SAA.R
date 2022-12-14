

#' Deploy SAA as standalone test
#'
#' @param num_items The number of items as a list.
#' @param item_bank The item bank (created with itembankr) to deployed with the test.
#' @param demographics Deploy demographic form?
#' @param demo Is demo?
#' @param feedback Give feedback after trials?
#' @param admin_password psychTestR admin password.
#' @param SNR_test Deploy signal-to-noise ratio test?
#' @param get_range Deploy a test to get the users range at test time and present stimuli accordingly?
#' @param absolute_url If using online, absolute URL?
#' @param examples No of examples.
#' @param final_results Display final results?
#' @param musicassessr_aws Is this being deployed on AWS via the musicassessr setup?
#' @param store_results_in_db Store results in a database?
#' @param test_username Is there a username for the user? This is different from a p_id.
#' @param gold_msi Deploy Gold-MSI form?
#' @param with_final_page Should there be a final page? FALSE if there will be more pages in the timeline.
#' @param item_length What melody lengths should the test be constrained to?
#' @param melody_sound Sound of melody? e.g, piano.
#' @param adjust_range Should the range of the user, recorded at test time, be adjusted based on heuristics?
#' @param test_name Custom name of the test.
#' @param show_socials Should social media sharing options be shown at the end?
#' @param headphones_test Should there be a headphone test?
#' @param get_user_info Grab user info via the browser?
#' @param microphone_test Deploy a microphone test?
#' @param copy_audio_to_location Where would you like the audio to be copied to?
#' @param allow_repeat_SNR_tests Logical. TRUE if participant can fail the SNR test threshold and try again.
#' @param append_trial_block_before A list of pages to go before the test.
#' @param append_trial_block_after A list of pages to go after the test.
#' @param stop_recording_after Stop recording after a certain amount of time.
#' @param max_goes How many goes can the user have per melody?
#' @param max_goes_forced Is this forced or optional?
#' @param long_tone_trials_as_screening Should long tone trials be used as a screening mechanism?
#' @param long_tone_trials_as_screening_failure_page Where should users be directed to if they fail the long tone screening?
#' @param success_on_completion_page Where should users be directed to when they complete successfully?
#' @param concise_wording TRUE for more detailed (but longer) instructions.
#' @param skip_setup TRUE to skip setup steps.
#' @param app_name Name of app.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
SAA_standalone <- function(num_items = list("long_tones" = 6L,
                                            "arrhythmic" = 10L,
                                            "rhythmic" = 10L),
                           item_bank = Berkowitz::Berkowitz,
                           demographics = TRUE,
                           demo = FALSE,
                           feedback = FALSE,
                           admin_password = "demo",
                           SNR_test = TRUE,
                           get_range = TRUE,
                           absolute_url = character(),
                           examples = 2L,
                           final_results = TRUE,
                           musicassessr_aws = FALSE,
                           store_results_in_db = FALSE,
                           test_username = character(),
                           gold_msi = TRUE,
                           with_final_page = TRUE,
                           item_length = c(3,15),
                           melody_sound = "piano",
                           adjust_range = TRUE,
                           test_name = "Singing Ability Assessment",
                           show_socials = FALSE,
                           headphones_test = TRUE,
                           get_user_info = TRUE,
                           microphone_test = TRUE,
                           copy_audio_to_location = 'audio',
                           allow_repeat_SNR_tests = TRUE,
                           append_trial_block_before = psychTestR::module("before"),
                           append_trial_block_after = psychTestR::module("after"),
                           stop_recording_after = 30,
                           max_goes = 3L,
                           max_goes_forced = FALSE,
                           long_tone_trials_as_screening = FALSE,
                           long_tone_trials_as_screening_failure_page = "http://www.google.com",
                           success_on_completion_page = character(),
                           concise_wording = TRUE,
                           skip_setup = FALSE,
                           app_name, ...) {

  timeline <- SAA(num_items,
                  item_bank,
                  demographics,
                  demo,
                  feedback,
                  admin_password,
                  SNR_test,
                  get_range,
                  absolute_url,
                  examples,
                  final_results,
                  musicassessr_aws,
                  store_results_in_db,
                  test_username,
                  gold_msi,
                  with_final_page,
                  item_length,
                  melody_sound,
                  adjust_range,
                  test_name,
                  show_socials,
                  headphones_test,
                  get_user_info,
                  microphone_test,
                  copy_audio_to_location,
                  allow_repeat_SNR_tests,
                  append_trial_block_before,
                  append_trial_block_after,
                  stop_recording_after,
                  max_goes,
                  max_goes_forced,
                  long_tone_trials_as_screening,
                  long_tone_trials_as_screening_failure_page,
                  success_on_completion_page,
                  concise_wording,
                  skip_setup,
                  app_name)


  # run the test
  psychTestR::make_test(
    elts = timeline,
    opt = psychTestR::test_options(title = test_name,
                                   admin_password = admin_password,
                                   display = psychTestR::display_options(
                                     left_margin = 1L,
                                     right_margin = 1L,
                                     css = system.file('www/css/musicassessr.css', package = "musicassessr")
                                   ),
                                   languages = c("en"),
                                   additional_scripts = musicassessr::musicassessr_js(musicassessr_aws = musicassessr_aws,
                                                                                      app_name = app_name), ...))
}







#' Deploy the SAA
#'
#' @param num_items
#' @param item_bank
#' @param demographics
#' @param demo
#' @param feedback
#' @param admin_password
#' @param SNR_test
#' @param get_range
#' @param absolute_url
#' @param examples
#' @param final_results
#' @param musicassessr_aws
#' @param store_results_in_db
#' @param test_username
#' @param gold_msi
#' @param with_final_page
#' @param item_length
#' @param melody_sound
#' @param adjust_range
#' @param test_name
#' @param show_socials
#' @param headphones_test
#' @param get_user_info
#' @param microphone_test
#' @param copy_audio_to_location
#' @param allow_repeat_SNR_tests
#' @param append_trial_block_before
#' @param append_trial_block_after
#' @param stop_recording_after
#' @param max_goes
#' @param max_goes_forced
#' @param long_tone_trials_as_screening
#' @param long_tone_trials_as_screening_failure_page
#' @param success_on_completion_page
#' @param concise_wording
#' @param skip_setup
#' @param app_name
#'
#' @return
#' @export
#'
#' @examples
SAA <- function(num_items = list("long_tones" = 6L,
                                 "arrhythmic" = 10L,
                                 "rhythmic" = 10L),
                item_bank = Berkowitz::Berkowitz,
                demographics = TRUE,
                demo = FALSE,
                feedback = FALSE,
                admin_password = "demo",
                SNR_test = TRUE,
                get_range = TRUE,
                absolute_url = character(),
                examples = 2,
                final_results = TRUE,
                musicassessr_aws = FALSE,
                store_results_in_db = FALSE,
                test_username = character(),
                gold_msi = TRUE,
                with_final_page = TRUE,
                item_length = c(3,15),
                melody_sound = "piano",
                adjust_range = TRUE,
                test_name = "Singing Ability Assessment",
                show_socials = FALSE,
                headphones_test = TRUE,
                get_user_info = TRUE,
                microphone_test = TRUE,
                copy_audio_to_location = 'audio',
                allow_repeat_SNR_tests = TRUE,
                append_trial_block_before = psychTestR::module("before"),
                append_trial_block_after = psychTestR::module("after"),
                stop_recording_after = 30,
                max_goes = 3L,
                max_goes_forced = FALSE,
                long_tone_trials_as_screening = FALSE,
                long_tone_trials_as_screening_failure_page = "http://www.google.com",
                success_on_completion_page = character(),
                concise_wording = TRUE,
                skip_setup = FALSE,
                app_name) {

  stopifnot(
    is.list(num_items),
    is.function(item_bank) | is.data.frame(item_bank),
    is.logical(demographics),
    is.logical(demo),
    is.logical(feedback),
    is.character(admin_password) & length(admin_password) == 1L,
    is.logical(SNR_test),
    is.logical(get_range) | is.character(get_range) & length(get_range) == 1,
    is.character(absolute_url),
    is.numeric(examples) & length(examples) == 1L,
    is.logical(final_results),
    is.logical(musicassessr_aws),
    is.logical(store_results_in_db),
    is.character(test_username),
    is.logical(gold_msi),
    is.logical(with_final_page),
    is.numeric(item_length) & length(item_length) <= 2,
    is.character(melody_sound) & length(melody_sound) == 1L,
    is.logical(adjust_range),
    is.character(test_name) & length(test_name) == 1L,
    is.logical(show_socials),
    is.logical(headphones_test),
    is.logical(get_user_info),
    is.logical(microphone_test),
    is.character(copy_audio_to_location) & length(copy_audio_to_location) == 1,
    is.logical(allow_repeat_SNR_tests),
    is.list(append_trial_block_before) | psychTestR::is.timeline(append_trial_block_before),
    is.list(append_trial_block_after) | psychTestR::is.timeline(append_trial_block_after),
    is.numeric(stop_recording_after) & length(stop_recording_after) == 1,
    is.numeric(max_goes) & length(max_goes) == 1,
    is.logical(max_goes_forced),
    is.logical(long_tone_trials_as_screening),
    is.character(long_tone_trials_as_screening_failure_page),
    is.character(success_on_completion_page),
    is.logical(concise_wording),
    is.logical(skip_setup),
    assertthat::is.string(app_name)
    )

  if(demo) warning('Running SAA in demo mode!')


  timeline <- psychTestR::join(
    psychTestR::new_timeline(
      psychTestR::join(

        psychTestR::module("SAA",
                           # introduction, same for all users
                           SAA_intro(demo,
                                     SNR_test,
                                     get_range,
                                     absolute_url,
                                     test_username,
                                     store_results_in_db,
                                     adjust_range,
                                     headphones_test,
                                     get_user_info,
                                     microphone_test,
                                     copy_audio_to_location,
                                     allow_repeat_SNR_tests,
                                     concise_wording,
                                     test_name,
                                     max_goes_forced,
                                     max_goes,
                                     skip_setup,
                                     app_name),

                           # arbitrary and optional trial block to go first
                           append_trial_block_before,


                           # long tone trials
                           musicassessr::long_tone_trials(num_items$long_tones, num_examples = examples, feedback = feedback,
                                                          long_tone_trials_as_screening = long_tone_trials_as_screening,
                                                          long_tone_trials_as_screening_failure_page = long_tone_trials_as_screening_failure_page),

                           # arrhythmic
                           musicassessr::arrhythmic_melody_trials(itembankr::subset_item_bank(item_bank("main"), item_length = item_length),
                                                                  num_items = num_items$arrhythmic,
                                                                  num_examples = examples,
                                                                  feedback = feedback,
                                                                  sound = melody_sound,
                                                                  page_text = "Click below to hear the melody. Sing back the melody. Click Stop when finished.",
                                                                  page_title = "Sing The Melody",
                                                                  instruction_text = "Now you will hear some melodies. Please try and sing the melodies.",
                                                                  max_goes = max_goes,
                                                                  max_goes_forced = max_goes_forced),

                           # rhythmic
                           musicassessr::rhythmic_melody_trials(item_bank = itembankr::subset_item_bank(item_bank("phrases"), item_length),
                                                                num_items = num_items$rhythmic,
                                                                num_examples = 0, # because it's effectively the same task as arrhythmic
                                                                feedback = feedback,
                                                                sound = melody_sound,
                                                                page_text = "Click below to hear the melody. Sing back the melody. Click Stop when finished.",
                                                                page_title = "Sing This Melody Plus Rhythm",
                                                                instruction_text = "Now you will hear melodies with rhythms. Please try and sing the melodies with the correct rhythm.",
                                                                max_goes = max_goes,
                                                                max_goes_forced = max_goes_forced),

                           # arbitrary and optional trial block to go after
                           append_trial_block_after,

                           musicassessr::elt_add_session_to_db(),


                           psychTestR::elt_save_results_to_disk(complete = TRUE),

                           if(final_results) final_results_saa(test_name = test_name,
                                                               url = absolute_url,
                                                               num_items$long_tones,
                                                               num_items$arrhythmic,
                                                               num_items$rhythmic,
                                                               show_socials)

        )
      ),
      dict = SAA_dict
    ),
    if(gold_msi) psyquest::GMS(subscales = c("Musical Training", "Singing Abilities")),
    musicassessr::deploy_demographics(demographics),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    musicassessr::final_page_or_continue_to_new_test(final = with_final_page, task_name = test_name)
  )

}



SAA_intro <- function(demo = FALSE,
                      SNR_test = TRUE,
                      get_range = TRUE,
                      absolute_url = character(),
                      test_username = NULL,
                      store_results_in_db = FALSE,
                      adjust_range = TRUE,
                      headphones_test,
                      get_user_info,
                      microphone_test,
                      copy_audio_to_location,
                      allow_repeat_SNR_tests,
                      concise_wording = TRUE,
                      test_name = "Singing Ability Assessment",
                      max_goes_forced,
                      max_goes,
                      skip_setup = FALSE,
                      app_name) {

  psychTestR::join(
    musicassessr::musicassessr_init(test = "SAA",
                                    test_username = test_username,
                                    store_results_in_db,
                                    app_name),

    # introduction page
    psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$h2(paste0(psychTestR::i18n("SAA_welcome"), ' ', test_name, "!")),
                                                       shiny::tags$img(src = 'https://adaptiveeartraining.com/assets/img/SAA_intro.png', height = 100, width = 100),
                                                       shiny::tags$p(psychTestR::i18n("SAA_welcome_1")),
                                                       shiny::tags$p(psychTestR::i18n("SAA_welcome_2"))),
                                button_text = psychTestR::i18n("Next")),

    musicassessr::setup_pages(input = "microphone",
                              demo = demo,
                              get_instrument_range = get_range,
                              SNR_test = SNR_test,
                              absolute_url = absolute_url,
                              adjust_range = adjust_range,
                              get_user_info = get_user_info,
                              headphones = headphones_test,
                              microphone_test = microphone_test,
                              allow_repeat_SNR_tests = allow_repeat_SNR_tests,
                              concise_wording = concise_wording,
                              skip_setup = skip_setup),
    # instructions
    if(!skip_setup) SAA_instructions(max_goes_forced, max_goes)
  )

}

SAA_instructions <- function(max_goes_forced, max_goes) {


  if(max_goes_forced) {
    SAA_instructions_5.1 <- "SAA_instructions_5.1.forced"
  } else {
    SAA_instructions_5.1 <- "SAA_instructions_5.1"
  }

  if(max_goes_forced > 1) {
    SAA_instructions_5.2 <- "SAA_instructions_5.2.multiple"
  } else {
    SAA_instructions_5.2 <- "SAA_instructions_5.2.singular"
  }

  c(

  psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("SAA_instructions1")),
                                                     shiny::tags$p(psychTestR::i18n("SAA_instructions2")),
                                                     shiny::tags$p(psychTestR::i18n("SAA_instructions3")),
                                                     shiny::tags$p(psychTestR::i18n("SAA_instructions4"))),
                              button_text = psychTestR::i18n("Next")),

  psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$h2("Instructions"),
                                                     shiny::tags$p(paste0(psychTestR::i18n(SAA_instructions_5.1), " "),
                                                                   shiny::tags$strong(max_goes),
                                                                   paste0(" ", psychTestR::i18n(SAA_instructions_5.2))),
                                                     if(!max_goes_forced & max_goes > 1) shiny::tags$p(psychTestR::i18n("SAA_instructions_5.3"))
                                                     ),
                              button_text = psychTestR::i18n("Next"))

  )
}




present_scores_saa <- function(res, num_items_long_tone, num_items_arrhythmic, num_items_rhythmic) {

  if(num_items_long_tone > 0) {
    # long tones
    long_tones <- as.data.frame(lapply(res$SAA.long_note_trials$long_tone_, paste0, collapse = ","))

    long_tone_summary <- long_tones %>%
      dplyr::select(note_accuracy, note_precision, dtw_distance) %>%
      dplyr::mutate_if(is.character,as.numeric) %>%
      dplyr::summarise(mean_note_accuracy = mean(note_accuracy, na.rm = TRUE),
                       note_precision = mean(note_precision, na.rm = TRUE),
                       mean_dtw_distance = mean(note_precision, na.rm = TRUE))
  }

  if(num_items_arrhythmic > 0) {

    # arrhythmic
    arrhythmic_melodies <- musicassessr::tidy_melodies(res$SAA.arrhythmic_melodies)

    if(is.null(arrhythmic_melodies$error)) {

      if(all(arrhythmic_melodies$error)) {
        arrhythmic_melody_summary <- data.frame(opti3 = 0)
      } else {
        arrhythmic_melody_summary <- arrhythmic_melodies %>% dplyr::select(opti3) %>%
          dplyr::mutate_if(is.character,as.numeric) %>% # previously this was using multiple vars
          dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))
      }
    } else {
      arrhythmic_melody_summary <- data.frame(opti3 = 0)
    }

  }

  if(num_items_rhythmic > 0) {
    # rhythmic
    rhythmic_melodies <- musicassessr::tidy_melodies(res$SAA.rhythmic_melodies)

    if(is.null(rhythmic_melodies$error)) {

      if(all(rhythmic_melodies$error)) {
        rhythmic_melody_summary <- data.frame(opti3 = 0)
      } else {
        rhythmic_melody_summary <- rhythmic_melodies %>% dplyr::select(opti3) %>%
          dplyr::mutate_if(is.character,as.numeric) %>% # previously this was using multiple vars
          dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))
      }
    } else {
      rhythmic_melody_summary <- data.frame(opti3 = 0)
    }
  }

  list("long_note" = ifelse(is.null(long_tone_summary), data.frame(mean_note_accuracy = 1, note_precision = 1, mean_dtw_distance = 1), long_tone_summary),
       "arrhythmic" = ifelse(is.null(arrhythmic_melody_summary), data.frame(opti3 = 0), arrhythmic_melody_summary),
       "rhythmic" = ifelse(is.null(rhythmic_melody_summary), data.frame(opti3 = 0), rhythmic_melody_summary))

}






final_results_saa <- function(test_name,
                          url,
                          num_items_long_tone,
                          num_items_arrhythmic,
                          num_items_rhythmic,
                          socials = FALSE,
                          hashtag = " ") {
  c(
    psychTestR::reactive_page(function(state, ...) {

      res <- as.list(psychTestR::get_results(state, complete = FALSE))

      processed_results <- present_scores_saa(res, num_items_long_tone, num_items_arrhythmic, num_items_rhythmic)

      final_score <- 1 + processed_results$arrhythmic[[1]] + processed_results$rhythmic[[1]] * 1000

      psychTestR::set_local("final_score", final_score, state) # leave this in; it gets used by musicassessr


      psychTestR::text_input_page(
        label = "final_score",
        prompt = shiny::tags$div(style = "width: 500px;",
                                 shiny::tags$h2('Final Results'),
                                 shiny::tags$h3('Long Note Scores'),

                                 shiny::renderTable({

                                   long_note_df <- processed_results$long_note[[1]]
                                   long_note_df_names <- names(long_note_df)
                                   long_note_df <- base::t(long_note_df)
                                   row.names(long_note_df) <- long_note_df_names
                                   long_note_df
                                 }, rownames = TRUE, colnames = FALSE, width = "50%"),

                                 shiny::tags$h3('Arrhythmic Melody Scores'),

                                 shiny::renderTable({

                                   arrhythmic_df <- processed_results$arrhythmic
                                   arrhythmic_df_names <- names(arrhythmic_df)
                                   arrhythmic_df <- base::t(arrhythmic_df)
                                   row.names(arrhythmic_df) <- arrhythmic_df_names
                                   arrhythmic_df
                                 }, rownames = TRUE, colnames = FALSE, width = "50%"),

                                 shiny::tags$h3('Rhythmic Melody Scores'),

                                 shiny::renderTable({

                                   rhythmic_df <- processed_results$rhythmic
                                   rhythmic_df_names <- names(rhythmic_df)
                                   rhythmic_df <- base::t(rhythmic_df)
                                   row.names(rhythmic_df) <- rhythmic_df_names
                                   rhythmic_df
                                 }, rownames = TRUE, colnames = FALSE, width = "50%"),

                                 shiny::tags$h3('Total Score'),
                                 shiny::tags$p(final_score),
                                 shiny::tags$p("Enter a username to see the scoreboard: ")

        )
      )

    }),

    musicassessr::share_score_page(test_name, url, hashtag, socials, leaderboard_name = 'SAA_leaderboard.rda')
  )
}



.onLoad <- function(...) {
  shiny::addResourcePath(
    prefix = "custom-assets", # custom prefix that will be used to reference your directory
    directoryPath = system.file("www", package = "SAA") # path to resource in your package
  )
  # shiny::addResourcePath(
  #   prefix = "item_banks", # custom prefix that will be used to reference your directory
  #   directoryPath = system.file("item_banks", package = "itembankr") # path to resource in your package
  # )
}

#
# SAA_standalone(num_items = list(long_tones = 1L, arrhythmic = 2L, rhythmic = 2L),
#                SNR_test = F, get_range = F,  musicassessr_aws = FALSE, examples = 0,
#                copy_audio_to_location = "/Users/sebsilas/Desktop/audio_test")


# musicassessr::musicassessr_js(musicassessr_aws = FALSE, copy_audio_to_location = "/Users/sebsilas/Desktop/audio_test")




# SAA_standalone(get_range = F, SNR_test = F,
#                num_items = list("long_tones" = 0L,
#                                 "arrhythmic" = 10L,
#                                 "rhythmic" = 10L))

