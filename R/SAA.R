

#' Deploy SAA as standalone test
#'
#' @param app_name Name of app.
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
#' @param melody_length What melody lengths should the test be constrained to?
#' @param melody_sound Sound of melody? e.g, piano.
#' @param adjust_range Should the range of the user, recorded at test time, be adjusted based on heuristics?
#' @param test_name Custom name of the test.
#' @param show_socials Should social media sharing options be shown at the end?
#' @param headphones_test Should there be a headphone test?
#' @param get_user_info Grab user info via the browser?
#' @param microphone_test Deploy a microphone test?
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
#' @param full_screen Should app be full screen?
#' @param validate_user_entry_into_test Should the user be validated against a session token?
#' @param additional_scoring_measures A function or list of functions with additional measures for scoring pYIN data.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
SAA_standalone <- function(app_name,
                           num_items = list("long_tones" = 6L,
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
                           melody_length = c(3,15),
                           melody_sound = "piano",
                           adjust_range = TRUE,
                           test_name = "Singing Ability Assessment",
                           show_socials = FALSE,
                           headphones_test = TRUE,
                           get_user_info = TRUE,
                           microphone_test = TRUE,
                           allow_repeat_SNR_tests = TRUE,
                           append_trial_block_before = psychTestR::module("before"),
                           append_trial_block_after = psychTestR::module("after"),
                           stop_recording_after = 30,
                           max_goes = 3L,
                           max_goes_forced = FALSE,
                           long_tone_trials_as_screening = FALSE,
                           long_tone_trials_as_screening_failure_page = "http://www.google.com",
                           success_on_completion_page = "https://adaptiveeartraining.com",
                           concise_wording = TRUE,
                           skip_setup = FALSE,
                           full_screen = FALSE,
                           validate_user_entry_into_test = FALSE,
                           additional_scoring_measures = NULL, ...) {

  timeline <- SAA(app_name,
                  num_items,
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
                  melody_length,
                  melody_sound,
                  adjust_range,
                  test_name,
                  show_socials,
                  headphones_test,
                  get_user_info,
                  microphone_test,
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
                  additional_scoring_measures)


  # run the test
  timeline %>%
    musicassessr::validate_user_entry_into_test(validate_user_entry_into_test, .) %>%
  psychTestR::make_test(
    opt = psychTestR::test_options(title = test_name,
                                   admin_password = admin_password,
                                   display = psychTestR::display_options(
                                     full_screen = full_screen,
                                     left_margin = 1L,
                                     right_margin = 1L,
                                     css = system.file('www/css/musicassessr.css', package = "musicassessr")
                                   ),
                                   languages = c("en"),
                                   additional_scripts = musicassessr::musicassessr_js(musicassessr_aws = musicassessr_aws,
                                                                                      app_name = app_name,
                                                                                      visual_notation = feedback), ...))
}







#' Deploy the SAA
#'
#' @param app_name Name of app.
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
#' @param melody_length What melody lengths should the test be constrained to?
#' @param melody_sound Sound of melody? e.g, piano.
#' @param adjust_range Should the range of the user, recorded at test time, be adjusted based on heuristics?
#' @param test_name Custom name of the test.
#' @param show_socials Should social media sharing options be shown at the end?
#' @param headphones_test Should there be a headphone test?
#' @param get_user_info Grab user info via the browser?
#' @param microphone_test Deploy a microphone test?
#' @param allow_repeat_SNR_tests Logical. TRUE if participant can fail the SNR test threshold and try again.
#' @param append_trial_block_before A list of pages to go before the test.
#' @param append_trial_block_after A list of pages to go after the test.
#' @param stop_recording_after Stop recording after a certain amount of time.
#' @param max_goes How many goes can the user have per melody?
#' @param max_goes_forced Is this forced or optional?
#' @param long_tone_trials_as_screening
#' @param long_tone_trials_as_screening Should long tone trials be used as a screening mechanism?
#' @param long_tone_trials_as_screening_failure_page Where should users be directed to if they fail the long tone screening?
#' @param success_on_completion_page Where should users be directed to when they complete successfully?
#' @param concise_wording TRUE for more detailed (but longer) instructions.
#' @param skip_setup TRUE to skip setup steps.
#' @param additional_scoring_measures A function or list of functions with additional measures for scoring pYIN data.
#'
#' @return
#' @export
#'
#' @examples
SAA <- function(app_name,
                num_items = list("long_tones" = 6L,
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
                melody_length = c(3,15),
                melody_sound = "piano",
                adjust_range = TRUE,
                test_name = "Singing Ability Assessment",
                show_socials = FALSE,
                headphones_test = TRUE,
                get_user_info = TRUE,
                microphone_test = TRUE,
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
                additional_scoring_measures = NULL) {


  stopifnot(
    assertthat::is.string(app_name),
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
    is.numeric(melody_length) & length(melody_length) <= 2,
    is.character(melody_sound) & length(melody_sound) == 1L,
    is.logical(adjust_range),
    is.character(test_name) & length(test_name) == 1L,
    is.logical(show_socials),
    is.logical(headphones_test),
    is.logical(get_user_info),
    is.logical(microphone_test),
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
    is.null(additional_scoring_measures) | is.function(additional_scoring_measures) | is.list(additional_scoring_measures)
    )

  shiny::addResourcePath(
    prefix = "custom-assets", # custom prefix that will be used to reference your directory
    directoryPath = system.file("www", package = "SAA") # path to resource in your package
  )

  if(long_tone_trials_as_screening) {
    stop("long_tone_trials_as_screening currently cannot be used, as the functionality is subject to reanalysis.")
  }

  if(demo) warning('Running SAA in demo mode!')

  # Instantiate the enclosure/function factory:

  pyin_with_additional <- musicassessr::get_answer_pyin_melodic_production_additional_measures(type = "both", melconv = FALSE, additional_scoring_measures = additional_scoring_measures)


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
                           musicassessr::arrhythmic_melody_trials(itembankr::subset_item_bank(item_bank("main"), item_length = melody_length),
                                                                  num_items = num_items$arrhythmic,
                                                                  num_examples = examples,
                                                                  feedback = feedback,
                                                                  sound = melody_sound,
                                                                  page_text = "Click below to hear the melody. Sing back the melody. Click Stop when finished.",
                                                                  page_title = "Sing The Melody",
                                                                  instruction_text = "Now you will hear some melodies. Please try and sing the melodies.",
                                                                  max_goes = max_goes,
                                                                  max_goes_forced = max_goes_forced,
                                                                  get_answer = pyin_with_additional),

                           # rhythmic
                           musicassessr::rhythmic_melody_trials(item_bank = itembankr::subset_item_bank(item_bank("phrases"), melody_length),
                                                                num_items = num_items$rhythmic,
                                                                num_examples = 0, # because it's effectively the same task as arrhythmic
                                                                feedback = feedback,
                                                                sound = melody_sound,
                                                                page_text = "Click below to hear the melody. Sing back the melody. Click Stop when finished.",
                                                                page_title = "Sing This Melody Plus Rhythm",
                                                                instruction_text = "Now you will hear melodies with rhythms. Please try and sing the melodies with the correct rhythm.",
                                                                max_goes = max_goes,
                                                                max_goes_forced = max_goes_forced,
                                                                get_answer = pyin_with_additional),

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




present_scores_saa <- function(res, num_items_long_note, num_items_arrhythmic, num_items_rhythmic) {

  if(num_items_long_note > 0) {
    # long notes
    long_note_scores <- res$SAA.long_tone_trials %>%
      purrr::map(function(l) {
        if(length(l$failed_tests) > 1) {
          l$failed_tests <- paste0(l$failed_tests, collapse = ",")
          l
        } else {
          l
        }
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::select(long_note_accuracy, long_note_dtw_distance, long_note_autocorrelation_mean,
                    long_note_run_test, long_note_no_cpts, long_note_beginning_of_second_cpt) %>%
      unique()

    long_note_pca_scores <- musicassessr::get_long_note_pcas(long_note_scores)

    } else {

      long_note_scores <- tibble::tibble(
        long_note_accuracy = NA, long_note_dtw_distance = NA, long_note_autocorrelation_mean = NA,
        long_note_run_test = NA, long_note_no_cpts = NA, long_note_beginning_of_second_cpt = NA)

      long_note_pca_scores <- tibble::tibble(pca_long_note_randomness = NA,
                                         pca_long_note_accuracy = NA,
                                         pca_long_note_scoop = NA)
  }

  if(num_items_arrhythmic > 0) {

    # arrhythmic
    arrhythmic_melodies <- musicassessr::tidy_melodies(res$SAA.arrhythmic_melodies, use_for_production = "pyin_pitch_track")

    arrhythmic_melody_tmp <- arrhythmic_melodies %>%
      dplyr::select(answer_meta_data.N, answer_meta_data.step.cont.loc.var, answer_meta_data.tonalness, answer_meta_data.log_freq, opti3, proportion_of_correct_note_events) %>%
      dplyr::mutate_if(is.character,as.numeric) %>%
      unique() %>%
      dplyr::rename_with(~stringr::str_remove(.x, "answer_meta_data.")) %>%
      dplyr::mutate(tmp_scores = opti3) # to match what psychTestRCAT/ME expects

    if(suppressWarnings(musicassessr::is_null_or_not_all_TRUE(arrhythmic_melodies$error))) {

      arrhythmic_melody_model_prediction <- arrhythmic_melody_tmp %>%
        dplyr::select(-proportion_of_correct_note_events) %>%
        psychTestRCATME::predict_based_on_mixed_effects_arrhythmic_model(musicassessr::lm2.2, .)

      arrhythmic_melody_summary <- arrhythmic_melody_tmp %>%
        dplyr::select(opti3, proportion_of_correct_note_events) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), mean, na.rm = TRUE)) %>%
        dplyr::mutate(SAA_Ability_Arrhythmic = arrhythmic_melody_model_prediction)

      arrhythmic_melody_score <- arrhythmic_melody_summary$SAA_Ability_Arrhythmic

    } else {
      arrhythmic_melody_score <- NA
    }
  }

  if(num_items_rhythmic > 0) {
    # rhythmic
    rhythmic_melodies <- musicassessr::tidy_melodies(res$SAA.rhythmic_melodies, use_for_production = "pyin_pitch_track")

    rhythmic_melody_tmp <- rhythmic_melodies %>%
      dplyr::select(answer_meta_data.N, answer_meta_data.step.cont.loc.var, answer_meta_data.log_freq, answer_meta_data.d.entropy, answer_meta_data.i.entropy, opti3, proportion_of_correct_note_events) %>%
      dplyr::mutate_if(is.character,as.numeric) %>%
      unique() %>%
      dplyr::rename_with(~stringr::str_remove(.x, "answer_meta_data.")) %>%
      dplyr::mutate(tmp_scores = opti3) # to match what psychTestRCAT/ME expects

    if(suppressWarnings(musicassessr::is_null_or_not_all_TRUE(rhythmic_melodies$error))) {

      rhythmic_melody_model_prediction <- rhythmic_melody_tmp %>%
        dplyr::select(-proportion_of_correct_note_events) %>%
        psychTestRCATME::predict_based_on_mixed_effects_rhythmic_model(musicassessr::lm3.2, .)

      rhythmic_melody_summary <- rhythmic_melody_tmp %>%
        dplyr::select(opti3, proportion_of_correct_note_events) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), mean, na.rm = TRUE)) %>%
        dplyr::mutate(SAA_Ability_Rhythmic = rhythmic_melody_model_prediction)

      rhythmic_melody_score <- rhythmic_melody_summary$SAA_Ability_Rhythmic

    } else {
      rhythmic_melody_score <- NA
    }
  }


  # Calculate note precision

  shared_names <- intersect(names(arrhythmic_melodies), names(rhythmic_melodies))
  arrhythmic_melodies <- arrhythmic_melodies %>% dplyr::select(shared_names)
  rhythmic_melodies <- rhythmic_melodies %>% dplyr::select(shared_names)
  all_melodies <- rbind(arrhythmic_melodies, rhythmic_melodies)

  melody_precision_vars <- all_melodies %>%
    dplyr::select(pyin_pitch_track.freq, stimuli,
                  pyin_pitch_track.nearest_stimuli_note,
                  pyin_pitch_track.interval, pyin_pitch_track.interval_cents) %>%
    dplyr::rename(freq = pyin_pitch_track.freq,
                  nearest_stimuli_note = pyin_pitch_track.nearest_stimuli_note,
                  interval = pyin_pitch_track.interval,
                  interval_cents = pyin_pitch_track.interval_cents) %>%
    dplyr::mutate(freq = as.numeric(freq),
                  nearest_stimuli_note = as.numeric(nearest_stimuli_note),
                  note = round(hrep::freq_to_midi(freq)),
                  interval = as.numeric(interval),
                  interval_cents = as.numeric(interval_cents))

    melody_note_precision <- melody_precision_vars %>%
      musicassessr::score_melody_note_precision()

    melody_interval_precision <- melody_precision_vars %>%
     musicassessr::score_melody_interval_precision()

    end_melody_summary <- all_melodies %>%
      dplyr::select(file, melody_interval_accuracy, melody_note_accuracy) %>%
      unique() %>%
      dplyr::select(-file) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
      dplyr::summarise(mean_melody_note_accuracy = mean(melody_note_accuracy, na.rm = TRUE),
                       mean_melody_interval_accuracy = mean(melody_interval_accuracy, na.rm = TRUE)
                       )

    pca_melodic_singing_accuracy <- predict(musicassessr::melody_pca2,
            data = tibble::tibble(melody_note_precision = melody_note_precision,
                                  interval_precision = melody_interval_precision,
                                  interval_accuracy = end_melody_summary$mean_melody_interval_accuracy),
            old.data = musicassessr::melody_pca2_data
            # you need to pass this for standardization or you will get NaNs
            # https://stackoverflow.com/questions/27534968/dimension-reduction-using-psychprincipal-does-not-work-for-smaller-data
    ) %>% as.numeric()

  list("Long_Note" = if(is.null(long_note_scores)) tibble::tibble(pca_long_note_randomness = NA, pca_long_note_accuracy = NA, pca_long_note_scoop = NA) else long_note_pca_scores,
       "SAA_Ability_Arrhythmic" = if(is.null(arrhythmic_melody_summary)) NA else arrhythmic_melody_score,
       "SAA_Ability_Rhythmic" = if(is.null(rhythmic_melody_summary)) NA else rhythmic_melody_score,
       "melody_note_precision" = melody_note_precision,
       "melody_interval_precision" = melody_interval_precision,
       "pca_melodic_singing_accuracy" = pca_melodic_singing_accuracy)

}


#
# r <- readRDS("/Users/sebsilas/SAA/test_apps/example/output/results/id=2&p_id=364814f4daf3585347c3b169c29e29ae128e4da159870984351b5b0948c862b7&save_id=6&pilot=false&complete=true.rds")
# t <- present_scores_saa(r, 2, 2, 2)






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


      # Convert scores to percentiles
      long_note_percentile <- get_long_note_score_percentile(.33 * processed_results$Long_Note$pca_long_note_randomness + .33 * processed_results$Long_Note$pca_long_note_scoop + .33 * processed_results$Long_Note$pca_long_note_accuracy)
      arrhythmic_percentile <- get_arrhythmic_score_percentile(processed_results$SAA_Ability_Arrhythmic)
      rhythmic_percentile <- get_rhythmic_score_percentile(processed_results$SAA_Ability_Rhythmic)

      # Calculate the score
      Final_SAA_Score <- weight_final_SAA_score(num_items_long_tone, num_items_arrhythmic, num_items_rhythmic,
                                                long_note_percentile, arrhythmic_percentile, rhythmic_percentile)

      psychTestR::set_local("final_score", Final_SAA_Score, state) # leave this in; it gets used by musicassessr


      psychTestR::text_input_page(
        label = "final_score",
        prompt = shiny::tags$div(style = "width: 500px;",
                                 shiny::tags$h2('Final Results'),
                                 shiny::tags$h3('Long Note Scores'),

                                 shiny::renderTable({

                                   long_note_df <- tibble::tibble(`Long Note` = round(get_long_note_score_percentile(.33 * processed_results$Long_Note$pca_long_note_randomness + .33 * processed_results$Long_Note$pca_long_note_scoop + .33 * processed_results$Long_Note$pca_long_note_accuracy), 2))
                                   long_note_df_names <- names(long_note_df)
                                   long_note_df <- base::t(long_note_df)
                                   row.names(long_note_df) <- long_note_df_names
                                   long_note_df
                                 }, rownames = TRUE, colnames = FALSE, width = "50%"),

                                 shiny::tags$h3('Arrhythmic Melody Scores'),

                                 shiny::renderTable({

                                   arrhythmic_df <- tibble::tibble(`Arrhythmic Melodies` = round(get_arrhythmic_score_percentile(processed_results$SAA_Ability_Arrhythmic), 2))
                                   arrhythmic_df_names <- names(arrhythmic_df)
                                   arrhythmic_df <- base::t(arrhythmic_df)
                                   row.names(arrhythmic_df) <- arrhythmic_df_names
                                   arrhythmic_df
                                 }, rownames = TRUE, colnames = FALSE, width = "50%"),

                                 shiny::tags$h3('Rhythmic Melody Scores'),

                                 shiny::renderTable({

                                   rhythmic_df <- tibble::tibble(`Rhythmic Melodies` = round(get_rhythmic_score_percentile(processed_results$SAA_Ability_Rhythmic), 2))
                                   rhythmic_df_names <- names(rhythmic_df)
                                   rhythmic_df <- base::t(rhythmic_df)
                                   row.names(rhythmic_df) <- rhythmic_df_names
                                   rhythmic_df
                                 }, rownames = TRUE, colnames = FALSE, width = "50%"),

                                 shiny::tags$h3('Total Score'),
                                 shiny::tags$p(Final_SAA_Score),
                                 shiny::tags$p("Enter a username to see the scoreboard: ")

        )
      )

    }),

    musicassessr::share_score_page(test_name,
                                   url,
                                   hashtag,
                                   socials,
                                   leaderboard_name = 'SAA_leaderboard.rda',
                                   distribution_mean = Final_SAA_Score_m,
                                   distribution_sd = Final_SAA_Score_sd)
  )
}


weight_final_SAA_score <- function(num_items_long_tone, num_items_arrhythmic, num_items_rhythmic,
                                   long_note_percentile, arrhythmic_percentile, rhythmic_percentile) {

  # If NA, change to 0th percentile
  long_note_percentile <- NA_to_0(long_note_percentile)
  arrhythmic_percentile <- NA_to_0(arrhythmic_percentile)
  rhythmic_percentile <- NA_to_0(rhythmic_percentile)

  # But make sure participants aren't unnecessarily penalised, by only using the scores that were actually included.

  if(num_items_long_tone > 0 & num_items_arrhythmic > 0 & num_items_rhythmic > 0) {
    Final_SAA_Score <- .33*arrhythmic_percentile + .33*rhythmic_percentile + .33*long_note_percentile
  } else if(num_items_long_tone == 0 & num_items_arrhythmic > 0 & num_items_rhythmic > 0) {
    Final_SAA_Score <- .50*arrhythmic_percentile + .50*rhythmic_percentile
  } else if(num_items_long_tone > 0 & num_items_arrhythmic == 0 & num_items_rhythmic > 0) {
    Final_SAA_Score <- .50*long_note_percentile + .50*rhythmic_percentile
  } else if(num_items_long_tone > 0 & num_items_arrhythmic > 0 & num_items_rhythmic == 0) {
    Final_SAA_Score <- .50*long_note_percentile + .50*arrhythmic_percentile
  } else if(num_items_long_tone > 0 & num_items_arrhythmic == 0 & num_items_rhythmic == 0) {
    Final_SAA_Score <- long_note_percentile
  } else if(num_items_long_tone == 0 & num_items_arrhythmic > 0 & num_items_rhythmic == 0) {
    Final_SAA_Score <- arrhythmic_percentile
  } else if(num_items_long_tone == 0 & num_items_arrhythmic == 0 & num_items_rhythmic > 0) {
    Final_SAA_Score <- rhythmic_percentile
  } else {
    stop('Unrecognised score structure.')
  }



  round(Final_SAA_Score, 2)
}

NA_to_0 <- function(val) {
  if(is.na(val)) 0 else val
}


FALSE_to_0 <- function(val) {
  if(val == FALSE) 0 else val
}

# SAA_standalone(get_range = FALSE, SNR_test = FALSE,
#                num_items = list("long_tones" = 0L,
#                                 "arrhythmic" = 10L,
#                                 "rhythmic" = 10L))

