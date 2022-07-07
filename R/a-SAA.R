

#' Deploy aSAA as standalone test
#'
#' @param num_items The number of items as a list.
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
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
aSAA_standalone <- function(num_items = list("long_tones" = 6L,
                                            "arrhythmic" = 15L,
                                            "rhythmic" = 10L),
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
                           concise_wording = TRUE, ...) {

  timeline <- aSAA(num_items,
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
                  concise_wording)


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
                                   additional_scripts = musicassessr::musicassessr_js(musicassessr_aws = musicassessr_aws, copy_audio_to_location = copy_audio_to_location), ...))
}







#' Deploy the aSAA
#'
#' @param num_items
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
#'
#' @return
#' @export
#'
#' @examples
aSAA <- function(num_items = list("long_tones" = 6L,
                                 "arrhythmic" = 10L,
                                 "rhythmic" = 10L),
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
                concise_wording = TRUE) {

  stopifnot(
    is.list(num_items),
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
    is.logical(concise_wording)
    )

  if(demo) warning('Running SAA in demo mode!')


  timeline <- psychTestR::join(
    psychTestR::new_timeline(
      psychTestR::join(

        psychTestR::module("aSAA",
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
                                     max_goes),

                           # arbitrary and optional trial block to go first
                           append_trial_block_before,
#
#
#                            # long tone trials
#                            musicassessr::long_tone_trials(num_items$long_tones, num_examples = examples, feedback = feedback,
#                                                           long_tone_trials_as_screening = long_tone_trials_as_screening,
#                                                           long_tone_trials_as_screening_failure_page = long_tone_trials_as_screening_failure_page),


                          adaptive_arrhythmic_melody_trials(num_items$arrhythmic,
                                                            Berk_arr_scaled,
                                                            Berkowitz::lm2.2_scaled,
                              fixed_effects = c("N",
                                                 "step.cont.loc.var",
                                                 "tonalness",
                                                 "log_freq"))

                           # adaptive_arrhythmic_melody_trials(num_items$arrhythmic,
                           #                                   Berk_arr_length_only,
                           #                                   Berkowitz::lm_arrhythmic_length_only,
                           #                                   fixed_effects = "N"),



                           # arbitrary and optional trial block to go after
                           # append_trial_block_after,
                           #
                           #
                           # psychTestR::elt_save_results_to_disk(complete = TRUE),
                           #
                           # if(final_results) final_results_saa(test_name = test_name,
                           #                                     url = absolute_url,
                           #                                     num_items$long_tones,
                           #                                     num_items$arrhythmic,
                           #                                     num_items$rhythmic,
                           #                                     show_socials)

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

show_item_arrhythmic <- function(item, state, ...) {

  print('show_item_arrhythmic')

  item_number <- psychTestRCATME::get_item_number(item)

  bottom_range <- psychTestR::get_global("bottom_range", state)
  top_range <- psychTestR::get_global("top_range", state)

  melody <- item %>%
    dplyr::pull(answer) %>%
    itembankr::str_mel_to_vector() %>%
    musicassessr:: rel_to_abs_mel_mean_centred(bottom_range, top_range)

  present_stimuli(stimuli = melody,
                  stimuli_type = "midi_notes",
                  display_modality = "auditory",
                  page_title = "Sing back the melody",
                  page_text = "Sing back the melody",
                  page_type = "record_audio_page",
                  get_answer = musicassessr::get_answer_pyin_melodic_production)

}




Berk_arr <- Berkowitz::Berkowitz_IRT_arrhythmic %>%
  dplyr::rename(answer = melody) %>%
  dplyr::mutate(discrimination = 1, guessing = 1, inattention = 1)

Berk_arr_scaled <- Berkowitz::Berkowitz_IRT_arrhythmic_scaled %>%
  dplyr::rename(answer = melody) %>%
  dplyr::mutate(discrimination = 1, guessing = 1, inattention = 1)

Berk_arr_length_only <- Berkowitz::Berkowitz_IRT_arrhythmic_length_only %>%
  dplyr::rename(answer = melody) %>%
  dplyr::mutate(discrimination = 1, guessing = 1, inattention = 1)




adaptive_arrhythmic_melody_trials <- function(num_items, item_bank, model, fixed_effects) {
  psychTestRCATME::adapt_test(label = "aSAA_arrhythmic",
                            item_bank = item_bank,
                            show_item = show_item_arrhythmic,
                            stopping_rule = psychTestRCATME::stopping_rule.num_items(n = num_items),
                            opt = psychTestRCATME::adapt_test_options(
                                next_item.criterion = "bOpt",
                                next_item.estimator = "BM",
                                final_ability.estimator = "WL",
                            mixed_effects_model = model,
                            eligible_first_items = which(dplyr::between(Berk_arr_scaled$difficulty,-.01, .01)
                                                         & dplyr::between(Berk_arr_scaled$N, -1.5, -0.5)),
                            continuous_response = TRUE,
                            dv_name = "opti3",
                            fixed_effects = fixed_effects,
                            demo = TRUE))
}




# aSAA_standalone(copy_audio_to_location = '/Users/sebsilas/Desktop/audio_test',
#                 SNR_test = F,
#                 get_range = F, final_results = FALSE, demographics = F, gold_msi = F)

