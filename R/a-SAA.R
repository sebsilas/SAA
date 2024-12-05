

#' Deploy adaptive SAA (aSAA) as standalone test
#'
#' @param app_name Name of app.
#' @param num_items The number of items as a list.
#' @param item_bank The (IRT) item bank that will be used for selecting items.
#' @param irt_model The IRT model that will be used for selecting items.
#' @param demographics Deploy demographic form?
#' @param demo Is demo?
#' @param feedback Give feedback after trials?
#' @param admin_password psychTestR admin password.
#' @param SNR_test Deploy signal-to-noise ratio test?
#' @param get_range Deploy a test to get the users range at test time and present stimuli accordingly?
#' @param absolute_url If using online, absolute URL?
#' @param examples No of examples.
#' @param final_results Display final results?
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
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
aSAA_standalone <- function(app_name,
                            num_items = list("long_tones" = 6L,
                                            "arrhythmic" = 15L,
                                            "rhythmic" = 10L),
                           item_bank = Berkowitz::Berkowitz_IRT_arrhythmic_scaled,
                           irt_model = Berkowitz::lm2.2_scaled,
                           demographics = TRUE,
                           demo = FALSE,
                           feedback = FALSE,
                           admin_password = "demo",
                           SNR_test = TRUE,
                           get_range = TRUE,
                           absolute_url = "",
                           examples = 2L,
                           final_results = TRUE,
                           test_username = "",
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
                           allow_repeat_SNR_tests = TRUE,
                           append_trial_block_before = psychTestR::module("before"),
                           append_trial_block_after = psychTestR::module("after"),
                           stop_recording_after = 30,
                           max_goes = 3L,
                           max_goes_forced = FALSE,
                           long_tone_trials_as_screening = FALSE,
                           long_tone_trials_as_screening_failure_page = "http://www.google.com",
                           success_on_completion_page = "",
                           concise_wording = TRUE,
                           skip_setup = FALSE,
                           ...) {

  timeline <- aSAA(app_name,
                   num_items,
                   item_bank,
                   irt_model,
                   demographics,
                   demo,
                   feedback,
                   admin_password,
                   SNR_test,
                   get_range,
                    absolute_url,
                    examples,
                    final_results,
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
                    skip_setup)


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
                                   languages = c("en", "de", "it"),
                                   additional_scripts = musicassessr::musicassessr_js(app_name = app_name), ...))
}







#' Deploy the adaptive SAA (aSAA)
#'
#' @param app_name
#' @param num_items
#' @param item_bank The (IRT) item bank that will be used for selecting items.
#' @param irt_model The IRT model that will be used for selecting items.
#' @param demographics
#' @param demo
#' @param feedback
#' @param admin_password
#' @param SNR_test
#' @param get_range
#' @param absolute_url
#' @param examples
#' @param final_results
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
#'
#' @return
#' @export
#'
#' @examples
aSAA <- function(app_name,
                 num_items = list("long_tones" = 6L,
                                 "arrhythmic" = 10L,
                                 "rhythmic" = 10L),
                 item_bank = Berkowitz::Berkowitz_IRT_arrhythmic_scaled,
                 irt_model = Berkowitz::lm2.2_scaled,
                demographics = TRUE,
                demo = FALSE,
                feedback = FALSE,
                admin_password = "demo",
                SNR_test = TRUE,
                get_range = TRUE,
                absolute_url = "",
                examples = 2,
                final_results = TRUE,
                test_username = "",
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
                allow_repeat_SNR_tests = TRUE,
                append_trial_block_before = psychTestR::module("before"),
                append_trial_block_after = psychTestR::module("after"),
                stop_recording_after = 30,
                max_goes = 3L,
                max_goes_forced = FALSE,
                long_tone_trials_as_screening = FALSE,
                long_tone_trials_as_screening_failure_page = "http://www.google.com",
                success_on_completion_page = "",
                concise_wording = TRUE,
                skip_setup = FALSE) {

  stopifnot(
    assertthat::is.string(app_name),
    is.list(num_items),
    is.scalar.logical(demographics),
    is.scalar.logical(demo),
    is.scalar.logical(feedback),
    is.scalar.character(admin_password),
    is.scalar.logical(SNR_test),
    is.scalar.logical(get_range) | is.scalar.character(get_range),
    is.scalar.character(absolute_url),
    is.scalar.numeric(examples),
    is.scalar.logical(final_results),
    is.scalar.character(test_username),
    is.scalar.logical(gold_msi),
    is.scalar.logical(with_final_page),
    is.numeric(item_length) & length(item_length) <= 2,
    is.scalar.character(melody_sound),
    is.scalar.logical(adjust_range),
    is.scalar.character(test_name),
    is.scalar.logical(show_socials),
    is.scalar.logical(headphones_test),
    is.scalar.logical(get_user_info),
    is.scalar.logical(microphone_test),
    is.scalar.logical(allow_repeat_SNR_tests),
    is.list(append_trial_block_before) | psychTestR::is.timeline(append_trial_block_before),
    is.list(append_trial_block_after) | psychTestR::is.timeline(append_trial_block_after),
    is.scalar.numeric(stop_recording_after),
    is.scalar.numeric(max_goes),
    is.scalar.logical(max_goes_forced),
    is.scalar.logical(long_tone_trials_as_screening),
    is.scalar.character(long_tone_trials_as_screening_failure_page),
    is.scalar.character(success_on_completion_page),
    is.scalar.logical(concise_wording),
    is.scalar.logical(skip_setup)
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

                           if(demo) musicassessr::set_instrument_range(),

                           # arbitrary and optional trial block to go first
                           append_trial_block_before,
#
#
#                            # long tone trials
#                            musicassessr::long_tone_trials(num_items$long_tones, num_examples = examples, feedback = feedback,
#                                                           long_tone_trials_as_screening = long_tone_trials_as_screening,
#                                                           long_tone_trials_as_screening_failure_page = long_tone_trials_as_screening_failure_page),


                          musicassessr::adaptive_arrhythmic_melody_trials(label = "aSAA_arrhythmic",
                                                                          num_items = num_items$arrhythmic,
                                                                          item_bank = Berkowitz::Berkowitz_IRT_arrhythmic_scaled,
                                                                          model = Berkowitz::lm2.2_scaled,
                              fixed_effects = c("N",
                                                 "step.cont.loc.var",
                                                 "tonalness",
                                                 "log_freq"),
                              demo = TRUE#, feedback = feedback,play_melody_loop = if(max_goes > 1L) TRUE else FALSE
                              )


#
#                            adaptive_arrhythmic_melody_trials("aSAA_arrhythmic",
#                                                             num_items$arrhythmic,
#                                                              Berk_arr_length_only,
#                                                              Berkowitz::lm_arrhythmic_length_only,
#                                                              fixed_effects = "N"),



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
      dict = musicassessr::musicassessr_dict
    ),
    if(gold_msi) psyquest::GMS(subscales = c("Musical Training", "Singing Abilities")),
    musicassessr::deploy_demographics(demographics),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    musicassessr::final_page_or_continue_to_new_test(final = with_final_page, task_name = test_name)
  )

}





