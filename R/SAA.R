

#' Deploy SAA as standalone test
#'
#' @param app_name Name of app.
#' @param num_items The number of items as a list.
#' @param num_examples The number of example items as a list.
#' @param arrhythmic_item_bank The item bank (created with itembankr) to deployed with the test.
#' @param rhythmic_item_bank The item bank (created with itembankr) to deployed with the test.
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
#' @param default_range A list of the range that stimuli should be presented in, if not collected at test time.
#' @param long_tone_paradigm Can be simultaneous_recall or call_and_response.
#' @param get_p_id if TRUE, get the participant to enter their ID at the beginning of the test.
#' @param languages The available languages.
#' @param volume_meter_on_melody_trials Should there be a volume meter displayed on melody trial pages?
#' @param volume_meter_on_melody_trials_type If so, what type? Can be 'default' or 'playful'.
#' @param long_tone_length How long should the long tone be in seconds? Default: 5
#' @param allow_SNR_failure If TRUE, allow a participant/researcher to proceed, even if the SNR test is failed.
#' @param requirements_page Show a requirements page?
#' @param report_SNR Report SNR after test?
#' @param show_introduction Should introduction be shown (or skipped)?
#' @param show_instructions Should instructions be shown (or skipped)?
#' @param asynchronous_api_mode Should asynchronous API mode br shown?
#' @param experiment_id Manually give an experiment ID when using musicassessrdb.
#' @param user_id Manually give a user ID when using musicassessrdb.
#' @param get_answer_melodic The get_answer function for melodic trials.
#' @param content_border The psychTestR border style.
#' @param css Path to css stylesheet.
#' @param sample_item_bank_via_api Is the item bank being sampled via an API?
#' @param pass_items_through_url_parameter Are items being passed through a URL parameter?
#' @param show_intro_text Should intro text be shown?
#' @param show_microphone_type_page Should you ask the participant what kind of microphone they are using?
#' @param num_items_review Number of review items.
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
                           num_examples = list("long_tones" = 2L,
                                               "arrhythmic" = 2L,
                                               "rhythmic" = 0L),
                           arrhythmic_item_bank = SAA::Berkowitz_item_bank_subset, # N.B. this has a log_freq column, which is required. No other itembankr columns do
                           rhythmic_item_bank = SAA::Berkowitz_item_bank_subset,
                           demographics = TRUE,
                           demo = FALSE,
                           feedback = FALSE,
                           admin_password = "demo",
                           SNR_test = TRUE,
                           get_range = TRUE,
                           absolute_url = character(),
                           examples = 2L,
                           final_results = TRUE,
                           test_username = character(),
                           gold_msi = TRUE,
                           with_final_page = TRUE,
                           melody_length = c(4,20),
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
                           success_on_completion_page = NULL,
                           concise_wording = TRUE,
                           skip_setup = FALSE,
                           full_screen = FALSE,
                           validate_user_entry_into_test = FALSE,
                           additional_scoring_measures = NULL,
                           default_range = NULL,
                           long_tone_paradigm = c("simultaneous_recall", "call_and_response"),
                           get_p_id = FALSE,
                           languages = c("en", "de", "it", "lv"),
                           volume_meter_on_melody_trials = FALSE,
                           volume_meter_on_melody_trials_type = 'default',
                           long_tone_length = 5,
                           allow_SNR_failure = FALSE,
                           requirements_page = TRUE,
                           report_SNR = FALSE,
                           show_introduction = TRUE,
                           show_instructions = TRUE,
                           asynchronous_api_mode = FALSE,
                           experiment_id = NULL,
                           user_id = NULL,
                           get_answer_melodic = musicassessr::get_answer_pyin_melodic_production,
                           content_border = "1px solid #e8e8e8",
                           css = system.file('www/css/musicassessr.css', package = "musicassessr"),
                           sample_item_bank_via_api = FALSE,
                           pass_items_through_url_parameter = FALSE,
                           show_intro_text = TRUE,
                           show_microphone_type_page = TRUE,
                           num_items_review = list("long_tones" = 0L,
                                                   "arrhythmic" = 0L,
                                                   "rhythmic" = 0L), ...) {


  timeline <- psychTestR::join(

        psychTestR::code_block(function(state, ...) {
          psychTestR::set_global("standalone", TRUE, state)
        }),

        SAA(app_name,
                  num_items,
                  num_examples,
                  arrhythmic_item_bank,
                  rhythmic_item_bank,
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
                  additional_scoring_measures,
                  default_range,
                  long_tone_paradigm,
                  get_p_id,
                  volume_meter_on_melody_trials,
                  volume_meter_on_melody_trials_type,
                  long_tone_length,
                  allow_SNR_failure,
                  requirements_page,
                  report_SNR,
                  show_introduction,
                  show_instructions,
                  asynchronous_api_mode,
                  experiment_id,
                  user_id,
                  get_answer_melodic,
                  sample_item_bank_via_api,
                  pass_items_through_url_parameter,
                  show_intro_text,
                  show_microphone_type_page,
                  num_items_review)
      )

  # Run the test
  timeline %>%
    musicassessrdb::validate_user_entry_into_test(validate_user_entry_into_test, .) %>%
  psychTestR::make_test(
    opt = psychTestR::test_options(title = test_name,
                                   admin_password = admin_password,
                                   display = psychTestR::display_options(
                                     full_screen = full_screen,
                                     left_margin = 1L,
                                     right_margin = 1L,
                                     css = css,
                                     content_border = content_border
                                   ),
                                   languages = languages,
                                   on_session_ended_fun = musicassessr::end_session(asynchronous_api_mode),
                                   additional_scripts = musicassessr::musicassessr_js(app_name = app_name,
                                                                                      visual_notation = feedback), ...))
}







#' Deploy the SAA
#'
#' @param app_name Name of app.
#' @param num_items The number of items as a list.
#' @param num_examples The number of example items as a list.
#' @param arrhythmic_item_bank The item bank (created with itembankr) to deployed with the test.
#' @param rhythmic_item_bank The item bank (created with itembankr) to deployed with the test.
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
#' @param default_range A list of the range that stimuli should be presented in, if not collected at test time.
#' @param long_tone_paradigm Can be simultaneous_recall or call_and_response.
#' @param get_p_id if TRUE, get the participant to enter their ID at the beginning of the test.
#' @param volume_meter_on_melody_trials Should there be a volume meter displayed on melody trial pages?
#' @param volume_meter_on_melody_trials_type = If so, what type? Can be 'default' or 'playful'.
#' @param long_tone_length How long should the long tone be in seconds? Default: 5
#' @param allow_SNR_failure If TRUE, allow a participant/researcher to proceed, even if the SNR test is failed.
#' @param requirements_page Show a requirements page on the setup?
#' @param report_SNR Report SNR after test?
#' @param show_introduction Should introduction be shown (or skipped)?
#' @param show_instructions Should instructions be shown (or skipped)?
#' @param asynchronous_api_mode Should asynchronous API mode be used when using musicassessrdb?
#' @param experiment_id The experiment ID, if using asynchronous_api_mode and applicable.
#' @param user_id The user's ID, if using asynchronous_api_mode and applicable.
#' @param get_answer_melodic The get_answer function for melodic files.
#' @param sample_item_bank_via_api Is the item bank being sampled via an API?
#' @param pass_items_through_url_parameter Are items being passed through a URL parameter?
#' @param show_intro_text Should intro text be shown?
#' @param show_microphone_type_page Should you ask the participant what kind of microphone they are using?
#' @param num_items_review Number of review items.
#' @return
#' @export
#'
#' @examples
SAA <- function(app_name,
                num_items = list("long_tones" = 6L,
                                 "arrhythmic" = 10L,
                                 "rhythmic" = 10L),
                num_examples = list("long_tones" = 2L,
                                    "arrhythmic" = 2L,
                                    "rhythmic" = 0L),
                arrhythmic_item_bank = SAA::Berkowitz_item_bank_subset, # N.B. this has a log_freq column, which is required. No other itembankr columns do
                rhythmic_item_bank = SAA::Berkowitz_item_bank_subset,
                demographics = TRUE,
                demo = FALSE,
                feedback = FALSE,
                admin_password = "demo",
                SNR_test = TRUE,
                get_range = TRUE,
                absolute_url = character(),
                examples = 2L,
                final_results = TRUE,
                test_username = character(),
                gold_msi = TRUE,
                with_final_page = FALSE,
                melody_length = c(4,20),
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
                success_on_completion_page = NULL,
                concise_wording = TRUE,
                skip_setup = FALSE,
                additional_scoring_measures = NULL,
                default_range = NULL,
                long_tone_paradigm = c("simultaneous_recall", "call_and_response"),
                get_p_id = FALSE,
                volume_meter_on_melody_trials = FALSE,
                volume_meter_on_melody_trials_type = 'default',
                long_tone_length = 5,
                allow_SNR_failure = FALSE,
                requirements_page = TRUE,
                report_SNR = FALSE,
                show_introduction = TRUE,
                show_instructions = TRUE,
                asynchronous_api_mode = FALSE,
                experiment_id = NULL,
                user_id = NULL,
                get_answer_melodic = musicassessr::get_answer_pyin_melodic_production,
                sample_item_bank_via_api = FALSE,
                pass_items_through_url_parameter = FALSE,
                show_intro_text = TRUE,
                show_microphone_type_page = TRUE,
                num_items_review = list(long_tones = 0L, arrhythmic = 0L, rhythmic = 0L)
                ) {

  long_tone_paradigm <- match.arg(long_tone_paradigm)

  stopifnot(
    melody_length[1] > 3,
    assertthat::is.string(app_name),
    is.list(num_items) && length(num_items) == 3L && setequal(names(num_items), c("long_tones", "arrhythmic", "rhythmic")),
    is.list(num_examples) && length(num_examples) == 3L && setequal(names(num_examples), c("long_tones", "arrhythmic", "rhythmic")),
    is(arrhythmic_item_bank, "item_bank"),
    is(rhythmic_item_bank, "item_bank"),
    is.logical(demographics),
    is.logical(demo),
    is.logical(feedback),
    is.character(admin_password) & length(admin_password) == 1L,
    is.logical(SNR_test),
    is.logical(get_range) | is.character(get_range) & length(get_range) == 1,
    is.character(absolute_url),
    is.scalar.numeric(examples),
    is.logical(final_results),
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
    is.list(append_trial_block_before) | psychTestR::is.test_element(append_trial_block_before),
    is.list(append_trial_block_after) | psychTestR::is.test_element(append_trial_block_after),
    is.numeric(stop_recording_after) & length(stop_recording_after) == 1,
    is.numeric(max_goes) & length(max_goes) == 1,
    is.logical(max_goes_forced),
    is.logical(long_tone_trials_as_screening),
    is.character(long_tone_trials_as_screening_failure_page),
    is.null.or(success_on_completion_page, is.character),
    is.logical(concise_wording),
    is.logical(skip_setup) | skip_setup == "except_microphone",
    is.null(additional_scoring_measures) | is.function(additional_scoring_measures) | is.list(additional_scoring_measures),
    is.null.or(default_range, function(x) is.list(x) && length(x) == 3 && setequal(names(x), c("bottom_range", "top_range", "clef")) ),
    assertthat::is.string(long_tone_paradigm),
    "log_freq" %in% names(arrhythmic_item_bank),
    is.scalar.logical(get_p_id),
    is.scalar.logical(volume_meter_on_melody_trials),
    assertthat::is.string(volume_meter_on_melody_trials_type),
    dplyr::between(long_tone_length, 1, 10),
    is.scalar.logical(allow_SNR_failure),
    is.scalar.logical(requirements_page),
    is.scalar.logical(report_SNR),
    is.scalar.logical(show_introduction),
    is.scalar.logical(show_instructions),
    is.scalar.logical(asynchronous_api_mode),
    is.null.or(experiment_id, is.integer),
    is.null.or(user_id, is.integer),
    is.function(get_answer_melodic),
    is.scalar.logical(sample_item_bank_via_api),
    is.scalar.logical(pass_items_through_url_parameter),
    is.scalar.logical(show_intro_text),
    is.scalar.logical(show_microphone_type_page),
    is.list(num_items_review) && length(num_items_review) == 3L && setequal(names(num_items_review), c("long_tones", "arrhythmic", "rhythmic"))
    )

  shiny::addResourcePath(
    prefix = "custom-assets", # custom prefix that will be used to reference your directory
    directoryPath = system.file("www", package = "SAA") # path to resource in your package
  )

  if(long_tone_trials_as_screening) {
    stop("long_tone_trials_as_screening currently cannot be used, as the functionality is subject to reanalysis.")
  }

  # Instantiate the enclosure/function factory:

  if(!is.null(additional_scoring_measures)) {
    get_answer_melodic <- musicassessr::get_answer_pyin_melodic_production_additional_measures(type = "both", melconv = FALSE, additional_scoring_measures = additional_scoring_measures)
  }


  if(!asynchronous_api_mode) {
    # Subset item bank
    arrhythmic_item_bank <- itembankr::subset_item_bank(arrhythmic_item_bank, melody_length, return_as_item_bank_class = TRUE)
    rhythmic_item_bank <- itembankr::subset_item_bank(rhythmic_item_bank, melody_length, return_as_item_bank_class = TRUE)
  }

  # Start test timeline
  timeline <- psychTestR::join(
    psychTestR::new_timeline(
      psychTestR::join(

        if(get_p_id) psychTestR::get_p_id(prompt = psychTestR::i18n("enter_id"), button_text = psychTestR::i18n("Next")),

        # In case there was a previous test in the same timeline which used an instrument,
        # we store the instrument and the reset it at the end of the test.

        psychTestR::code_block(function(state, ...) {
          previous_inst <- psychTestR::get_global("inst", state)
          if(!is.null(previous_inst)) {
            psychTestR::set_global("previous_inst", previous_inst, state)
          }
        }),

        # Init musicassessr
        musicassessr::musicassessr_init(app_name = app_name,
                                        experiment_id = experiment_id,
                                        user_id = user_id,
                                        asynchronous_api_mode = asynchronous_api_mode,
                                        instrument_id = 1,
                                        inst = "Voice"),


        # Set Test
        if(asynchronous_api_mode) musicassessr::set_test(test_name = "SAA", test_id = 1L),

        # Set instrument to Voice
        musicassessr::set_instrument(instrument_id = 1L),

        # Set default range (which will overwrite the range set by set_instrument)
        if(!is.null(default_range)) musicassessr::set_instrument_range(bottom_range = default_range$bottom_range, top_range = default_range$top_range),

        psychTestR::module("SAA",

                           psychTestR::join(

                           # Introduction, same for all users
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
                                      app_name,
                                      allow_SNR_failure,
                                      requirements_page,
                                      report_SNR,
                                      volume_meter_on_melody_trials_type,
                                      show_instructions,
                                      asynchronous_api_mode,
                                      (num_items$arrhythmic + num_items$rhythmic + num_examples$arrhythmic + num_examples$rhythmic),
                                      melody_length,
                                      sample_item_bank_via_api,
                                      show_intro_text,
                                      show_microphone_type_page),

                           # Arbitrary and optional trial block to go first
                           append_trial_block_before,

                           # Long tone trials
                           musicassessr::long_tone_trials(num_items = num_items$long_tones,
                                                          num_examples = num_examples$long_tones,
                                                          feedback = feedback,
                                                          long_tone_trials_as_screening = long_tone_trials_as_screening,
                                                          long_tone_trials_as_screening_failure_page = long_tone_trials_as_screening_failure_page,
                                                          paradigm = long_tone_paradigm,
                                                          long_tone_length = long_tone_length,
                                                          show_instructions = show_instructions),

                           # Arrhythmic melody trials
                           musicassessr::arrhythmic_melody_trials(item_bank = arrhythmic_item_bank,
                                                                  num_items = num_items$arrhythmic,
                                                                  num_examples = num_examples$arrhythmic,
                                                                  feedback = feedback,
                                                                  sound = melody_sound,
                                                                  page_text = shiny::tags$div(
                                                                    shiny::tags$img(src = "https://musicassessr.com/assets/img/singing.png", height = 100, width = 100),
                                                                    shiny::tags$br(),
                                                                    shiny::tags$p(psychTestR::i18n("sing_melody_page_text"))
                                                                  ),
                                                                  page_title = psychTestR::i18n("Sing_the_Melody"),
                                                                  instruction_text = psychTestR::i18n("sing_melody_instruction_text"),
                                                                  max_goes = max_goes,
                                                                  max_goes_forced = max_goes_forced,
                                                                  get_answer = get_answer_melodic,
                                                                  volume_meter = volume_meter_on_melody_trials,
                                                                  volume_meter_type = volume_meter_on_melody_trials_type,
                                                                  sample_item_bank_via_api = sample_item_bank_via_api,
                                                                  presampled = sample_item_bank_via_api,
                                                                  pass_items_through_url_parameter = pass_items_through_url_parameter,
                                                                  asynchronous_api_mode = asynchronous_api_mode),

                           # Rhythmic melody trials
                           musicassessr::rhythmic_melody_trials(item_bank = rhythmic_item_bank,
                                                                num_items = num_items$rhythmic,
                                                                num_examples = num_examples$rhythmic,
                                                                feedback = feedback,
                                                                sound = melody_sound,
                                                                page_text = shiny::tags$div(
                                                                  shiny::tags$img(src = "https://musicassessr.com/assets/img/singing.png", height = 100, width = 100),
                                                                  shiny::tags$br(),
                                                                  shiny::tags$p(psychTestR::i18n("sing_rhythmic_melodies_page_text"))
                                                                ),
                                                                page_title = psychTestR::i18n("sing_rhythmic_melodies_page_title"),
                                                                instruction_text = psychTestR::i18n("sing_rhythmic_melodies_instruction_text"),
                                                                max_goes = max_goes,
                                                                max_goes_forced = max_goes_forced,
                                                                get_answer = get_answer_melodic,
                                                                volume_meter = volume_meter_on_melody_trials,
                                                                volume_meter_type = volume_meter_on_melody_trials_type,
                                                                sample_item_bank_via_api = sample_item_bank_via_api,
                                                                presampled = sample_item_bank_via_api,
                                                                start_from_sampled_trial_no = num_items$rhythmic + num_examples$rhythmic,
                                                                pass_items_through_url_parameter = pass_items_through_url_parameter,
                                                                asynchronous_api_mode = asynchronous_api_mode),



                           # Review (only supporting rhythmic melody trials currently)
                           if (num_items_review$rhythmic > 0L) {
                             musicassessr::rhythmic_melody_trials(item_bank = rhythmic_item_bank,
                                                                  num_items = num_items$rhythmic,
                                                                  num_examples = num_examples$rhythmic,
                                                                  feedback = feedback,
                                                                  sound = melody_sound,
                                                                  page_text = shiny::tags$div(
                                                                    shiny::tags$img(src = "https://musicassessr.com/assets/img/singing.png", height = 100, width = 100),
                                                                    shiny::tags$br(),
                                                                    psychTestR::i18n("sing_rhythmic_melodies_page_text")
                                                                  ),
                                                                  page_title = psychTestR::i18n("sing_rhythmic_melodies_page_title"),
                                                                  instruction_text = psychTestR::i18n("sing_rhythmic_melodies_instruction_text"),
                                                                  max_goes = max_goes,
                                                                  max_goes_forced = max_goes_forced,
                                                                  get_answer = get_answer_melodic,
                                                                  volume_meter = volume_meter_on_melody_trials,
                                                                  volume_meter_type = volume_meter_on_melody_trials_type,
                                                                  sample_item_bank_via_api = sample_item_bank_via_api,
                                                                  presampled = sample_item_bank_via_api,
                                                                  start_from_sampled_trial_no = num_items$rhythmic + num_examples$rhythmic,
                                                                  pass_items_through_url_parameter = pass_items_through_url_parameter,
                                                                  asynchronous_api_mode = asynchronous_api_mode,
                                                                  review = TRUE,
                                                                  phase = "review")
                           },

                           # Arbitrary and optional trial block to go after other trial blocks
                           append_trial_block_after,

                           # Add final session information to DB (if asynchronous_api_mode and not standalone)
                           if(asynchronous_api_mode) {

                             psychTestR::conditional(function(state, ...) {
                                ! is.null(psychTestR::get_global("standalone", state))
                             }, logic = musicassessrdb::elt_add_final_session_info_to_db(asynchronous_api_mode))

                            },

                           if(!asynchronous_api_mode) psychTestR::elt_save_results_to_disk(complete = TRUE),

                           if(!asynchronous_api_mode) final_results_saa(final_results, test_name = test_name,  url = absolute_url, num_items$long_tones, num_items$arrhythmic, num_items$rhythmic, show_socials)

        )
       )
      ),
      dict = SAA_dict
    ),
    if(gold_msi) psyquest::GMS(subscales = c("Musical Training", "Singing Abilities")),
    musicassessr::deploy_demographics(demographics),

    # Reset instrument if there was one previously in the timline
    psychTestR::code_block(function(state, ...) {
      previous_inst <- psychTestR::get_global("previous_inst", state)
      if(!is.null(previous_inst)) {
        psychTestR::set_global("inst", previous_inst, state)
      }
    }),

    if(!asynchronous_api_mode) psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::new_timeline(
                    musicassessr::final_page_or_continue_to_new_test(final = with_final_page, task_name = test_name, img = 'https://adaptiveeartraining.com/assets/img/bird.png', redirect_url = success_on_completion_page),
                    dict = musicassessr::musicassessr_dict)
  )

  return(timeline)

}



SAA_intro <- function(demo = FALSE,
                      SNR_test = TRUE,
                      get_range = TRUE,
                      absolute_url = character(),
                      test_username = NULL,
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
                      app_name,
                      allow_SNR_failure = FALSE,
                      requirements_page = TRUE,
                      report_SNR = FALSE,
                      volume_meter_on_melody_trials_type = FALSE,
                      show_instructions = TRUE,
                      asynchronous_api_mode = FALSE,
                      num_items = NULL, # Only needed for async API mode
                      melody_length = NULL, # Only needed for async API mode
                      sample_item_bank_via_api = FALSE,
                      show_intro_text = TRUE,
                      show_microphone_type_page = TRUE) {

  if(test_name == "Singing Ability Assessment") {
    test_name <- psychTestR::i18n("SAA_test_name")
  }


  psychTestR::join(

    # Introduction page
    psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$h2(paste0(psychTestR::i18n("SAA_welcome"), ' ', test_name, "!")),
                                                       shiny::tags$img(src = 'https://adaptiveeartraining.com/assets/img/bird.png', height = 100, width = 100),
                                                       if(show_intro_text) shiny::tags$p(psychTestR::i18n("SAA_welcome_1")),
                                                       if(show_intro_text) shiny::tags$p(psychTestR::i18n("SAA_welcome_2"))),
                                button_text = psychTestR::i18n("Next")),

    # Setup pages
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
                              skip_setup = skip_setup,
                              allow_SNR_failure = allow_SNR_failure,
                              requirements_page = requirements_page,
                              report_SNR = report_SNR,
                              playful_volume_meter_setup = volume_meter_on_melody_trials_type == 'playful',
                              show_microphone_type_page = show_microphone_type_page,
                              asynchronous_api_mode = asynchronous_api_mode),


    # Sample from item bank now we have range
    #if(asynchronous_api_mode && sample_item_bank_via_api) musicassessrdb::sample_from_item_bank_elts(item_bank_name = "Berkowitz_ngram", num_items, melody_length),


    #if(asynchronous_api_mode && sample_item_bank_via_api) sample_from_item_bank_elts_saa(item_bank_name = "Berkowitz_ngram", num_items, melody_length),


    # psychTestR::code_block(function(state, ...) {
    #
    #   span <- psychTestR::get_global('span', state)
    #
    #   span <- 10
    #   item_bank_name <- "Berkowitz_ngram"
    #   num_items <- 4
    #   melody_length <- "5,15"
    #
    #   logging::logwarn("Forcing span of 10 for now..")
    #
    #   logging::loginfo("Sampling %s items from %s item bank via API", num_items, item_bank_name)
    #   logging::loginfo("Melody length: %s", melody_length)
    #   logging::loginfo("Span: %s", span)
    #
    #   # Make sure in correct future mode...
    #   future::plan(future::multisession)
    #
    #   item_bank_sample <- future::future({
    #
    #     musicassessrdb::sample_from_item_bank_api(item_bank_name, num_items, span, melody_length)
    #
    #     # store_db_session_api(condition_id = NA,
    #     #                      user_id = 1L,
    #     #                      psychTestR_session_id = "00",
    #     #                      time_started = Sys.time(),
    #     #                      experiment_id = NA)
    #
    #   }) %...>% (function(result) {
    #
    #     logging::loginfo("Returning promise message: %s", result$message)
    #
    #     if(result$status == 200) {
    #       sample <- result
    #       #sample <- dplyr::bind_rows(result$sample)
    #       logging::loginfo("Returning promise result: %s", sample)
    #
    #       return(sample)
    #     } else {
    #       return(NA)
    #     }
    #   })
    #
    #   psychTestR::set_global('sampled_item_bank_from_api', item_bank_sample, state)
    #
    #
    # }),

    # Instructions
    if(show_instructions) SAA_instructions(max_goes_forced, max_goes)

  )

}


sample_from_item_bank_elts_saa <- function(item_bank_name = "WJD_ngram", num_items, melody_length) {

  psychTestR::code_block(function(state, ...) {

    span <- psychTestR::get_global('span', state)

    span <- 10

    logging::logwarn("Forcing span of 10 for now..")

    logging::loginfo("Sampling %s items from %s item bank via API", num_items, item_bank_name)
    logging::loginfo("Melody length: %s", melody_length)
    logging::loginfo("Span: %s", span)

    # Make sure in correct future mode...
    future::plan(future::multisession)

    item_bank_sample <- future::future({

      #sample_from_item_bank_api(item_bank_name, num_items, span, melody_length)

      store_db_session_api(condition_id = NA,
                           user_id = 1L,
                           psychTestR_session_id = "00",
                           time_started = Sys.time(),
                           experiment_id = NA)

    }) %...>% (function(result) {

      logging::loginfo("Returning promise message: %s", result$message)

      if(result$status == 200) {
        sample <- result
        #sample <- dplyr::bind_rows(result$sample)
        logging::loginfo("Returning promise result: %s", sample)

        return(sample)
      } else {
        return(NA)
      }
    })

    psychTestR::set_global('sampled_item_bank_from_api', item_bank_sample, state)


  })

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

  psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Instructions")),
                                                     shiny::tags$p(paste0(psychTestR::i18n(SAA_instructions_5.1), " "),
                                                                   shiny::tags$strong(max_goes),
                                                                   paste0(" ", psychTestR::i18n(SAA_instructions_5.2))),
                                                     if(!max_goes_forced & max_goes > 1) shiny::tags$p(psychTestR::i18n("SAA_instructions_5.3"))
                                                     ),
                              button_text = psychTestR::i18n("Next"))

  )
}




present_scores_saa <- function(res, num_items_long_note, num_items_arrhythmic, num_items_rhythmic) {

  results_long_note <- sort_long_note_scores(num_items_long_note, res)

  results_arrhythmic <- sort_arrhythmic_scores(num_items_arrhythmic, res)

  results_rhythmic <- sort_rhythmic_scores(num_items_rhythmic, res)

  all_melodies <- compile_melodies_together(results_arrhythmic$arrhythmic_melodies, results_rhythmic$rhythmic_melodies)


  if(is_na_scalar(all_melodies)) {

    arrhythmic_melody_summary <- NA
    rhythmic_melody_summary <- NA
    pca_melodic_singing_accuracy <- NA
    melody_note_precision <- NA
    melody_interval_precision <- NA

  } else {

    arrhythmic_melody_summary <- results_arrhythmic$arrhythmic_melody_summary
    rhythmic_melody_summary <- results_rhythmic$rhythmic_melody_summary

    melody_precision_vars <- all_melodies %>%
      { if("durations" %in% names(.)) dplyr::select(., -durations) else . } %>% # Because we want the version that comes from answer_meta_data, which is edited on-the-fly, in the case where the melody is made arrhythmic
      dplyr::rename_with(~stringr::str_remove(.x, "pyin_pitch_track.")) %>%
      dplyr::rename_with(~stringr::str_remove(.x, "answer_meta_data."))


    if( length(intersect(c('freq', 'stimuli', 'nearest_stimuli_note', 'interval', 'interval_cents'), names(melody_precision_vars) )) == 5 ) {

      melody_precision_vars <- melody_precision_vars %>%
        dplyr::select(freq, stimuli, nearest_stimuli_note, interval, interval_cents) %>%
        dplyr::mutate(freq = as.numeric(freq),
                      nearest_stimuli_note = as.numeric(nearest_stimuli_note),
                      interval = as.numeric(interval),
                      interval_cents = as.numeric(interval_cents)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(note = if(is.na(freq)) NA else round(hrep::freq_to_midi(freq))) %>%
        dplyr::ungroup()

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
    } else {
      arrhythmic_melody_summary <- NA
      rhythmic_melody_summary <- NA
      pca_melodic_singing_accuracy <- NA
      melody_note_precision <- NA
      melody_interval_precision <- NA
    }


  }

  list("Long_Note" = if(is_null_scalar(results_long_note$long_note_scores) | is_na_scalar(results_long_note$long_note_scores)) tibble::tibble(pca_long_note_randomness = NA, pca_long_note_accuracy = NA, pca_long_note_scoop = NA) else results_long_note$long_note_pca_scores,
       "SAA_Ability_Arrhythmic" = if(is_null_scalar(arrhythmic_melody_summary) | is_na_scalar(arrhythmic_melody_summary)) NA else results_arrhythmic$arrhythmic_melody_score,
       "SAA_Ability_Rhythmic" = if(is_null_scalar(rhythmic_melody_summary) | is_na_scalar(rhythmic_melody_summary)) NA else results_rhythmic$rhythmic_melody_score,
       "melody_note_precision" = melody_note_precision,
       "melody_interval_precision" = melody_interval_precision,
       "pca_melodic_singing_accuracy" = pca_melodic_singing_accuracy)

}

compile_melodies_together <- function(arrhythmic_melodies, rhythmic_melodies) {

  if(!is_na_scalar(arrhythmic_melodies) & !is_na_scalar(rhythmic_melodies)) {
    shared_names <- intersect(names(arrhythmic_melodies), names(rhythmic_melodies))
    arrhythmic_melodies <- arrhythmic_melodies %>% dplyr::select(shared_names)
    rhythmic_melodies <- rhythmic_melodies %>% dplyr::select(shared_names)
    all_melodies <- rbind(arrhythmic_melodies, rhythmic_melodies)
  } else if(is_na_scalar(arrhythmic_melodies) & !is_na_scalar(rhythmic_melodies)) {
    all_melodies <- rhythmic_melodies
    arrhythmic_melody_summary <- NA
  } else if(!is_na_scalar(arrhythmic_melodies) & is_na_scalar(rhythmic_melodies)) {
    all_melodies <- arrhythmic_melodies
    rhythmic_melody_summary <- NA
  }  else if(is_na_scalar(arrhythmic_melodies) & is_na_scalar(rhythmic_melodies)) {
    all_melodies <- NA
  } else {
    stop('Something is not right')
  }
  all_melodies
}

sort_long_note_scores <- function(num_items_long_note, res) {
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

  list(long_note_scores = long_note_scores,
       long_note_pca_scores = long_note_pca_scores)
}


sort_arrhythmic_scores <- function(num_items_arrhythmic, res) {
  if(num_items_arrhythmic > 0) {

    # arrhythmic
    arrhythmic_melodies <- musicassessr::tidy_melodies(res$SAA.arrhythmic_melodies, use_for_production = "pyin_pitch_track")

    if(!"proportion_of_correct_note_events" %in% names(arrhythmic_melodies)) {
      arrhythmic_melodies$proportion_of_correct_note_events <- NA
    }

    arrhythmic_melody_tmp <- arrhythmic_melodies %>%
      { if("durations" %in% names(.)) dplyr::select(., -durations) else . } %>% # Because we want the version that comes from answer_meta_data, which is edited on-the-fly, in the case where the melody is made arrhythmic
      { if("ngrukkon" %in% names(.)) dplyr::select(., -ngrukkon) else . } %>%
      dplyr::rename_with(~stringr::str_remove(.x, "answer_meta_data.")) %>%
      dplyr::select(N, step.cont.loc.var, tonalness, log_freq, opti3, proportion_of_correct_note_events) %>%
      dplyr::mutate_if(is.character,as.numeric) %>%
      unique() %>%
      dplyr::mutate(tmp_scores = opti3) # to match what psychTestRCAT/ME expects

    as <- get_arrhythmic_summary_and_scores(arrhythmic_melodies, arrhythmic_melody_tmp)
    arrhythmic_melody_summary <- as$arrhythmic_melody_summary
    arrhythmic_melody_score <- as$arrhythmic_melody_score
  } else {
    arrhythmic_melodies <- NA
    arrhythmic_melody_summary <- NA
    arrhythmic_melody_score <- NA

  }
  list(arrhythmic_melody_score = arrhythmic_melody_score,
       arrhythmic_melodies = arrhythmic_melodies,
       arrhythmic_melody_summary = arrhythmic_melody_summary)
}

sort_rhythmic_scores <- function(num_items_rhythmic, res) {
  if(num_items_rhythmic > 0) {
    # rhythmic
    rhythmic_melodies <- musicassessr::tidy_melodies(res$SAA.rhythmic_melodies, use_for_production = "pyin_pitch_track")

    if(!"proportion_of_correct_note_events" %in% names(rhythmic_melodies)) {
      rhythmic_melodies$proportion_of_correct_note_events <- NA
    }

    rhythmic_melody_tmp <- rhythmic_melodies %>%
      { if("durations" %in% names(.)) dplyr::select(., -durations) else . } %>% # Because we want the version that comes from answer_meta_data, which is edited on-the-fly, in the case where the melody is made arrhythmic
      { if("ngrukkon" %in% names(.)) dplyr::select(., -ngrukkon) else . } %>%
      dplyr::rename_with(~stringr::str_remove(.x, "answer_meta_data.")) %>%
      dplyr::select(N, step.cont.loc.var, d.entropy, i.entropy, opti3, proportion_of_correct_note_events) %>%
      dplyr::mutate_if(is.character,as.numeric) %>%
      unique() %>%
      dplyr::mutate(tmp_scores = opti3) # to match what psychTestRCAT/ME expects

    rs <- get_rhythmic_summary_and_scores(rhythmic_melodies, rhythmic_melody_tmp)
    rhythmic_melody_summary <- rs$rhythmic_melody_summary
    rhythmic_melody_score <- rs$rhythmic_melody_score

  } else {
    rhythmic_melodies <- NA
    rhythmic_melody_summary <- NA
    rhythmic_melody_score <- NA
  }

  list(rhythmic_melody_score = rhythmic_melody_score,
       rhythmic_melodies = rhythmic_melodies,
       rhythmic_melody_summary = rhythmic_melody_summary)
}


get_arrhythmic_summary_and_scores <- function(arrhythmic_melodies, arrhythmic_melody_tmp) {
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
    arrhythmic_melody_summary <- NA
  }
  list(
    arrhythmic_melody_score = arrhythmic_melody_score,
    arrhythmic_melody_summary = arrhythmic_melody_summary
  )
}



get_rhythmic_summary_and_scores <- function(rhythmic_melodies, rhythmic_melody_tmp) {
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
    rhythmic_melody_summary <- NA
  }
  list(
    rhythmic_melody_score = rhythmic_melody_score,
    rhythmic_melody_summary = rhythmic_melody_summary
    )
}






final_results_saa <- function(final_results,
                              test_name,
                              url,
                              num_items_long_tone,
                              num_items_arrhythmic,
                              num_items_rhythmic,
                              socials = FALSE,
                              hashtag = " ") {

  psychTestR::join(

    psychTestR::code_block(function(state, ...) {

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
      psychTestR::set_local("processed_results", processed_results, state) # leave this in; it gets used by musicassessr


      # Save results
      psychTestR::save_result(place = state,
                              label = "Final_SAA_Scores",
                              value = c(
                                processed_results,
                                list(
                                  "Long_Note_Percentile" = long_note_percentile,
                                  "Arrhythmic_Percentile" = arrhythmic_percentile,
                                  "Rhythmic_Percentile" = rhythmic_percentile,
                                  "Final_SAA_Score" = Final_SAA_Score)))

    }),

    if(final_results) {
      psychTestR::join(
        psychTestR::reactive_page(function(state, ...) {

          # Present results
          Final_SAA_Score <- psychTestR::get_local("final_score", state) # leave this in; it gets used by musicassessr
          processed_results <- psychTestR::get_local("processed_results", state) # leave this in; it gets used by musicassessr

          present_scores_page(Final_SAA_Score, processed_results)

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

  )
}


present_scores_page <- function(Final_SAA_Score, processed_results) {
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
                             shiny::tags$p(psychTestR::i18n("enter_username_to_see_scoreboard"))

    ),
    save_answer = FALSE
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


