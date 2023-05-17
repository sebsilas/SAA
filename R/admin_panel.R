

#' Run an SAA admin app
#'
#' @param app_locations
#'
#' @return
#' @export
#'
#' @examples
run_saa_admin_app <- function(app_locations) {

  if(Sys.getenv("CONFIG") == "local") {
    app_locations <- setNames(app_locations, basename(app_locations))
  } else {
    nms <- stringr::str_remove(app_locations, '/srv/shiny-server/') %>% stringr::str_remove('/output/results')
    app_locations <- setNames(app_locations, nms)
  }

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::selectInput("app_to_monitor", label = "App to Monitor", choices = app_locations),
    shiny::textOutput("success"),
    shiny::textOutput("no_responses"),
    shiny::textOutput("no_complete"),
    shiny::textOutput("pr_complete"),
    shiny::plotOutput('hist_plot') %>% shinycssloaders::withSpinner(),
    shiny::plotOutput('error_pr_plot')  %>% shinycssloaders::withSpinner()
  )


  # Define the server code
  server <- function(input, output) {

    data <- shiny::reactive({
      shiny::req(input$app_to_monitor)
      print(input$app_to_monitor)
      sort_saa_results_data(input$app_to_monitor)
    })

    output$hist_plot <- shiny::renderPlot({
      plot <- data()$hist_plot
      if(is_na_scalar(plot)) plot.new() else plot
    })

    output$error_pr_plot <- shiny::renderPlot({
      plot <- data()$error_pr_plot
      if(is_na_scalar(plot)) plot.new() else plot
    })

    output$no_responses  <- shiny::renderText({
      paste0("No. Responses: ", data()$no_responses)
    })

    output$no_complete  <- shiny::renderText({
      paste0("No. Complete Responses: ", data()$no_complete)
    })
    output$pr_complete  <- shiny::renderText({
      paste0("Proportion Complete Responses: ", round(data()$pr_complete, 2), "%")
    })

    output$success  <- shiny::renderText({
      if(is_na_scalar(data()$pr_complete) & is_na_scalar(data()$hist_plot)) {
        t <- "There is not sufficient data to present results yet."
      }
      else {
        t <- " "
      }
      t
    })


  }

  # Return a Shiny app object
  shiny::shinyApp(ui = ui, server = server)
}



sort_saa_results_data <- function(results_dir) {

  files <- list.files(results_dir, pattern = "\\.rds$", full.names = TRUE)

  #files <- list.files('/srv/shiny-server/italian_saa/output/results', pattern = "\\.rds$", full.names = TRUE)

  dat <- purrr::map_dfr(files, read_p)

  if(is.null(dat$no_data)) {

    dat <- dat %>%
      dplyr::select(num_restarts, complete, error, p_id, answer_meta_data.abs_melody,
                    opti3, attempt, answer_meta_data.trial_no, answer_meta_data.N,
                    no_note_events, trial_length) %>%
      unique() %>%
      dplyr::mutate(dplyr::across(opti3:answer_meta_data.trial_no, as.numeric))

    no_responses <- unique(dat$p_id) %>% length()

    no_complete <- dat %>%
      dplyr::select(p_id, complete) %>%
      unique() %>%
      dplyr::filter(complete == TRUE) %>%
      nrow()

    pr_complete <- (no_complete/no_responses) * 100

    avg_num_restarts <- dat %>%
      dplyr::select(p_id, num_restarts) %>%
      unique() %>%
      dplyr::summarise(avg_num_restarts = mean(num_restarts, na.rm = TRUE)) %>%
      dplyr::pull(avg_num_restarts)

    hist_plot <- dat %>%
      dplyr::select(answer_meta_data.trial_no, opti3, attempt, answer_meta_data.N, no_note_events, trial_length) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
      itembankr::hist_item_bank()

    error_pr_plot <- dat %>%
      dplyr::rename(Error = error) %>%
      dplyr::count(Error) %>%
      ggplot2::ggplot(ggplot2::aes(x="", y=n, fill=Error)) +
      ggplot2::geom_bar(stat="identity", width=1) +
      ggplot2::coord_polar("y", start=0) +
      ggplot2::theme_void() +
      ggplot2::scale_fill_manual(values = c("green", "red"))


    list(hist_plot = hist_plot,
         error_pr_plot = error_pr_plot,
         no_responses = no_responses,
         no_complete = no_complete,
         pr_complete = pr_complete)

  } else {

    list(hist_plot = NA,
         error_pr_plot = NA,
         no_responses = NA,
         no_complete = NA,
         pr_complete = NA)
  }





}




read_p <- function(f) {

  res <- readRDS(f) %>%
    unclass()

  session <- res$session
  p_id <- session$p_id
  session <- tibble::as_tibble(session)

  user_info <- res$SAA.musicassessr_setup$user_info$user_info

  brands <- user_info$userAgentData$brands %>%
    purrr::map_dfr(tibble::as_tibble)

  mobile <- user_info$userAgentData$mobile
  platform <- user_info$userAgentData$platform
  user_info$userAgentData <- NULL


  user_info <- purrr::map(user_info, function(x) {
    if(length(x) == 0) {
      NA
    } else if(length(x) > 1) {
      paste0(x, collapse = ", ")
    }
    else {
      x
    }
  })


  musicassessr_setup <- user_info %>%
    tibble::as_tibble() %>%
    dplyr::mutate(userAgentData_mobile = mobile,
                  userAgentData_platform = platform,
                  brands = list(brands),
                  p_id = !!p_id)


  long_notes <- res$SAA.long_tone_trials %>%
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
      unique() %>%
      dplyr::mutate(p_id = !! p_id)

    long_note_pca_scores <- long_notes %>%
      dplyr::select(-p_id) %>%
      musicassessr::get_long_note_pcas()  %>%
      dplyr::mutate(p_id = !! p_id)


    arrhythmic_melodies <- if(is.null(res$SAA.arrhythmic_melodies)) NA else {
          res$SAA.arrhythmic_melodies %>%
          musicassessr::tidy_melodies(use_for_production = "production")  %>%
          dplyr::mutate(p_id = !! p_id)
    }

    rhythmic_melodies <- if(is.null(res$SAA.rhythmic_melodies)) NA else {
      res$SAA.rhythmic_melodies %>%
        musicassessr::tidy_melodies(use_for_production = "production")  %>%
        dplyr::mutate(p_id = !! p_id)
    }

    joined <- if(!is_na_scalar(arrhythmic_melodies) & !is_na_scalar(rhythmic_melodies)) {
      arrhythmic_melodies %>% dplyr::left_join(rhythmic_melodies, by = "p_id")
    } else if(is_na_scalar(arrhythmic_melodies) & !is_na_scalar(rhythmic_melodies)) {
      rhythmic_melodies
    } else if(!is_na_scalar(arrhythmic_melodies) & is_na_scalar(rhythmic_melodies)) {
      arrhythmic_melodies
    } else {
      warning('There is currently no usable data.')
      return(tibble::tibble(no_data = TRUE))
    }

    joined <- joined %>%
      dplyr::left_join(long_notes, by = "p_id") %>%
      dplyr::left_join(long_note_pca_scores, by = "p_id") %>%
      dplyr::left_join(session, by = "p_id") %>%
      dplyr::left_join(musicassessr_setup, by = "p_id")

}


# run_saa_admin_app(c('/Users/sebsilas/Singing-Tests/input/SAA_data'))




