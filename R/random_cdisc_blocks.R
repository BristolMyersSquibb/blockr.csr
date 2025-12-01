#' Random CDISC ADSL Block
#'
#' Generates a random ADSL dataset using random.cdisc.data package.
#'
#' @param seed Random seed for reproducibility
#' @param n_subjects Number of subjects (default 400)
#' @param ... Additional arguments passed to new_block
#'
#' @export
new_random_adsl_block <- function(
  seed = 1,
  n_subjects = 400,
  ...
) {
  blockr.core::new_data_block(
    server = function(id) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_seed <- shiny::reactiveVal(seed)
          r_n_subjects <- shiny::reactiveVal(n_subjects)

          shiny::observeEvent(input$seed, {
            r_seed(input$seed)
          })

          shiny::observeEvent(input$n_subjects, {
            r_n_subjects(input$n_subjects)
          })

          list(
            expr = shiny::reactive({
              seed_val <- r_seed()
              n_val <- r_n_subjects()

              shiny::req(seed_val, n_val)

              bquote(
                random.cdisc.data::radsl(
                  N = .(n_val),
                  seed = .(seed_val)
                )
              )
            }),
            state = list(
              seed = r_seed,
              n_subjects = r_n_subjects
            )
          )
        }
      )
    },
    ui = function(id) {
      shiny::tagList(
        shiny::div(
          class = "block-container",
          shiny::div(
            class = "block-form-grid",
            shiny::div(
              class = "block-section",
              shiny::tags$h4("Random ADSL Configuration"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::numericInput(
                    shiny::NS(id, "seed"),
                    label = "Seed",
                    value = seed,
                    min = 1
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::numericInput(
                    shiny::NS(id, "n_subjects"),
                    label = "Number of Subjects",
                    value = n_subjects,
                    min = 10,
                    max = 1000
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "random_adsl_block",
    ...
  )
}


#' Random CDISC ADaM Block
#'
#' Generates a random ADaM dataset using random.cdisc.data package.
#' Requires ADSL as input.
#'
#' @param dataset Dataset to generate (default "qs")
#' @param seed Random seed for reproducibility
#' @param ... Additional arguments passed to new_block
#'
#' @export
new_random_adam_block <- function(
  dataset = "qs",
  seed = 1,
  ...
) {
  # Available datasets that take adsl as input
  # Values are the suffixes for rad* functions (e.g., "qs" -> radqs)
  available_datasets <- c(
    "qs", "ae", "vs", "lb", "tte",
    "eg", "cm", "mh", "ex"
  )
  names(available_datasets) <- c(
    "ADQS", "ADAE", "ADVS", "ADLB", "ADTTE",
    "ADEG", "ADCM", "ADMH", "ADEX"
  )

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_dataset <- shiny::reactiveVal(dataset)
          r_seed <- shiny::reactiveVal(seed)

          shiny::observeEvent(input$dataset, {
            r_dataset(input$dataset)
          })

          shiny::observeEvent(input$seed, {
            r_seed(input$seed)
          })

          list(
            expr = shiny::reactive({
              dataset_val <- r_dataset()
              seed_val <- r_seed()

              shiny::req(dataset_val, seed_val)

              # Build function call using do.call
              func_name <- paste0("rad", dataset_val)

              bquote(
                do.call(
                  utils::getFromNamespace(.(func_name), "random.cdisc.data"),
                  list(adsl = data, seed = .(seed_val))
                )
              )
            }),
            state = list(
              dataset = r_dataset,
              seed = r_seed
            )
          )
        }
      )
    },
    ui = function(id) {
      shiny::tagList(
        shiny::div(
          class = "block-container",
          shiny::div(
            class = "block-form-grid",
            shiny::div(
              class = "block-section",
              shiny::tags$h4("Random ADaM Configuration"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "dataset"),
                    label = "Dataset",
                    choices = available_datasets,
                    selected = dataset
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::numericInput(
                    shiny::NS(id, "seed"),
                    label = "Seed",
                    value = seed,
                    min = 1
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "random_adam_block",
    ...
  )
}
