#' Output Switch Block - Case Study
#'
#' A minimal block to investigate runtime output type switching in blockr.
#' This block outputs iris data and lets you switch between data.frame and GT rendering.
#'
#' @param output_type Initial output type: "data.frame" or "gt"
#' @param ... Additional arguments passed to new_block
#'
#' @importFrom blockr.core block_output block_ui
#' @export
new_output_switch_block <- function(
    output_type = c("data.frame", "gt"),
    ...) {

  output_type <- match.arg(output_type)

  blockr.core::new_data_block(
    server = function(id) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_output_type <- shiny::reactiveVal(output_type)

          shiny::observeEvent(input$output_type, {
            r_output_type(input$output_type)
          })

          list(
            expr = shiny::reactive({
              quote(datasets::iris)
            }),
            state = list(
              output_type = r_output_type
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::selectInput(
          ns("output_type"),
          label = "Output Type",
          choices = c("Data Frame" = "data.frame", "GT Table" = "gt"),
          selected = output_type
        )
      )
    },
    class = "output_switch_block",
    ...
  )
}


# Dynamic output type switching pattern:
# - block_output receives the PARENT block's session
# - The expr module is namespaced under "expr", so inputs are at session$input[["expr-*"]]
# - Use uiOutput + renderUI for dynamic switching between widget types

#' @export
block_ui.output_switch_block <- function(id, x, ...) {
  shiny::uiOutput(shiny::NS(id, "result"))
}

#' @export
block_output.output_switch_block <- function(x, result, session) {
  shiny::renderUI({
    output_type <- session$input[["expr-output_type"]]

    if (is.null(output_type) || output_type == "data.frame") {
      DT::datatable(result, options = list(pageLength = 10))
    } else {
      shiny::HTML(gt::as_raw_html(gt::gt(result)))
    }
  })
}
