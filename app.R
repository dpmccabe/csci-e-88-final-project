library(shiny)
library(elastic)
library(jsonlite)
library(dplyr)
library(tidyr)
library(DT)
library(sankeyD3)

connect()

# get top hits for some time period
top_hits <- function(t1, t2, n = 20) {
  # generate the Elasticsearch query
  search_body <- list(
    "_source" = unbox(FALSE),
    "size" = unbox(0),
    "query" = list(
      "bool" = list(
        "filter" = list(
          list(
            "range" = list(
              "ts" = list(
                "gte" = unbox(t1),
                "lte" = unbox(t2)
              )
            )
          )
        )
      )
    ),
    "aggs" = list(
      "currs" = list(
        "terms" = list(
          "field" = unbox("curr"),
          "size" = unbox(n)
        )
      )
    )
  )

  search_body <- toJSON(search_body, pretty = TRUE)
  cat(search_body)

  # get the results
  res <- Search(
    "wikipedia",
    "wikipedia",
    body = search_body,
    asdf = T
  )

  # extract the data.frame containing the bucket counts
  d <- res$aggregations$currs$buckets
  if (length(d) == 0) return(tibble())

  # return the data.frame
  as_tibble(d) %>% select(page = key, n = doc_count)
}

prev_next_pages <- function(which_kind, page, t1, t2, n = 20) {
  # generate the Elasticsearch query
  # we assume that which_kind == 'prev'
  search_body <- list(
    "_source" = unbox(FALSE),
    "size" = unbox(0),
    "query" = list(
      "bool" = list(
        "filter" = list(
          list(
            "term" = list(
              "curr" = list("value" = unbox(page))
            )
          ),
          list(
            "range" = list(
              "ts" = list(
                "gte" = unbox(t1),
                "lte" = unbox(t2)
              )
            )
          )
        )
      )
    ),
    "aggs" = list(
      "prev_next" = list(
        "terms" = list(
          "field" = unbox("prev"),
          "size" = unbox(n)
        )
      )
    )
  )

  # getting prev vs. next pages require very similar queries, so if
  # which_kind == "next" then make some minor changes
  if (which_kind == "next") {
    names(search_body$query$bool$filter[[1]]$term) <- "prev"
    search_body$aggs$prev_next$terms$field <- unbox("curr")
  }

  search_body <- toJSON(search_body, pretty = TRUE)
  cat(search_body)

  # get the results
  res <- Search(
    "wikipedia",
    "wikipedia",
    body = search_body,
    asdf = T
  )

  # extract the data.frame containing the bucket counts
  d <- res$aggregations$prev_next$buckets
  if (length(d) == 0) return(tibble())

  # return the data.frame
  as_tibble(d) %>% select(page = key, n = doc_count)
}

ui <- fluidPage(
  titlePanel("Wikipedia hits"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      selectInput(
        "time_range_since",
        label = "Time range",
        choices = c(
          "Last 10 seconds",
          "Last minute",
          "Last 10 minutes",
          "Last hour"
        ),
        selected = "Last hour"
      ),

      actionButton("update", "Update", icon = icon("refresh"))
    ),

    mainPanel(
      uiOutput("current_time_range"),
      DTOutput("top_hits_table"),
      sankeyNetworkOutput("referrers", height = "500px")
    )
  )
)

server <- function(input, output) {
  # when the update button is pressed or the time range changes,
  # update this reactive
  time_range <- eventReactive(
    list(input$update, input$time_range_since), {
    t2 <- Sys.time()

    t1 <- case_when(
      input$time_range_since == "Last 10 seconds" ~ t2 - 10,
      input$time_range_since == "Last minute" ~ t2 - 60,
      input$time_range_since == "Last 10 minutes" ~ t2 - 600,
      input$time_range_since == "Last hour" ~ t2 - 3600
    )

    strftime(c(t1, t2), format = "%F %T")
  })

  # debugging code above the data table
  output$current_time_range <- renderUI({
    p(paste(
      "Showing top hits between",
      time_range()[1],
      "and",
      time_range()[2],
      collapse = " "
    ))
  })

  # top hits for the current time range
  top_hits_results <- reactive({
    req(time_range())
    top_hits(time_range()[1], time_range()[2])
  })

  # a data table containing the current top hits
  output$top_hits_table <- renderDT({
    datatable(
      top_hits_results(),
      style = "bootstrap",
      rownames = FALSE,
      selection = "single",
      options = list(
        sDom = '<"top">rt<"bottom">ip'
      )
    )
  })

  # update the selected page in the data table when a row is clicked
  selected_hit_page <- reactiveVal(NULL)
  selected_hit_n <- reactiveVal(NULL)

  observeEvent(input$top_hits_table_rows_selected, {
    selected_hit_page(top_hits_results()$page[input$top_hits_table_rows_selected])
    selected_hit_n(top_hits_results()$n[input$top_hits_table_rows_selected])
  })

  # generate a Sankey diagram for the most popular previous and next pages
  # for the selected page
  output$referrers <- renderSankeyNetwork({
    req(selected_hit_page(), selected_hit_n())

    prev_pages <- prev_next_pages(
      which_kind = "prev",
      page = selected_hit_page(),
      t1 = time_range()[1],
      t2 = time_range()[2]
    )

    next_pages <- prev_next_pages(
      which_kind = "next",
      page = selected_hit_page(),
      t1 = time_range()[1],
      t2 = time_range()[2]
    )

    # this will be the center node
    cur_page <- tibble(
      id = as.integer(0),
      page = selected_hit_page(),
      n = selected_hit_n()
    )

    # assign IDs to the other nodes
    prev_pages <- prev_pages %>% mutate(id = seq_len(n()))
    next_pages <- next_pages %>% mutate(id = as.integer(seq_len(n()) + nrow(prev_pages)))

    nodes <- bind_rows(cur_page, prev_pages, next_pages)

    # make the links between the nodes
    links <- bind_rows(
      prev_pages %>% rename(source = id),
      next_pages %>% rename(target = id)
    ) %>%
      replace_na(list(source = as.integer(0), target = as.integer(0)))

    # color nodes by course code
    nodes$group <- nodes$page
    links$group <- links$page

    sankeyNetwork(
      Links = as.data.frame(links),
      Nodes = as.data.frame(nodes),
      NodeGroup = "group",
      LinkGroup = "group",
      Source = "source",
      Target = "target",
      Value = "n",
      NodeID = "page",
      margin = 0,
      fontSize = 14,
      numberFormat = "d",
      showNodeValues = TRUE,
      nodeLabelMargin = 10,
      nodePadding = 10,
      nodeWidth = 50,
      align = "justify",
      linkOpacity = 0.2,
      nodeStrokeWidth = 0
    )
  })
}

shinyApp(ui = ui, server = server)
