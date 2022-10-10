
rule_to_node <- function(rule) {
    id <- rule_id(rule)
    name <- rule_name(rule)
    desc <- rule_desc(rule)

    desc <- if(typeof(desc) == "character") {
                paste(desc, collapse = "\n")
            }
            else {
                paste(desc[[1]], collapse = "\n")
            }

    tooltip <- paste0("<p><b>", name,"</b><br>", desc, "</p>")

    data.frame(id = id, label = name, title = tooltip, shape = "circle", value = 40, size = 50)
}

rule_to_edge <- function(rule) {
    id <- rule_id(rule)

    deps <- rule_deps(rule)
    dep_ids <- map_dbl(deps, rule_id)

    n <- length(deps)

    data.frame(from = dep_ids,
               to   = rep.int(id, n))
}

rule_book <- function(rule, book = new.env(parent = emptyenv())) {
    name <- rule_name(rule)

    if(exists(name, book)) {
        stop(sprintf("duplicate rule name '%s'", name))
    }

    book[[name]] <- rule

    for(d in rule_deps(rule)) {
        rule_book(d, book)
    }

    book
}


#' @importFrom purrr map map_dbl map_dfr
rule_to_network <- function(rule) {

    book <- rule_book(rule)

    rule_names <- ls(book)
    rules      <- map(rule_names, function(name) book[[name]])

    nodes <- map_dfr(rules, rule_to_node)
    edges <- map_dfr(rules, rule_to_edge)

    list(nodes = nodes, edges = edges)
}

#' @importFrom visNetwork renderVisNetwork
#' @importFrom visNetwork visNetwork visEdges
#' @importFrom visNetwork visHierarchicalLayout
#' @importFrom visNetwork visInteraction
#' @importFrom visNetwork visConfigure
#' @importFrom magrittr %>%
make_server <- function(rule) {
    ntwk <- rule_to_network(rule)

    nodes <- ntwk$nodes
    edges <- ntwk$edges

    print(nodes)
    print(edges)
    function(input, output) {
        output$mynetworkid <- renderVisNetwork({
            visNetwork(nodes, edges) %>%
                visEdges(arrows = "to") %>%
                ## http://datastorm-open.github.io/visNetwork/interaction.html
                visInteraction(dragNodes = FALSE,
                               dragView = FALSE,
                               navigationButtons = TRUE,
                               keyboard = TRUE) %>%
                ## same as   visLayout(hierarchical = TRUE)
                visHierarchicalLayout() ##%>% visConfigure(enabled = TRUE)
        })
    }
}

#' @importFrom visNetwork visNetworkOutput
#' @importFrom shiny fluidPage
make_ui <- function(rule) {
    fluidPage(
        visNetworkOutput("mynetworkid")
    )
}

#' A shiny based web interface for interacting with rules.
#'
#' @param rule Rule.
#' @param port Port for starting the shiny server.
#'
#' @importFrom shiny shinyApp
#' @export
rule_web <- function(rule, port = 8000) {
    ui <- make_ui(rule)
    server <- make_server(rule)

    shinyApp(ui = ui, server = server, options = list(port = port))
}
