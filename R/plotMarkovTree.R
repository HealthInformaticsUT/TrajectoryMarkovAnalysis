#' Plot a Custom Tree-Based Markov Model
#'
#' Creates a tree-based visualization of a Markov chain using a custom-defined
#' structure. It conditionally recalculates edge probabilities at each branching point
#' based on a given transition matrix. Special "Split" nodes act as branch placeholders.
#'
#' @param tree_df A data.frame with a column named `path`, defining custom paths
#'        using `>` as the separator between states.
#' @param transition_matrix A square numeric matrix of transition probabilities.
#'        Row and column names must match state labels used in the tree paths.
#'
#' @return A DiagrammeR plot object representing the custom tree-structured Markov model.
#'
#' @examples
#' tree_df <- data.frame(
#'   path = c(
#'     "Chain>Healthy",
#'     "Chain>Healthy>Death",
#'     "Chain>Healthy>Split",
#'     "Chain>Healthy>Split>Post Mace",
#'     "Chain>Healthy>Split>Split",
#'     "Chain>Healthy>Split>Split>Treatment",
#'     "Chain>Healthy>Split>Split>Healthy"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' states <- c("Healthy", "Death", "Post Mace", "Treatment", "Death Mace")
#' transition_matrix <- matrix(c(
#'   0.9173, 0.0093, 0.0118, 0.0541, 0.0075,
#'   0.0000, 1.0000, 0.0000, 0.0000, 0.0000,
#'   0.0000, 0.0474, 0.9526, 0.0000, 0.0000,
#'   0.0000, 0.0053, 0.0071, 0.9778, 0.0098,
#'   0.0000, 0.0000, 0.0000, 0.0000, 1.0000
#' ), nrow = 5, byrow = TRUE, dimnames = list(states, states))
#'
#' plot_custom_markov_tree(tree_df, transition_matrix)
#'
#' @export
plot_custom_markov_tree <- function(tree_df, transition_matrix) {
  # Helper: get last active (non-split/non-chain) state
  get_active_state <- function(from_path) {
    parts <- unlist(strsplit(from_path, ">"))
    non_split <- parts[!parts %in% c("Split", "Chain")]
    if (length(non_split) == 0) return(NA)
    return(utils::tail(non_split, 1))
  }

  all_paths <- strsplit(tree_df$path, ">")

  # Build edge list
  edges <- data.frame(
    from_id = character(), to_id = character(),
    from_label = character(), to_label = character(),
    raw_from = character(), raw_to = character(),
    stringsAsFactors = FALSE
  )

  for (path in all_paths) {
    for (i in seq_len(length(path) - 1)) {
      from_raw <- paste(path[1:i], collapse = ">")
      to_raw <- paste(path[1:(i + 1)], collapse = ">")
      edges <- rbind(edges, data.frame(
        from_id = gsub(">", "_", from_raw),
        to_id = gsub(">", "_", to_raw),
        from_label = path[i],
        to_label = path[i + 1],
        raw_from = from_raw,
        raw_to = to_raw,
        stringsAsFactors = FALSE
      ))
    }
  }

  # Deduplicate edges
  edges <- edges[!duplicated(paste(edges$from_id, edges$to_id)), ]
  edges$label <- ""

  unique_froms <- unique(edges$from_id)
  used_transitions <- list()

  for (fid in unique_froms) {
    sub_edges <- edges[edges$from_id == fid, ]
    edge_idxs <- which(edges$from_id == fid)
    to_labels <- sub_edges$to_label
    from_raw <- gsub("_", ">", fid)
    active_state <- get_active_state(from_raw)
    if (is.na(active_state) || !(active_state %in% rownames(transition_matrix))) next

    state_key <- paste0("state:", active_state)
    previously_used <- used_transitions[[state_key]]
    if (is.null(previously_used)) previously_used <- character(0)

    # Get remaining possible targets
    remaining_targets <- setdiff(colnames(transition_matrix), previously_used)
    full_probs <- transition_matrix[active_state, remaining_targets]
    if (sum(full_probs) == 0) next
    norm_probs <- full_probs / sum(full_probs)

    # Assign probabilities to matched children
    actual_targets <- intersect(to_labels, names(norm_probs))
    for (target in actual_targets) {
      prob <- norm_probs[target]
      idx <- edge_idxs[to_labels == target]
      edges$label[idx] <- paste0("p=", round(prob, 4))
      previously_used <- union(previously_used, target)
    }

    # Assign to "Split" if needed
    if ("Split" %in% to_labels) {
      used_probs <- transition_matrix[active_state, actual_targets]
      used_probs <- if (sum(full_probs) > 0) used_probs / sum(full_probs) else used_probs
      remainder <- max(0, 1 - sum(used_probs))
      split_idx <- edge_idxs[to_labels == "Split"]
      edges$label[split_idx] <- paste0("p=", round(remainder, 4))
    }

    used_transitions[[state_key]] <- previously_used
  }

  # Build DOT graph string
  dot <- "digraph markov_tree {\n"
  dot <- paste0(dot, "  graph [rankdir=LR];\n")
  dot <- paste0(dot, "  node [fontname=Helvetica];\n")

  all_nodes <- unique(c(edges$from_id, edges$to_id))
  for (nid in all_nodes) {
    label <- utils::tail(strsplit(nid, "_")[[1]], 1)
    shape <- if (label == "Split") "diamond" else "ellipse"
    dot <- paste0(dot, sprintf("  \"%s\" [label=\"%s\", shape=%s];\n", nid, label, shape))
  }

  for (i in seq_len(nrow(edges))) {
    label <- ifelse(edges$label[i] == "", " ", edges$label[i])
    dot <- paste0(dot, sprintf("  \"%s\" -> \"%s\" [label=\"%s\"];\n",
                               edges$from_id[i], edges$to_id[i], label))
  }

  dot <- paste0(dot, "}")
  DiagrammeR::grViz(dot)
}