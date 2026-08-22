## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6.5,
  fig.height = 4.5,
  dev = "svglite",
  fig.ext = "svg",
  dev.args = list(pointsize = 14)
)
library(corrselect)

# Figure palette, light mode hex; the pkgdown CSS swaps in dark counterparts
PAL <- c(blue = "#2E6F9E", red = "#B5342B", green = "#3F7A3D",
         orange = "#9E5E0F", purple = "#7A4F9E", teal = "#2A7A7A",
         grey = "#5C6166")

## -----------------------------------------------------------------------------
# Construct a simple 4x4 correlation matrix
cor_4var <- matrix(c(
  1.00, 0.85, 0.10, 0.15,
  0.85, 1.00, 0.12, 0.18,
  0.10, 0.12, 1.00, 0.75,
  0.15, 0.18, 0.75, 1.00
), nrow = 4, byrow = TRUE)

colnames(cor_4var) <- rownames(cor_4var) <- paste0("V", 1:4)

# Display matrix
print(cor_4var)

## ----fig.width=6, fig.height=4, fig.alt="Text output showing adjacency matrix for threshold graph with 4 variables. Matrix displays 1 where edges exist (variables with absolute correlation at or below 0.7 can coexist) and 0 where edges don't exist (variables with absolute correlation above 0.7 cannot coexist). The pattern reveals which variable pairs are compatible for inclusion in the same maximal subset."----
# Adjacency matrix for threshold graph (edges where |cor| <= 0.7)
adj_matrix <- abs(cor_4var) <= 0.7
diag(adj_matrix) <- FALSE  # No self-loops

# Visualize as adjacency matrix
cat("Threshold graph edges (1 = edge exists):\n")
print(adj_matrix * 1)

## ----fig.height=4, fig.alt="Graph visualization with 4 nodes (V1, V2, V3, V4) arranged in a square pattern. An edge connects each variable pair with absolute correlation at or below the 0.7 threshold. Red labels above and below the square give the two correlations that exceed the threshold, where no edge is drawn. The graph structure reveals four maximal cliques of size two, listed to the right of the square: {V1,V3}, {V1,V4}, {V2,V3} and {V2,V4}, corresponding to maximal subsets where all pairwise correlations remain at or below threshold. Each clique in the list carries the color of the edge it comes from."----
# Node positions (arranged in a square for clarity)
node_pos <- matrix(c(
  0, 1,    # V1 (top-left)
  2, 1,    # V2 (top-right)
  0, 0,    # V3 (bottom-left)
  2, 0     # V4 (bottom-right)
), ncol = 2, byrow = TRUE)

# Plot setup, with a right-hand gutter reserved for the clique list
par(mar = c(1, 1, 3, 1))
plot(node_pos, type = "n", xlim = c(-0.4, 4.1), ylim = c(-0.6, 1.6),
     xlab = "", ylab = "", axes = FALSE, asp = 1,
     main = "Threshold Graph (τ = 0.7)")

# Edges are the pairs with correlation < 0.7, and here each one is also a
# maximal clique, so both get the same color
edges <- which(adj_matrix & upper.tri(adj_matrix), arr.ind = TRUE)
edges <- edges[order(edges[, "row"], edges[, "col"]), , drop = FALSE]
edge_cols <- unname(PAL[c("blue", "orange", "teal", "purple")])

for (k in seq_len(nrow(edges))) {
  i <- edges[k, "row"]
  j <- edges[k, "col"]
  segments(node_pos[i, 1], node_pos[i, 2],
           node_pos[j, 1], node_pos[j, 2],
           col = edge_cols[k], lwd = 2)
}

# Draw nodes
node_size <- 0.18
for (i in 1:4) {
  # Node circle
  symbols(node_pos[i, 1], node_pos[i, 2],
          circles = node_size, add = TRUE,
          inches = FALSE, bg = "white", fg = "black", lwd = 2)

  # Node label
  text(node_pos[i, 1], node_pos[i, 2],
       labels = paste0("V", i), cex = 1.25, font = 2)
}

# Add correlation annotations
text(1, 1.45, "cor = 0.85, no edge", col = PAL[["red"]])
text(1, -0.45, "cor = 0.75, no edge", col = PAL[["red"]])

# Maximal cliques, listed in the reserved gutter and colored to their edge
text(2.7, 1.1, "Maximal cliques", font = 2, adj = 0)
clique_labels <- sprintf("{V%d, V%d}", edges[, "row"], edges[, "col"])
text(2.7, 0.75 - (seq_along(clique_labels) - 1) * 0.22, clique_labels,
     col = edge_cols, adj = c(0, 1))

## ----fig.width=6.5, fig.height=6.5, fig.alt="Network graph visualization of 20 variables organized into 4 correlation blocks. Node outlines and labels are colored by block: red (Block 1, V1-V5, high correlation), orange (Block 2, V6-V10, moderate), teal (Block 3, V11-V15, low), and blue (Block 4, V16-V20, minimal). Grey edges connect variables with absolute correlation at or below 0.7 threshold. The force-directed layout clusters highly correlated variables together, revealing the block structure. Variables within blocks have few connections (high correlation), while variables across blocks have many connections (low correlation), illustrating which combinations can form maximal cliques."----
data(cor_example)

# Build threshold graph (edges where |correlation| <= 0.7)
threshold <- 0.7
adj_mat <- abs(cor_example) <= threshold
diag(adj_mat) <- FALSE

if (requireNamespace("igraph", quietly = TRUE)) {
  library(igraph)

  # Create graph from adjacency matrix
  g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected")

  # Find maximal cliques
  cliques <- max_cliques(g)
  cat(sprintf("Found %d maximal cliques at threshold %.1f\n", length(cliques), threshold))

  # Color nodes by which block they belong to
  block_pal <- PAL[c("red", "orange", "teal", "blue")]
  block_colors <- rep(block_pal, each = 5)

  # Plot network, with bottom margin reserved for the legend
  par(mar = c(4, 1, 3, 1))
  plot(g,
       vertex.size = 22,
       vertex.color = "white",
       vertex.frame.color = block_colors,
       vertex.frame.width = 2,
       vertex.label.color = block_colors,
       edge.color = adjustcolor(PAL[["grey"]], alpha.f = 0.4),
       edge.width = 1.5,
       layout = layout_with_fr(g),
       main = sprintf("Threshold Graph (τ = %.1f)", threshold))

  # Add legend below the network
  legend("bottom", inset = -0.16, xpd = TRUE, ncol = 2,
         legend = c("Block 1 (V1-V5): high cor",
                   "Block 2 (V6-V10): moderate",
                   "Block 3 (V11-V15): low",
                   "Block 4 (V16-V20): minimal"),
         fill = block_pal, border = block_pal, bty = "n")
} else {
  cat("Install igraph for network visualization: install.packages('igraph')\n")
  cat("Adjacency matrix (first 5×5 block):\n")
  print(adj_mat[1:5, 1:5] * 1)
}

## -----------------------------------------------------------------------------
results <- MatSelect(cor_4var, threshold = 0.7, method = "bron-kerbosch")
show(results)

## -----------------------------------------------------------------------------
# Create example correlation matrix
set.seed(123)
cor_6var <- matrix(c(
  1.00, 0.85, 0.75, 0.20, 0.15, 0.10,
  0.85, 1.00, 0.80, 0.25, 0.20, 0.15,
  0.75, 0.80, 1.00, 0.30, 0.25, 0.20,
  0.20, 0.25, 0.30, 1.00, 0.65, 0.55,
  0.15, 0.20, 0.25, 0.65, 1.00, 0.60,
  0.10, 0.15, 0.20, 0.55, 0.60, 1.00
), nrow = 6, byrow = TRUE)

rownames(cor_6var) <- colnames(cor_6var) <- paste0("V", 1:6)

# Display correlation matrix
print(round(cor_6var, 2))

## ----fig.height=5, fig.alt="Correlation matrix heatmap for the 6-variable example, with blue (negative), white (zero), and red (positive) colors, numerical values overlaid on each cell, and black dashed lines separating the two correlation blocks. Variables V1 to V3 form one high-correlation block and V4 to V6 another, with low correlations between the blocks."----
# Build adjacency matrix for threshold graph
tau <- 0.7
adj_matrix <- abs(cor_6var) <= tau
diag(adj_matrix) <- FALSE

# Correlation heatmap
col_pal <- colorRampPalette(c("#3B4992", "white", "#EE0000"))(100)
image(1:6, 1:6, t(cor_6var[6:1, ]),
      col = col_pal,
      xlab = "", ylab = "",
      main = "Correlation Matrix",
      axes = FALSE,
      zlim = c(-1, 1))
axis(1, at = 1:6, labels = colnames(cor_6var))
axis(2, at = 6:1, labels = colnames(cor_6var))

# Add correlation values
for (i in 1:6) {
  for (j in 1:6) {
    col_text <- if (abs(cor_6var[j, i]) > 0.5) "white" else "black"
    text(i, 7 - j, sprintf("%.2f", cor_6var[j, i]), col = col_text, font = 2)
  }
}
abline(h = 3.5, lwd = 2, lty = 2, col = "black")
abline(v = 3.5, lwd = 2, lty = 2, col = "black")

## ----fig.height=4, fig.alt="Threshold graph for the 6-variable example, with the two correlation blocks on opposite sides: red nodes V1 to V3 on the left, teal nodes V4 to V6 on the right. An edge connects each pair whose absolute correlation is at or below 0.7. Solid blue edges run between the blocks and dashed orange edges within one. All nine cross-block pairs have edges; the only within-block edges are the three among V4, V5 and V6, and V1 to V3 have none among themselves. Maximal cliques therefore combine V4, V5 and V6 with one variable from the left block."----
# Threshold graph (edges where |cor| <= tau)
# The bottom margin holds the legend clear of the nodes
par(mar = c(4, 1, 3, 1))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
title(main = sprintf("Threshold Graph (τ = %.1f)", tau))

# Node positions: one block per side, each block a triangle so that a
# within-block edge never runs through the third node
pos <- matrix(c(
  0.20, 0.85,  # V1
  0.05, 0.50,  # V2
  0.20, 0.15,  # V3
  0.80, 0.85,  # V4
  0.95, 0.50,  # V5
  0.80, 0.15   # V6
), ncol = 2, byrow = TRUE)

# Draw edges, separating pairs inside one block from pairs across the two,
# since whether a block is internally compatible is what the cliques turn on
block <- rep(1:2, each = 3)
for (i in 1:5) {
  for (j in (i + 1):6) {
    if (adj_matrix[i, j]) {
      within <- block[i] == block[j]
      lines(c(pos[i, 1], pos[j, 1]), c(pos[i, 2], pos[j, 2]),
            col = if (within) PAL[["orange"]] else PAL[["blue"]],
            lty = if (within) 2 else 1, lwd = 2)
    }
  }
}

# Draw nodes, colored by block
node_cols <- c(rep(PAL[["red"]], 3), rep(PAL[["teal"]], 3))
points(pos[, 1], pos[, 2], pch = 21, cex = 4,
       bg = "white", col = node_cols, lwd = 2)

# Add labels
text(pos[, 1], pos[, 2], labels = colnames(cor_6var),
     col = node_cols, font = 2)

# Add legend
legend("bottom", inset = -0.28, xpd = TRUE,
       legend = c("Block 1 (V1-V3)", "Block 2 (V4-V6)",
                  "Edge across blocks", "Edge within a block"),
       pch = c(21, 21, NA, NA),
       pt.bg = c("white", "white", NA, NA),
       pt.cex = 1.6,
       lty = c(NA, NA, 1, 2),
       col = c(PAL[["red"]], PAL[["teal"]], PAL[["blue"]], PAL[["orange"]]),
       lwd = 2,
       ncol = 2,
       bty = "n")

## -----------------------------------------------------------------------------
# Run MatSelect to find all maximal subsets
results <- MatSelect(cor_6var, threshold = 0.7, method = "els")
show(results)

## -----------------------------------------------------------------------------
sessionInfo()

