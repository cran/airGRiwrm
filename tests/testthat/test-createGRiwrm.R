test_that("extra columns work (#64)", {
  text = "id_amont	lambert2.x	lambert2.y	area	nom	id_aval	distance_aval	model
H8100021	537912.994	2455749.314	64420.94	La Seine à Vernon	NA	NA	RunModel_CemaNeigeGR4J
H7900010	578113	2437649	61642.28	La Seine à Poissy	H8100021	76.28	RunModel_CemaNeigeGR4J
H5920010	602213	2427449	43824.66	La Seine à Paris [Austerlitz après création lacs]	H7900010	82.26	RunModel_CemaNeigeGR4J"

  BS_reseau <- read.csv(text = text, sep = "\t")

  expect_s3_class(CreateGRiwrm(
    BS_reseau,
    cols = list(
      id = "id_amont",
      down = "id_aval",
      length = "distance_aval"
    ),
    keep_all = TRUE
  ),
  "GRiwrm")

})

# Setup a simple data.frame for GRiwrm
nodes <- loadSevernNodes()

test_that("Hydrological model nodes must have numeric area", {
  nodes$area[nodes$id == "54057"] <- NA
  expect_error(CreateGRiwrm(nodes),
               regexp = "hydrological")
})

test_that("Duplicated nodes",  {
  nodes <- rbind(nodes, nodes[4,])
  expect_error(CreateGRiwrm(nodes),
               regexp = "Duplicated nodes detected")
})

test_that("Ungauged nodes without gauged node at downstream should throw an error", {
  nodes$model[nodes$id == "54057"] <- "Ungauged"
  expect_error(CreateGRiwrm(nodes),
               regexp = "downstream the node")
})

test_that("Diversion node", {
  nodes <- rbind(nodes,
                 data.frame(id = "54029", down = "54002", length = 50, area = NA, model = "Diversion"))
  expect_s3_class(CreateGRiwrm(nodes), "GRiwrm")
  n99 <- nodes
  n99$area[n99$model == "Diversion"] <- 99
  expect_error(CreateGRiwrm(n99),
               regexp = "Diversion node must have its area")
  n_orphan <- nodes
  n_orphan$id[n_orphan$model == "Diversion"] <- "54999"
  expect_error(CreateGRiwrm(n_orphan),
               regexp = "Diversion node must have the same `id` of")
})

test_that("Allow several downstream ends", {
  nodes <- rbind(nodes,
                 data.frame(id = "54029", down = NA, length = NA, area = NA, model = "Diversion"))
  expect_s3_class(CreateGRiwrm(nodes), "GRiwrm")
})

test_that("Derivated ungauged node without downstream node should have derivated node as donor", {
  nodes <- loadSevernNodes()
  nodes <- nodes[nodes$id %in% c("54095", "54001", "54032"), ]
  nodes[nodes$id %in% c("54032", "54001"), c("down", "length")] <- NA
  nodes$model[nodes$id %in% c("54095", "54001")] <- "Ungauged"
  nodes <- rbind(nodes,
                 data.frame(id = "54001", down = "54032", length = 45, area = NA, model = "Diversion"))
  g <- CreateGRiwrm(nodes)
  expect_equal(g$donor, c("54032", "54032", NA, "54032"))
})

test_that("Reservoir between ungauged and gauged node should be in ungauged cluster", {
  # Reservoir between Ungauged and gauged nodes
  n_rsrvr$model[n_rsrvr$id == "54095"] <- "Ungauged"
  g <- CreateGRiwrm(n_rsrvr) # Network provided by helper_RunModel_Reservoir.R
  expect_equal(unique(g$donor), "54001")
})

test_that("Several Diversion on same node should raise error", {
  nodes <- n_rsrvr
  nodes <- rbind(nodes,
                 data.frame(id = rep("Dam", 2),
                            down = rep(as.character(NA), 2),
                            length = rep(as.numeric(NA), 2),
                            area = rep(as.numeric(NA), 2),
                            model = rep("Diversion", 2)))
  expect_error(CreateGRiwrm(nodes),
               regexp = "Diversion")
})

test_that("Upstream donor works", {
  nupd <- loadSevernNodes()
  nupd$donor[nupd$id == "54032"] <- "Wrong_node"
  expect_error(CreateGRiwrm(nupd),
               regexp = "The 'donor' id Wrong_node is not found in the 'id' column")
  nupd$donor[nupd$id == "54032"] <- "54001"
  nupd$model[nupd$id == "54032"] <- "Ungauged"
  g <- CreateGRiwrm(nupd)
  expect_equal(g$donor[g$id == "54032"], "54001")
  nupd$donor[nupd$id == "54002"] <- "54029"
  nupd$model[nupd$id == "54002"] <- "Ungauged"
  g <- CreateGRiwrm(nupd)
  expect_equal(g$donor[g$id == "54002"], "54029")
})

test_that("Donor node can't be Ungauged nor DirectInjection nor Reservoir", {
  n <- loadSevernNodes()
  n$model[n$id == "54001"] <- "Ungauged"
  n$donor[n$id == "54001"] <- "54032"
  n$model[n$id == "54032"] <- "Ungauged"
  expect_error(CreateGRiwrm(n),
               regexp = "must be an hydrological model")
  n$model[n$id == "54032"] <- NA
  expect_error(CreateGRiwrm(n),
               regexp = "must be an hydrological model")
  n$model[n$id == "54032"] <- "RunModel_Reservoir"
  expect_error(CreateGRiwrm(n),
               regexp = "must be an hydrological model")
  n$donor[n$id == "54001"] <- NA
  n$donor[n$id == "54032"] <- "54032"
  expect_message(CreateGRiwrm(n),
                 regexp = "'54001' automatically gets the node '54057' as parameter donor")
})
