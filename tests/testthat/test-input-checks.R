# Checks at the start of the exported functions (roadmap #18a): the object
# they are given, then their arguments. Exact messages, no snapshot.

# Every exported function that takes an ScMaSigPro object, called on `ob`
calls_on <- function(ob, gene) {
    list(
        sc.squeeze = function() sc.squeeze(ob),
        sc.restruct = function() {
            sc.restruct(ob,
                end_node_list = list("Path1"), root_node = "Path2",
                link_node_list = list(), verbose = FALSE
            )
        },
        pb_counts = function() pb_counts(ob),
        sc.set.poly = function() sc.set.poly(ob),
        sc.p.vector = function() sc.p.vector(ob, verbose = FALSE),
        sc.t.fit = function() sc.t.fit(ob, verbose = FALSE),
        sc.filter = function() sc.filter(ob, vars = "groups"),
        sc.cluster.trend = function() sc.cluster.trend(ob),
        showCoeff = function() showCoeff(ob),
        showInflu = function() showInflu(ob),
        showTS = function() showTS(ob),
        showSol = function() showSol(ob),
        showSigProf = function() showSigProf(ob),
        showPoly = function() showPoly(ob),
        showParams = function() showParams(ob),
        showGroupCoeff = function() showGroupCoeff(ob),
        queryCoeff = function() queryCoeff(ob, verbose = FALSE),
        plotBinTile = function() plotBinTile(ob),
        plotIntersect = function() plotIntersect(ob, return = TRUE),
        plotTrend = function() plotTrend(ob, feature_id = gene),
        plotTrendCluster = function() plotTrendCluster(ob, verbose = FALSE),
        plotDiagnostics = function() plotDiagnostics(ob, feature_id = gene)
    )
}

test_that("an object of another class is refused by every function", {
    for (f in calls_on(42, "Gene1")) {
        expect_error(f(), "Please provide object of class 'ScMaSigPro'",
            fixed = TRUE
        )
    }
})

test_that("an object saved before the version slot stops before any work", {
    old <- splat_clustered()
    attr(old, "version") <- NULL
    for (f in calls_on(old, splat_sig_gene())) {
        expect_error(f(), "update it with updateObject() first.",
            fixed = TRUE
        )
    }
})

test_that("a Dense slot without counts asks for pb_counts()", {
    msg <- paste(
        "The 'Dense' slot has no 'bulk.counts' assay: run pb_counts()",
        "after changing the bins with cDense<-."
    )
    ob <- splat_design(2)
    suppressMessages(cDense(ob) <- cDense(ob)[-1, ])
    expect_error(sc.p.vector(ob, verbose = FALSE), msg, fixed = TRUE)
    ob <- splat_clustered()
    suppressMessages(cDense(ob) <- cDense(ob)[-1, ])
    gene <- splat_sig_gene()
    expect_error(sc.t.fit(ob, verbose = FALSE), msg, fixed = TRUE)
    expect_error(plotTrend(ob, feature_id = gene), msg, fixed = TRUE)
    expect_error(showSigProf(ob, includeInflu = TRUE), msg, fixed = TRUE)
    expect_error(plotDiagnostics(ob, feature_id = gene), msg, fixed = TRUE)
})

test_that("the fits need counts for the bins of the design", {
    msg <- "The 'bulk.counts' assay has no counts for some bins of the design"
    # Bins renamed with cDense<- before sc.set.poly(), without pb_counts()
    ob <- splat_binned()
    bins <- cDense(ob)
    rownames(bins) <- paste0("bin", seq_len(nrow(bins)))
    cDense(ob) <- bins
    ob <- sc.set.poly(ob, poly_degree = 2)
    expect_error(sc.p.vector(ob, verbose = FALSE), msg, fixed = TRUE)
    # pb_counts() gives the counts the new names
    expect_identical(
        sc.p.vector(pb_counts(ob), verbose = FALSE)@Profile,
        splat_fitted("default")@Profile
    )
    # Renamed after sc.set.poly(): the design and the counts still agree
    ob <- splat_design(2)
    cDense(ob) <- bins
    expect_identical(
        sc.p.vector(ob, verbose = FALSE)@Profile,
        splat_fitted("default")@Profile
    )
})

test_that("the steps say which earlier step is missing", {
    expect_error(sc.p.vector(splat_binned(), verbose = FALSE),
        "No polynomial design found. Please run 'sc.set.poly()', first.",
        fixed = TRUE
    )
    msg <- "No gene with a non-flat profile found. Please run 'sc.p.vector()'"
    expect_error(sc.t.fit(splat_design(2), verbose = FALSE), msg,
        fixed = TRUE
    )
    # sc.p.vector() that selected no gene (ledger Z1)
    none <- suppressMessages(
        sc.p.vector(splat_design(2), p_value = 1e-300, verbose = FALSE)
    )
    expect_error(sc.t.fit(none, verbose = FALSE), msg, fixed = TRUE)
    expect_error(sc.cluster.trend(splat_fitted("default")),
        "'Significant' slot is empty, please run 'sc.filter()' first.",
        fixed = TRUE
    )
})
