# Console output of the object at every stage of the workflow.

test_that("show() output is stable at every stage", {
    expect_snapshot(print(splat_scmp()))
    expect_snapshot(print(splat_binned()))
    expect_snapshot(print(splat_design(2)))
    expect_snapshot(print(splat_fitted("default")))
    expect_snapshot(print(splat_clustered()))
    # Appended last: unnamed snapshots are matched by position, so adding it here
    # leaves the five snapshots above untouched.
    expect_snapshot(print(multi_restruct()))
})

test_that("show() lists the paths in the order of the bins", {
    lines <- function(ob) {
        out <- capture.output(show(ob))
        out[grepl("^Branching|^Number of bins|^Average bin", out)]
    }
    # A factor path column: its levels, before and after binning
    ob <- splat_scmp()
    cells <- cSparse(ob)
    cells$Path <- factor(cells$Path, levels = c("Path2", "Path1"))
    cSparse(ob) <- cells
    expect_identical(lines(ob), "Branching Paths: Path2, Path1")
    expect_identical(lines(sc.squeeze(ob)), c(
        "Branching Paths: Path2, Path1",
        "Number of bins-> Path2: 8 Path1: 8",
        "Average bin Size-> Path2: 13 Path1: 12"
    ))
    # The order of end_node_list, as sc.restruct() binds the end paths
    ob <- sc.restruct(multi_binned(),
        end_node_list = list("Y_52", "Y_15", "Y_18"), root_node = "Root",
        link_node_list = list("Y_18_links_Y_15"), verbose = FALSE
    )
    expect_identical(lines(ob), c(
        "Branching Paths: Y_52, Y_15, Y_18",
        "Number of bins-> Y_52: 19 Y_15: 23 Y_18: 24",
        "Average bin Size-> Y_52: 50 Y_15: 35 Y_18: 49"
    ))
})

test_that("show() ends the printed summary with a newline", {
    printed <- function(ob) {
        con <- rawConnection(raw(0), "w")
        on.exit(close(con))
        sink(con)
        show(ob)
        sink()
        rawToChar(rawConnectionValue(con))
    }
    expect_true(endsWith(printed(splat_scmp()), "\n"))
    expect_true(endsWith(printed(splat_clustered()), "\n"))
})

test_that("showPoly() prints the full model formula", {
    expect_snapshot(showPoly(splat_design(1)))
    expect_snapshot(showPoly(splat_design(2)))
    expect_snapshot(showPoly(splat_design(3)))
})

test_that("showParams() lists every parameter used so far", {
    params <- showParams(splat_fitted("default"))
    expect_s3_class(params, "data.frame")
    expect_identical(names(params), c("parameters", "value"))
    expect_true(all(
        c("poly_degree", "p_value", "offset", "link", "distribution") %in%
            params$parameters
    ))
    expect_false("log_offset" %in% params$parameters)
    expect_snapshot(params)
})

test_that("show*() accessors have no view or return argument", {
    fns <- list(
        showCoeff = showCoeff, showGroupCoeff = showGroupCoeff,
        showInflu = showInflu, showParams = showParams,
        showSigProf = showSigProf, showSol = showSol, showTS = showTS
    )
    for (fn in names(fns)) {
        expect_false(any(c("view", "return") %in% names(formals(fns[[fn]]))),
            label = fn
        )
    }
    # Calls that still pass view = or return = now fail
    expect_error(showCoeff(splat_fitted("default"), view = TRUE))
})
