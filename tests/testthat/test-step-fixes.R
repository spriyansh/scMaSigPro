# Wrong results and crashes inside the pipeline steps (roadmap #18b), one
# block per ledger item. Exact messages, no snapshot.

# The design of `ob` with its columns renamed
rename_design <- function(ob, names) {
    m <- predictors(ob)
    colnames(m) <- names
    predictors(ob) <- m
    ob
}

test_that("sc.filter(vars = 'each') gives one list per term (ledger F1)", {
    ob <- splat_fitted("default")
    sol <- showSol(ob)
    terms <- sub("p.valor_", "", colnames(sol)[-(1:2)], fixed = TRUE)
    kept <- rownames(sol)[sol[["R-squared"]] >= 0.7 & sol[["p-value"]] <= 0.05]
    # Every term selects the same genes: apply() returned a matrix
    each <- sc.filter(ob, vars = "each", term_p_value = 1)@Significant@genes
    expect_identical(names(each), terms)
    expect_identical(unname(each), rep(list(kept), length(terms)))
    # No term selects a gene
    none <- sc.filter(ob, vars = "each", term_p_value = 0)@Significant@genes
    expect_identical(none, setNames(rep(list(character(0)), 6), terms))
    expect_identical(
        sc.filter(ob, vars = "each", rsq = 1.1)@Significant@genes, none
    )
    # Control: terms that select different numbers of genes
    some <- sc.filter(ob, vars = "each")@Significant@genes
    expect_identical(
        lengths(some), setNames(c(31L, 9L, 14L, 23L, 8L, 13L), terms)
    )
})

test_that("sc.t.fit() uses its p_value argument (ledger b)", {
    pv <- splat_fitted("default")
    strict <- sc.t.fit(pv, p_value = 1e-4, verbose = FALSE)
    expect_identical(strict@Parameters@p_value, 1e-4)
    terms <- as.matrix(showSol(strict))[, -(1:2)]
    expect_true(all(terms <= 1e-4, na.rm = TRUE))
    expect_false(identical(strict@Estimate, pv@Estimate))
    # Control: the stored level is the default
    same <- sc.t.fit(pv, p_value = pv@Parameters@p_value, verbose = FALSE)
    expect_identical(same@Estimate, pv@Estimate)
    # At level 0 no gene keeps a coefficient
    expect_error(sc.t.fit(pv, p_value = 0, verbose = FALSE),
        "No gene has a coefficient significant at p_value = 0.",
        fixed = TRUE
    )
})

test_that("sc.p.vector() records a run without genes (ledger B8)", {
    design <- splat_design(2)
    expect_message(
        none <- sc.p.vector(design, p_value = 1e-300, verbose = FALSE),
        "No significant genes detected. Try changing parameters.",
        fixed = TRUE
    )
    expect_identical(none@Profile@non_flat, character(0))
    expect_identical(none@Profile@p_values, splat_fitted()@Profile@p_values)
    expect_identical(none@Parameters@p_value, 1e-300)
    expect_true(validObject(none))
    # A second run replaces the profile of the first, so sc.t.fit() stops
    rerun <- suppressMessages(
        sc.p.vector(splat_fitted(), p_value = 1e-300, verbose = FALSE)
    )
    expect_identical(rerun@Profile, none@Profile)
    expect_error(sc.t.fit(rerun, verbose = FALSE),
        "No gene with a non-flat profile found",
        fixed = TRUE
    )
})

test_that("sc.p.vector() fits one gene left by the filters (ledger Z3)", {
    design <- splat_design(2)
    counts <- eDense(design)
    gene <- rownames(counts)[1]
    full <- sc.p.vector(design, offset = FALSE, verbose = FALSE)
    ob <- design
    # The other genes have only zero counts, then only missing counts
    for (other in c(0, NA)) {
        x <- counts
        x[-1, ] <- other
        suppressWarnings(eDense(ob, "bulk.counts") <- x)
        one <- suppressMessages(
            sc.p.vector(ob, offset = FALSE, verbose = FALSE)
        )
        expect_identical(one@Profile@p_values, full@Profile@p_values[gene])
    }
})

test_that("the fits do not depend on the design names (ledger N1)", {
    design <- splat_design(2)
    p <- ncol(predictors(design))
    ref <- list(backward = splat_fitted("default"))
    ref$two.ways.forward <- sc.t.fit(ref$backward,
        selection_method = "two.ways.forward", verbose = FALSE
    )
    for (nm in list(
        paste0("term ", seq_len(p), "|x"),
        rep("t", p),
        replace(colnames(predictors(design)), 2, "y"),
        replace(colnames(predictors(design)), 2, "(Intercept)"),
        paste0("t", seq_len(p))
    )) {
        pv <- sc.p.vector(rename_design(design, nm), verbose = FALSE)
        expect_identical(pv@Profile, ref$backward@Profile, info = nm[2])
        for (m in names(ref)) {
            tf <- sc.t.fit(pv, selection_method = m, verbose = FALSE)
            info <- paste(nm[2], m)
            expect_identical(
                unname(showSol(tf)), unname(showSol(ref[[m]])),
                info = info
            )
            expect_identical(
                unname(showCoeff(tf)), unname(showCoeff(ref[[m]])),
                info = info
            )
            expect_identical(colnames(showCoeff(tf)),
                c("beta0", paste0("beta", nm)),
                info = info
            )
        }
    }
})

test_that("Mclust names one-component clusters by gene (ledger C1)", {
    skip_if_not_installed("mclust")
    ob <- splat_filtered()
    genes <- rownames(showCoeff(ob))
    genes <- genes[genes %in% Reduce(union, ob@Significant@genes)]
    clusters <- sc.cluster.trend(ob,
        geneSet = "union", k = 1, cluster_method = "Mclust"
    )@Significant@clusters
    expect_identical(clusters, as.list(setNames(rep(1, length(genes)), genes)))
})

test_that("hclust with another distance clusters the genes (ledger S4)", {
    ob <- splat_filtered()
    genes <- Reduce(union, ob@Significant@genes)
    coeff <- as.matrix(showCoeff(ob))
    coeff <- coeff[rownames(coeff) %in% genes, , drop = FALSE]
    coeff[is.na(coeff)] <- 0
    for (method in c("euclidean", "manhattan")) {
        expected <- cutree(
            hclust(dist(coeff, method = method), method = "ward.D"),
            k = 4
        )
        clusters <- sc.cluster.trend(ob,
            geneSet = "union", k = 4, hclust.distance = method
        )@Significant@clusters
        expect_identical(clusters, as.list(expected), info = method)
    }
    # Control: the default correlation distance
    expect_identical(
        sc.cluster.trend(ob, geneSet = "union", k = 4)@Significant@clusters,
        splat_clustered()@Significant@clusters
    )
})
