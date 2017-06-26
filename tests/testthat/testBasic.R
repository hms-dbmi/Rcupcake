library(testthat)

context("Basic cupcake data")

test_that("Test of object returned from 'psygenetGene' is S4 and 'PsyGeNET.Psy'", {
    load(system.file("extdata", "RcupcakeExData.RData", package="Rcupcake"))
    
    expect_true(isS4(RcupcakeExData), info="'RcupcakeExData' is not a S4 object.")
    expect_is(RcupcakeExData, "cupcakeData", info="'RcupcakeExData' is not an 'DataGeNET.Psy'.")
})
