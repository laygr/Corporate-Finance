namespace CorporateFinance.Tests

module Options =

    open NUnit.Framework
    open FsUnit

    open CorporateFinance.Options.Binomial

    [<Test>]
    let abandonment() =
        let option = Abandonment 190.
        let currentPrice, multiplierWhenUp, multiplierWhenDown, exerciseValue, riskFreeRate, periods = 200., 1.25, 0.8, 0., 0.05, 2
        let value = optionalCallValuation currentPrice multiplierWhenUp multiplierWhenDown exerciseValue riskFreeRate periods [option]
        value - 212.6984127
        |> should be (lessThan 0.0001)

    [<Test>]
    let contraction() =
        let option = Contraction (0.2, 50.)
        let currentPrice, multiplierWhenUp, multiplierWhenDown, exerciseValue, riskFreeRate, periods = 200., 1.25, 0.8, 0., 0.05, 2
        let value = optionalCallValuation currentPrice multiplierWhenUp multiplierWhenDown exerciseValue riskFreeRate periods [option]
        value - 209.8586266
        |> should be (lessThan 0.0001)

    [<Test>]
    let expansion() =
        let option = Expansion (0.3, 70.)
        let currentPrice, multiplierWhenUp, multiplierWhenDown, exerciseValue, riskFreeRate, periods = 200., 1.25, 0.8, 0., 0.05, 2
        let value = optionalCallValuation currentPrice multiplierWhenUp multiplierWhenDown exerciseValue riskFreeRate periods [option]
        value - 206.64875
        |> should be (lessThan 0.0001)

    [<Test>]
    let ``multiples opciones``() =
        let abandonment = Abandonment 190.
        let contraction = Contraction (0.2, 50.)
        let expansion = Expansion (0.3, 70.)
        let options = [abandonment; contraction; expansion]
        let currentPrice, multiplierWhenUp, multiplierWhenDown, exerciseValue, riskFreeRate, periods = 200., 1.25, 0.8, 0., 0.05, 2
        let value = optionalCallValuation currentPrice multiplierWhenUp multiplierWhenDown exerciseValue riskFreeRate periods options
        value - 222.2362196
        |> should be (lessThan 0.0001)
    

    open CorporateFinance.Options
    [<Test>]
    let ``ejercicio black and scholes``() =
        let wacc = 0.12
        let riskFreeRate = 0.05
        let stdDev = 0.4
        let div = 600000.
        let currentPrice = 6000000.
        let exerciseValue = 5000000.
        let time = 1.
        let v = callValue currentPrice exerciseValue div wacc riskFreeRate stdDev time
        v - 1208473.5232
        |> should be (lessThan 0.0001)