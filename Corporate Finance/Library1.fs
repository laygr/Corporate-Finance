namespace CorporateFinance

module Investment =
        let npv values rate =
            values
            |> List.mapi (fun i v -> v / (rate + 1.) ** (float i))
            |> List.sum

        let pv value rate time =
            value / (1. + rate) ** time

module BlackAndScholes =
    open Investment

    let blackAndScholes currentPrice exerciseValue annualVolatility timeToMaturity riskFreeRate =
        let pv = pv exerciseValue riskFreeRate timeToMaturity

        let d1 = 
            log(currentPrice / pv) / (annualVolatility * sqrt(timeToMaturity))
            + annualVolatility * sqrt(timeToMaturity) / 2.

        let d2 =
            d1 - annualVolatility * sqrt(timeToMaturity)

        let normEst = new MathNet.Numerics.Distributions.Normal()

        let n1 = normEst.CumulativeDistribution(d1)
        let n2 = normEst.CumulativeDistribution(d2)
        currentPrice * n1 - pv * n2

module Options =
    module Binomial =
        
        let replicaPortfolio callWhenUp callWhenDown priceWhenUp priceWhenDown stockPrice riskFreeRate : float=
            let numberOfStocksToBuy = (callWhenUp - callWhenDown) / (priceWhenUp - priceWhenDown)
            let shortedRiskFree = (callWhenDown - priceWhenDown * numberOfStocksToBuy) / (1. + riskFreeRate)
            numberOfStocksToBuy * stockPrice + shortedRiskFree

        type OptionType =
             | Abandonment of float
             | Contraction of float * float //reduction percentage, extra flow
             | Expansion of float * float //expansion percentage, cost
             | None

        let valueForOption value pv option =
            match option with
            | Abandonment abandonment -> max pv abandonment
            | Contraction (reductionP, extraFlow) -> max pv (value * (1.-reductionP) + extraFlow)
            | Expansion (expansionP, cost) -> max pv (value * (1. + expansionP) - cost)
            | None -> value

        let valueForOptions value pv options =
            List.map (valueForOption value pv) options
            |> List.max

        let rec optionalCallValuation' currentPrice multiplierWhenUp multiplierWhenDown exerciseValue riskFreeRate periods remainingPeriods options =
            if remainingPeriods = 0
            then
                let presentValue = (currentPrice - exerciseValue)
                valueForOptions currentPrice presentValue options
            else

            let priceWhenUp = currentPrice * multiplierWhenUp
            let priceWhenDown = currentPrice * multiplierWhenDown

            let alternativePayoffForUp = optionalCallValuation' priceWhenUp multiplierWhenUp multiplierWhenDown exerciseValue riskFreeRate periods (remainingPeriods - 1) options
            let alternativePayoffForDown = optionalCallValuation' priceWhenDown multiplierWhenUp multiplierWhenDown exerciseValue riskFreeRate periods (remainingPeriods - 1) options

            let callWhenUp = max (priceWhenUp - exerciseValue) alternativePayoffForUp
            let callWhenDown = max (priceWhenDown - exerciseValue) alternativePayoffForDown

            let presentValue = replicaPortfolio callWhenUp callWhenDown priceWhenUp priceWhenDown currentPrice riskFreeRate
            if periods = remainingPeriods
            then presentValue
            else valueForOptions currentPrice presentValue options

        let optionalCallValuation currentPrice multiplierWhenUp multiplierWhenDown exerciseValue riskFreeRate periods options =
            optionalCallValuation' currentPrice multiplierWhenUp multiplierWhenDown exerciseValue riskFreeRate periods periods options

    open BlackAndScholes
    open Investment
    let callValue currentPrice exerciseValue div wacc riskFreeRate stdDev time =
        let actualValue = currentPrice - (pv div wacc time) // currentPrice without dividends
        blackAndScholes actualValue exerciseValue stdDev time riskFreeRate
