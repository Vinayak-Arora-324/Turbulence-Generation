GenerateGaussianCore[seed_Integer, (length_Integer)?Positive, 
     OptionsPattern[]] := Module[{method, signal}, 
     method = OptionValue["RandomMethod"]; 
      signal = BlockRandom[SeedRandom[seed, Method -> method]; 
         RandomVariate[NormalDistribution[0., 1.], length]]; 
      Association["Model" -> "GaussianReference", "Seed" -> seed, 
       "Length" -> length, "Signal" -> signal, "Innovations" -> signal, 
       "Activity" -> signal^2, "ConditionalScale" -> ConstantArray[1., 
         length], "Parameters" -> Association["Distribution" -> 
          "NormalDistribution[0,1]", "Independent" -> True, 
         "RandomMethod" -> method]]]
 
Options[GenerateGaussianCore] = {"RandomMethod" -> "MersenneTwister"}
 
GaussianMarketAdapter[core_Association, OptionsPattern[]] := 
    Module[{initialPrice, annualizedLogDrift, annualizedVolatility, 
      periodsPerYear, periodDrift, periodScale, returns, prices}, 
     initialPrice = N[OptionValue["InitialPrice"]]; annualizedLogDrift = 
       N[OptionValue["AnnualizedLogDrift"]]; annualizedVolatility = 
       N[OptionValue["AnnualizedVolatility"]]; periodsPerYear = 
       OptionValue["PeriodsPerYear"]; periodDrift = annualizedLogDrift/
        periodsPerYear; periodScale = annualizedVolatility/
        Sqrt[periodsPerYear]; returns = periodDrift + 
        periodScale*core["Signal"]; prices = 
       Exp[Log[initialPrice] + Accumulate[returns]]; 
      Join[core, Association["Adapter" -> "Market", "Returns" -> returns, 
        "Prices" -> prices, "Volatility" -> ConstantArray[periodScale, 
          Length[returns]], "RelativeVolatility" -> ConstantArray[1., 
          Length[returns]], "MarketParameters" -> Association[
          "InitialPrice" -> initialPrice, "AnnualizedLogDrift" -> 
           annualizedLogDrift, "AnnualizedVolatility" -> 
           annualizedVolatility, "PeriodsPerYear" -> periodsPerYear, 
          "PeriodScale" -> periodScale]]]]
 
Options[GaussianMarketAdapter] = {"InitialPrice" -> 100., 
     "AnnualizedLogDrift" -> 0., "AnnualizedVolatility" -> 0.2, 
     "PeriodsPerYear" -> 252}
 
AddIndependentPeakFrequency[baseCore_Association, OptionsPattern[]] := 
    Module[{peakProbability, peakAmplification, eventSeed, randomMethod, 
      length, peakIndicator, varianceNormalization, activityMultiplier, 
      peakSignal}, peakProbability = OptionValue["PeakProbability"]; 
      peakAmplification = N[OptionValue["PeakAmplification"]]; 
      eventSeed = OptionValue["EventSeed"]; randomMethod = 
       OptionValue["RandomMethod"]; length = baseCore["Length"]; 
      peakIndicator = BlockRandom[SeedRandom[eventSeed, 
          Method -> randomMethod]; RandomVariate[BernoulliDistribution[
           peakProbability], length]]; varianceNormalization = 
       Sqrt[(1. - peakProbability) + peakProbability*peakAmplification^2]; 
      activityMultiplier = (1. + (peakAmplification - 1.)*peakIndicator)/
        varianceNormalization; peakSignal = activityMultiplier*
        baseCore["Signal"]; Join[baseCore, Association[
        "Model" -> "OneDimensionalTurbulence", "Stage" -> 
         "IndependentPeakFrequency", "Version" -> "0.2", 
        "ParentStage" -> "GaussianBase", "GaussianSource" -> 
         baseCore["Signal"], "Signal" -> peakSignal, 
        "Activity" -> peakSignal^2, "PeakIndicator" -> peakIndicator, 
        "ConditionalScale" -> activityMultiplier, "Parameters" -> 
         Join[baseCore["Parameters"], Association["PeakProbability" -> 
            peakProbability, "PeakAmplification" -> peakAmplification, 
           "EventSeed" -> eventSeed, "VarianceNormalization" -> 
            varianceNormalization, "TheoreticalSignalVariance" -> 1.]]]]]
 
Options[AddIndependentPeakFrequency] = {"PeakProbability" -> 0.02, 
     "PeakAmplification" -> 3., "EventSeed" -> 20260818, 
     "RandomMethod" -> "MersenneTwister"}
 
AddPeakSeverityDistribution[peakCore_Association, OptionsPattern[]] := 
    Module[{severityLogMean, severityLogStandardDeviation, severitySeed, 
      randomMethod, length, peakProbability, peakIndicator, gaussianSource, 
      severityExcess, severityMultiplier, meanSeverityExcess, 
      secondMomentSeverityExcess, eventSeveritySecondMoment, 
      varianceNormalization, relativeActivityScale, severitySignal}, 
     severityLogMean = N[OptionValue["SeverityLogMean"]]; 
      severityLogStandardDeviation = 
       N[OptionValue["SeverityLogStandardDeviation"]]; 
      severitySeed = OptionValue["SeveritySeed"]; 
      randomMethod = OptionValue["RandomMethod"]; 
      length = peakCore["Length"]; peakProbability = peakCore["Parameters"][
        "PeakProbability"]; peakIndicator = peakCore["PeakIndicator"]; 
      gaussianSource = peakCore["GaussianSource"]; severityExcess = 
       BlockRandom[SeedRandom[severitySeed, Method -> randomMethod]; 
         RandomVariate[LogNormalDistribution[severityLogMean, 
           severityLogStandardDeviation], length]]; severityMultiplier = 
       1. + peakIndicator*severityExcess; meanSeverityExcess = 
       Exp[severityLogMean + severityLogStandardDeviation^2/2.]; 
      secondMomentSeverityExcess = Exp[2.*severityLogMean + 
         2.*severityLogStandardDeviation^2]; eventSeveritySecondMoment = 
       1. + 2.*meanSeverityExcess + secondMomentSeverityExcess; 
      varianceNormalization = Sqrt[(1. - peakProbability) + 
         peakProbability*eventSeveritySecondMoment]; 
      relativeActivityScale = severityMultiplier/varianceNormalization; 
      severitySignal = relativeActivityScale*gaussianSource; 
      Join[peakCore, Association["Model" -> "OneDimensionalTurbulence", 
        "Stage" -> "PeakSeverityDistribution", "Version" -> "0.3", 
        "ParentStage" -> "IndependentPeakFrequency", 
        "Signal" -> severitySignal, "Activity" -> severitySignal^2, 
        "PeakSeverityExcess" -> severityExcess, "PeakSeverityMultiplier" -> 
         severityMultiplier, "ConditionalScale" -> relativeActivityScale, 
        "Parameters" -> Join[peakCore["Parameters"], Association[
           "SeverityDistribution" -> "1 + LogNormalDistribution[mu,sigma]", 
           "SeverityLogMean" -> severityLogMean, 
           "SeverityLogStandardDeviation" -> severityLogStandardDeviation, 
           "SeveritySeed" -> severitySeed, "ExpectedEventSeverity" -> 
            1. + meanSeverityExcess, "ExpectedEventSeverityRMS" -> 
            Sqrt[eventSeveritySecondMoment], "VarianceNormalization" -> 
            varianceNormalization, "TheoreticalSignalVariance" -> 1.]]]]]
 
Options[AddPeakSeverityDistribution] = {"SeverityLogMean" -> 0.44, 
     "SeverityLogStandardDeviation" -> 0.55, "SeveritySeed" -> 20260819, 
     "RandomMethod" -> "MersenneTwister"}
 
AddPersistentPeakEpisodes[severityCore_Association, OptionsPattern[]] := 
    Module[{persistence, peakProbability, severityLogMean, 
      severityLogStandardDeviation, peakIndicator, severityExcess, 
      gaussianSource, peakImpulse, meanSeverityExcess, 
      secondMomentSeverityExcess, meanImpulse, varianceImpulse, 
      stationaryMeanActivity, stationaryVarianceActivity, 
      stationarySecondMomentActivity, varianceNormalization, initialActivity, 
      persistentActivity, relativeActivityScale, persistentSignal, 
      activityHalfLife}, persistence = N[OptionValue["Persistence"]]; 
      If[persistence < 0. || persistence >= 1., 
       Return[Failure["InvalidPersistence", Association[
          "Message" -> "Persistence must satisfy 0 <= rho < 1."]]]]; 
      peakProbability = severityCore["Parameters"]["PeakProbability"]; 
      severityLogMean = severityCore["Parameters"]["SeverityLogMean"]; 
      severityLogStandardDeviation = severityCore["Parameters"][
        "SeverityLogStandardDeviation"]; peakIndicator = 
       severityCore["PeakIndicator"]; severityExcess = 
       severityCore["PeakSeverityExcess"]; gaussianSource = 
       severityCore["GaussianSource"]; peakImpulse = 
       peakIndicator*severityExcess; meanSeverityExcess = 
       Exp[severityLogMean + severityLogStandardDeviation^2/2.]; 
      secondMomentSeverityExcess = Exp[2.*severityLogMean + 
         2.*severityLogStandardDeviation^2]; meanImpulse = 
       peakProbability*meanSeverityExcess; varianceImpulse = 
       peakProbability*secondMomentSeverityExcess - meanImpulse^2; 
      stationaryMeanActivity = meanImpulse/(1. - persistence); 
      stationaryVarianceActivity = varianceImpulse/(1. - persistence^2); 
      stationarySecondMomentActivity = stationaryVarianceActivity + 
        stationaryMeanActivity^2; varianceNormalization = 
       Sqrt[1. + 2.*stationaryMeanActivity + stationarySecondMomentActivity]; 
      initialActivity = stationaryMeanActivity; persistentActivity = 
       Rest[FoldList[persistence*#1 + #2 & , initialActivity, peakImpulse]]; 
      relativeActivityScale = (1. + persistentActivity)/
        varianceNormalization; persistentSignal = relativeActivityScale*
        gaussianSource; activityHalfLife = If[persistence == 0., 0., 
        Log[0.5]/Log[persistence]]; Join[severityCore, 
       Association["Model" -> "OneDimensionalTurbulence", 
        "Stage" -> "PersistentPeakEpisodes", "Version" -> "0.4", 
        "ParentStage" -> "PeakSeverityDistribution", 
        "Signal" -> persistentSignal, "Activity" -> persistentSignal^2, 
        "PeakImpulse" -> peakImpulse, "PersistentActivity" -> 
         persistentActivity, "ConditionalScale" -> relativeActivityScale, 
        "Parameters" -> Join[severityCore["Parameters"], 
          Association["Persistence" -> persistence, "ActivityHalfLife" -> 
            activityHalfLife, "StationaryMeanActivity" -> 
            stationaryMeanActivity, "StationaryVarianceActivity" -> 
            stationaryVarianceActivity, "VarianceNormalization" -> 
            varianceNormalization, "TheoreticalSignalVariance" -> 1.]]]]]
 
Options[AddPersistentPeakEpisodes] = {"Persistence" -> 0.85}
 
AddDrivenLinearDissipativeCascade[persistentCore_Association, 
     OptionsPattern[]] := Module[{transferRates, dissipationRates, 
      observationWeights, activityCoupling, momentIterations, numberOfScales, 
      peakProbability, severityLogMean, severityLogStandardDeviation, 
      peakImpulse, gaussianSource, meanSeverityExcess, 
      secondMomentSeverityExcess, meanImpulse, varianceImpulse, 
      transitionMatrix, forcingVector, innovationCovariance, 
      stationaryMeanState, stationaryCovariance, linearScaleStates, 
      linearTransferFluxes, dissipationFluxes, weightedScaleActivity, 
      stationaryMeanActivity, stationaryVarianceActivity, 
      stationarySecondMomentActivity, varianceNormalization, 
      relativeActivityScale, linearCascadeSignal, baseParameters}, 
     transferRates = N[OptionValue["TransferRates"]]; 
      dissipationRates = N[OptionValue["DissipationRates"]]; 
      observationWeights = N[OptionValue["ObservationWeights"]]; 
      activityCoupling = N[OptionValue["ActivityCoupling"]]; 
      momentIterations = OptionValue["MomentIterations"]; 
      numberOfScales = Length[dissipationRates]; 
      If[Length[transferRates] != numberOfScales - 1, 
       Return[Failure["InvalidTransferRates", Association[
          "Message" -> 
           "TransferRates must have one fewer element than DissipationRates."]\
]]]; If[Length[observationWeights] != numberOfScales, 
       Return[Failure["InvalidObservationWeights", Association[
          "Message" -> 
           "ObservationWeights must have one element per scale."]]]]; 
      If[Total[observationWeights] <= 0., 
       Return[Failure["InvalidObservationWeights", Association[
          "Message" -> "ObservationWeights must have a positive total."]]]]; 
      If[ !AllTrue[Join[transferRates, dissipationRates], #1 >= 0. & ], 
       Return[Failure["NegativeRate", Association["Message" -> 
           "Transfer and dissipation rates must be non-negative."]]]]; 
      If[ !AllTrue[MapThread[Plus, {dissipationRates, Append[transferRates, 
            0.]}], #1 <= 1. & ], Return[Failure["InvalidActivityBudget", 
         Association["Message" -> 
           "Dissipation plus outgoing transfer cannot exceed one at any \
scale."]]]]; observationWeights = observationWeights/
        Total[observationWeights]; peakProbability = 
       persistentCore["Parameters"]["PeakProbability"]; 
      severityLogMean = persistentCore["Parameters"]["SeverityLogMean"]; 
      severityLogStandardDeviation = persistentCore["Parameters"][
        "SeverityLogStandardDeviation"]; peakImpulse = 
       persistentCore["PeakImpulse"]; gaussianSource = 
       persistentCore["GaussianSource"]; meanSeverityExcess = 
       Exp[severityLogMean + severityLogStandardDeviation^2/2.]; 
      secondMomentSeverityExcess = Exp[2.*severityLogMean + 
         2.*severityLogStandardDeviation^2]; meanImpulse = 
       peakProbability*meanSeverityExcess; varianceImpulse = 
       peakProbability*secondMomentSeverityExcess - meanImpulse^2; 
      transitionMatrix = DiagonalMatrix[1. - dissipationRates - 
         Append[transferRates, 0.]]; Do[transitionMatrix[[scale + 1,scale]] = 
        transferRates[[scale]], {scale, 1, numberOfScales - 1}]; 
      If[Max[Abs[Eigenvalues[transitionMatrix]]] >= 1., 
       Return[Failure["UnstableTransitionMatrix", Association[
          "Message" -> 
           "The cascade transition matrix is not stationary."]]]]; 
      forcingVector = UnitVector[numberOfScales, 1]; 
      stationaryMeanState = LinearSolve[IdentityMatrix[numberOfScales] - 
         transitionMatrix, forcingVector*meanImpulse]; 
      innovationCovariance = varianceImpulse*Outer[Times, forcingVector, 
         forcingVector]; stationaryCovariance = 
       Nest[transitionMatrix . #1 . Transpose[transitionMatrix] + 
          innovationCovariance & , ConstantArray[0., {numberOfScales, 
          numberOfScales}], momentIterations]; linearScaleStates = 
       Rest[FoldList[transitionMatrix . #1 + forcingVector*#2 & , 
         stationaryMeanState, peakImpulse]]; linearTransferFluxes = 
       (#1*transferRates & ) /@ linearScaleStates[[All,
         1 ;; numberOfScales - 1]]; dissipationFluxes = 
       (#1*dissipationRates & ) /@ linearScaleStates; 
      weightedScaleActivity = linearScaleStates . observationWeights; 
      stationaryMeanActivity = observationWeights . stationaryMeanState; 
      stationaryVarianceActivity = observationWeights . 
        stationaryCovariance . observationWeights; 
      stationarySecondMomentActivity = stationaryVarianceActivity + 
        stationaryMeanActivity^2; varianceNormalization = 
       Sqrt[1. + 2.*activityCoupling*stationaryMeanActivity + 
         activityCoupling^2*stationarySecondMomentActivity]; 
      relativeActivityScale = (1. + activityCoupling*weightedScaleActivity)/
        varianceNormalization; linearCascadeSignal = relativeActivityScale*
        gaussianSource; baseParameters = KeyDrop[persistentCore[
         "Parameters"], {"Persistence", "ActivityHalfLife", 
         "StationaryMeanActivity", "StationaryVarianceActivity", 
         "VarianceNormalization", "TheoreticalSignalVariance"}]; 
      Join[KeyDrop[persistentCore, {"PersistentActivity", "ConditionalScale", 
         "Signal", "Activity"}], Association["Model" -> 
         "OneDimensionalTurbulence", "Stage" -> 
         "DrivenLinearDissipativeCascade", "Version" -> "0.5", 
        "ParentStage" -> "PersistentPeakEpisodes", "ForcingType" -> 
         "ExternallyDriven", "TransferType" -> "Linear", 
        "DissipationType" -> "LinearScaleDependent", 
        "Signal" -> linearCascadeSignal, "Activity" -> linearCascadeSignal^2, 
        "ScaleStates" -> linearScaleStates, "TransferFluxes" -> 
         linearTransferFluxes, "DissipationFluxes" -> dissipationFluxes, 
        "CascadeActivity" -> weightedScaleActivity, "ConditionalScale" -> 
         relativeActivityScale, "Parameters" -> Join[baseParameters, 
          Association["NumberOfScales" -> numberOfScales, "TransferRates" -> 
            transferRates, "DissipationRates" -> dissipationRates, 
           "ObservationWeights" -> observationWeights, "ActivityCoupling" -> 
            activityCoupling, "StationaryMeanScaleState" -> 
            stationaryMeanState, "StationaryMeanCascadeActivity" -> 
            stationaryMeanActivity, "StationaryVarianceCascadeActivity" -> 
            stationaryVarianceActivity, "VarianceNormalization" -> 
            varianceNormalization, "TheoreticalSignalVariance" -> 1.]]]]]
 
Options[AddDrivenLinearDissipativeCascade] = 
    {"TransferRates" -> {0.08, 0.16, 0.28}, "DissipationRates" -> 
      {0.01, 0.03, 0.08, 0.35}, "ObservationWeights" -> {0.1, 0.2, 0.3, 0.4}, 
     "ActivityCoupling" -> 2., "MomentIterations" -> 1000}
 
AddDrivenNonlinearMultiscaleCascade[linearCore_Association, 
     OptionsPattern[]] := Module[{maximumTransferFractions, 
      transferThresholds, dissipationRates, observationWeights, 
      fineDissipationWeight, activityReference, activityCoupling, 
      numberOfScales, coarseForcing, gaussianSource, initialState, 
      nonlinearFlux, cascadeStep, statePath, scaleStates, transferFluxes, 
      dissipationFluxes, fineScaleDissipation, weightedScaleEnergy, 
      cascadeActivity, boundedCascadeActivity, relativeActivityScale, 
      nonlinearSignal, baseParameters}, maximumTransferFractions = 
       N[OptionValue["MaximumTransferFractions"]]; transferThresholds = 
       N[OptionValue["TransferThresholds"]]; dissipationRates = 
       N[OptionValue["DissipationRates"]]; observationWeights = 
       N[OptionValue["ObservationWeights"]]; fineDissipationWeight = 
       N[OptionValue["FineDissipationWeight"]]; activityReference = 
       N[OptionValue["ActivityReference"]]; activityCoupling = 
       N[OptionValue["ActivityCoupling"]]; numberOfScales = 
       Length[dissipationRates]; If[Length[maximumTransferFractions] != 
         numberOfScales - 1 || Length[transferThresholds] != 
         numberOfScales - 1 || Length[observationWeights] != numberOfScales, 
       Return[Failure["InvalidCascadeDimensions", Association[
          "Message" -> "Transfer fractions and thresholds must have one fewer \
element than the number of scales."]]]]; 
      If[ !AllTrue[transferThresholds, #1 > 0. & ], 
       Return[Failure["InvalidTransferThreshold", Association[
          "Message" -> "Every transfer threshold must be positive."]]]]; 
      If[ !AllTrue[Join[maximumTransferFractions, dissipationRates], 
         #1 >= 0. & ], Return[Failure["NegativeRate", 
         Association["Message" -> 
           "Transfer fractions and dissipation rates must be non-negative."]]]\
]; If[ !AllTrue[MapThread[Plus, {dissipationRates, 
           Append[maximumTransferFractions, 0.]}], #1 <= 1. & ], 
       Return[Failure["InvalidActivityBudget", Association[
          "Message" -> 
           "Maximum transfer plus dissipation cannot exceed one at any \
scale."]]]]; If[Total[observationWeights] <= 0. || activityReference <= 0., 
       Return[Failure["InvalidObservationMap", Association[
          "Message" -> 
           "Observation weights and ActivityReference must be positive."]]]]; 
      observationWeights = observationWeights/Total[observationWeights]; 
      coarseForcing = linearCore["PeakImpulse"]; gaussianSource = 
       linearCore["GaussianSource"]; initialState = ConstantArray[0., 
        numberOfScales]; nonlinearFlux = Function[state, 
        MapThread[Function[{energy, maximumFraction, threshold}, 
          maximumFraction*energy*(Sqrt[energy/threshold]/
            (1. + Sqrt[energy/threshold]))], {Most[state], 
          maximumTransferFractions, transferThresholds}]]; 
      cascadeStep = Function[{state, forcing}, 
        Module[{outgoingFlux, dissipation, nextState}, 
         outgoingFlux = nonlinearFlux[state]; dissipation = 
           dissipationRates*state; nextState = state - dissipation - 
            Append[outgoingFlux, 0.] + Prepend[outgoingFlux, 0.]; 
          nextState[[1]] += forcing; (Max[0., #1] & ) /@ nextState]]; 
      statePath = FoldList[cascadeStep, initialState, coarseForcing]; 
      scaleStates = Rest[statePath]; transferFluxes = 
       nonlinearFlux /@ scaleStates; dissipationFluxes = 
       (dissipationRates*#1 & ) /@ scaleStates; fineScaleDissipation = 
       dissipationFluxes[[All,-1]]; weightedScaleEnergy = 
       scaleStates . observationWeights; cascadeActivity = 
       weightedScaleEnergy + fineDissipationWeight*fineScaleDissipation; 
      boundedCascadeActivity = cascadeActivity/(activityReference + 
         cascadeActivity); relativeActivityScale = 
       1. + activityCoupling*boundedCascadeActivity; 
      nonlinearSignal = relativeActivityScale*gaussianSource; 
      baseParameters = KeyDrop[linearCore["Parameters"], 
        {"TransferRates", "StationaryMeanScaleState", 
         "StationaryMeanCascadeActivity", 
         "StationaryVarianceCascadeActivity", "VarianceNormalization", 
         "TheoreticalSignalVariance"}]; Join[KeyDrop[linearCore, 
        {"ScaleStates", "TransferFluxes", "DissipationFluxes", 
         "CascadeActivity", "ConditionalScale", "Signal", "Activity"}], 
       Association["Model" -> "OneDimensionalTurbulence", 
        "Stage" -> "DrivenNonlinearMultiscaleCascade", "Version" -> "0.6", 
        "ParentStage" -> "DrivenLinearDissipativeCascade", 
        "ForcingType" -> "ExternallyDriven", "TransferType" -> 
         "BoundedNonlinearE3Over2", "DissipationType" -> 
         "LinearScaleDependent", "Signal" -> nonlinearSignal, 
        "Activity" -> nonlinearSignal^2, "ScaleStates" -> scaleStates, 
        "TransferFluxes" -> transferFluxes, "DissipationFluxes" -> 
         dissipationFluxes, "CascadeActivity" -> cascadeActivity, 
        "ConditionalScale" -> relativeActivityScale, 
        "FineScaleDissipation" -> fineScaleDissipation, 
        "BoundedCascadeActivity" -> boundedCascadeActivity, 
        "Parameters" -> Join[baseParameters, Association["NumberOfScales" -> 
            numberOfScales, "MaximumTransferFractions" -> 
            maximumTransferFractions, "TransferThresholds" -> 
            transferThresholds, "DissipationRates" -> dissipationRates, 
           "ObservationWeights" -> observationWeights, 
           "FineDissipationWeight" -> fineDissipationWeight, 
           "ActivityReference" -> activityReference, "ActivityCoupling" -> 
            activityCoupling, "SignalVarianceNormalization" -> "None"]]]]]
 
Options[AddDrivenNonlinearMultiscaleCascade] = 
    {"MaximumTransferFractions" -> {0.25, 0.4, 0.55}, 
     "TransferThresholds" -> {0.35, 0.14, 0.06}, "DissipationRates" -> 
      {0.01, 0.03, 0.08, 0.45}, "ObservationWeights" -> 
      {0.1, 0.15, 0.25, 0.5}, "FineDissipationWeight" -> 2., 
     "ActivityReference" -> 0.15, "ActivityCoupling" -> 3.}
