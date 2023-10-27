namespace Algevo

open System

type Population =
    val mutable private Genotypes: Genotype array
    val private LastFitnessValues: float32 array
    val private MutationChance: float32
    val private CrossoverChance: float32
    val private FitnessFunction: FitnessFunction

    new(
        size: int,
        mutationChance: float32,
        crossoverChance: float32,
        fitnessFunction: FitnessFunction
    ) =
        if size < 2 then
            failwith "Population must have at least two genotypes"
        if mutationChance < 0.0f || mutationChance > 1.0f then
            failwith "Mutation chance must be between 0.0 and 1.0"
        if crossoverChance < 0.0f || crossoverChance > 1.0f then
            failwith "Crossover chance must be between 0.0 and 1.0"
        {
            Genotypes = [| for _ in 1 .. size -> Genotype.Random(fitnessFunction.GenotypeSize) |];
            LastFitnessValues = [| for _ in 1 .. size -> 0.0f |];
            MutationChance = mutationChance;
            CrossoverChance = crossoverChance;
            FitnessFunction = fitnessFunction;
        }

    member this.BestFitness =
        let mutable bestFitness = this.LastFitnessValues.[0]
        for i in 0 .. this.Genotypes.Length - 1 do
            if this.LastFitnessValues.[i] > bestFitness then
                bestFitness <- this.LastFitnessValues.[i]
        bestFitness

    member this.AverageFitness =
        let mutable totalFitness = this.LastFitnessValues.[0]
        for i in 0 .. this.Genotypes.Length - 1 do
            totalFitness <- totalFitness + this.LastFitnessValues.[i]
        totalFitness / float32 this.Genotypes.Length

    member this.WorstFitness =
        let mutable worstFitness = this.LastFitnessValues.[0]
        for i in 1 .. this.Genotypes.Length - 1 do
            if this.LastFitnessValues.[i] < worstFitness then
                worstFitness <- this.LastFitnessValues.[i]
        worstFitness

    member private this.Evaluate() =
        for i in 0 .. this.Genotypes.Length - 1 do
            this.LastFitnessValues.[i] <- this.FitnessFunction.Evaluate(this.Genotypes.[i])
    
    member private this.LastFitnessValueWithOffset(index: int) =
        let minVal = this.LastFitnessValues |> Array.min
        this.LastFitnessValues.[index] - minVal

    member private this.SelectWeightedRandom() =
        let mutable totalFitness = 0.0f
        for i in 0 .. this.Genotypes.Length - 1 do
            totalFitness <- totalFitness + this.LastFitnessValueWithOffset(i)
        let mutable randomValue = Random.Shared.NextSingle() * totalFitness
        let mutable index = 0
        while randomValue > 0.0f do
            randomValue <- randomValue - this.LastFitnessValueWithOffset(index)
            index <- index + 1
        index <- index - 1
        this.Genotypes.[index]

    member private this.Evolve() =
        for i in 0 .. this.Genotypes.Length - 1 do
            let mutable genotype = this.Genotypes.[i]
            genotype <- genotype.BitFlipMutation(this.MutationChance)
            if Random.Shared.NextSingle() < this.CrossoverChance then
                genotype <- Genotype.OnePointCrossover(this.SelectWeightedRandom(), this.SelectWeightedRandom())
            this.Genotypes.[i] <- genotype

    member this.RunGenerations(generations: int, report: int -> unit) =
        for i in 1 .. generations do
            this.Evaluate()
            this.Evolve()
            report(i)
        this.Evaluate()
        let mutable bestFitness = 0.0f
        let mutable bestGenotype = this.Genotypes.[0]
        for i in 0 .. this.Genotypes.Length - 1 do
            if this.LastFitnessValues.[i] > bestFitness then
                bestFitness <- this.LastFitnessValues.[i]
                bestGenotype <- this.Genotypes.[i]
