namespace Algevo

open System.Collections
open System

type Genotype =
    val private Length: int
    val private Genes: BitArray

    new(genes: BitArray) =
        if genes.Length = 0 then
            failwith "Genotype must have at least one gene"
        { Length = genes.Length; Genes = genes }

    member this.AsLong() =
        let mutable result = 0L
        for i in 0 .. this.Length - 1 do
            if this.Genes.[i] then
                result <- result ||| (1L <<< i)
        result

    member this.BitFlipMutation(chance: float32) =
        let mutable result = this.Genes
        for i in 0 .. this.Length - 1 do
            if Random.Shared.NextSingle() < chance then
                result.[i] <- not result.[i]
        Genotype(result)

    static member Random(length: int) =
        let genes = BitArray([| for _ in 1 .. length -> Random.Shared.Next(2) = 1 |])
        Genotype(genes)

    static member OnePointCrossover(parent1: Genotype, parent2: Genotype) =
        let length = parent1.Length
        let mutable child1 = BitArray(length)
        let crossoverPoint = Random.Shared.Next(length)
        for i in 0 .. length - 1 do
            if i < crossoverPoint then
                child1.[i] <- parent1.Genes.[i]
            else
                child1.[i] <- parent2.Genes.[i]
        Genotype(child1)


