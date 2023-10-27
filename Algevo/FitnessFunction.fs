namespace Algevo

open System.Collections

type FitnessFunction =
    val minValue: int64;
    val maxValue: int64;
    val eval: int64 -> float32;

    new(minValue: int64, maxValue: int64, eval: int64 -> float32) =
        if minValue > maxValue then
            failwith "minValue must be less than or equal to maxValue"
        {
            minValue = minValue;
            maxValue = maxValue;
            eval = eval
        }

    member this.GenotypeSize =
        let mutable result = 0
        let mutable value = this.maxValue - this.minValue
        while value > 0L do
            value <- value >>> 1
            result <- result + 1
        result

    member this.Evaluate(genotype: Genotype) =
        let mutable value = this.minValue + genotype.AsLong()
        if value > this.maxValue then
            value <- this.maxValue
        this.eval(value)
