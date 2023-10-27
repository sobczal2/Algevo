namespace Algevo.Fx

open System
open Algevo
open System.IO


module Program =
    
    let private fitFunc = fun x -> - 0.5f * (float32)x * (float32)x + 10.0f * (float32)x + 13.0f

    [<EntryPoint>]
    let main args =
        let mutable populationSize: int = 0;
        let mutable numberOfGenerations: int = 0;
        let mutable crossoverChance: float32 = 0f;
        let mutable mutationChance: float32 = 0f;
        try
            printfn "Enter population size:";
            populationSize <- Console.ReadLine() |> int;
            printfn "Enter number of generations:";
            numberOfGenerations <- Console.ReadLine() |> int;
            printfn "Enter chance of crossover:";
            crossoverChance <- Console.ReadLine() |> float32;
            printfn "Enter chance of mutation:";
            mutationChance <- Console.ReadLine() |> float32;
        with _ ->
            printfn "Invalid input"
            exit 1

        let fitnessFunction = FitnessFunction(
            minValue = -1L,
            maxValue = 21L,
            eval = fitFunc
        )

        let population = Population(
            size = populationSize,
            mutationChance = mutationChance,
            crossoverChance = crossoverChance,
            fitnessFunction = fitnessFunction
        )


        let mutable bestChartData = []
        let mutable averageChartData = []
        let mutable worstChartData = []

        population.RunGenerations(
            numberOfGenerations,
            fun i ->
                bestChartData <- bestChartData @ [ float population.BestFitness ]
                averageChartData <- averageChartData @ [ float population.AverageFitness ]
                worstChartData <- worstChartData @ [ float population.WorstFitness ]
        )

        let fitnessChartData = [ for i in -1 .. 21 -> fitnessFunction.eval(i) ] |> List.map float

        let docsDir = Path.GetFullPath(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\docs"));
        Directory.CreateDirectory(docsDir) |> ignore

        let xAxis = [ for i in -1 .. 21 -> float i ]

        let plot = new ScottPlot.Plot(800, 600);
        plot.AddScatter(xAxis |> Array.ofList, fitnessChartData |> Array.ofList, System.Drawing.Color.Red, label="Fitness function") |> ignore;

        plot.XLabel("x value");
        plot.YLabel("fitness");
        plot.Title("Fitness function");
        plot.Legend() |> ignore;

        plot.SaveFig(Path.Combine(docsDir, "fitness-function.png")) |> ignore;

        let plot = new ScottPlot.Plot(800, 600);
        let xAxis = [ for i in 0 .. numberOfGenerations - 1 -> float i ]

        plot.AddScatter(xAxis |> Array.ofList, bestChartData |> Array.ofList, System.Drawing.Color.Green, label="Best fitness") |> ignore;
        plot.AddScatter(xAxis |> Array.ofList, averageChartData |> Array.ofList, System.Drawing.Color.Blue, label="Average fitness") |> ignore;
        plot.AddScatter(xAxis |> Array.ofList, worstChartData |> Array.ofList, System.Drawing.Color.Yellow, label="Worst fitness") |> ignore;

        plot.Title("Population fitness");
        plot.XLabel("generation");
        plot.YLabel("fitness");
        plot.Legend() |> ignore;

        plot.SaveFig(Path.Combine(docsDir, "population-fitness.png")) |> ignore;
        0

