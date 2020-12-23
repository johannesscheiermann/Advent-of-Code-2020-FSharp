open Xunit
open FsUnit

type Combinate<'t> = 't list -> int -> 't list list

let rec combinate: Combinate<'t> =
    fun inputs size ->
        if size < 0 then
            failwith "The pair size can not be negative"
        else if size = 0 then
            []
        else if size = 1 then
            inputs |> List.map (fun it -> [ it ])
        else
            match inputs with
            | [] -> []
            | head :: tail ->
                (combinate tail (size - 1)
                 |> List.map (fun it -> head :: it))
                @ combinate tail size

[<Fact>]
let ``Combining an empty list leads to an empty list`` () =
    combinate [] 0 |> should be Empty
    combinate [] 1 |> should be Empty

[<Fact>]
let ``A PairSize can not be negative`` () =
    fun () -> combinate [ 1; 2; 3; 4; 5 ] -1 |> ignore
    |> should throw typeof<System.Exception>

[<Fact>]
let ``A PairSize of 0 leads to an empty list`` () =
    combinate [ 1; 2; 3; 4; 5 ] 0 |> should be Empty

[<Fact>]
let ``With a PairSize of 1, the list items are each returned in their own lists`` () =
    combinate [ 1; 2; 3; 4; 5 ] 1
    |> should equal [ [ 1 ]; [ 2 ]; [ 3 ]; [ 4 ]; [ 5 ] ]

[<Fact>]
let ``Combines correctly for a PairSize of 2`` () =
    combinate [ 1; 2; 3; 4 ] 2
    |> should
        equal
           [ [ 1; 2 ]
             [ 1; 3 ]
             [ 1; 4 ]
             [ 2; 3 ]
             [ 2; 4 ]
             [ 3; 4 ] ]

[<Fact>]
let ``Combines correctly for PairSizes larger than 2`` () =
    combinate [ 1; 2; 3; 4; 5 ] 3
    |> should
        equal
           [ [ 1; 2; 3 ]
             [ 1; 2; 4 ]
             [ 1; 2; 5 ]
             [ 1; 3; 4 ]
             [ 1; 3; 5 ]
             [ 1; 4; 5 ]
             [ 2; 3; 4 ]
             [ 2; 3; 5 ]
             [ 2; 4; 5 ]
             [ 3; 4; 5 ] ]

[<EntryPoint>]
let main argv =
    let data =
        System.IO.File.ReadLines "C:\Users\Privat\Repos\AdventOfCode\Day1\Input"
        |> List.ofSeq
        |> List.map System.Int32.Parse
    combinate data 3
        |> List.find (fun l -> l |> List.sum = 2020 )
        |> List.reduce (fun a b -> a*b)
        |> (fun it -> printfn "%i" it)
    0 // return an integer exit code
