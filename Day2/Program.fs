open System
open System.Text.RegularExpressions

module Password =
    type UnvalidatedPassword = UnvalidatedPassword of string

    //type ValidatedPassword =
    //    private
    //    | Valid of string
    //    | NotValid of string

    type PasswordConstraint =
        { Letter: char
          MinOccurences: int
          MaxOccurences: int }

    let isValid1 (UnvalidatedPassword password) constr =
//        let isValid =
            password
            |> Seq.filter (fun c -> c = constr.Letter)
            |> Seq.length
            |> fun count ->
                count
                >= constr.MinOccurences
                && count <= constr.MaxOccurences
//
//        match isValid with
//        | true -> Valid password
//        | false -> NotValid password

    let isValid2 (UnvalidatedPassword password) constr =
        let count = 
            ((password.[constr.MinOccurences-1] |> Char.ToString)+(password.[constr.MaxOccurences-1] |> Char.ToString))
            |> Seq.filter (fun it -> it = constr.Letter)
            |> Seq.length
        count = 1
        
open Password

type Input =
    { password: UnvalidatedPassword
      constr: PasswordConstraint }

let extractInputData (line: string): Input =
    let r = Regex @"(\d+)-(\d+) (\w): (\w+)"
    let m = r.Match line
    let e = (m.Groups.Values |> Seq.toList)
    { password = UnvalidatedPassword e.[4].Value
      constr =
          { Letter = e.[3].Value.[0]
            MinOccurences = e.[1].Value |> Int32.Parse
            MaxOccurences = e.[2].Value |> Int32.Parse } }

open Xunit
open FsUnit

[<Fact>]
let ``Extracts the input data correctly`` () =
    extractInputData "10-19 n: nnnnnnnnnnnnnnnnnnnn"
    |> should
        equal
           { password = UnvalidatedPassword "nnnnnnnnnnnnnnnnnnnn"
             constr =
                 { Letter = 'n'
                   MinOccurences = 10
                   MaxOccurences = 19 } }
           
[<Fact>]
let ``tmp`` () =
    let data = extractInputData "6-11 z: zzkzpzzzwzf"
    isValid2 data.password data.constr |> should equal 1
      
[<EntryPoint>]
let main argv =
    let validPasswordCount =
        System.IO.File.ReadAllLines "C:\Users\Privat\Repos\AdventOfCode\Day2\Input"
        |> Seq.map extractInputData
        |> Seq.filter (fun it -> isValid2 it.password it.constr)
        |> Seq.length
    printfn "valid password count: %i" validPasswordCount
    0 // return an integer exit code
