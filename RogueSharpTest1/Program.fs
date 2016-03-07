// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open RogueSharpGame

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    use g = new Game1()
    g.Run()
    0 // return an integer exit code
