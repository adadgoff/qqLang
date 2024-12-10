module qqlang.main

open FParsec
open ast
open parser

[<EntryPoint>]
let main argv =
    let codeSample = // INSERT YOUR CODE HERE.
        """
    def rec fact(n) {
        if n == 1 then {
            1
        }
        else {
            n * fact(n - 1)
        }
    }
    print(fact(5))
    
    var PI = 3.14
    var radius = 10
    var radiusSquare = radius * radius
    var square = PI * radiusSquare
    print(square)
    
    def double(x) {
        x * 2
    }
    print(double(4))
    """

    match run pQQLanguage codeSample with
    | Success(result, _, _) ->
        eval result Map.empty |> ignore
    | Failure(err, _, _) -> printfn $"%A{err}"

    0
