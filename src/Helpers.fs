module Helpers

type Result<'TSuccess, 'TFailure> = 
| Success of 'TSuccess
| Failure of 'TFailure

let bind f x = 
    match x with
    | Success s -> f s
    | Failure f -> Failure f

type EitherBuilder () =
    member __.Bind(x, f) = bind f x

    member __.Return x = Success x
    member __.ReturnFrom (x:Result<'a, 'b>) = x
    member __.YieldFrom (x:Result<'a, 'b>) = x
    member __.Zero () = Success ()
    member __.Combine a b =
        match a,b with
        | Success a', Success b' -> Success [a'; b']
        | Success a', Failure _ -> Success [a']
        | Failure _, Success b' -> Success [b']
        | Failure _, Failure _ -> Success []

     member __.Delay(f) = 
        f ()

let either = new EitherBuilder()

let tryItemResult arr index ifErrorMessage =
    match Array.tryItem index arr with
    | Some item -> Success item
    | None      -> Failure ifErrorMessage

let toDouble validation whenFailure (input:string) =
    match System.Double.TryParse input with
    | true, v when validation v -> Success v
    | _ -> Failure (whenFailure ())