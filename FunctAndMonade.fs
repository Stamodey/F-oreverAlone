open System

type Maybe<'a> =
    | Just of 'a
    | Nothing


module Functor =
    let map f maybe =
        match maybe with
        | Just x -> Just (f x)
        | Nothing -> Nothing

module Applicative =
    let pure x = Just x

    let apply mf mx =
        match mf, mx with
        | Just f, Just x -> Just (f x)
        | _ -> Nothing


module Monad =
    let bind f maybe =
        match maybe with
        | Just x -> f x
        | Nothing -> Nothing

    let returnM = Applicative.pure

let testFunctorLaws () =
    let id x = x
    let f x = x + 1
    let g x = x * 2

    let just = Just 5

    let leftIdentity = Functor.map id just = just
    let composition = 
        Functor.map (f >> g) just = Functor.map g (Functor.map f just)

    leftIdentity && composition

let testApplicativeLaws () =
    let id x = x
    let compose f g x = f (g x)

    let pure = Applicative.pure
    let apply = Applicative.apply

    let just = Just 5

    let identityLaw = apply (pure id) just = just
    let compositionLaw =
        let u = pure ((+) 1)
        let v = pure ((*) 2)
        let w = just
        let left = apply (apply (apply (pure compose) u) v) w
        let right = apply u (apply v w)
        left = right

    let homomorphismLaw =
        apply (pure ((*) 2)) (pure 3) = pure (2 * 3)

    identityLaw && compositionLaw && homomorphismLaw

let testMonadLaws () =
    let returnM = Monad.returnM
    let bind = Monad.bind

    let f x = Just (x * 2)
    let g x = Just (x + 3)
    let m = Just 5

    let leftIdentity = bind f (returnM 5) = f 5
    let rightIdentity = bind returnM m = m
    let associativity = bind g (bind f m) = bind (fun x -> bind g (f x)) m

    leftIdentity && rightIdentity && associativity

printfn "Законы функтора выполняются: %b" (testFunctorLaws ())
printfn "Законы аппликативного функтора выполняются: %b" (testApplicativeLaws ())
printfn "Законы монады выполняются: %b" (testMonadLaws ())

let double x = x * 2
let maybeDouble = Applicative.pure double

Just 5
|> Functor.map double
|> printfn "Результат map: %A"

Applicative.apply maybeDouble (Just 3)
|> printfn "Результат apply: %A"

Monad.bind (fun x -> Just (x * 3)) (Just 4)
|> printfn "Результат bind: %A"
