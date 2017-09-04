(**
- title : Extending F# through Computation Expressions
- description : Explore patterns for extending the F# language using the computation expressions feature
- author : Ryan Riley
- theme : night
- transition : default

***
*)

(*** hide ***)
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#load "../.paket/load/net462/main.group.fsx"
open System
open System.Net
open System.Reactive
open System.Reactive.Linq
open FSharp.Control.Reactive
open FSharp.Control.Reactive.Builders

(**

## Extending F# through Computation Expressions

### Ryan Riley

***

## Prerequisites

' This is a somewhat advanced level talk in that I assume
' you know or are familiar with a few of the more advanced
' aspects of F# or similar languages, including:
' - how to write a computation expression
' - monoids, monads, applicatives, etc.
' - statically resolved type parameters
' - .NET method overloading

***

## Motivation

' One of the goals of this conference was to not dive into
' language features for their own sake. I don't intend to
' just tell you about computation expressions, nor do I intend
' to cover the academic aspects, which you can find in other
' talks and papers.
' I want to convey the power of computation expressions as they
' related to solving problems. What sorts of problems? I hope
' to show computation expressions are useful for solving many
' different types of problems, as many as may be solved by
' writing computer programs.
' That seems quite broad, and I agree. It's quite possible that
' computation expressions are really just a fancy way to do
' what we already do in F# or any other language, but I'll leave
' you to decide that at the end. I hope to show you that, at the
' very least, computation expressions can offer a useful model
' for writing correct programs for a given domain.

***

## Examples

' Let's begin with a few examples built into the language.

***

### Seq<'T>
*)

(*** define-output: seq-example ***)
seq {
    for i in 1..10 do
    yield i
}
(*** include-it: seq-example ***)

(**

' F# has several list-like comprehensions, including list,
' array, and seq. Here I've chosen to show seq since it looks
' the most like the standard computation expression syntax.
' It's worth noting that there are several seq-related
' optimizations built into the compiler (according to Dave Thomas)
' that are not available to normal computation expressions,
' but this serves as a useful example for the list-like
' syntax.

***

### Concatenating Seq<'T>
*)

(*** define-output: monoid-example ***)
let xs = seq { for i in 1..10 -> i }
let ys = seq { for i in 11..20 -> i }
seq {
    yield! xs
    yield! ys
}
(*** include-it: monoid-example ***)

(**

' In addition to generating a sequence, we can also
' compose multiple sequences into a single sequence
' by concatenating them. This has the same effect as
' calling `Seq.concat` on the two sequences. However,
' the computation expression provides a useful way
' of defining the concatenation according to the rules
' behind each of the bindings. In the former example,
' we used `for` and `yield`. In this example, we used
' two `yield!`. Each binding has a translation rule
' used to compose the program which restrict what can
' be done.

***

### Async<'T>
*)

(*** define-output: async-example ***)
async {
    let req = WebRequest.Create("http://openfsharp.org/")
    let! resp = req.AsyncGetResponse()
    use stream = resp.GetResponseStream()
    let! bytes = stream.AsyncRead(91)
    let text = Text.Encoding.UTF8.GetString(bytes)
    return (resp :?> HttpWebResponse).StatusCode, text
}
|> Async.RunSynchronously
(*** include-it: async-example ***)

(**

' Now let's look at an example using Async Workflows.
' It's almost 10 years ago today that [Don Syme](https://twitter.com/dsyme) posted
' [Introducing F# Asynchronous Workflows](https://blogs.msdn.microsoft.com/dsyme/2007/10/10/introducing-f-asynchronous-workflows/).
' When we think of asynchronous and concurrent programming these days, we think of an `async`/`await` pair.
' To the best of my knowledge, this pattern started with F# in 2007.
' Here, we are requesting the Open F# web site, then reading the
' first 91 bytes (through the title) and returning the status code
' and first 91 bytes as text.
' Async computations are delayed and require a call to start them.
' Not all computation expressions have to provide this delay.

***

## Patterns

' Now that we've discussed our motivation and looked
' at some examples, let's move into the patterns for
' building computation expressions.
' I'm sure you already noticed that the previous examples
' use wrapper, or container, types. In other words, these
' are generic types that contain values. Computation expressions
' provide us a mechanism for working with these container types.
' But not all containers are created equal.

***

> Three Rings for the Elven-kings under the sky,<br />
> Seven for the Dwarf-lords in their halls of stone,<br />
> Nine for Mortal Men doomed to die,<br />
> One for the Dark Lord on his dark throne<br />
> <br />
> In the Land of Mordor where the Shadows lie.<br />
> One Ring to rule them all, One Ring to find them,<br />
> One Ring to bring them all and in the darkness bind them<br />
> In the Land of Mordor where the Shadows lie.<br />

— J.R.R. Tolkien's epigraph to The Lord of the Rings 

' I find computation expressions quite poetic, so what better
' way to outline the patterns for building computation expressions
' than with a poem?
' <Read the poem>
' I imagine you are wondering why such a dark poem to describe
' this wonderful tool?
' I have found that many of us have followed a similar path in
' understanding computation expressions, and the path starts with
' the bottom of the first stanza then works its way up.

***

## One for the Dark Lord on his dark throne

' We'll come back to this one in just a bit. The motivating
' factor for many when first encroaching into computation
' expressions is to replicate what is found in Haskell or some
' other programming language. For reasons good and bad, the
' topic of monads, which is _not_ the point of this talk, compells
' programmers towards this goal. That leads us to:

***

## Nine for Mortal Men doomed to die

' The Nine became the Nazgul, slaves to the Dark Lord Sauron.
' I found this to be true of me when first trying to understand
' computation expressions, and it blinded me for years to seeing
' and understanding their potential.
' To say that this talk is not about monads isn't entirely true;
' it's just they aren't the main point. We've already looked at two,
' though it's really not important that you fully understand what
' is a monad (or a monoid, applicative, etc.) though it certainly
' won't hurt you to understand why they are valuable and how they
' might help you reason about your code.
' Let's look at another example using `Option<'T>`.

***

### OptionBuilder
*)

type OptionMonad() =
    member __.Bind(m, f) = Option.bind f m
    member __.Return(x) = Some x

let opt = OptionMonad()

(**

[Computation Expressions](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions)

' This is the basic definition of a monad written as a
' computation expression. The computation expression is
' written as a simple .NET class with members having
' certain names and matching a limited range of type signatures.
' We are not going to cover all of the methods and their
' signatures today, but you can find them listed in the Microsoft
' documentation online.
' Unfortunately, this does not quite complete what's possible.

***

### OptionBuilder
*)

type OptionBuilder() =
    member __.Bind(m, f) = Option.bind f m
    member __.Return(x) = Some x
    member __.ReturnFrom(m: 'T option) = m
    member __.Zero() = Some ()
    member __.Combine(m, f: unit -> _) = Option.bind f m
    member __.Delay(f: unit -> 'T) = f
    member __.Run(f) = f()

let maybe = OptionBuilder()

(**

' The previous implementation eagerly evaluates the computation.
' Computation Expressions allow you to insert delays into the
' computation so they don't run eagerly. In addition, we've
' added members to help with combining computations return unit
' with continuing computations.

***

### Delayed Computations
*)

(*** define-output: option-builder ***)
let one = maybe { return 1 }
let double x = maybe { return x * 2 }
let carryOn = (*true*) false
maybe {
    if carryOn then
        printfn "proceeding"
        let! x = one
        let! y = double x
        return x + y
    else return! None
}
(*** include-it: option-builder ***)

(**

' Here we see that we can retrieve the values of one and two
' and add them together if carryOn is true. Otherwise, we
' can return a None directly using ReturnFrom.

***

### Why should we care?

*)

if carryOn then
    printfn "proceeding"
    match one with
    | Some x ->
        match double x with
        | Some y -> Some(x + y)
        | None -> None
    | None -> None
else None

(**

' The above could be written like this, with nested match expressions.
' The computation expression clearly wins out in terms of succinctness.
' We also don't have to thread all the `None -> None` cases. This may
' not seems like such a big deal until you get into a situation where
' you have to cascade many option values in a long arrowhead pattern,
' e.g. if you are doing some parsing.
' In other words, computation expressions allow us to streamline our
' code such that it looks mostly like normal F# code while making side-
' effects explicit.

***

### Back to the One Ring

* [FSharpPlus](https://github.com/gusty/FSharpPlus)
* [FSharpx.Extras](https://github.com/fsprojects/FSharpx.Extras/tree/master/src/FSharpx.Extras/ComputationExpressions)
* [Higher](https://github.com/palladin/Higher)

' Much like the Nazgul, seduced by Sauron's power through the One Ring,
' you may get caught in the trap of trying to implement the one, true monad CE.
' Many computations can be written almost exactly alike, assuming they are
' monads. However, F# is not a purely functional, lazy language like Haskell,
' and the implementations cannot be implemented exactly alike.
' The One Ring, in this story, is the Haskell do-notation. FSharpPlus achieved
' do-notation, which is a single computation expression abstraced over any
' monadic type. The implementation is interesting and worth your time to
' investigate, but we won't be diving deeper in this talk.
' Nevertheless, these implementations offer hints to the potential power
' of F# computation expressions we'll see in a bit.

***

## Seven for the Dwarf-lords in their halls of stone

' Okay, let's move towards the light. Or at least a different kind of
' darkness. The rings of the dwarves don't factor into Tolkien's stories
' much, so I can't make a great analogy, but let's assume they were more
' powerful than the rings for men. In that case, I would relate them to
' the query expressions introduced in F# 3.0.

***

### QueryBuilder

*)

(*** define-output: query-builder ***)
query {
    for x in 1..10 do
    for y in 11..20 do
    where (x % 2 = 1 && y % 2 = 0)
    select (x + y)
}

(*** include-it: query-builder ***)

(**

' F# 3.0, released in 2012, introduced query expressions. In order
' to support query expressions, F# also introduced the `CustomOperationAttribute`.
' Unfortunately, F# 3.0 also introduced type providers, and these drew
' the most excitement in the community. So much so that experimentation with
' computation expressions stagnated. However, some work still continued with
' interesting results.

***

### [FSharp.Control.Reactive](http://fsprojects.github.io/FSharp.Control.Reactive/)

*)

(*** define-output: rxquery-zip ***)
rxquery {
    for x in (Observable.ofSeq [|1..10|]) do
    zip y in (Observable.ofSeq [|11..20|])
    select (x + y)
}
|> Observable.subscribe (printfn "%i")

(*** include-output: rxquery-zip ***)

(**

' Rx.NET works just fine from F#, but FSharp.Control.Reactive provides extensions
' to the Observable module to fit the F# style. It also provides an `observe`
' computation expression for the monadic operations and `rxquery` for query
' expressions. 
' The `for` is one of the standard operators used in computation expressions.
' However, `zip` and `select` are not standard. They are not part of the core
' F# keywords. How do they appear within the language?

***

### RxQueryBuilder Select and Zip

*)

type RxQueryBuilder with

    [<CustomOperation("select", AllowIntoPattern=true)>]
    member __.Select (s:IObservable<_>, 
                      [<ProjectionParameter>] selector : _ -> _) =
        s.Select(selector)

    [<CustomOperation("zip", IsLikeZip=true)>]
    member __.Zip (s1:IObservable<_>,
                   s2:IObservable<_>,
                   [<ProjectionParameter>] resultSelector : _ -> _) =
        s1.Zip(s2, new Func<_,_,_>(resultSelector))

(**

' These are two custom operations introduced in F# 3.0 with the
' query expressions. Note that we give the name in the `CustomOperationAttribute`.
' The attribute accepts additional parameters, such as `AllowIntoPattern`,
' `IsLikeZip`, etc. The full set of options is outside the scope of this talk,
' but you can find them listed in the Microsoft docs online and in the full
' implementation of the `RxQueryBuilder`.
' The FSharp.Control.Reactive library made an interesting choice in providing
' both the `observe` CE and the `rxquery` QE. There's no reason you cannot mix
' the two in one builder. It turns out, you can also extend an existing builder
' with new custom operations.

***

### Extending Existing Builders

*)

type FSharp.Control.AsyncBuilder with

    [<CustomOperation("and!", IsLikeZip=true)>]
    member __.Merge(x, y, f) =
        async {
            let! token = Async.CancellationToken
            let! x' = Async.StartChildAsTask x
            let! y' = Async.StartChildAsTask y
            do System.Threading.Tasks.Task.WaitAll([|x';y'|], cancellationToken = token)
            let! x'' = Async.AwaitTask x'
            let! y'' = Async.AwaitTask y'
            return f x'' y''
        }

    member __.For(m, f) = __.Bind(m, f)

(**

' If you ever wished you could run two or more async computations in parallel
' within an async computation expression, now you know you can by extending the
' AsyncBuilder provided with FSharp.Core.

***

### Async Applicative Example

' Jump to VSCode to run this in FSI, as the code snippet is too long.
' The code compares awaiting two sleeps and shows that the applicative,
' which does not require awaiting the first value, can freely run in
' parallel, whereas the monad version using `let!` must wait for each
' in turn.
' We could argue about the best syntax, but this version uses that proposed
' in the VisualFSharp issues: https://github.com/fsharp/fslang-suggestions/issues/579

*)

(*** hide ***)

let a (sw:Diagnostics.Stopwatch) = async {
    printfn "starting a %O" sw.ElapsedMilliseconds
    do! Async.Sleep 1000
    printfn "returning a %O" sw.ElapsedMilliseconds
    return 1
}

let b (sw:Diagnostics.Stopwatch) = async {
    printfn "starting b %O" sw.ElapsedMilliseconds
    do! Async.Sleep 500
    do printfn "returning b %O" sw.ElapsedMilliseconds
    return 2
}

let comp sw = async {
    for x in a sw do
    ``and!`` y in b sw
    return x + y
}

let compBind sw = async {
    let! x = a sw
    let! y = b sw
    return x + y
}

let sw = Diagnostics.Stopwatch.StartNew()
let result = comp sw |> Async.RunSynchronously
sw.Stop()
printfn "comp ran in %Oms with result %i" sw.ElapsedMilliseconds result

// Compare with
sw.Reset()
sw.Start()
let resultBind = compBind sw |> Async.RunSynchronously
sw.Stop()
printfn "compBind ran in %Oms with result %i" sw.ElapsedMilliseconds resultBind

let c (sw:Diagnostics.Stopwatch) = async {
    printfn "starting c %O" sw.ElapsedMilliseconds
    do! Async.Sleep 1500
    do printfn "returning c %O" sw.ElapsedMilliseconds
    return 3
}

let comp3 sw = async {
    for x in a sw do
    ``and!`` y in b sw
    ``and!`` z in c sw
    return x + y + z
}

sw.Reset()
sw.Start()
let result3 = comp3 sw |> Async.RunSynchronously
sw.Stop()
printfn "comp3 ran in %Oms with result %i" sw.ElapsedMilliseconds result3

(**

***

## Three Rings for the Elven-kings under the sky

***

## [Freya](https://freya.io/)

***

## [ILBuilder](https://github.com/kbattocchi/ILBuilder)
## [LicenseToCIL](https://github.com/rspeele/LicenseToCIL)

***

## Questions?

***

## Why Not Macros?

' Since we are talking about language extensions, we must first
' talk about macros. After all, languages in the Lisp family
' support macros, to the extent that much of the language is built
' in macros. What could be more powerful? In short, nothing.
' However, F# does not provide a mechanism for writing macros,
' and it's a fair question to ask, "Why not?"
' Macros require a pre-compiler or a compiler plugin mechanism.
' When you introduce something like this, you are either on
' some other tool to handle some of the work of compiling a program.
' This can drastically increase and complicate compile times or even
' create very difficult-to-debug compiler crashes.
' In addition, macros don't play very well with typed languages
' as you are essentially relying on something typed to rely on
' something untyped-until-generated.
' Type Providers are the closest thing F# has to macros, but these
' only provide types and don't extend the language.
' Computation expressions don't carry the full power of macros,
' but they cover the most important use cases to the point you won't
' likely miss macros.

***

## Will F# ever get Type Classes?

### [Classes for the Masses](https://github.com/MattoWindsor91/visualfsharp/blob/hackathon-vs/examples/fsconcepts.md)

' Incidentally, type classes, which Haskell uses to encode monads and other
' container types, have a proven encoding in F# and may be coming in a future
' release. Hopefully this will alleviate some of the pit of computation-
' expressions-as-monad-encodings that many fall into.

***

## References

1. [Computation Expressions](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions)
2. [Query Expressions](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/query-expressions)
3. [Computation Expresssions in F# 3.0](https://vimeo.com/47218436)
4. [Introducing F# Asynchronous Workflows](https://blogs.msdn.microsoft.com/dsyme/2007/10/10/introducing-f-asynchronous-workflows/)
5. [The F# Computation Expression Zoo](http://tomasp.net/academic/papers/computation-zoo/computation-zoo.pdf)
6. [F# for fun and profit](https://fsharpforfunandprofit.com/series/computation-expressions.html)
7. [Support let! .. and... for applicative functors](https://github.com/fsharp/fslang-suggestions/issues/579)

***
*)
