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
#load "../.paket/load/net461/Hopac.fsx"
#load "../.paket/load/net461/LicenseToCIL.fsx"
#load "../.paket/load/net461/main.group.fsx"
open System
open System.Net
open System.Reactive
open System.Reactive.Linq
open System.Threading.Tasks
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

' This talk was prepared for Open FSharp 2017.
' One of the goals of this conference was to not dive into
' language features for their own sake. I don't intend to
' just tell you about computation expressions, nor do I intend
' to cover the academic aspects, which you can find in other
' talks and papers.
' I want to convey the power of computation expressions as they
' relate to solving problems. What sorts of problems? I hope
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

## Topics

' Computation Expressions is both a language feature and a general
' label for several, related features and capabilities.

---

## [Computation Expressions](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions)

---

## [Query Expressions](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/query-expressions)

---

## Type Extensions

---

## Domain Specific Languages

***

## Examples

' Let's begin with a few examples built into FSharp.Core.

---

### Seq<'T>
*)

(*** define-output: seq-example-1 ***)
seq { for i in 1..10 -> i }
(*** include-it: seq-example-1 ***)

(*** define-output: seq-example-2 ***)
seq { for i in 1..10 do yield i }
(*** include-it: seq-example-2 ***)

(**

' F# has several list-like comprehensions, including list,
' array, and seq. Here I've chosen to show seq since it looks
' the most like the standard computation expression syntax.
' It's worth noting that there are several seq-related
' optimizations built into the compiler (according to Dave Thomas)
' that are not available to normal computation expressions,
' but this serves as a useful example for the list-like
' syntax.

---

### Concatenating Seq<'T>
*)

(*** define-output: seq-append ***)
Seq.append xs ys

(*** include-it: seq-append ***)

(**

' Now, I know what you are thinking. Why all the fuss? I can do that
' with Seq.append, too. It turns out that everything we see in this
' talk could be written with modules of functions. Some people prefer
' that style, and that's fine. However, as we move further along,
' I think you'll see that CEs are a very good option for expressing
' computations that modules of functions cannot.

---

### Concatenating Seq<'T>
*)

(*** define-output: monoid-example ***)
let xs = seq { for i in 1..10 do yield i }
let ys = seq { for i in 11..20 do yield i }

seq {
    yield! xs
    yield! ys
}
(*** include-it: monoid-example ***)

(**

' In addition to generating a sequence, we can also
' compose multiple sequences into a single sequence
' by concatenating them. This has the same effect as
' calling `Seq.append` on the two sequences. However,
' the computation expression provides a useful way
' of defining the concatenation according to the rules
' behind each of the bindings. In the former example,
' we used `for` and `yield`. In this example, we used
' two `yield!`. Each binding has a translation rule
' used to compose the program which restrict what can
' be done.

---

### Async<'T>
*)

(*** define-output: async-example ***)
async {
    let req = WebRequest.Create("https://www.openfsharp.org/")
    let! resp = req.AsyncGetResponse()
    use stream = resp.GetResponseStream()
    let! bytes = stream.AsyncRead(95)
    let text = Text.Encoding.UTF8.GetString(bytes)
    return (resp :?> HttpWebResponse).StatusCode, text
}
|> Async.RunSynchronously

(**

    val it : HttpStatusCode * string =
      (OK {value__ = 200;},
       "<!DOCTYPE html>
    <html lang="en-us">
        <head>
            <meta charset="UTF-8">
            <title>Open F#</title>
    ")

' Now let's look at an example using Async Workflows.
' [Don Syme](https://twitter.com/dsyme) posted
' [Introducing F# Asynchronous Workflows](https://blogs.msdn.microsoft.com/dsyme/2007/10/10/introducing-f-asynchronous-workflows/)
' ten years ago.
' When we think of asynchronous and concurrent programming these days, we think of an `async`/`await` pair.
' To the best of my knowledge, this pattern started with F# in 2007.
' Here, we are requesting the Open F# web site, then reading the
' first 95 bytes (through the title) and returning the status code
' and first 95 bytes as text.
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

[![The One Ring](images/one-ring.jpg)](http://stock-wallpapers.com/wp-content/uploads/2015/01/Lord-of-the-Rings-Wallpapers-Free-Download-9.jpg)

' We'll come back to this one in just a bit. The motivating
' factor for many when first encroaching into computation
' expressions is to replicate what is found in Haskell or some
' other programming language. For reasons good and bad, the
' topic of monads, which is _not_ the point of this talk, compells
' programmers towards this goal. That leads us to:

***

## Nine for Mortal Men doomed to die

[![Nine rings for mortal men](images/9rings.jpg)](http://www.itsvery.net/lord-of-the-rings.html)

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

---

## [Computation Expressions](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions)

---

### Why should we care?

*)

let one = Some 1

let tryDivide num den =
    match den with
    | 0 -> None
    | n -> Some(num / n)

match one with
| Some x ->
    match tryDivide x 0 with
    | Some y -> Some(x + y)
    | None -> None
| None -> None

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

---

### Option Monad
*)

type OptionMonad() =
    member __.Bind(m, f) = Option.bind f m
    member __.Return(x) = Some x
let opt = OptionMonad()

(**

' This is the basic definition of a monad written as a
' computation expression. The computation expression is
' written as a simple .NET class with members having
' certain names and matching a limited range of type signatures.
' We are not going to cover all of the methods and their
' signatures today, but you can find them listed in the Microsoft
' documentation online.
' Unfortunately, this does not quite complete what's possible.

---

### OptionBuilder
*)

type OptionBuilder() =
    member __.Bind(m, f) = Option.bind f m
    member __.Return(x) = Some x
    member __.ReturnFrom(m: 'T option) = m
    member __.Zero() = None
    member __.Combine(m, f: unit -> _) =
        match m with Some _ -> m | None -> f()
    member __.Delay(f: unit -> 'T) = f
    member __.Run(f) = f()

let maybe = OptionBuilder()

(**

' The previous implementation eagerly evaluates the computation.
' Computation Expressions allow you to insert delays into the
' computation so they don't run eagerly. In addition, we've
' added members to help with combining computations return unit
' with continuing computations.

---

### Expansion

*)

maybe { return 1 }

maybe.Run(
    maybe.Delay(fun () ->
        maybe.Return 1))

(**

' The F# compiler expands the computation into calls to the builder's methods.
' Here, you can see the expansion for a simple return. Because we implemented
' Run and Delay, they wrap the computation.

---

### Expansion

*)

maybe {
    let! value = maybe { return 1}
    return value
}

maybe.Run(
    maybe.Delay(fun () ->
        maybe.Bind(maybe.Return 1, fun value ->
            maybe.Return value)))

(**

' Here's a more involved expansion for a bind and return.
' Just like LINQ's Select, SelectMany, etc., you can write this yourself,
' but you probably don't want to do that. However, it can be important
' to know what you are allocating in some performance sensitive areas.
' For example, you may want to write `maybe.Return 1` rather than
' `maybe { return 1 }`.

---

### Delayed Computations
*)

type OptionMonad2() =
    inherit OptionMonad()

    member __.ReturnFrom(m) = m
    member __.Combine(m1, m2) =
        match m1 with Some _ -> m1 | None -> m2
    member __.Delay(f) = f()

let optMany = OptionMonad2()

optMany {
    return 1
    printfn "delayed should not print"
    return! None
}

(**

    delayed should not print
    val it : int option = Some 1

' Here we see that we can retrieve the values of one and two
' and add them together if carryOn is true. Otherwise, we
' can return a None directly using ReturnFrom.
' However, the important thing to notice is that the printfn
' did not execute. The computation is only following the path
' selected in the if branch. The Delay is wrapped around the
' computation and keeps that branch of the computation from
' being evaluated.
' This example also highlights the fact that we are using
' F#'s support for object programming by means of `inherit`.
' We could also have `OptionMonad2` create its own instance
' of `OptionMonad` and use its members in its own, which is
' the preferred approach in F#.

---

### Delayed Computations (cont)

*)

(*** define-output:option-builder ***)
maybe {
    return 1
    printfn "delayed should not print"
    return! None
}

(*** include-it:option-builder ***)

(**

' In this slightly modified example, you can see that the maybe
' computations embedded within the outer computation are never
' executed at all. This may be useful with computations that are
' expensive.

---

### Function Wrapper Types

*)

type Maybe<'T> = Maybe of (unit -> 'T option)

(**

' Of course, you can create a wrapper type that delays
' execution of the computation by using a function. The result
' then must be unwrapped and called, or the Run member could do
' the work as above.

---

### Back to the One Ring ...

[![Eye of Sauron](images/eye-of-sauron.jpg)](http://lotr.wikia.com/wiki/File:Worst-Does-all-of-this-prequel-Sauron-stuff-line-up-with-LOTR.jpg?file=Worst-Does-all-of-this-prequel-Sauron-stuff-line-up-with-LOTR.jpg)

---

[![Nazgul](images/nazgul.jpg)](http://pin.it/gwNsF5C)

' Much like the Nazgul, seduced by Sauron's power through the One Ring,
' you may get caught in the trap of trying to implement the one, true monad CE.
' Many computations can be written almost exactly alike, assuming they are
' monads. However, F# is not a purely functional, lazy language like Haskell,
' and the implementations cannot be implemented exactly alike.

---

### Monad (+more) libraries

* [FSharpPlus](https://github.com/gusty/FSharpPlus)
* [FSharpx.Extras](https://github.com/fsprojects/FSharpx.Extras/tree/master/src/FSharpx.Extras/ComputationExpressions)
* [Higher](https://github.com/palladin/Higher)

' The One Ring, in this story, is the Haskell do-notation. FSharpPlus achieved
' do-notation, which is a single computation expression abstraced over any
' monadic type. The implementation is interesting and worth your time to
' investigate, but we won't be diving deeper in this talk.
' Nevertheless, these implementations offer hints to the potential power
' of F# computation expressions we'll see in a bit.

***

## Seven for the Dwarf-lords in their halls of stone

[![Dwarf lords](images/dwarf-lords.jpg)](https://heirsofdurin.files.wordpress.com/2013/10/dwarf-lords.jpg)

' Okay, let's move towards the light. Or at least a different kind of
' darkness. The rings of the dwarves don't factor into Tolkien's stories
' much, so I can't make a great analogy, but let's assume they were more
' powerful than the rings for men. In that case, I would relate them to
' the query expressions introduced in F# 3.0.

---

## [Query Expressions](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/query-expressions)

---

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

---

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

---

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

---

## Extending Existing Builders

*)

(*** hide ***)
open System
open System.Threading.Tasks

(*** show ***)
type FSharp.Control.AsyncBuilder with

    [<CustomOperation("and!", IsLikeZip=true)>]
    member __.Merge(x, y,
                    [<ProjectionParameter>] resultSelector : _ -> _) =
        async {
            let x' = Async.StartAsTask x
            let y' = Async.StartAsTask y
            do Task.WaitAll(x',y')
            return resultSelector x'.Result y'.Result
        }

    member __.For(m, f) = __.Bind(m, f)

(**

' If you ever wished you could run two or more async computations in parallel
' within an async computation expression, now you know you can by extending the
' AsyncBuilder provided with FSharp.Core.

---

### Parallel Async Example

' Jump to VSCode to run this in FSI, as the code snippet is too long.
' The code compares awaiting two sleeps and shows that the applicative,
' which does not require awaiting the first value, can freely run in
' parallel, whereas the monad version using `let!` must wait for each
' in turn.
' We could argue about the best syntax, but this version uses that proposed
' in the VisualFSharp issues: https://github.com/fsharp/fslang-suggestions/issues/579

---

### Async with 1000ms Sleep

*)

let a (sw:Diagnostics.Stopwatch) = async {
    printfn "starting a %O" sw.ElapsedMilliseconds
    do! Async.Sleep 1000
    printfn "returning a %O" sw.ElapsedMilliseconds
    return 1
}

(**

---

### Async with 500ms Sleep

*)

let b (sw:Diagnostics.Stopwatch) = async {
    printfn "starting b %O" sw.ElapsedMilliseconds
    do! Async.Sleep 500
    do printfn "returning b %O" sw.ElapsedMilliseconds
    return 2
}

(**

---

### Sequential

*)

let compBind sw = async {
    let! x = a sw
    let! y = b sw
    return x + y
}
let sw = Diagnostics.Stopwatch.StartNew()
let resultBind = compBind sw |> Async.RunSynchronously
sw.Stop()
printfn "compBind ran in %Oms with result %i" sw.ElapsedMilliseconds resultBind

(**

    starting a 7
    returning a 1010
    starting b 1016
    returning b 1518
    compBind ran in 1521ms with result 3

---

### Parallel

*)

let comp sw = async {
    for x in a sw do
    ``and!`` y in b sw
    return x + y
}
sw.Reset()
sw.Start()
let result = comp sw |> Async.RunSynchronously
sw.Stop()
printfn "comp ran in %Oms with result %i" sw.ElapsedMilliseconds result

(**

    starting a 4
    starting b 4
    returning b 505
    returning a 1004
    comp ran in 1005ms with result 3

---

### Another Async with 1500ms Sleep

*)

let c (sw:Diagnostics.Stopwatch) = async {
    printfn "starting c %O" sw.ElapsedMilliseconds
    do! Async.Sleep 1500
    do printfn "returning c %O" sw.ElapsedMilliseconds
    return 3
}

(**
---

### 2+ Asyncs

*)

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

    starting a starting b 3
    3
    starting c 4
    returning b 504
    returning a 1004
    returning c 1506
    comp3 ran in 1510ms with result 6

---

### `CustomOperation` Overloading
*)

type FSharp.Control.AsyncBuilder with

    //[<CustomOperation("and!", IsLikeZip=true)>]
    member __.Merge(x: Task<'a>, y: Task<'b>,
                    [<ProjectionParameter>] resultSelector : _ -> _) =
        async {
            do Task.WaitAll(x,y)
            let! x' = Async.AwaitTask x
            let! y' = Async.AwaitTask y
            return resultSelector x' y'
        }

(**

' You may assume that method overloading is as easy as adding another,
' similar named method. After all, many people extend `AsyncBuilder`
' with implementations of `Bind` that deal with `Task<'T>` and `Task`
' so that they don't have to deal with the `Async.AwaitTask`.
' Interestingly, that's not allowed with custom operations if you:
'   1) attempt to add it directly to the builder class and/or
'   2) attempt to add the same attribute again with the same name.
' If you add an extension member outside the type definition and assign
' the name to a CustomOperation attribute on only one member, you _can_
' add overloads. This appears to work through the class method dispatch
' and is technically a bug, but it will allow you to overload custom
' operations until official support is released, which is currently a
' work in progress:
' https://github.com/fsharp/fslang-design/blob/master/RFCs/FS-1056-allow-custom-operation-overloads.md

---

*)

async {
    for x in Task.FromResult(1) do
    ``and!`` y in Task.FromResult(2)
    return x + y
}

(**

> The custom operation 'and!' refers to a method which is overloaded.
> The implementations of custom operations may not be overloaded.<br />
> custom operation: and! var in collection <br />
> Calls AsyncBuilder.Merge

' But it will cause the custom operation to be unusable.

---

### [Other Limitations](https://github.com/fsharp/fsharp/blob/master/src/fsharp/FSComp.txt#L1206)

' You can find additional limitations, specifically with query builders
' in the error codes defined in FSharp.Core:
' https://github.com/fsharp/fsharp/blob/master/src/fsharp/FSComp.txt#L1206

***

## Three Rings for the Elven-kings under the sky

[![Elven kings](images/elven-rings.jpg)](http://www.itsvery.net/lord-of-the-rings.html)

' The rings of the elven kings were supposedly very powerful.
' We've seens some interesting building blocks. It's time to
' put everything together and find out what we can really do
' with computation expressions.
' Let's return to our motivation: to extend F#.
' How might we accomplish this? We have seen we can extend the
' available keywords by means of custom operations within computation
' expressions, though we must remember we have to work within
' certain constraints defined by the feature.

---

## Domain Specific Languages

---

### Builds with [Xake](https://github.com/FakeBuild/Xake)

    #r "paket:
      nuget Xake ~> 1.1 prerelease
      nuget Xake.Dotnet ~> 1.1 prerelease //"
    
    open Xake
    open Xake.Dotnet
    
    do xakeScript {
      rules [
        "main" <== ["helloworld.exe"]

        "helloworld.exe" ..> csc {src !!"helloworld.cs"}
      ]
    }

---

### Testing with [Expecto](https://github.com/haf/expecto)

*)

(*** include: expecto ***)

(**

---

### Emitting CIL with [LicenseToCIL](https://github.com/rspeele/LicenseToCIL)

*)

(*** include: licensetocil ***)

(**

' Everyone is familiar with the concept of domain specific
' languages. Scott is going over this same topic in the
' other room. So CEs can enable embedded domain specific
' languages but are not necessary for that purpose.

---

### HTTP Protocol with [Freya](https://freya.io/)

*)

(*** include: freya-machine ***)

(*** include: freya-router ***)

(**

' Protocols are another avenue we frequently ignore. HTTP
' is an application protocol often dismissed as a transport
' protocol. Both are protocols for networked communications.
' In the case of HTTP, the server behavior is what's really
' encoded. Clients may behave in almost any way they please.
' TCP/IP, UDP, etc. are all protocols and define certain
' characterisitcs of behavoior.

---

### MVC with [Saturn](https://saturnframework.org/)

*)

(*** include: saturn-controller ***)

(*** include: saturn-app ***)

(**

---

### How?

' This is all well and good, but how would you do something like
' this?
' The trick is thinking beyond the obvious. The obvious thing
' to do, and the trap that the monad path leads you, is to think
' of computation expressions as providing computations around
' a specific data type or data structure, e.g. Async, Seq, Option,
' etc.
' However, you don't have to limit yourself to the visible type.
' Async works this way. There are many types that make up Async
' workflows. Query Expressions, too, hide a lot more than we
' saw earlier.

---

### Query Expressions Revisited

*)

(*** hide ***)
#r "System.Core.dll"
#r "System.Xml.Linq.dll"
open System.Xml.Linq

(*** define-output: xml-query ***)
let xn s = XName.Get s
let xml = """<people>
    <person firstName="Mathias" lastName="Brandewinder" />
    <person firstName="Andrew" lastName="Cherry" />
    <person firstName="" lastName="Riley" />
</people>"""
let doc = XDocument.Parse xml
query {
    for el in doc.Descendants(xn "person") do
    where (el.Attribute(xn "firstName").Value <> "")
    select (el.Attribute(xn "lastName").Value)
}

(*** include-it: xml-query ***)

(**

' F#'s query expressions appear to work with Seq<'T>, but
' actually works with expressions and is intended to be used
' with LINQ queries.
' The most obvious example would be working with a database,
' and you can find an example of this in the Microsoft docs.
' Here we look at an example using an XDocument, which also
' uses LINQ.
' While the built-in query expression works just fine with
' Seq<'T>, it is clearly doing more under the covers.

---

### Freya's Graph

![HTTP state diagram](images/http-state-diagram.png)

' Freya does something similar. It wraps a bit of state behind
' it's computation and then uses a graph model to facilitate
' the request/response state machine of HTTP. It exposes hooks
' to tap into the state machine, rather than having programmers
' specify exactly how to process each request. Freya goes even
' further in allowing programmers to compose the graph itself
' with the `using` expression.

---

<img alt="Freya's core components" src="images/freya/components.core.png" width="40%" />

' Freya hides a graph of components that are used to run the
' application once configured.

---

<img alt="Freya's responses" src="images/freya/specifications.responses.png" width="60%" />

' The graph is composed of smaller graphs.

---

![Freya's validations](images/freya/specifications.validations.png)

' Each exposes functionality that can be tapped into.
' This is the goal of declarative programming, after all:
' specify the what, not the how.
' The point is to remind you to think beyond the immediate data
' structure. I don't think we've reached the limit to what we
' can achieve with computation expressions.

***

## Questions?

***

## Why Not Macros?

' Since we are talking about language extensions, we must
' talk about macros. After all, languages in the Lisp family
' support macros, to the extent that much of the language is built
' in macros. What could be more powerful? In short, nothing.
' However, F# does not provide a mechanism for writing macros,
' and it's a fair question to ask, "Why not?"
' Macros require a pre-compiler or a compiler plugin mechanism.
' When you introduce something like this, you rely on another
' tool to handle some of the work of compiling a program.
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

---

### Research

#### [MacroML](https://www.cs.indiana.edu/~sabry/papers/macroml.pdf)
#### [MetaML](https://pdfs.semanticscholar.org/339f/4370bd02b977fc0d61fca50cc6a0ea26a24b.pdf)

' Despite the challenges of implementing macros with statically typed
' languages, several research efforts are making progress. Follow the
' links to read more.

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

(*** hide ***)
open Expecto
open Freya.Core
open Freya.Machines.Http
open Freya.Types.Http
open Freya.Routers.Uri.Template
open LicenseToCIL
open LicenseToCIL.Builder
open LicenseToCIL.Ops
open Giraffe
open Giraffe.Core
open Saturn
open Saturn.Pipeline
open Saturn.Router

(*** define: expecto ***)
let tests =
  test "A simple test" {
    let subject = "Hello World"
    Expect.equal subject "Hello World" "The strings should equal"
  }

(*** define: licensetocil ***)
cil {
    yield ldc'i4 1
    yield ldc'i4 2
    yield add
    yield ldc'i4 3
    yield add
    yield ret
} |> toDelegate<Func<int>> "myFunction1"

(*** define: freya-name ***)
let name =
    freya {
        let! name = Freya.Optic.get (Route.atom_ "name")

        match name with
        | Some name -> return name
        | _ -> return "World" }

(*** define: freya-hello ***)
let hello =
    freya {
        let! name = name

        return Represent.text (sprintf "Hello %s!" name) }

(*** define: freya-machine ***)
let machine =
    freyaMachine {
        methods [GET; HEAD; OPTIONS]
        handleOk hello }

(*** define: freya-router ***)
let router =
    freyaRouter {
        resource "/hello{/name}" machine }


(*** define: saturn-controller ***)
let commentController userId = controller {
    index (fun ctx -> (sprintf "Comment Index handler for user %i" userId ) |> Controller.text ctx)
    add (fun ctx -> (sprintf "Comment Add handler for user %i" userId ) |> Controller.text ctx)
    show (fun (ctx, id) -> (sprintf "Show comment %s handler for user %i" id userId ) |> Controller.text ctx)
    edit (fun (ctx, id) -> (sprintf "Edit comment %s handler for user %i" id userId )  |> Controller.text ctx)
}

(*** define: saturn-app ***)
let app = application {
    pipe_through endpointPipe

    router topRouter
    url "http://0.0.0.0:8085/"
    memory_cache
    use_static "static"
    use_gzip
}
