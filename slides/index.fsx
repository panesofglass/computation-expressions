(**
- title : Extending F# through Computation Expressions
- description : Explore patterns for extending the F# language using the computation expressions feature
- author : Ryan Riley
- theme : night
- transition : default

***

## Extending F# through Computation Expressions

### Ryan Riley

***

## Prerequisites

' This is a somewhat advanced level talk in that I assume
' you know or are familiar with a few of the more advanced
' aspects of F# or similar languages, including:
' - how to write a basic computation expression
' - monoids, monads, applicatives, etc.
' - statically resolved type parameters

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

## Examples

' Let's begin with a few examples built into the language.

***

### Seq<'T>
*)

(*** include: seq-example ***)
(*** include-it: seq-example-out ***)

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

(*** include: monoid-example ***)
(*** include-it: monoid-example-out ***)

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

(*** include: async-example ***)
(*** include-it: async-example-out ***)

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

### One for the Dark Lord on his dark throne

' We'll come back to this one in just a bit. The motivating
' factor for many when first encroaching into computation
' expressions is to replicate what is found in Haskell or some
' other programming language. For reasons good and bad, the
' topic of monads, which is _not_ the point of this talk, compells
' programmers towards this goal. That leads us to:

***

### Nine for Mortal Men doomed to die

' Uplifting, isn't it? To say that this talk is not about monads
' isn't entirely true; it's just they aren't the main point. We've
' already looked at two, though it's really not important that you
' fully understand what is a monad (or a monoid, applicative, etc.)
' though it certainly won't hurt you to understand why they are
' valuable and how they might help you reason about your code.
' Let's look at another example using `Option<'T>`.

***

### OptionBuilder
*)

(*** include: option-builder ***)
(*** include-it: option-builder-out ***)

(**

*)

(**

***

## Questions?

***

## References

1. 
2. 

***
*)

(*** hide ***)
open System
open System.Net

(*** define: seq-example ***)
seq {
    for i in 1..10 do
    yield i
}
(*** define-output: seq-example-out ***)
seq {
    for i in 1..10 do
    yield i
}

(*** define: monoid-example ***)
let xs = seq { for i in 1..10 -> i }
let ys = seq { for i in 11..20 -> i }
seq {
    yield! xs
    yield! ys
}
(*** define-output: monoid-example-out ***)
seq {
    yield! xs
    yield! ys
}

(*** define: async-example ***)
async {
    let req = WebRequest.Create("http://openfsharp.org/")
    let! resp = req.AsyncGetResponse()
    use stream = resp.GetResponseStream()
    let! bytes = stream.AsyncRead(91)
    let text = Text.Encoding.UTF8.GetString(bytes)
    return (resp :?> HttpWebResponse).StatusCode, text
}
|> Async.RunSynchronously
(*** define-output: async-example-out ***)
async {
    let req = WebRequest.Create("http://openfsharp.org/")
    let! resp = req.AsyncGetResponse()
    use stream = resp.GetResponseStream()
    let! bytes = stream.AsyncRead(91)
    let text = Text.Encoding.UTF8.GetString(bytes)
    return (resp :?> HttpWebResponse).StatusCode, text
}
|> Async.RunSynchronously

(*** define: option-builder ***)
type OptionBuilder() =
    member __.Return(x) = Some x
    member __.Bind(m, f) = Option.bind f m
let opt = OptionBuilder()
opt { return 1 }
(*** define-output: option-builder-out ***)
opt { return 1 }
