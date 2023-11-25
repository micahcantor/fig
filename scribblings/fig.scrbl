#lang scribble/manual

@(require (for-label racket/base))
@(require scribble/bnf)

@title{fig: Simple and Extensible Configuration}
@author{Micah Cantor}

@defmodulelang[fig]

@(define (fig x) (code #:lang "fig" x))

@section{Introduction}
Fig is a domain-specific language for composing configuration files.
Fig is a super-set of JSON with some additional features to reduce repetition and increase correctness.
These features include:

@itemlist[@item{@bold{Variables}: Don't repeat yourself! Name and reuse parts of the configuration.}
          @item{@bold{Input}: Include an environment of input variables to generalize the configuration to different contexts.}
          @item{@bold{Merge}: Create small, reusable objects; then combine them into more complex parts.}]

Here's a configuration for a web service that demonstrates these features, saved in a file named @code{config.fig}:

@codeblock|{
  #lang fig

  let user-info = {
    "username": @username,
    "email": @email
  }
  let server-info = {
    "base": if @local then "http://localhost:3000" else "https://website.com",
    "endpoints": ["/cats", "/dogs"]
  }
  user-info & server-info
}|

Here we have two objects, @code{user-info} and @code{server-info} that expect a few input variables (each
prefixed with the @code{@"@"} operator) to be provided: a username, an email, and a flag for if the service is running
locally. These two objects are merged togetherusing the @code{&} operator.

Then, in the following Racket program, we instantiate this Fig configuration by providing the required variables:

@codeblock|{
  #lang racket/base

  (require "config.fig")

  (fig->json (hash "username" "cat"
                   "email" "cat@email.com"
                   "local" #t))
}|

By requiring the Fig file, we obtain the @code{fig->json} procedure for that configuration.
By applying this procedure to a table of inputs, we obtain the following JSON output:

@verbatim|{
{
  "username": "cat",
  "email": "cat@cat.com"
  "base": "http://localhost:3000",
  "endpoints": ["/cats", "/dogs"]
}
}|

@section{The Fig Language}

Fig is designed to be a simple extension of JSON, so most features should be straightforward to understand.
However, this section will explain the finer details of the language.

@subsection{Literals}

Literals in Fig are the same as those available in JSON, corresponding to the following Racket types:

@itemlist[@item{@bold{Numbers} like @fig{1.5} are represented as @code{number?}.}
          @item{@bold{Strings} like @fig{"hello"} are represented as @code{string?}.}
          @item{@bold{Booleans} like @fig{true} and @fig{false} are represented as @code{boolean?}.}
          @item{@bold{Null} is written @fig{null} and is represented as @code{'null}.}
          @item{@bold{Lists} like @fig{[1, 2, 3]} are represented as @code{list?}.}
          @item{@bold{Objects} like @fig{{"hello": 5}} are represented as @code{hash?}.}]

@subsection{Variables}

Variable bindings in Fig can be introduced at the top level:

@verbatim|{
  let hello = "world"
}|

This form directly expands to @code{define}.

@subsection{Environment and Input}

When a Fig program is instantiated, an environment may optionally be provided.
When using Fig from a Racket program, the environment is provided to the @code{fig->hash} or @code{fig->json} procedure:

@codeblock|{
  (require "example.fig")

  (fig->hash (hash "hello" "world"))
}|

Keys in the environment must be strings, while values can be any Racket type.
To reference a key from the environment, prefix the name of the key with @code{@"@"} like @code{@"@hello"}.

@subsection{Conditionals and Equality}

Fig supports conditionals through the syntax:

@verbatim|{
  if CONDITION then CONSEQUENT else ALTERNATE
}|

This form expands directly to Racket @code{if}.
Fig also supports an equality operator @code{@"=="} that expands to Racket's @code{equal?}:

@verbatim|{
  let hello = "world"
  if hello == "world" then "yes!" else "no!"
}|

@subsection{Merge}

Fig provides a recursive object merge operator @code{&}. Merge is a commutative operator, meaning that for any two expressions @code{e1} and @code{e2}:

@codeblock|{
  e1 & e2 == e2 & e1
}|

The simplest case for merge is when @code{e1} and @code{e2} are objects with no shared keys.
In this case, the result of @code{e1 & e2} is a new object with the key/value pairs from @code{e1} and @code{e2}:

@codeblock|{
 {"key1": 1} & {"key2": 2}
}|

In the case that @code{e1} and @code{e2} share a common key, Fig will attempt to recursively merge the values of these keys:

@codeblock|{
 {"key1": {"key2": 2}} & {"key1": {"key3": 3}}
}|

evaluates to @codeblock|{
 {"key1": {"key2": 2, "key3": 3}} 
}|

Merge can fail in the following cases:

@itemlist[@item{@code{e1} and @code{e2} are different types.}
          @item{@code{e1} and @code{e2} are both non-objects but are not @code{equal?}.}
          @item{@code{e1} and @code{e2} are objects that share a key where the values fail one of the two previous criteria.}]

@subsection{Comments and Trailing Commas}

Fig supports line comments beginning with @code{//}:

@verbatim|{
let hello = "world" // this is a comment!
}|

Unlike in JSON, Fig also supports objects to have trailing commas:

@verbatim|{
{
  "key1": 1,
  "key2: 2, // this comma is optional
}
}|

@subsection{Grammar}

The full grammar for Fig in @(hyperlink "https://docs.racket-lang.org/brag/" "brag") BNF notation is as follows:

@codeblock|{
#lang brag

fig-program: [fig-let]* fig-expr
fig-let: /"let" ID /"=" fig-expr
@fig-expr: fig-object | fig-list | fig-merge | fig-apply | fig-equal | fig-cond | fig-env-ref | fig-lit
fig-object: /"{" [fig-kvpair (/"," fig-kvpair)* [/","]?] /"}"
fig-list: /"[" [fig-expr (/"," fig-expr)*] /"]"
fig-merge: fig-expr /"&" fig-expr
fig-apply : /"(" [fig-expr]+ /")"
fig-equal: fig-expr /"==" fig-expr
fig-env-ref: ENVREF
fig-cond: /"if" fig-expr /"then" fig-expr /"else" fig-expr
@fig-lit: ID | STRING | NUMBER | "true" | "false" | "null"
fig-kvpair: STRING /":" fig-expr
}|

@section{Using Fig}

Fig is designed to be used either from within the Racket ecosystem or completely separate from it.

@subsection{From a Racket Program}

Since Fig expands to a Racket module, it can be required from a Racket program.
In particular, the module provides two procedures:

@defproc[(fig->hash [environment (hash/c string? any/c)])
         (hash/c symbol? any/c)]{
  Instantiates a Fig program to a hash table.
  If provided, the keys in the @code{environment} are available in Fig as input variables.
}

@defproc[(fig->json [environment (hash/c string? any/c)])
         string?]{
  Instantiates JSON, represented as a string.
  Internally, uses the @(hyperlink "https://docs.racket-lang.org/json/" "json") library on the result of @code{fig->hash}.
  If provided, the keys in the @code{environment} are available in Fig as input variables.
}

@subsection{From the Command Line}

A Fig program can also be run from the command line.
The first command line argument should be the Fig file.
After that, any number of arguments can be specified as key/value pairs to be added to the environment.
All values in the environment will be strings.

@verbatim|{
$ racket main.rkt example.fig "hello" 1
}|