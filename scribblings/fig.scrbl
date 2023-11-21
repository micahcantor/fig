#lang scribble/manual

@(require (for-label racket/base))

@title{fig: Simple and Extensible Configuration}
@author{Micah Cantor}

@defmodulelang[fig]

@section{Introduction}
Fig is a domain-specific language for composing configuration files.
Fig is a super-set of JSON with some additional features to reduce repetition and increase correctness.
These features include:

@itemlist[@item{@bold{Variables}: Don't repeat yourself! Name and reuse parts of the configuration.}
          @item{@bold{Input}: Include an environment of constants or functions to generalize the configuration to different contexts.}
          @item{@bold{Merge}: Create small, reusable objects; then combine them into more complex parts.}]

Here's a configuration for a web service that demonstrates these features, saved in a file named @code{config.fig}:

@codeblock|{
  #lang fig

  let user = {
    "username": @username,
    "email": @email
  }
  let server-info = {
    "base": if @local then "http://localhost:3000" else "https://website.com",
    "endpoints": ["/cats", "/dogs"]
  }
  user & server-info
}|

Here we have two objects, @code{user} and @code{server-info} that expect a few input variables (each
prefixed with the at-sign operator) to be provided: a username, an email, and a flag for if the service is running
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