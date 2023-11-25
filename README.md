# Fig: Simple and Extensible Configuration

Fig is a domain-specific language for composing con*fig*uration files.
Fig is a super-set of JSON with some additional features to reduce repetition and increase correctness.
Here's a small example:

```rkt
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
```
Here we have two objects, `user-info` and `server-info` that expect a few input variables (each
prefixed with the `@` operator) to be provided by the user: a username, an email, and a flag for if the service is running
locally. These two objects are merged togetherusing the `&` operator.

Fig is built in the Racket ecosystem, but can be used outside of Racket from the command line. 
It can be compiled to either to a Racket hash table or to JSON.

## Using Fig from Racket

If we save the above file as `example.fig`, then we can use it from another Racket program:

```
#lang racket/base

(require "example.fig")

(fig->json (hash "username" "cat"
                 "email" "cat@email.com"
                 "local" #t))
```

## Using Fig from the CLI

To compile, execute

```
$ raco exe fig-cli/main.rkt
```

Then to execute a Fig program, run

```
$ ./fig-cli/main example.fig
```

After the first argument (the filename) all other arguments will be passed to the Fig program as the environment as key/value pairs.

## Documentation

Documentation can be found on [the Racket package directory](https://docs.racket-lang.org/fig/).
