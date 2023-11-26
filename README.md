# Fig: Simple and Extensible Configuration

Fig is a domain-specific language for composing con*fig*uration files.
Fig is a super-set of JSON with some additional features to reduce repetition and increase correctness.
It is built in the Racket ecosystem, and can be compiled either to a Racket value or to JSON.
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

## Using Fig from Racket

If we save the above file as `example.fig`, then we can use it from another Racket program:

```
#lang racket/base

(require "example.fig")

(fig->json (hash "username" "cat"
                 "email" "cat@email.com"
                 "local" #t))
```

This produces the following JSON:

```json
{
  "username": "cat",
  "email": "cat@cat.com"
  "base": "http://localhost:3000",
  "endpoints": ["/cats", "/dogs"]
}
```

## Documentation

Documentation can be found on [the Racket package directory](https://docs.racket-lang.org/fig/).
