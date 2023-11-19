#lang brag

fig-program: [fig-let]* fig-expr
fig-let: /"let" ID /"=" fig-expr
@fig-expr: fig-object | fig-list | fig-merge | fig-equal | fig-cond | fig-lit
fig-object: /"{" [fig-kvpair (/"," fig-kvpair)* [/","]?] /"}"
fig-list: /"[" [fig-expr (/"," fig-expr)*] /"]"
fig-merge: fig-expr /"&" fig-expr
fig-equal: fig-expr /"==" fig-expr
fig-cond: /"if" fig-expr /"then" fig-expr /"else" fig-expr
@fig-lit: ID | STRING | NUMBER | "true" | "false" | "null"
fig-kvpair: STRING /":" fig-expr