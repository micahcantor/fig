#lang brag

silver-program: [silver-let]* silver-expr
silver-let: /"let" ID /"=" silver-expr
@silver-expr: silver-object | silver-list | silver-merge | silver-equal | silver-cond | silver-lit
silver-object: /"{" [silver-kvpair (/"," silver-kvpair)* [/","]?] /"}"
silver-list: /"[" [silver-expr (/"," silver-expr)*] /"]"
silver-merge: silver-expr /"&" silver-expr
silver-equal: silver-expr /"==" silver-expr
silver-cond: /"if" silver-expr /"then" silver-expr /"else" silver-expr
@silver-lit: ID | STRING | NUMBER | "true" | "false" | "null"
silver-kvpair: STRING /":" silver-expr