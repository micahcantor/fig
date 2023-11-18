#lang brag

silver-program: (silver-expr)*
silver-expr: silver-object | silver-list | silver-bool | STRING | NUMBER | "null"
silver-object: "{" [silver-kvpair ("," silver-kvpair)* [","]+] "}"
silver-list: "[" [silver-expr ("," silver-expr)*] "]"
silver-bool: "true" | "false"
silver-kvpair: STRING ":" silver-expr