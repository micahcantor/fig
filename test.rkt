#lang silver

let a = {"one": 1, "two": {"three": 3}}
let b = {"one": 1, "two": {"four": 4}}
let c = {"one": if 1 == 2 then 1 else 0}
a & b & c