module CS334

open System
open AlphaReducti
open Parser
open LambdaParser


let rec sub (v: char) (with_e: Expr) (in_e: Expr) : Expr = 
    