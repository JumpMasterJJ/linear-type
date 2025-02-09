(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

val eval : term -> value

val typeof : term -> ty