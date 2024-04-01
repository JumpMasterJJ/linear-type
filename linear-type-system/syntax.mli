(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type qualifier =
  | QLinear
  | QUnrestricted

type pre_ty =
  | PBool
  | PPair of ty * ty
  | PFunc of ty * ty

and ty =
  | TyQual of qualifier * pre_ty

type boolean =
  | BTrue
  | BFalse

  type var =
  | Var of string

type term =
  | TmVar of var * info
  | TmBool of qualifier * boolean * info
  | TmIf of term * term * term * info
  | TmPair of qualifier * term * term * info
  | TmSplit of term * var * var * term * info
  | TmAbs of qualifier * var * ty * term * info
  | TmApp of term * term * info

type binding =
  | VarTyBind of var * ty

type context = binding list

type command =
    Import of string
  | Eval of term * info



(* Printing *)
val print_tm: term -> unit

(* Misc *)
val tmInfo: term -> info

