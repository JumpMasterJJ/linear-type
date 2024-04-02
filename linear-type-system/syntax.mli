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

(* Run-time data *)
type pre_value =
  | VBool of boolean
  | VPair of var * var
  | VAbs of var * term

type value = qualifier * pre_value

type store

val empty_store : store

val add_var : store -> var -> value -> store

val find_var : store -> var -> value

(* TODO: with qualifier *)
val del_var : store -> var -> store

(* [x |-> y] *)
val sub_var : store -> var -> var -> store

val noname : unit -> var

val add_noname : store -> value -> store


type binding =
  | VarTyBind of var * ty

type context = binding list

type command =
    Import of string
  | Eval of term * info



(* Printing *)
val print_tm: term -> unit

val print_value: value -> unit

val print_store: store -> unit

val print_var: var -> unit

(* Misc *)
val tmInfo: term -> info