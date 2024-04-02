open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

(* Syntax *)
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

type store = (var * value) list

let empty_store : store = []

let add_var s x v : store =
  (x, v) :: s

let find_var s x : value =
  let (_, v) = List.find (fun (y,_) -> x = y) s in v

(* TODO: with qualifier *)
let del_var s x : store =
  List.filter (fun (y,_) -> x != y) s

(* [x |-> y] *)
let sub_var s x y : store =
  let v = find_var s y in
  let s' = del_var s x in
  add_var s' x v

let index = ref 0

let noname () = Var ("noname" ^ string_of_int !index)

let add_noname s v =
  incr index;
  (noname (), v) :: s
  


type binding =
  | VarTyBind of var * ty

type context = binding list

type command =
    Import of string
  | Eval of term * info

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
  | TmVar(_,fi) -> fi
  | TmBool(_,_,fi) -> fi
  | TmIf(_,_,_,fi) -> fi
  | TmPair(_,_,_,fi) -> fi
  | TmSplit(_,_,_,_,fi) -> fi
  | TmAbs(_,_,_,_,fi) -> fi
  | TmApp(_,_,fi) -> fi

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let rec print_tm t =
  match t with
  | TmVar(x,_) -> 
      obox0();  
      print_var x;
      cbox()
  | TmBool(q,b,_) ->
      obox();
      print_qual q; pr " "; print_b b;
      cbox()
  | TmIf(t,t1,t2,_) ->
      obox0();
      pr "if "; print_tm t; pr " then";
        print_tm t1; break();
      pr "else";
        print_tm t2; break();
      cbox()
  | TmPair(q,t1,t2,_) ->
      obox0();
      print_qual q; pr " <"; print_tm t1;
      pr ","; print_tm t2; pr ">";
      cbox()
  | TmSplit(t1,x,y,t2,_) ->
      obox0();
      pr "split "; print_tm t1; pr " as ";
      print_var x; pr ","; print_var y;
      pr " in "; print_tm t2;
      cbox()
  | TmAbs(q,x,ty,t,_) ->
      obox0();
      print_qual q; pr " lambda ";
      print_var x; pr ":"; print_ty ty;
      pr "."; print_tm t;
      cbox()
  | TmApp(t1,t2,_) ->
      obox0();
      print_tm t1; pr " "; print_tm t2;
      cbox()

and print_var = function
  | Var(x) -> obox0(); pr x; cbox()

and print_qual = function
  | QLinear -> obox0(); pr "lin"; cbox()
  | QUnrestricted -> obox0(); pr "un"; cbox()

and print_b = function
  | BTrue -> obox0(); pr "true"; cbox()
  | BFalse -> obox0(); pr "false"; cbox()

and print_ty = function
  | TyQual(q,pt) ->
      obox0();
      print_qual q; pr " "; print_prety pt;
      cbox()

and print_prety = function
  | PBool ->
      obox0(); pr "Bool"; cbox()
  | PPair(ty1,ty2) ->
      obox0();
      print_ty ty1; pr "*"; print_ty ty2;
      cbox();
  | PFunc(ty1,ty2) ->
      obox0();
      print_ty ty1; pr "->"; print_ty ty2;
      cbox()


let print_pv pv =
  match pv with
  | VBool b ->
      obox0();
      print_b b;
      cbox()
  | VPair(v1,v2) ->
      obox0();
      pr "<"; print_var v1; pr ","; print_var v2; pr ">";
      cbox()
  | VAbs(x,t) ->
      obox0();
      pr "lambda "; print_var x; pr "."; print_tm t;
      cbox() 

let print_value (q, pv) =
  obox0();
  print_qual q; pr " "; print_pv pv;
  cbox()

let print_store (s:store) =
  obox0();
  List.iter (fun (x,v) -> print_var x; pr " |-> "; print_value v; break()) s;
  cbox()
