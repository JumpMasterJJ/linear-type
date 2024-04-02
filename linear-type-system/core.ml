open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies


(* Evaluation : big step *)
let rec eval s t : store * var =
  match t with
  | TmVar(x,_) ->
      s , x
  | TmBool(q,b,fi) ->
      let s' = add_noname s (q, VBool b) in
      let x' = noname () in
        s' , x'
  | TmIf(t,t1,t2,_) ->
      let (s', x) = eval s t in
      let s' = del_var s' x in
      (match find_var s' x with
      | (q, VBool BTrue) -> eval s' t1
      | (q, VBool BFalse) -> eval s' t2
      | _ -> raise NoRuleApplies)
  | TmPair(q,t1,t2,_) ->
      let (s', x) = eval s t1 in
      let (s', y) = eval s' t2 in
      let s' = add_noname s' (q, VPair(x,y)) in
      let x' = noname () in
        s' , x'
  | TmSplit(t1,x,y,t2,_) ->
      let (s', p) = eval s t1 in
      let q, vx, vy =
        (match find_var s' p with
        | q, VPair(vx, vy) -> q, vx, vy
        | _ -> raise NoRuleApplies)
      in
      let s'' = del_var s p in
      let s'' = sub_var s'' x vx in
      let s'' = sub_var s'' y vy in
        eval s'' t2
  | TmAbs(q,x,_,t,_) ->
      let s' = add_noname s (q, VAbs(x,t)) in
      let x' = noname () in
        s', x'
  | TmApp(t1,t2,_) ->
      let (s', x1) = eval s t1 in
      let (s', x2) = eval s' t2 in
      (match find_var s' x1 with
      | q, VAbs(x,t) ->
          let s'' = del_var s' x1 in
            eval (sub_var s'' x x2) t
      | _ -> raise NoRuleApplies)

let eval t =
  let (s, x) = eval empty_store t in
  find_var s x