open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

(* Evaluation : big step *)
let rec eval s t : store * var =
  match t with
  | TmVar (x, _) ->
      (s, x)
  | TmBool (q, b, fi) ->
      let s' = add_noname s (q, VBool b) in
      let x' = noname () in
      (s', x')
  | TmIf (t, t1, t2, _) -> (
      let s', x = eval s t in
      let s' = del_var s' x in
      match find_var s' x with
      | q, VBool BTrue ->
          eval s' t1
      | q, VBool BFalse ->
          eval s' t2
      | _ ->
          raise NoRuleApplies )
  | TmPair (q, t1, t2, _) ->
      let s', x = eval s t1 in
      let s', y = eval s' t2 in
      let s' = add_noname s' (q, VPair (x, y)) in
      let x' = noname () in
      (s', x')
  | TmSplit (t1, x, y, t2, _) ->
      let s', p = eval s t1 in
      let q, vx, vy =
        match find_var s' p with q, VPair (vx, vy) -> (q, vx, vy) | _ -> raise NoRuleApplies
      in
      let s'' = del_var s p in
      let s'' = sub_var s'' x vx in
      let s'' = sub_var s'' y vy in
      eval s'' t2
  | TmAbs (q, x, _, t, _) ->
      let s' = add_noname s (q, VAbs (x, t)) in
      let x' = noname () in
      (s', x')
  | TmApp (t1, t2, _) -> (
      let s', x1 = eval s t1 in
      let s', x2 = eval s' t2 in
      match find_var s' x1 with
      | q, VAbs (x, t) ->
          let s'' = del_var s' x1 in
          eval (sub_var s'' x x2) t
      | _ ->
          raise NoRuleApplies )


let eval t =
  let s, x = eval empty_store t in
  find_var s x


(* ------------------------   Typing  ------------------------ *)

exception UsedVariable

exception NotExhaustAllLinear

exception UnmatchedType

exception UnIncludedLin

exception UsedVariableOrOutOfScope

exception UnusedVariable

type binding = var * ty

type context = binding list

let empty_ctx = []

let bind x t ctx = (x, t) :: ctx

let unbind x ctx =
  let pred (y, ty) = x = y in
  match List.find_opt pred ctx with
  | Some _ ->
      List.remove_assoc x ctx
  | None ->
      raise UsedVariableOrOutOfScope


let diff x ctx =
  let pred (y, ty) = x = y in
  match List.find_opt pred ctx with
  | Some (_, TyQual (QLinear, _)) ->
      raise UnusedVariable
  | Some (_, TyQual (QUnrestricted, _)) ->
      List.remove_assoc x ctx
  | None ->
      raise UsedVariableOrOutOfScope


let find x ctx =
  let pred (y, ty) = x = y in
  match List.find_opt pred ctx with Some (_, ty) -> ty | None -> raise UsedVariableOrOutOfScope


let partial_order q ty =
  let (TyQual (q', _)) = ty in
  match (q, q') with QUnrestricted, QLinear -> false | _ -> true


let ( << ) = partial_order

let partial_order q ctx = List.for_all (fun (_, ty) -> partial_order q ty) ctx

let ( <<< ) = partial_order

(* Context Spliting *)

(* Typing Rules *)
let rec typeof_aux t ctx : ty * context =
  match t with
  | TmVar (x, fi) -> (
      let (TyQual (q, _) as ty) = find x ctx in
      match q with QLinear -> (ty, unbind x ctx) | QUnrestricted -> (ty, ctx) )
  | TmBool (q, b, fi) ->
      (TyQual (q, PBool), ctx)
  | TmIf (t, t1, t2, fi) -> (
      let ty_bool, ctx' = typeof_aux t ctx in
      let ty1, ctx2 = typeof_aux t1 ctx' in
      let ty2, ctx3 = typeof_aux t2 ctx' in
      match ty_bool with
      | TyQual (_, PBool) when ty1 = ty2 && ctx2 = ctx3 ->
          (ty1, ctx2)
      | _ ->
          raise UnmatchedType )
  | TmPair (q, t1, t2, fi) ->
      let ty1, ctx' = typeof_aux t1 ctx in
      let ty2, ctx'' = typeof_aux t2 ctx' in
      if q << ty1 && q << ty2 then (TyQual (q, PPair (ty1, ty2)), ctx'') else raise UnIncludedLin
  | TmSplit (t1, x, y, t2, fi) -> (
      let ty_pair, ctx' = typeof_aux t1 ctx in
      match ty_pair with
      | TyQual (_, PPair (ty_x, ty_y)) ->
          let ty, ctx'' = ctx |> bind x ty_x |> bind y ty_y |> typeof_aux t2 in
          (ty, ctx'' |> diff x |> diff y)
      | _ ->
          raise UnmatchedType )
  | TmAbs (q, x, ty, t, fi) -> (
      let ty', ctx' = ctx |> bind x ty |> typeof_aux t in
      let ctx_expected = diff x ctx' in
      match (q = QUnrestricted, ctx = ctx_expected) with
      | true, false ->
          raise UnIncludedLin
      | _ ->
          (TyQual (q, PFunc (ty, ty')), ctx_expected) )
  | TmApp (t1, t2, fi) -> (
      let ty1, ctx' = typeof_aux t1 ctx in
      let ty2, ctx'' = typeof_aux t2 ctx' in
      match (ty1, ty2) with
      | TyQual (_, PFunc (ty11, ty12)), ty11' when ty11 = ty11' ->
          (ty12, ctx'')
      | _ ->
          raise UnmatchedType )


let typeof t : ty =
  let ty, ctx = typeof_aux t empty_ctx in
  if QUnrestricted <<< ctx then ty else raise NotExhaustAllLinear
