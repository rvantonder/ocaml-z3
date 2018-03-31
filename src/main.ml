open Smtlib
open Format

(** Asks Z3 to solve this:
*
* (set-option :print-success true)
*
* (declare-const x String)
*
* (assert (= (str.++ "hello " x " world") "hello quux world"))
* (check-sat)
* (get-model)
*)

let solver_path = "/Users/rvt/z3/z3-4.5.0-x64-osx-10.11.6/bin/z3"

let pp term =
  Format.printf "%s@." (term |> term_to_sexp |> sexp_to_string)

let get_assertion_result solver (lhs : Smtlib.term list) (rhs : Smtlib.term) =
  let term = (Smtlib.equals (Smtlib.Str.concat lhs) rhs) in
  Smtlib.assert_ solver term;
  match Smtlib.check_sat solver with
  | Sat -> Smtlib.get_model solver
  | _ -> failwith "No sat"

let () =
  let solver = make_solver solver_path in
  Smtlib.declare_const solver (Id "x") (Sort (Id "String"));
  let left_pattern =
    [ Smtlib.QString "hello "
    ; Smtlib.Const (Id "x")
    ; Smtlib.QString " world"
    ]
  in
  let right_pattern = Smtlib.QString "hello quux world" in
  let result = get_assertion_result solver left_pattern right_pattern in
  List.iter
    (fun (Id id, term) ->
       match term with
       | String s -> Format.printf "@.id: %s = %s@." id s;
       | _ -> Format.printf "@.id %s is a non-string term I didn't expect" id)
    result
