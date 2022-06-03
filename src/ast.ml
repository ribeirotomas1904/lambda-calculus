module Env = Map.Make (String)
module StringSet = Set.Make (String)

type binary_operator = Addition | Multiplication

type expression =
  | Variable of string
  | Abstraction of { parameter : string; body : expression }
  | Application of { function' : expression; argument : expression }
  | E_int of int
  | Binary_operation of {
      operator : binary_operator;
      left : expression;
      right : expression;
    }

type value =
  | Closure of { env : value Env.t; parameter : string; body : expression }
  | V_int of int

exception Unbounded_variable of string

let raise_unbounded_variable variable = raise (Unbounded_variable variable)

let rec get_unbounded_variable bounded_variables expression =
  match expression with
  | Variable identifier ->
      let has_binding = StringSet.mem identifier bounded_variables in
      if has_binding then None else Some identifier
  | Abstraction { parameter; body } ->
      let bounded_variables' = StringSet.add parameter bounded_variables in
      get_unbounded_variable bounded_variables' body
  | Application { function'; argument } -> (
      (* TODO: refactor *)
      let unbounded_variable =
        get_unbounded_variable bounded_variables function'
      in
      match unbounded_variable with
      | Some _ -> unbounded_variable
      | None -> get_unbounded_variable bounded_variables argument)
  | E_int _ -> None
  | Binary_operation { left; right; _ } -> (
      (* TODO: refactor *)
      let unbounded_variable = get_unbounded_variable bounded_variables left in
      match unbounded_variable with
      | Some _ -> unbounded_variable
      | None -> get_unbounded_variable bounded_variables right)

(* TODO: should this be tail recursive? *)
let rec eval env expression =
  match expression with
  | Variable identifier -> (
      let value_opt = Env.find_opt identifier env in
      match value_opt with
      | None -> raise_unbounded_variable identifier
      | Some value -> value)
  | Abstraction { parameter; body } -> (
      let bounded_variables =
        Env.bindings env |> List.map fst |> StringSet.of_list
        |> StringSet.add parameter
      in
      let unbounded_variable = get_unbounded_variable bounded_variables body in
      match unbounded_variable with
      | Some identifier -> raise_unbounded_variable identifier
      | None -> Closure { env; parameter; body })
  | Application { function'; argument } -> (
      let argument_value = eval env argument in
      let function_value = eval env function' in
      match function_value with
      | Closure { env; parameter; body } ->
          let env' = Env.add parameter argument_value env in
          eval env' body
      | _ ->
          (* TODO: improve error message for applying a value that is not a closure *)
          failwith "TODO")
  | E_int i -> V_int i
  | Binary_operation { operator; left; right } -> (
      (* TODO: refactor for code reuse and better error messages *)
      let left_value = eval env left in
      let right_value = eval env right in
      match operator with
      | Addition -> (
          match (left_value, right_value) with
          | V_int x, V_int y -> V_int (x + y)
          | _ -> failwith "TODO")
      | Multiplication -> (
          match (left_value, right_value) with
          | V_int x, V_int y -> V_int (x * y)
          | _ -> failwith "TODO"))

let x =
  Abstraction
    {
      parameter = "x";
      body = Application { function' = Variable "x"; argument = Variable "x" };
    }

let xx = Application { function' = x; argument = x }

(* let _ = eval Env.empty xx *)

(* (fun x -> fun y -> x + y) *)
let add =
  Abstraction
    {
      parameter = "x";
      body =
        Abstraction
          {
            parameter = "y";
            body =
              Binary_operation
                {
                  operator = Addition;
                  left = Variable "x";
                  right = Variable "y";
                };
          };
    }

(* (fun x -> fun y -> x + y) 40 2 *)
let apply_add_to_40_2 =
  Application
    {
      function' = Application { function' = add; argument = E_int 40 };
      argument = E_int 2;
    }
