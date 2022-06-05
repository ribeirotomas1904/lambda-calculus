module Env = Map.Make (String)
module StringSet = Set.Make (String)

type binary_operator =
  | Addition
  | Multiplication
  | Subtraction
  | Division
  | Logical_and
  | Logical_or

type unary_operator = Logical_not

type expression =
  | Variable of string
  | Abstraction of { parameter : string; body : expression }
  | Application of { function' : expression; argument : expression }
  | E_int of int
  | E_bool of bool
  | E_unit
  | Binary_operation of {
      operator : binary_operator;
      left : expression;
      right : expression;
    }
  | Unary_operation of { operator : unary_operator; operand : expression }

type value =
  | Closure of { env : value Env.t; parameter : string; body : expression }
  | Primitive_function of (value -> value)
  | V_int of int
  | V_bool of bool
  | V_unit

let string_of_value = function
  | V_int i -> string_of_int i
  | V_bool b -> string_of_bool b
  | V_unit -> "()"
  | Closure _ | Primitive_function _ -> "<fun>"

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
  | E_bool _ -> None
  | E_unit -> None
  | Binary_operation { left; right; _ } -> (
      (* TODO: refactor *)
      let unbounded_variable = get_unbounded_variable bounded_variables left in
      match unbounded_variable with
      | Some _ -> unbounded_variable
      | None -> get_unbounded_variable bounded_variables right)
  | Unary_operation { operand; _ } ->
      get_unbounded_variable bounded_variables operand

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
      | Primitive_function f -> f argument_value
      | _ ->
          (* TODO: improve error message for applying a value that is not a closure *)
          failwith "TODO")
  | E_int i -> V_int i
  | E_bool b -> V_bool b
  | E_unit -> V_unit
  | Binary_operation { operator; left; right } -> (
      (* TODO: refactor for code reuse and better error messages *)
      match operator with
      | Addition -> (
          let left_value = eval env left in
          let right_value = eval env right in
          match (left_value, right_value) with
          | V_int x, V_int y -> V_int (x + y)
          | _ -> failwith "TODO")
      | Multiplication -> (
          let left_value = eval env left in
          let right_value = eval env right in
          match (left_value, right_value) with
          | V_int x, V_int y -> V_int (x * y)
          | _ -> failwith "TODO")
      | Subtraction -> (
          let left_value = eval env left in
          let right_value = eval env right in
          match (left_value, right_value) with
          | V_int x, V_int y -> V_int (x - y)
          | _ -> failwith "TODO")
      | Division -> (
          let left_value = eval env left in
          let right_value = eval env right in
          match (left_value, right_value) with
          | V_int x, V_int y -> V_int (x / y)
          | _ -> failwith "TODO")
      | Logical_and -> (
          let left_value = eval env left in
          match left_value with
          | V_bool true -> (
              let right_value = eval env right in
              match right_value with
              | V_bool _ -> right_value
              | _ -> failwith "TODO")
          | V_bool false -> V_bool false
          | _ -> failwith "TODO")
      | Logical_or -> (
          let left_value = eval env left in
          match left_value with
          | V_bool true -> V_bool true
          | V_bool false -> (
              let right_value = eval env right in
              match right_value with
              | V_bool _ -> right_value
              | _ -> failwith "TODO")
          | _ -> failwith "TODO"))
  | Unary_operation { operator; operand } -> (
      let operand_value = eval env operand in
      match operator with
      | Logical_not -> (
          match operand_value with
          | V_bool b -> V_bool (not b)
          | _ -> failwith "TODO"))

let print =
  Primitive_function
    (fun value ->
      string_of_value value |> print_endline;
      V_unit)

let primitive_function_with_multiple_arguments =
  Primitive_function
    (fun a ->
      Primitive_function
        (fun b ->
          Primitive_function
            (fun c ->
              match (a, b, c) with
              | V_int a, V_int b, V_unit ->
                  eval
                    (Env.empty |> Env.add "print" print)
                    (Application
                       {
                         function' = Variable "print";
                         argument = E_int (a + b);
                       })
              | _ -> failwith "TODO")))

let env_with_primitive_functions =
  Env.empty |> Env.add "print" print
  |> Env.add "primitive_function_with_multiple_arguments"
       primitive_function_with_multiple_arguments

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

let print_42 =
  Application { function' = Variable "print"; argument = apply_add_to_40_2 }

let primitive_function_with_multiple_arguments =
  Application
    {
      function' =
        Application
          {
            function' =
              Application
                {
                  function' =
                    Variable "primitive_function_with_multiple_arguments";
                  argument = E_int 40;
                };
            argument = E_int 2;
          };
      argument = E_unit;
    }

let xor =
  Abstraction
    {
      parameter = "a";
      body =
        Abstraction
          {
            parameter = "b";
            body =
              Binary_operation
                {
                  operator = Logical_or;
                  left =
                    Binary_operation
                      {
                        operator = Logical_and;
                        left = Variable "a";
                        right =
                          Unary_operation
                            { operator = Logical_not; operand = Variable "b" };
                      };
                  right =
                    Binary_operation
                      {
                        operator = Logical_and;
                        left =
                          Unary_operation
                            { operator = Logical_not; operand = Variable "a" };
                        right = Variable "b";
                      };
                };
          };
    }

let apply_xor a b =
  Application
    { function' = Application { function' = xor; argument = a }; argument = b }
