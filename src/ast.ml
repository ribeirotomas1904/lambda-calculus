module Env = Map.Make (String)
module StringSet = Set.Make (String)

type binary_operator =
  | Addition
  | Multiplication
  | Subtraction
  | Division
  | Logical_and
  | Logical_or
  | Structural_equality
  | Greater_than
  | Less_than
  | Sequencing

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
  | If_then_else of {
      predicate : expression;
      then_branch : expression;
      else_branch : expression;
    }
  | Let_rec_in of {
      variable_name : string;
      recursive_function : expression;
      in_branch : expression;
    }

type value =
  | Closure of { env : value Env.t; parameter : string; body : expression }
  | Native_function of (value -> value)
  | V_int of int
  | V_bool of bool
  | V_unit

let string_of_value = function
  | V_int i -> string_of_int i
  | V_bool b -> string_of_bool b
  | V_unit -> "()"
  | Closure _ | Native_function _ -> "<fun>"

exception Unbounded_variable of string

let raise_unbounded_variable variable = raise (Unbounded_variable variable)

(* TODO: think in a better name *)
(* TODO: test *)
let some_or_else lazy_opt opt =
  match opt with Some _ -> opt | None -> lazy_opt ()

let rec get_unbounded_variable bounded_variables expression =
  match expression with
  | Variable identifier ->
      let has_binding = StringSet.mem identifier bounded_variables in
      if has_binding then None else Some identifier
  | Abstraction { parameter; body } ->
      let bounded_variables' = StringSet.add parameter bounded_variables in
      get_unbounded_variable bounded_variables' body
  | Application { function'; argument } ->
      (* TODO: test *)
      get_unbounded_variable bounded_variables function'
      |> some_or_else (fun () ->
             get_unbounded_variable bounded_variables argument)
  | E_int _ -> None
  | E_bool _ -> None
  | E_unit -> None
  | Binary_operation { left; right; _ } ->
      (* TODO: test *)
      get_unbounded_variable bounded_variables left
      |> some_or_else (fun () -> get_unbounded_variable bounded_variables right)
  | Unary_operation { operand; _ } ->
      get_unbounded_variable bounded_variables operand
  | If_then_else { predicate; then_branch; else_branch } ->
      (* TODO: test *)
      get_unbounded_variable bounded_variables predicate
      |> some_or_else (fun () ->
             get_unbounded_variable bounded_variables then_branch)
      |> some_or_else (fun () ->
             get_unbounded_variable bounded_variables else_branch)
  | Let_rec_in { variable_name; recursive_function; in_branch } ->
      let bounded_variables' = StringSet.add variable_name bounded_variables in
      get_unbounded_variable bounded_variables recursive_function
      |> some_or_else (fun () ->
             get_unbounded_variable bounded_variables' in_branch)

let z_combinator_part =
  Abstraction
    {
      parameter = "x";
      body =
        Application
          {
            function' = Variable "f";
            argument =
              Abstraction
                {
                  parameter = "v";
                  body =
                    Application
                      {
                        function' =
                          Application
                            {
                              function' = Variable "x";
                              argument = Variable "x";
                            };
                        argument = Variable "v";
                      };
                };
          };
    }

let z_combinator =
  Abstraction
    {
      parameter = "f";
      body =
        Application
          { function' = z_combinator_part; argument = z_combinator_part };
    }

let apply_z_combinator f =
  Application { function' = z_combinator; argument = f }

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
      | Native_function f -> f argument_value
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
          let right_value = eval env right in
          let left_value = eval env left in
          match (left_value, right_value) with
          | V_int x, V_int y -> V_int (x + y)
          | _ -> failwith "TODO")
      | Multiplication -> (
          let right_value = eval env right in
          let left_value = eval env left in
          match (left_value, right_value) with
          | V_int x, V_int y -> V_int (x * y)
          | _ -> failwith "TODO")
      | Subtraction -> (
          let right_value = eval env right in
          let left_value = eval env left in
          match (left_value, right_value) with
          | V_int x, V_int y -> V_int (x - y)
          | _ -> failwith "TODO")
      | Division -> (
          let right_value = eval env right in
          let left_value = eval env left in
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
          | _ -> failwith "TODO")
      | Structural_equality -> (
          let right_value = eval env right in
          let left_value = eval env left in
          match (left_value, right_value) with
          | V_int x, V_int y -> V_bool (x = y)
          | _ -> failwith "TODO")
      | Greater_than -> (
          let right_value = eval env right in
          let left_value = eval env left in
          match (left_value, right_value) with
          | V_int x, V_int y -> V_bool (x > y)
          | _ -> failwith "TODO")
      | Less_than -> (
          let right_value = eval env right in
          let left_value = eval env left in
          match (left_value, right_value) with
          | V_int x, V_int y -> V_bool (x < y)
          | _ -> failwith "TODO")
      | Sequencing -> (
          let left_value = eval env left in
          match left_value with
          | V_unit -> eval env right
          | _ -> failwith "TODO"))
  | Unary_operation { operator; operand } -> (
      let operand_value = eval env operand in
      match operator with
      | Logical_not -> (
          match operand_value with
          | V_bool b -> V_bool (not b)
          | _ -> failwith "TODO"))
  | If_then_else { predicate; then_branch; else_branch } -> (
      let predicate_value = eval env predicate in
      match predicate_value with
      | V_bool true -> eval env then_branch
      | V_bool false -> eval env else_branch
      | _ -> failwith "TODO")
  | Let_rec_in { variable_name; recursive_function; in_branch } -> (
      match recursive_function with
      | Abstraction _ ->
          let recursive_function' =
            Abstraction { parameter = variable_name; body = recursive_function }
          in
          let closure = apply_z_combinator recursive_function' |> eval env in
          let env' = Env.add variable_name closure env in
          eval env' in_branch
      | _ -> failwith "TODO")

let print =
  Native_function
    (fun value ->
      string_of_value value |> print_endline;
      V_unit)

let native_function_with_multiple_arguments =
  Native_function
    (fun a ->
      Native_function
        (fun b ->
          Native_function
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

let env_with_native_functions =
  Env.empty |> Env.add "print" print
  |> Env.add "native_function_with_multiple_arguments"
       native_function_with_multiple_arguments

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

let native_function_with_multiple_arguments =
  Application
    {
      function' =
        Application
          {
            function' =
              Application
                {
                  function' = Variable "native_function_with_multiple_arguments";
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

(* why can this run forever? how is that evaluated? *)
(* let rec f = (fun x -> (fun _ -> f (x + 1)) (print x)) in f 0 *)
let let_rec_in_test =
  Let_rec_in
    {
      variable_name = "f";
      recursive_function =
        Abstraction
          {
            parameter = "x";
            body =
              Application
                {
                  function' =
                    Abstraction
                      {
                        parameter = "_";
                        body =
                          Application
                            {
                              function' = Variable "f";
                              argument =
                                Binary_operation
                                  {
                                    operator = Addition;
                                    left = Variable "x";
                                    right = E_int 1;
                                  };
                            };
                      };
                  argument =
                    Application
                      { function' = Variable "print"; argument = Variable "x" };
                };
          };
      in_branch = Application { function' = Variable "f"; argument = E_int 0 };
    }
