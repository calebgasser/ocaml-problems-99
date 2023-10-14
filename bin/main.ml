exception Problem_error of string

type test_status = 
| TPass
| TFail

type test_data = {
  simple_list: string list;
  palindrome: string list;
}

module TestOutput = struct

  let string_of_option_string = function 
    | None -> "None"
    | Some s -> s

  let rec string_of_list_aux = function
    | [] -> ""
    | [a] -> a ^ ""
    | h :: t -> h ^ ";" ^ string_of_list_aux t

  let string_of_list lst = Printf.sprintf "[%s]" (string_of_list_aux lst)  

  let string_of_option_list = function
    | None -> "None"
    | Some l -> string_of_list l

  let get_pass_fail exp got = 
    if exp = got then TPass else TFail
  
  let print_pass_fail exp got = 
    if exp = got then Printf.sprintf "[PASS]" else Printf.sprintf "[FAIL]" 

  let print name input exp got = 
    Format.printf "---- %s %s -----\nInput: %s\nExpected: %s\nGot: %s\n" (print_pass_fail exp got) name  input exp got;
    get_pass_fail exp got

end

module Problems99 = struct

  (** Write a function last : 'a list -> 'a option that returns the last element of a list *)
  let rec last = function
    | [] -> None
    | [a] -> Some a
    | _ :: t -> last t

  (** Find the last but one (last and penultimate) elements of a list. *)
  let rec last_two = function
    | [] -> None
    | [_] -> None
    | [a; b] -> Some [a; b]
    | _ :: t -> last_two t


  (** Find the N'th element of a list.*)
  let rec nth current = function
    | [] -> raise (Problem_error "List out of bounds")
    | h :: t -> if current = 0 then h else nth (current - 1) t


  (** This function is tail-recursive: it uses a constant amount of stack memory regardless of list size. *)
  let length lst =
    let rec length_aux len = function
      | [] -> len
      | _ :: t -> length_aux (len + 1) t 
    in
    length_aux 0 lst

  (** Reverse a list.*)
  let rev lst =
    let rec rev_aux acc = function
      | [] -> acc
      | h :: t -> rev_aux (h :: acc) t 
    in
    rev_aux [] lst

  let is_palindrome lst =
    lst = (List.rev lst)

end

module Tests = struct
  type pass_fail_count = {
    pass: int;
    fail: int;
  }

  let test_last lst =   
    let input = lst
      |> TestOutput.string_of_list in
    let expected = "d" in
    let actual = lst
      |> Problems99.last
      |> TestOutput.string_of_option_string in 
    TestOutput.print "Tail of a List" input expected actual;;

  let test_last_two lst =
    let input = lst
      |> TestOutput.string_of_list in
    let expected = Some ["c"; "d";]
      |> TestOutput.string_of_option_list in
    let actual = lst
      |> Problems99.last_two 
      |> TestOutput.string_of_option_list in
    TestOutput.print "Last Two Elements of a List" input expected actual;;

  let test_nth lst =
    let input = lst
      |> TestOutput.string_of_list in
    let expected = "c" in
    let actual = Problems99.nth 2 lst
    in 
    TestOutput.print "Nth element of a List" input expected actual;;

  let test_nth_out_of_bounds lst =
    let input = lst
      |> TestOutput.string_of_list in
    let expected = "List out of bounds" in
    let actual = try Problems99.nth (List.length lst) lst with
      Problem_error msg -> msg
    in 
    TestOutput.print "Nth element of a List is out of bounds" input expected actual;;

  let test_length lst =
    let input = lst
      |> TestOutput.string_of_list in
    let expected = string_of_int 4 in
    let actual = lst
      |> Problems99.length 
      |> string_of_int in 
      TestOutput.print "Length of list" input expected actual;;
    
  let test_reverse lst =
    let input = lst
      |> TestOutput.string_of_list in
    let expected = lst
      |> List.rev
      |> TestOutput.string_of_list  in
    let actual = lst
      |> Problems99.rev
      |> TestOutput.string_of_list in 
      TestOutput.print "Reverse list" input expected actual;;

  let test_palindrome lst =
    let input = lst
      |> TestOutput.string_of_list in
    let expected = string_of_bool true in
    let actual = lst
      |> Problems99.is_palindrome
      |> string_of_bool in 
      TestOutput.print "Is Palindrome" input expected actual;;

  let rec get_summary_counts lst num_pass num_fail = match lst with
    | [] ->  {pass = num_pass; fail = num_fail}
    | h :: t ->
      match h with
        | TFail -> get_summary_counts t num_pass (num_fail + 1)
        | TPass -> get_summary_counts t (num_pass + 1 ) num_fail

  let print_summary counts = 
    Format.printf "\n[Pass: %d - Fail: %d]" counts.pass counts.fail

  let test_all test_data =
    let results = [ test_last test_data.simple_list;
    test_last_two test_data.simple_list;
    test_nth test_data.simple_list;
    test_nth_out_of_bounds test_data.simple_list;
    test_length test_data.simple_list;
    test_reverse test_data.simple_list;
    test_palindrome test_data.palindrome]
    in
      get_summary_counts results 0 0 |> print_summary;
    

end

let data: test_data = {
  simple_list = ["a"; "b"; "c"; "d"];
  palindrome = ["r"; "a"; "c"; "e"; "c"; "a"; "r"];
}
let () = data |> Tests.test_all


