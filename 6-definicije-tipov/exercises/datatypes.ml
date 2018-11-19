(* ========== Exercise 3: Types  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 When modeling money, we usually use floats. However we quickly run into
 problems when currency is introduced.
 We shall look at two attempts to improve safety when using currency.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Define the types [euro] and [dollar], where each has only one constructor,
 which accepts a float.
 Then define the functions [euro_to_dollar] and [dollar_to_euro] which convert
 between the two currencies (get the correct factors online or make them up).

 Hint: Marvel at how informative the types are.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)

type dollar = Dollar of float
type euro = Euro of float

let dollar_to_euro (Dollar d) = Euro (0.875425238 *. d)
let euro_to_dollar (Euro e) = Dollar (e /. 0.875425238)

(* 
let dollar_to_euro x =
      match x with
      | Dollar y -> Euro (y *. 0.875425238)
*)

(*----------------------------------------------------------------------------*]
 Define the type [currency] as a single variant type with constructors for the
 currencies yen, pound and krona. Then define the function [to_pound], which
 converts the given currency to the pound.

 Hint: Additionally add the franc as a currency and get excited over the fact
       that Ocaml reminds you to correct the function [to_pound].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)

type currency = 
  | Yen of float
  | Pound of float
  | Krona of float
  | Franc of float

let to_pound x = 
  match x with
  | Yen y -> Pound (y *. 0.006894)
  | Krona y -> Pound (y *. 0.0863844)
  | Franc y -> Pound (y *. 0.779851)

let to_pound' z = 
      let rate, value = match z with
            | Yen x -> (0.006894, x)
            | Pound x -> (1., x)
            | Krona x -> (0.0863844, x)
            | Franc x -> (0.779851, x)
      in
      Pound (rate *. value)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 We wish to use lists that keep integers as well as booleans. This can be
 solved by introducing a type of integer or boolean values, however we will
 instead introduce a new type for lists.

 Recall that the type [list] uses a constructor for the empty list [Nil]
 (or [] in Ocaml) and a constructor for an element [Cons(x, xs)] (or x :: xs in
 Ocaml).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Define the type [intbool_list] with constructors for:
  1.) the empty list,
  2.) an integer element,
  3.) a boolean element.

 Define an example, which represents "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)

type intbool_list =
  | Empty
  | IntCons of int * intbool_list
  | BoolCons of bool * intbool_list 

let x = IntCons (7, BoolCons (true, BoolCons (false, IntCons (7, Empty))));;

(*----------------------------------------------------------------------------*]
 The function [intbool_map f_int f_bool ib_list] maps the values of [ib_list]
 into a new [intbool_list] using the appropriate function out of [f_int] and
 [f_bool].
[*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool ib_list =
      match ib_list with
      | Empty -> Empty
      | IntCons (n, tail) -> let tail = intbool_map f_int f_bool tail in 
                             IntCons(f_int n, tail)
      | BoolCons (n, tail) -> let tail = intbool_map f_int f_bool tail in
                              BoolCons(f_bool n, tail)

(*----------------------------------------------------------------------------*]
 The function [intbool_reverse] reverses the order of elements of an
 [intbool_list]. The function is tail-recursive.
[*----------------------------------------------------------------------------*)

let intbool_reverse ib_list = 
      let rec reverse_aux acc ib_list =
            match ib_list with
            | Empty -> acc
            | IntCons (n, tail) -> reverse_aux IntCons(n, acc) tail
            | BoolCons (b, tail) -> reverse_aux BoolCons(b, acc) tail
      in
      reverse_aux Empty ib_list

(*----------------------------------------------------------------------------*]
 The function [intbool_separate ib_list] separates the values of [ib_list] into
 a pair of regular [list] lists, where the first one includes all integers and
 the second one all boolean values. The function is tail-recursive and does not
 change the order of elements.
[*----------------------------------------------------------------------------*)

let rec intbool_separate = ()

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 You were chosen to be the database administrator for a world renowned wizard
 university "Effemef". Your task is to construct a simple system for data
 management.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Wizards are classified according to their chosen school of magic. Define a
 type [magic] which includes the magic of fire, frost and arcane.

 After being employed, a wizard can decide to be a historian, a teacher or
 a researcher. Define the type [specialisation] that represents those choices.
[*----------------------------------------------------------------------------*)

type magic = 
      | Fire
      | Frost
      | Arcane

type specialisation = 
      | Historian
      | Teacher
      | Researcher

(*----------------------------------------------------------------------------*]
 Every wizard starts out as a newbie. Afterwards they become a student and in
 the end, they may get employed. Define the type [status] which determines if a
 wizard is:
  a.) a newbie,
  b.) a student (also what school of magic they study and for how long),
  c.) an employee (also their school of magic and specialisation).

 Then define a record type [wizard] which has a field for the wizards name and
 a field for their status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)

type status = 
      | Newbie
      | Student of magic * int
      | Employee of magic * specialisation

type wizard = {
      name : string;
      status : status
}

let profesor = {name = "Matija" ; status = Employee (Fire, Teacher)}

(*----------------------------------------------------------------------------*]
 We want to count how many users of a certain school of magic are currently in
 the group.
 Define a record type [magic_counter] which has an integer field for every
 school of magic.
 Then define the function [update counter magic] that returns a new counter
 with an updated field depending on the value of [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)

type magic_counter = {
      fire : int;
      frost : int;
      arcane : int;
}

let update counter magic =
      match magic with
      | Fire -> {counter with fire = counter.fire + 1}
      | Frost -> {counter with frost = }

(*----------------------------------------------------------------------------*]
 The function [count_magic] accepts a list of wizards and counts the users of
 different schools of magic.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

let count_magic =
      let rec count_aux counter = function
            | [] -> counter
            | {name; status} :: wizards ->
                  let counter =
                        begin match status with
                        | Newbie -> count_aux wizards
                        | Student -> (m, _) -> update counter m
                        | Employee -> (m, _) update counter m
                        end
                  in
                  count_aux counter wizards
      in
      count_aux { fire = 0 ; frost = 0 ; arcane = 0} wizards

(*----------------------------------------------------------------------------*]
 We wish to find a possible candidate for a job offer. A student can become a
 historian after studying for at least 3 years, a researcher after 4 years and
 a teacher after 5 years.
 The function [find_candidate magic specialisation wizard_list] searches
 through the list of wizards and returns the name of a suitable candidate for
 the [specialisation] if they are studying [magic]. If there is no candidate,
 it should return [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let rec find_candidate = ()
