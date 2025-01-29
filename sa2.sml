(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Aidan DiNunzio                         *)
(* Time spent on HW6:
*)

(* Collaborators and references: Phyllis Spence, chatGPT
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)
fun mynull []       = true
  | mynull (_::_)   = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true

val () =
    Unit.checkExpectWith Bool.toString "mynull [2, 4] should be false"
    (fn () => mynull [2, 4])
    false



(**** Problem B ****)
fun firstVowel [] = false
  | firstVowel (#"a"::_) = true
  | firstVowel (#"e"::_) = true
  | firstVowel (#"i"::_) = true
  | firstVowel (#"o"::_) = true
  | firstVowel (#"u"::_) = true
  | firstVowel _ = false;

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true
val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'bak' should be false"
    (fn () => firstVowel [#"b",#"a",#"k"])
    false
val () =
    Unit.checkExpectWith Bool.toString "firstVowel '' should be false"
    (fn () => firstVowel [])
    false


(**** Problem C ****)
fun reverse (intList : int list) : int list = 
  foldl (fn (x, acc) => x :: acc) [] intList;


val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]


(**** Problem D ****)
fun minlist (intList : int list) : int =
  case intList of
    [] => raise Match
  | _  => foldl (fn (x, acc) => Int.min(x, acc)) 1073741823 intList;
  
val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])
val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0


(**** Problem E ****)
exception Mismatch
fun zip ([],[]) = []
  | zip ([], _::_) = raise Mismatch
  | zip (_::_, []) = raise Mismatch
  | zip (x::xs, y::ys) = (x,y) :: zip (xs, ys);
  
  
val () =
  Unit.checkExpectWith (fn l => "[" ^ String.concatWith ", " (List.map (fn (x, y) => "(" ^ Int.toString x ^ ", " ^ Int.toString y ^ ")") l) ^ "]")
  "zip ([1,3,5], [2,4,6]) should be [(1,2), (3,4), (5,6)]"
  (fn () => zip ([1,3,5], [2,4,6]))
  [(1,2), (3,4), (5,6)]
  
(*
val () =
  Unit.checkExpectWith (Int.toString Unit.listString) 
  "zip ([1,3,5], [2,4,6]) should be (1,2), (3,4), (5,6)"
  (fn () => zip ([1,3,5], [2,4,6]))
  [(1,2), (3,4), (5,6)]
*)
(*
val () = Unit.checkExnWith 
  "zip ([1,3,5], [2,4]) should raise Mismatch"
  (fn () => zip ([1,3,5], [2,4]))
*)
(**** Problem F ****)
fun concat [] = []
  | concat (x::xs) = x @ concat xs


val () =
  Unit.checkExpectWith 
    (fn l => "[" ^ String.concatWith ", " (List.map Int.toString l) ^ "]")
    "concat [[1], [2, 3, 4], [], [5, 6]] should be [1, 2, 3, 4, 5, 6]"
    (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
    [1, 2, 3, 4, 5, 6]  
(**** Problem G ****)
fun isDigit c = 
  case c of 
    #"0" => true
    | #"1" => true
    | #"2" => true
    | #"3" => true
    | #"4" => true
    | #"5" => true
    | #"6" => true
    | #"7" => true
    | #"8" => true
    | #"9" => true
    | _ => false;
    

val () =
  Unit.checkExpectWith Bool.toString
    "isDigit #'5' should return true"
    (fn () => isDigit #"5")
    true

val () =
  Unit.checkExpectWith Bool.toString
    "isDigit #'a' should return false"
    (fn () => isDigit #"a")
    false


(**** Problem H ****)
fun isAlpha c = 
  let 
    val charNum = Char.ord c
  in
    (charNum >= Char.ord #"a" andalso charNum <= Char.ord #"z") orelse
    (charNum >= Char.ord #"A" andalso charNum <= Char.ord #"Z")
  end;

val () =
  Unit.checkExpectWith Bool.toString
    "isAlpha #'a' should return true"
    (fn () => isAlpha #"a")
    true

val () =
  Unit.checkExpectWith Bool.toString
    "isAlpha #'1' should return false"
    (fn () => isAlpha #"1")
    false


(**** Problem I ****)
fun svgCircle (cx, cy, r, fill) = 
  "<circle cx=\"" ^ Int.toString cx ^ 
  "\" cy=\"" ^ Int.toString cy ^ 
  "\" r=\"" ^ Int.toString r ^ 
  "\" fill=\"" ^ fill ^ "\" />";

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";


(**** Problem J ****)
(*
fun partition p (x :: xs) = ([],[])

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);
*)

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
