type ide = string;;

type typeSet = Int | Bool | String;;

(*added types for String*)
type exp = Eint of int |
 Ebool of bool |
 Estring of string|
 EtypeSet of typeSet|
 Den of ide |
 Prod of exp * exp |
 Sum of exp * exp |
 Diff of exp * exp |
 Concat of exp * exp|
 Eq of exp * exp |
 Minus of exp |
 IsZero of exp |
 IsEmptyStr of exp | 
 Or of exp * exp |
 And of exp * exp |
 Not of exp |
 Ifthenelse of exp * exp * exp |
 Let of ide * exp * exp |
 Fun of ide * exp |
 FunCall of exp * exp |
 Letrec of ide * exp * exp
 (* types for Set *)
 	|EmptySet of exp
 	|Singleton of exp
 	|Of of exp * exp
	(* basic ops specific to sets *)
	| Union of exp * exp 
	| Intersection of exp * exp 
	| Difference of exp * exp 
	| Add of exp * exp 
	| Remove of exp * exp 
	| IsInside of exp * exp
	| Head of exp 
	| Length of exp
	| GetMax of exp 
	| GetMin of exp 
	(* funct ops  *)
	| IsSubset of exp * exp 
	| IsEmpty of exp 
	| ForAll of exp * exp
	| Exists of exp * exp 
	| Filter of exp * exp;;


(*ambiente polimorfo*)
type 't env = ide -> 't;;
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;

(*tipi esprimibili*)
type evT = Int of int 
| Bool of bool 
| String of string
| SetVal of set
| Unbound 
| FunVal of evFun 
| RecFunVal of ide * evFun
and set = Empty of typeSet | Set of evT list * typeSet 
and evFun = ide * exp * evT env;;

(*rts*)
(*type checking*)
let typecheck (s : string) (v : evT) : bool = match s with
	"int" -> (match v with Int(_) -> true |	_ -> false) |
	"bool" -> (match v with Bool(_) -> true | _ -> false) |
	(*extended to string & set*)
	"string" -> (match v with String(_) -> true | _ -> false)|
	"set" -> (match v with SetVal(_) -> true | _ -> false)|
	_ -> failwith("not a valid type");;


(* This function takes an expressible value and returns its type*)   
let get_type (v : evT) : typeSet =
   match v with 
      | Int(_) -> (Int : typeSet)
      | String(_) -> (String : typeSet)
      | Bool(_) -> (Bool : typeSet)
      | _ -> failwith("wrong type error");;

(* This function turns a typeSet into a string*)
let to_str (t : typeSet) =
   match t with
      | Int -> "int"
      | Bool -> "bool"
      | String -> "string"
      | _ -> failwith("wrong type error");;


(*funzioni primitive*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n*u))
	else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n+u))
	else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n-u))
	else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Bool(n=u))
	else failwith("Type error");;

let minus x = if (typecheck "int" x) 
	then (match x with
	   	Int(n) -> Int(-n))
	else failwith("Type error");;

let iszero x = if (typecheck "int" x)
	then (match x with
		Int(n) -> Bool(n=0))
	else failwith("Type error : Zero");;

(*checks if the string is empty*)
let isEmptyString s = if (typecheck "string" s)
	then (match s with
		String(s) -> Bool(s="")
	) else failwith("Type error : Empty String");;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> (Bool(b||e)))
	else failwith("Type error");;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> Bool(b&&e))
	else failwith("Type error");;

let non x = if (typecheck "bool" x)
	then (match x with
		Bool(true) -> Bool(false) |
		Bool(false) -> Bool(true))
	else failwith("Type error");;

(*concat strings*)

let concat s t = if (typecheck "string" s) && (typecheck "string" t)
	then (match (s, t) with
		(String(s), String(t)) -> String(s^t)
	) else failwith("Type error concat");;

(*Sets operations*)

let expt_to_type (e : exp) : typeSet = 
	match e with
		EtypeSet(t) -> t|
		_ -> failwith("incompatible type");;

(* creates an empty set*)
let set_empty (t : typeSet) : evT = SetVal(Empty(t));;

let isEmptySet (t: evT) = match t with 
	SetVal(Empty(_))|SetVal(Set([], _)) -> (Bool true) |
	SetVal(_) -> Bool(false)|
	_ -> failwith("Set not valid");;

(* creates a singleton set*)
let set_singleton (v : evT) = SetVal(Set([v], get_type v));;

(* checks if the element is contained in the set*)
let rec belongsToSet (v : evT) (s : evT) : evT = 
	match s with
		SetVal(ls) ->
			let exp_type = get_type v in
					(match ls with
						Empty(_) -> (Bool false) |
						Set(lst, set_type) -> if exp_type <> set_type then failwith("incompatible types")
							else
								match lst with
								[] -> (Bool false) |
								h::t -> if h = v then (Bool true) else belongsToSet v (SetVal(Set(t, set_type)))
					)
		| _ -> failwith("Value is not a Set");;

(* adds an element to the set*)

let addSet (v : evT) (s : evT ) =
	match s with
		SetVal(Empty(set_type)) -> if get_type v <> set_type then failwith("incompatible types") else SetVal(Set([v], get_type v))  |
		SetVal(Set(lst, set_type))-> if get_type v <> set_type then failwith("incompatible types")
			else if (belongsToSet v s) = (Bool true) then s else (SetVal(Set(v::lst, get_type v))) |
		_ -> failwith("Set not valid");;


(* removes an element from the set*)

let removeSet (v : evT) (s : evT ) : evT =
	match s with
		SetVal(Empty(set_type)|Set([],set_type)) -> if set_type = (get_type v) then s else failwith("incompatible types")  |
		SetVal(Set(lst, set_type))-> if (get_type v) <> set_type 
		then failwith("incompatible types") else 
			let rec find_remove e = 
				match e with
					[] -> [] | 
					h::t -> if h = v then t else h::(find_remove t) 
			in let out = ( find_remove lst) in
			 	match out with
					[] -> SetVal(Empty(set_type)) |
					_  -> SetVal(Set(out, set_type)) 
		|_ -> failwith("Set not valid");;


(*end copy-paste*)


(*interprete*)
let rec eval (e : exp) (r : evT env) : evT = match e with
	Eint n -> Int n |
	Ebool b -> Bool b |
	Estring s -> String s|
	EtypeSet t -> String ""|
	IsZero a -> iszero (eval a r) |
	IsEmptyStr s -> isEmptyString (eval s r) |
	Concat(s, t) -> concat (eval s r) (eval t r)|
	Den i -> applyenv r i |
	Eq(a, b) -> eq (eval a r) (eval b r) |
	Prod(a, b) -> prod (eval a r) (eval b r) |
	Sum(a, b) -> sum (eval a r) (eval b r)|
	Diff(a, b) -> diff (eval a r) (eval b r) |
	Minus a -> minus (eval a r) |
	And(a, b) -> et (eval a r) (eval b r) |
	Or(a, b) -> vel (eval a r) (eval b r) |
	Not a -> non (eval a r) |
	Ifthenelse(a, b, c) -> 
		let g = (eval a r) in
			if (typecheck "bool" g) 
				then (if g = Bool(true) then (eval b r) else (eval c r))
				else failwith ("nonboolean guard") |
	Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r)) |
	Fun(i, a) -> FunVal(i, a, r) |
	FunCall(f, eArg) -> 
		let fClosure = (eval f r) in

			(match fClosure with
				FunVal(arg, fBody, fDecEnv) -> 
					eval fBody (bind fDecEnv arg (eval eArg r)) |
				RecFunVal(g, (arg, fBody, fDecEnv)) -> 
					let aVal = (eval eArg r) in
						let rEnv = (bind fDecEnv g fClosure) in
							let aEnv = (bind rEnv arg aVal) in
								eval fBody aEnv |
				_ -> failwith("non functional value")) |
   Letrec(f, funDef, letBody) ->
        		(match funDef with
            		Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
                         			                eval letBody r1 |
            		_ -> failwith("non functional def")) |
   (*Set related evals*)
   Singleton s -> set_singleton(eval s r) |
   EmptySet e -> set_empty(expt_to_type e)|
   Of(t, set) -> belongsToSet (eval t r) (eval set r)|
   IsEmpty s -> isEmptySet (eval s r) |
   Add(t, set) -> addSet (eval t r) (eval set r) |
   Remove(t, set) -> removeSet (eval t r) (eval set r);;
   (* | Union | Intersection| Difference | IsInside | Head | Length | GetMax | GetMin	| IsSubset | ForAll | Exists | Filter;;*)
