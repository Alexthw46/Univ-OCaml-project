type ide = string;;

type typeSet = TInt | TBool | TString;;

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
	| Include of exp * exp
	| Head of exp 
	| Length of exp
	| GetMax of exp 
	| GetMin of exp 
	(* funct ops  *)
	| IsEmpty of exp 
	| ForAll of exp * exp
	| Exists of exp * exp 
	| Filter of exp * exp
	| Map of exp * exp;;


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


(* This function takes a compatible value and returns its type*)   
let get_type (v : evT) : typeSet =
   match v with 
      | Int(_) -> (TInt : typeSet)
      | String(_) -> (TString : typeSet)
      | Bool(_) -> (TBool : typeSet)
      | SetVal(Empty(t)|Set(_,t)) -> t
      | _ -> failwith("incompatible type");;

(* This function turns a typeSet into a string*)
let to_str (t : typeSet) =
   match t with
      | TInt -> "int"
      | TBool -> "bool"
      | TString -> "string";;

let evTToExp (e : evT) : exp =
	match e with
	|Int(i) -> Eint(i)
	|Bool(b) -> Ebool(b)
	|String(s) -> Estring(s)
	|_ -> failwith("unexpected error");;

(*funzioni primitive*)
let prod (x: evT) (y: evT) = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		|(Int(n),Int(u)) -> Int(n*u)
		| _ -> failwith ("typechecker error") )
	else failwith("Type error");;

let sum (x: evT) (y: evT) = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		|(Int(n),Int(u)) -> Int(n+u)
			| _ -> failwith ("typechecker error") )

	else failwith("Type error");;

let diff (x: evT) (y: evT) = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		|(Int(n),Int(u)) -> Int(n-u)
		| _ -> failwith ("typechecker error") 
	)
	else failwith("Type error");;

let eq (x: evT) (y: evT) = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		|(Int(n),Int(u)) -> Bool(n=u)
		| _ -> failwith ("typechecker error") 
	)
	else failwith("Type error");;

let minus (x: evT) = if (typecheck "int" x) 
	then (match x with
	   |Int(n) -> Int(-n)
		| _ -> failwith ("typechecker error") 
	)
	else failwith("Type error");;

let iszero (x: evT) = if (typecheck "int" x)
	then (match x with
		|Int(n) -> Bool(n=0)
		| _ -> failwith ("typechecker error") 
	)
	else failwith("Type error : Zero");;

(*checks if the string is empty*)
let isEmptyString (s: evT) = if (typecheck "string" s)
	then (match s with
		|String(s) -> Bool(s="")
		| _ -> failwith ("typechecker error") 
) else failwith("Type error : Not a String");;

let vel (x: evT) (y: evT) = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		|(Bool(b),Bool(e)) -> (Bool(b||e))
		| _ -> failwith ("typechecker error") 
)
	else failwith("Type error");;

let et (x: evT) (y: evT) = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		|(Bool(b),Bool(e)) -> Bool(b&&e)
		| _ -> failwith ("typechecker error") 
)
	else failwith("Type error");;

let non (x: evT) = if (typecheck "bool" x)
	then (match x with
		|Bool(true) -> Bool(false)
		|Bool(false) -> Bool(true)
		| _ -> failwith ("typechecker error") 
	)
	else failwith("Type error");;

(*concat strings*)

let concat (s: evT) (t: evT) = if (typecheck "string" s) && (typecheck "string" t)
	then (match (s, t) with
		|(String(s), String(t)) -> String(s^t)
		| _ -> failwith ("typechecker error") 
	) else failwith("Type error concat");;

(*Sets operations*)

(* converts exp to typeSet *)
let exp_to_type (e : exp) : typeSet = 
	match e with
		EtypeSet(TInt) -> (TInt:typeSet)|
		EtypeSet(TBool) -> (TBool:typeSet)|
		EtypeSet(TString) -> (TString:typeSet)|
		_ -> failwith("incompatible type for sets");;


(* creates un set vuoto *)
let set_empty (t : typeSet) : evT = SetVal(Empty(t));;

let isEmptySet (t: evT) = match t with 
	SetVal(Empty(_))|SetVal(Set([], _)) -> (Bool true) |
	SetVal(_) -> Bool(false)|
	_ -> failwith("Set not valid");;

(* crea un set singleton*)
let setSingleton (v : evT) = SetVal(Set([v], get_type v));;

(* controlla se l'elemento v appartiene al set s *)
let rec belongsToSet (v : evT) (s : evT) : evT = 
	match s with
		SetVal(ls) ->
			let exp_type = get_type v in
					(match ls with
						Empty(_) -> (Bool false) |
						Set(lst, set_type) -> if exp_type <> set_type
							then failwith("incompatible types")
							else match lst with
								[] -> (Bool false) |
								h::t -> if h = v then (Bool true) else belongsToSet v (SetVal(Set(t, set_type)))
					)
		| _ -> failwith("Set not valid");;

(* aggiunge un elemento v al set s*)

let addSet (v : evT) (s : evT ) =
	match s with
		SetVal(Empty(set_type)) -> if get_type v <> set_type then failwith("incompatible types") else SetVal(Set([v], get_type v))  |
		SetVal(Set(lst, set_type))-> if get_type v <> set_type then failwith("incompatible types")
			else if (belongsToSet v s) = (Bool true) then s else (SetVal(Set(v::lst, get_type v))) |
		_ -> failwith("Set not valid");;


(* rimuove un elemento v dal set s*)

let removeSet (v : evT) (s : evT ) : evT =
	match s with
		SetVal(Empty(set_type)|Set([],set_type)) -> if set_type = (get_type v) then s else failwith("incompatible types")  |
		SetVal(Set(lst, set_type))-> if (get_type v) <> set_type 
		then failwith("incompatible types") else 
			let rec find_remove e = 
				match e with
					[] -> [] | 
					h::t -> if h = v then t else h::(find_remove t) 
			in (let out = ( find_remove lst) in
			 	match out with
					[] -> SetVal(Empty(set_type)) |
					_  -> SetVal(Set(out, set_type))) 
		|_ -> failwith("Set not valid");;

(* set comparators *)
(* unione insiemistica set1 U set2 *)
let setUnion (s1 : evT) (s2: evT) : evT = 
	match (s1,s2) with 
		| SetVal(Empty(t1)|Set(_,t1)), SetVal(Empty(t2)) -> if t1 = t2 then s1 else failwith("incompatible types") 
		| SetVal(Empty(t1)), SetVal(Set(_, t2)) -> if t1 = t2 then s2 else failwith("incompatible types")
		| SetVal(Set(lst1, t1)), SetVal(Set(lst2, t2)) -> if t1 <> t2 then failwith("incompatible types") else (let rec union list acc =
			match list with 
			|[] -> acc
			|h::t -> union t (addSet h acc)
		in union lst1 s2)
		|_ -> failwith("Set not valid");;

(* differenza insiemistica set1 - set2 *)
let setDiff (s1 : evT) (s2 : evT ) : evT  =
match (s1,s2) with 
		| SetVal(Empty(t1)|Set(_,t1)), SetVal(Empty(t2)) -> if t1 = t2 then s1 else failwith("incompatible types") 
		| SetVal(Empty(t1)), SetVal(Set(_, t2)) -> if t1 = t2 then s2 else failwith("incompatible types")
		| SetVal(Set(lst1, t1)), SetVal(Set(lst2, t2)) -> if t1 <> t2 then failwith("incompatible types") else ( let rec sdiff list acc =
			match list with
			|[] -> acc
			|h::t -> if (belongsToSet h s2) = (Bool true) then sdiff t acc else sdiff t (addSet h acc)
		in sdiff lst1 (SetVal(Empty(t1))))
		|_ -> failwith("Set not valid");;


(* intersezione insiemistica set1 set2 *)
let setInts (s1 : evT) (s2 : evT ) : evT  =
match (s1,s2) with 
		| SetVal(Empty(t1)|Set(_,t1)), SetVal(Empty(t2)) -> if t1 = t2 then s1 else failwith("incompatible types") 
		| SetVal(Empty(t1)), SetVal(Set(_, t2)) -> if t1 = t2 then s2 else failwith("incompatible types")
		| SetVal(Set(lst1, t1)), SetVal(Set(lst2, t2)) -> if t1 <> t2 then failwith("incompatible types") else ( let rec inters list acc =
			match list with
			|[] -> acc
			|h::t -> if (belongsToSet h s2) = (Bool true) then inters t (addSet h acc) else inters t acc 
		in inters lst1 (SetVal(Empty(t1))))
		|_ -> failwith("Set not valid");;

(* inclusione insiemistica set1 C set2 *)
let includeSet (s1 : evT) (s2 :evT) : evT = match (s1,s2) with 
		| SetVal(Empty(t1)),SetVal(Empty(t2)|Set(_,t2)) ->  if t1 = t2 then (Bool true) else failwith("incompatible types") 
		| SetVal(Set(_,t1)), SetVal(Empty(t2)) -> if t1 = t2 then (Bool false) else failwith("incompatible types")
		| SetVal(Set(lst1,t1)), SetVal(Set(lst2,t2)) -> if t1 <> t2 then failwith ("incompatible types") else (
			let rec inclusion sub main = 
			match (sub, main) with
			|SetVal(Empty(_)|Set([], _)), SetVal(_) -> (Bool true) 
			|_, (SetVal(Empty(_)|Set([],_ ))) -> (Bool false) 
			|SetVal(Set(h1::t1,typeS)),SetVal(Set(h2::t2,_)) -> et (belongsToSet h1 main) (inclusion (SetVal(Set(t1, typeS))) (SetVal(Set(t2, typeS))))
			|(_,_) -> failwith("unexpected error")
		in inclusion s1 s2
		)
		|_ -> failwith("Set not valid");;

(* set data getters *)

(* restituisce il primo elemento del set *)
let head (s : evT) = match s with
	|SetVal(Set(h::t,_)) -> h 
	|SetVal(Empty(_)|Set([],_)) -> failwith ("Set Empty")
	|_ -> failwith ("Set not valid");;

(* restituisce la grandezza del set *)
let length (s : evT) = match s with
	|SetVal(Set([],_)|Empty(_)) -> (Int 0) 
	|SetVal(Set(lst,_)) -> (Int (List.length lst)) 
	|_ -> failwith ("Set not valid");;

(* restituisce l'elemento massimo dell'insieme *)
let getSetMax (s:evT) : evT option = 
	let rec max t cmax =
		match t with 
			SetVal(Empty(_)|Set([],_)) -> Some(cmax) |
			SetVal(Set(h::t, ts)) -> if h > cmax then max (SetVal(Set(t,ts))) h else max (SetVal(Set(t,ts))) cmax
			|_ -> failwith ("unexpected error")
	in match s with
		SetVal(Empty(_)|Set([],_)) -> None|
		SetVal(Set(h::t, _)) -> max s h|
		_ -> failwith("Set not valid");;

(* restituisce l'elemento minimo dell'insieme *)
let getSetMin (s:evT) : evT option = 
	let rec min t cmin =
		match t with 
			|SetVal(Empty(_)|Set([],_)) -> Some(cmin) 
			|SetVal(Set(h::t, ts)) -> if h < cmin then min (SetVal(Set(t,ts))) h else min (SetVal(Set(t,ts))) cmin
			|_ -> failwith ("unexpected error")
	in match s with
		|SetVal(Empty(_)|Set([],_)) -> None
		|SetVal(Set(h::t, _)) -> min s h
		|_ -> failwith("Set not valid");;

(*interprete*)
let rec eval (e : exp) (r : evT env) : evT = match e with
	Eint n -> Int n |
	Ebool b -> Bool b |
	Estring s -> String s|
	EtypeSet t -> String (to_str t)|
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
   EmptySet e -> set_empty(exp_to_type e)|
   Singleton s -> setSingleton(eval s r) |
   Of(t, set) -> let s = (eval set r) in
    		if (typecheck "set" s) then belongsToSet (eval t r) s
    		else failwith ("not a set")|
   Union(s1, s2) -> let set1 = eval s1 r in let set2 = eval s2 r in
   		(match (typecheck "set" set1 , typecheck "set" set2) with 
   			|(true, true) -> setUnion set1 set2
   			|(_,_) ->  failwith ("not a set"))|
   Intersection(s1, s2)-> let set1 = eval s1 r in let set2 = eval s2 r in
   		(match (typecheck "set" set1 , typecheck "set" set2) with 
   			|(true, true) -> setInts set1 set2
   			|(_,_) ->  failwith ("not a set"))|
   Difference (s1, s2)-> let set1 = eval s1 r in let set2 = eval s2 r in
   		(match (typecheck "set" set1 , typecheck "set" set2) with 
   			|(true, true) -> setDiff set1 set2
   			|(_,_) ->  failwith ("not a set"))|
   Add(t, set) -> let s = (eval set r) in
    		if (typecheck "set" s) then addSet (eval t r) s
    		else failwith ("not a set") |
   Remove(t, set) -> let s = (eval set r) in
    		if (typecheck "set" s) then removeSet (eval t r) s
    		else failwith ("not a set")|
   Include (s1, s2) -> let set1 = eval s1 r in let set2 = eval s2 r in
   		(match (typecheck "set" set1 , typecheck "set" set2) with 
   			|(true, true) -> includeSet set1 set2
   			|(_,_) ->  failwith ("not a set"))| 
   Head s -> let set = (eval s r) in
    		if (typecheck "set" set) then head set
    		else failwith ("not a set") |
   Length s -> let set = (eval s r) in
    		if (typecheck "set" set) then length set
    		else failwith ("not a set") |
   GetMax s -> let set = (eval s r) in
    		if (typecheck "set" set) then (
    			let m = getSetMax set in
   			(match m with
   				Some(t) -> t |
   				None -> failwith ("Empty set")
   			)
   		)
    		else failwith ("not a set")|
   GetMin s -> let set = (eval s r) in 
   		if (typecheck "set" set) then (
   			let m = getSetMin set in
   			(match m with
   				Some(t) -> t |
   				None -> failwith ("Empty set")
   			)
   		)
   		else failwith ("not a set")| 
   IsEmpty s -> let set = (eval s r) in 
   		if (typecheck "set" set) then isEmptySet set
   		else failwith("not a set")|
   ForAll (p, s) -> let set = (eval s r) in 
   		if (typecheck "set" set) then testAll p set 
   		else failwith ("not a set")| 
   Exists (p, s) -> let set = (eval s r) in 
   		if (typecheck "set" set) then existSet p set 
   		else failwith("not a set")| 
   Filter (p, s) -> let set = (eval s r) in 
   		if (typecheck "set" set) then filterSet p set 
   		else failwith("not a set")|
	Map (f, s) -> let set = (eval s r) in 
   		if (typecheck "set" set) then mapSet f set 
   		else failwith("not a set")

(* set elements ops requiring eval*)

(* controlla se tutti gli elementi dell'insieme soddisfano p *)
and testAll (p : exp) (s : evT): evT = 
	match s with
	|SetVal(Set([],_)|Empty(_)) -> (Bool true)
	|SetVal(Set(lst, typeS)) -> (match p with
		|Fun(_,_) -> let env0 = emptyenv Unbound
			in (let rec test (l : evT list) acc =
				match l with
					| [] -> acc
					| h::t -> test t ( (eval (FunCall(p, evTToExp(h))) env0) :: acc )  
			in List.fold_left et ( Bool true) (test lst [])
			)
		| _ -> failwith(" Predicate not valid"))
	|_ -> failwith("Set not valid")

(* controlla se almeno un elemento dell'insieme soddisfa p *)
and existSet (p : exp) (s : evT): evT = match s with
	|SetVal(Set([],_)|Empty(_)) -> (Bool false)
	|SetVal(Set(lst, typeS)) -> (match p with
		|Fun(_,_) -> let env0 = emptyenv Unbound
			in (let rec search (l : evT list) acc =
				match l with
					| [] -> acc
					| h::t -> search t ( (eval (FunCall(p, evTToExp(h))) env0) :: acc )  
			in List.fold_left vel ( Bool true) (search lst [])
			)
		| _ -> failwith(" Predicate not valid"))
	|_ -> failwith("Set not valid")

(* restituisce un insieme di elementi che soddisfa p a partire da s*)
and filterSet (p : exp) (s : evT): evT = match s with
	|SetVal(Set([],_)|Empty(_)) -> s
	|SetVal(Set(lst, typeS)) -> (match p with
		|Fun(_,_) -> let env0 = emptyenv Unbound
			in ( let rec filter (l : evT list) acc =
				match l with
					| [] -> acc
					| h::t -> if ( eval (FunCall(p, evTToExp(h))) env0) = (Bool true) then filter t (h::acc) else filter t acc
		in let res = SetVal(Set((filter lst []), typeS)) 
		in (match res with
			|SetVal(Set([],_)) -> SetVal(Empty(typeS))
			|_ -> res
		))
		|_ -> failwith("Predicate not valid"))
	|_ -> failwith("Set not valid")
		
(* applica f all'insieme di elementi p e restituisce l'insieme risultante*)
and mapSet (f : exp) (s : evT): evT = match s with
	|SetVal(Empty(_))|SetVal(Set([], _)) -> s
	|SetVal(Set(lst, typeS)) ->
		(match f with
			Fun(_, _) -> (* Fun(arg, fBody) *)
				let env0 = emptyenv Unbound in
				(let rec map (l : evT list) acc =
					match l with
						|[] -> acc
						| h::t -> map t ((eval (FunCall(f, evTToExp(h))) env0) :: acc)
				in SetVal(Set((map lst []), typeS)))
			|_ -> failwith("Predicate not valid"))
	|_ -> failwith("Set not valid");;
