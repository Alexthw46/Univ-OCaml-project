#use "interpeter.ml";;

(* ================= BASIC TESTS  ================= *)

(* creazione di env e set vuoto*)
let env0 = emptyenv Unbound;;

let es = EmptySet(EtypeSet(TInt));;

(* creazione e valutazione espressione per il singleton (interi) *)
let singleInt = Singleton(Eint(20));;
let singleString = Singleton(Estring("single"));;
let singleBool = Singleton(Ebool(true));;

let singletonValue = eval singleInt env0;;

(* controllo di un insieme vuoto e uno non vuoto*)

let isThisEmpty = IsEmpty(es);;

let emptyCheck = eval isThisEmpty env0;;

let isThisEmpty = IsEmpty(singleInt);;

let emptyCheck = eval isThisEmpty env0;;

(* tentativo di inserimento di un duplicato nel set *)
let ss_double = eval (Add(Eint 20, singleInt)) env0;;

(* espressione per insieme di due elementi a partire dal singleton *)
let dualSetEx = Add(Eint 10, singleInt);;
let dualSetVal = eval dualSetEx env0;;

(* rimozione di un elemento dal Set *)
let removedSetEx = Remove(Eint 10, dualSetEx);;
let search10 = eval (Of(Eint 10, removedSetEx)) env0;;
(* esempi di operazioni sui set *)
 
 (* unione: [10,20] con singleton [30] *)
let unionEx = Union(dualSetEx, Singleton(Eint 30));;
let unionExVal = eval unionEx env0;;

 (* differenza: [10,20] - [20] *)
let diffEx = eval (Difference(dualSetEx, singleInt)) env0;;


 (* intersezione: [20] con [10,20,30] *)
let intersEx = eval (Intersection(singleInt, unionEx)) env0;;

 (* inclusione: [10,20] C [10,20,30] *)

let includEx = eval (Include(dualSetEx, unionEx)) env0;;

(* creo un set più lungo per gli esempi successivi*)
let longset = Add(Eint 1, Singleton(Eint(0)));;
let longset = Add(Eint 2, longset);;
let longset = Add(Eint 3, longset);;
let longset = Add(Eint 4, longset);;
let longset = Add(Eint 5, longset);;

let longSetLength = eval (Length(longset)) env0;;

let longSetHead = eval (Head(longset)) env0;;

let longsetMax = eval (GetMax(longset)) env0;;
let longsetMIn = eval (GetMin(longset)) env0;;

(* esempi sulle operazioni con funzioni*)

(* funzioni *)
let square = Fun("x", Prod(Den "x", Den "x"));;
let isZero = Fun("x", Ifthenelse(IsZero(Den "x"), Ebool true, Ebool false));; 

let mapEx = eval (Map(square,longset)) env0;;
let filterEx = eval (Filter(isZero,longset)) env0;;
let forAllEx = eval (ForAll(isZero,longset)) env0;;
let existEx = eval (Exists(isZero,longset)) env0;;

(* test del typechecker *)

(* sum on bool values*)
mapSet (Fun("x", Sum(Den "x", Eint 1))) (SetVal(Set([Bool true; Bool false], TBool)));;
(* concat on int values*)
mapSet (Fun("x", Concat(Den "x", Estring "check"))) (SetVal(Set([Int 0;Int 1;Int 2;Int 3], TInt)));;
(* et on string values*)
mapSet (Fun("x", Not(Den "x"))) (SetVal(Set([String "hi"; String "cat"], TString)));;

(* Add on non-Set values, dynamic tc is only on eval *)
eval (Add((Eint 5), Sum(Eint 4, Eint 5))) env0;;



(* ================= MORE TESTS  ================= *)

(* per comodità, questi test sono eseguiti direttamente su evT, in modo da creare Set senza passare per Add(evT, exp) *)

(* ricerca elementi *)

belongsToSet (Int 1 ) (SetVal(Set([Int 0;Int 1;Int 2;Int 3], TInt)));; (* true *)
belongsToSet (Bool false) (SetVal(Set([Bool false; Bool true], TBool)));; (* true *)
belongsToSet (String "not found") (SetVal(Set([String "found";String "not";String "string"],TString)));; (* false *)
belongsToSet (Int 6) (SetVal(Set([String "these";String "are";String "strings"],TString)));; (* tipi non compatibili *)

(* aggiunta elementi *)

addSet (Int 1) ( eval singleInt env0);; (* SetVal (Set ([Int 1; Int 20], IntegerType)) *)
addSet (Bool false) (eval singleBool env0);; (* SetVal (Set ([Bool false; Bool true], TBool)) *)
addSet (String "couple") (eval singleString env0);; (* SetVal (Set ([String "couple"; String "single"], TString)) *)
addSet (Int 3) (eval singleString env0);; (* tipi non compatibili*)


(* rimozione elementi *)

removeSet (Int 0) (SetVal(Set([Int 0;Int 1;Int 2;Int 3], TInt)));; (* SetVal(Set([Int 1;Int 2;Int 3], TInt)) *)
removeSet (String "bye") (SetVal(Set([String "stay"; String "bye"], TString)));; (* ["stay"] *)
removeSet (Bool true) (SetVal(Set([Bool false; Bool true], TBool)));; (* SetVal(Set([Bool false], TBool)) *)
removeSet (Int 3) (SetVal(Set([Int 0;Int 1;Int 2], TInt)));; (* elemento non presente*)
removeSet (Int 4) (SetVal(Set([String "wrong"], TString)));; (* tipi non compatibili *)

(* sottoinsieme *)

includeSet (SetVal(Empty(TInt))) (SetVal(Set([Int 0], TInt)));; (* true *)
includeSet (SetVal(Set([Int 0], TInt))) (SetVal(Empty(TInt)));; (* false *)
includeSet (SetVal(Set([Int 1], TInt))) (SetVal(Set([Int 1; Int 2; Int 3], TInt)));; (* true *)
includeSet (SetVal(Set([Int 2], TInt))) (SetVal(Set([Int 1; Int 3], TInt)));; (* false *)

(* unione *)
setUnion (SetVal(Set([Int 0;Int 1;Int 2;Int 3],TInt))) (SetVal(Set([Int 3;Int 4;Int 5;Int 6], TInt)));; (* unione int : SetVal(Set([Int 2; Int 1; Int 0; Int 3; Int 4; Int 5; Int 6], TInt)) *)
setUnion (SetVal(Set([Bool true], TBool))) (SetVal(Set([Bool false], TBool)));; (* unione bool : SetVal (Set ([Bool true; Bool false], TBool)) *)
setUnion (SetVal(Set([String "is"; String "This"], TString))) (SetVal(Set([String "an"; String "Union"], TString)));; (* unione string : SetVal (Set ([String "This"; String "is"; String "an"; String "Union"], TString)) *)
setUnion (SetVal(Empty(TBool))) (SetVal(Set([Int 0], TInt)));; (* tipi non compatibili *)

(* differenza *)

setDiff (SetVal(Set([Int 0;Int 1;Int 2;Int 3],TInt))) (SetVal(Set([Int 2;Int 3;Int 4;Int 5], TInt)));; (* diff int : SetVal (Set ([Int 1; Int 0], TInt)) *)
setDiff (SetVal(Set([Bool true; Bool false], TBool))) (SetVal(Set([Bool false], TBool)));; (* diff bool : SetVal (Set ([Bool true], TBool)) *)
setDiff (SetVal(Set([String "a Difference";String "not"; String "is"; String "This"], TString))) (SetVal(Set([String "not"; String "Union"], TString)));; (* SetVal (Set ([String "This"; String "is"; String "a Difference"], TString)) *)
setDiff (SetVal(Empty(TBool))) (SetVal(Set([Int 0], TInt)));; (* tipi non compatibili *)

(* intersezione *)

setInts (SetVal(Set([Int 0;Int 1;Int 2;Int 3],TInt))) (SetVal(Set([Int 2;Int 3;Int 4;Int 5], TInt)));; (* ints int : SetVal (Set ([Int 3; Int 2], TInt))*)
setInts (SetVal(Set([Bool true; Bool false], TBool))) (SetVal(Set([Bool false], TBool)));; (* ints bool : SetVal (Set ([Bool false], TBool)) *)
setInts (SetVal(Set([String "Set";String "a";String "is";String "this"], TString))) (SetVal(Set([String "Set"; String "a" ; String "am"; String "i"], TString)));; (* ints string : SetVal (Set ([String "a"; String "Set"], TString))*)
setInts (SetVal(Empty(TBool))) (SetVal(Set([Int 0], TInt)));; (* tipi non compatibili *)

(* massimo e minimo *)

getSetMax (SetVal(Set([Int 30;Int 46;Int (-55);Int 7], TInt)));; (* max int : Some (Int 46) *)
getSetMin (SetVal(Set([Int 30;Int 46;Int (-55);Int 7], TInt)));; (* min int : Some (Int (-55)) *)
getSetMax (SetVal(Set([Bool true; Bool false], TBool)));; (* min bool : Some (Bool true) *)
getSetMin (SetVal(Set([Bool true; Bool false], TBool)));; (* max bool : Some (Bool false) *)
getSetMax (SetVal(Set([String "min"; String "or"; String "max"],TString)));; (* max string : Some (String "or") *)
getSetMin (SetVal(Set([String "min"; String "or"; String "max"],TString)));; (* min string : Some (String "max") *)
getSetMax (SetVal(Empty(TInt)));; (* min or max on empty : None *)
getSetMin (SetVal(Set([],TString)));; (* min or max on empty : None *)


(* map *)

mapSet (Fun("x", Prod(Den "x", Eint 2))) (SetVal(Set([Int 0;Int 1;Int 2;Int 3],TInt)));; (* map ints : *)
mapSet (Fun("x", Not(Den "x"))) (SetVal(Set([Bool true; Bool false], TBool)));; (* map bools : SetVal (Set ([Bool true; Bool false], TBool)) (l'array si inverte ovviamente)*)
mapSet (Fun("x", Concat(Den "x", Estring "concat"))) (SetVal(Set([String "chain";String "a";String "is"; String "This"], TString)));; (* map strings : SetVal(Set([String "Thisconcat"; String "isconcat"; String "aconcat"; String "chainconcat"], TString)) *)

(* filter *)

filterSet (Fun("x", Ifthenelse(Or(IsZero(Den "x"), Eq(Den "x", Eint 3)), Ebool true, Ebool false))) (SetVal(Set([Int 0;Int 1;Int 2;Int 3],TInt)));; (* filter int : SetVal (Set ([Int 3; Int 0], TInt)) *)
filterSet (Fun("x", Ifthenelse(Eq(Den "x", Ebool true), Ebool true, Ebool false))) (SetVal(Set([Bool true; Bool false], TBool)));; (* filter true bool : SetVal (Set ([Bool true], TBool)) *)
filterSet (Fun("x", Ifthenelse(Or(IsEmptyStr(Den "x"),Eq(Den "x", Estring "filtered")), Ebool true, Ebool false))) (SetVal(Set([String "filtered"; String ""; String "not filtered"], TString)));;

(* forAll *)

testAll (Fun("x", Ifthenelse(Or(IsZero(Den "x"), Eq(Den "x", Eint 3)), Ebool true, Ebool false))) (SetVal(Set([Int 0;Int 1;Int 2;Int 3],TInt)));; (* test all if (x = 0 or x = 3) : Bool false*)
testAll (Fun("x", Eq(Den "x", Ebool true))) (SetVal(Set([Bool true], TBool)));; (* test all if (x = true) : Bool true *)
testAll (Fun("x", Ifthenelse(IsEmptyStr(Den "x"), Ebool false, Ebool true))) (SetVal(Set([String "empty"; String "not"; String "This is"], TString)));; (* test all if strings are not empty : Bool true *)

(* exist *)

existSet (Fun("x", Ifthenelse(Eq(Diff(Den "x", Eint 1), Eint 0), Ebool true, Ebool false))) (SetVal(Set([Int 0;Int 1;Int 2;Int 3],TInt)));; (* search if x exist ( x-1 = 0 ) : Bool true *)
existSet (Fun("x", Eq(Den "x", Ebool true))) (SetVal(Set([Bool false], TBool)));; (* search if x exist ( x = true) : Bool false *)
existSet (Fun("x", Ifthenelse(IsEmptyStr(Den "x"), Ebool false, Ebool true))) (SetVal(Set([String "filtered"; String ""; String "not filtered"], TString)));; (* search if x exist ( x = "" ) : Bool true *)
 



