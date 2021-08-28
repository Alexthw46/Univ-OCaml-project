(* =============================  TESTS  ================= *)

#use "interpeter.ml";;

(* creazione di env e set vuoto*)
let env0 = emptyenv Unbound;;

let es = EmptySet(EtypeSet(TInt));;


(* creazione e valutazione espressione per il singleton (interi) *)
let ss = Singleton(Eint(20));;
let singleton_value = eval ss env0;;

(* controllo di un insieme vuoto e uno non vuoto*)

let isThisEmpty = IsEmpty(es);;

let emptyCheck = eval isThisEmpty env0;;

let isThisEmpty = IsEmpty(ss);;

let emptyCheck = eval isThisEmpty env0;;

(* tentativo di inserimento di un duplicato e verifica proprietà del set *)
let ss_double = Add(Eint 20, ss);;

let ss_double_value = eval ss_double env0;;

(* espressione per insieme di due elementi a partire dal singleton *)
let dualSetEx = Add(Eint 10, ss);;
eval dualSetEx env0;;

(* rimozione di un elemento dal Set *)

let removedSetEx = Remove(Eint 10, dualSetEx);;
eval removedSetEx env0;;

(* esempi di operazioni sui set *)
 
 (* unione: [10,20] con singleton [30] *)
let unionEx = Union(dualSetEx, Singleton(Eint 30));;
eval unionEx env0;;

 (* differenza: [10,20] - [20] *)
let diffEx = eval (Difference(dualSetEx, ss)) env0;;


 (* intersezione: [10,20] con [20,30] *)
let intersEx = eval (Intersection(dualSetEx, unionEx)) env0;;

 (* inclusione: [10,20] C [20,30] *)

let includEx = eval (Include((Add(Eint 30,dualSetEx)), unionEx)) env0;;

(* creo un set più lungo per gli esempi successivi*)
let longset = Add(Eint 1, es);;
let longset = Add(Eint 2, longset);;
let longset = Add(Eint 3, longset);;
let longset = Add(Eint 4, longset);;
let longset = Add(Eint 5, longset);;
let longset = Add(Eint 0, longset);;

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