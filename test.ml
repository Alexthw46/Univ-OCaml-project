(* =============================  TESTS  ================= *)

(* basico: no let *)
let env0 = emptyenv Unbound;;

let e1 = FunCall(Fun("y", Sum(Den "y", Eint 1)), Eint 3);;

eval e1 env0;;

let es = EmptySet(EtypeSet(Int));;

let emptysetvalue = eval es env0;;

(* creazione espressione per il singleton di interi *)
let ss = Singleton(Eint(22));;
(* valutazione dell'espressione *)
let singleton_value = eval ss env0;;

(* inserimento di un duplicato *)

let ss_double = Add(Eint 11, ss);;

eval ss_double env0;;

(* espressione per insieme di due elementi a partire dal singleton *)
let coupleSetExp = Add(Eint 11, ss);;

(* controllo di un insieme vuoto*)

let isThisEmpty = IsEmpty(es);;

eval isThisEmpty env0;;

let isThisEmpty = IsEmpty(coupleSetExp);;

eval isThisEmpty env0;;

(* rimozione di un elemento dal Set *)

let removedSetExp = Remove(Eint 11, coupleSetExp);;


