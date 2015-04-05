structure Arith: ARITH =
struct

datatype t 
  = True
  | False
  | If of t * t * t
  | Zero
  | Succ of t
  | Pred of t
  | IsZero of t

exception AddYourCodeHere
          
fun isNumber t =
    case t
     of Zero => true
      | Succ t' => isNumber t'
      | _ => false
            

			
exception NoRule
          


fun pp t =
    case t
     of True => print "True"
      | False => print "False"
      | If (t1, t2, t3) =>
        (print "If("
       ; pp t1; print ", "; pp t2; print ", "
       ; pp t3; print ")")
      | Zero => print "Zero"
      | Succ t' =>
        (print "Succ("; pp t'; print ")")
      | Pred t' =>
        (print "Pred("; pp t'; print ")")
      | IsZero t' => 
        (print "IsZero("; pp t'; print ")")

(* one-step evaluator *)
fun eval t =
    case t
     of If (True, t2, _) => t2
      | If (False, _, t3) => t3
      | If (t1, t2, t3) =>
        If (eval t1, t2, t3)
      | Succ t' => Succ (eval t')
      | Pred Zero => Zero
      | Pred (Succ t') =>
        if isNumber t'
        then t'
        else Pred (eval (Succ t'))
      | Pred t' => Pred (eval t')
      | IsZero Zero => True
      | IsZero (Succ t') =>
        if isNumber t'
        then False
        else IsZero (eval (Succ t'))
      | IsZero t' => IsZero (eval t')
      | _ => raise NoRule
		
		
fun evalAll t =
    (let val t' = (eval t)
        val _ = pp t'
        val _ = print "\n"
    in evalAll t'
    end) handle NoRule => t 
	

fun evalAllWithoutPrint t = 
	(let val t' = (eval t)
	(*
         val _ = pp t'
         val _ = print "\n"
     *)
	 in evalAllWithoutPrint t'
     end) handle NoRule => t 
	 

(* Your job: *)
fun evalBig t = 
	case t
	 of True => True
	  | False => False
	  | Zero => Zero	  
	  | If (t1, t2, t3) =>
	    (let val condition = (evalAllWithoutPrint t1)
		 in
		   if condition = True
		   then evalAllWithoutPrint t2
		   else if condition = False
				then evalAllWithoutPrint t3
				else If (evalBig t1, t2, t3)
		 end)  
	  | Succ t' =>
		(let val temp = evalAllWithoutPrint t' 
		in
		  if isNumber temp
		  then Succ temp
		  else Succ (evalBig t')
		end)
	  | Pred t' =>
	    (let val temp = (evalAllWithoutPrint t') 
		 in 
		   if temp = Zero
		   then Zero
		   else if isNumber temp andalso not(temp = Zero)
				then evalAllWithoutPrint (Pred temp)
				else Pred (evalBig t')
		end)	  
	  | IsZero t' =>
	    (let val temp = (evalAllWithoutPrint t')
		 in
		   if temp = Zero
		   then True
		   else False
		end)
end(*  structure Arith *)

(* a unit test *)

val e = Arith.True

val e1 = Arith.Succ (Arith.Succ (Arith.Succ Arith.Zero))

val e2 = Arith.If (Arith.IsZero (Arith.Succ Arith.Zero), Arith.Pred (Arith.Succ Arith.Zero), Arith.Succ Arith.Zero)

val e3 = Arith.Pred (Arith.Succ (Arith.Succ Arith.Zero))

val e4 = Arith.IsZero (Arith.Pred (Arith.Succ Arith.Zero))

val e5 = Arith.If (Arith.If(Arith.True, Arith.True, Arith.False), Arith.Pred (Arith.Succ Arith.Zero), Arith.Succ Arith.Zero)

(*

*)

val _ = (Arith.pp e5; print "\n")
val _ = (Arith.pp (Arith.evalBig e5); print "\n")

(*

val _ = Arith.evalAll e1
*)