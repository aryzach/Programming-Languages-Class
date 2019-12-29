fun compose_opt (f, g) =
    fn x => case g x of
		NONE => NONE
	      | SOME i => case f i of
			      NONE => NONE
			   | SOME j => SOME j

(*infix !>
fun x !> f !> g = g(f x)*)
					   
fun do_until f g x = case g(f x) of
				  false => f x
				| _ => do_until f g (f x);

fun do_until_new f x = do_until f (fn y => y = x)  x;

(* need to finish implementing factorial
fun do_fact x = x * (do_until ((fn x => (x * (x-1),x-1)), (fn (x,y) => y > 1))) x
 *)

fun fact num = (do_until (fn x => x * (fact (x-1))) (fn y => y >= 2)) num

fun fixed_point f x = do_until_new f x

fun map2 (f, (x,y)) = (f x, f y);

		      
fun app_all f l x =
    let val lst = map f (l x)
    in
	let fun cclst lol =
		case lol of
		    [] => []
		  | y::ys => y @ (cclst ys)
	in cclst lst
	end
    end

(* (int -> int) list -> int -> int list *)				 
fun map_fun_on_int fnlst i =
    let val tup = (fnlst, i)
    in
	case tup of
	    ([],i) => []
	  | ((x::xs),i) => (x i)::(map_fun_on_int xs i)
    end

fun g n = [n,2*n,3*n];
val r = app_all g g 1;


fun fold f acc xs =
    case xs of
	[] => acc
      | x::xs => fold f (f(acc,x)) xs;

fun sum xs = fold (fn (acc,x) => acc + x) 0 xs;

val sorted3 = fn x => fn y => fn z => x<y andalso y<z

fun curry f = fn x => fn y => f (x,y)

fun range (i,j) = if i > j then [] else i::range(i+1,j)
					

fun range_curried i j = range(i,j);

val new_range = curry range;

val x = new_range 5 8;

fun uncurry f (x,y) = f x y;

fun new_map (x,y) = uncurry map (x,y);

		     
fun fold_l f acc xs =
    case xs of
	[] => acc
      | x::xs => fold_l f (f(acc,x)) xs;

val x = fold_l (fn (x,y) => x*y) 1 [3,4,10,100];

fun fold_r f acc xs =
    case xs of
	[] => acc
     | x::[] => f(acc,x)
     | x::xs => f( (fold_r f acc xs), x );

fun partition f lst =
    let fun aux f lst l1 l2 =
	     case lst of
		 [] => (l1,l2)
	       | x::xs => if f x
			  then aux f xs (x::l1) l2
			  else aux f xs l1 (x::l2)
    in
	aux f lst [] []
    end
	
			      
fun unfold f n =
    let val maybe = f n
    in
	case maybe of
	    NONE => []
	  | SOME (a,b) => a::(unfold f b)
    end;

fun do_fact num =
    case do_until (fn (a,b) => if b = 0 orelse b = ~1
			       then (a,b) else (a*b,b-1))
		  (fn (a,b) => case (a,b) of
				   (0,0) => false
				 | (0,~1) => false		
				 | (a,0) => false
				 | (a,1) => false
				 | _ => true)
		  (num,num-1)
     of
	(0,~1) => 1
      | (0,0) => 1
      | (a,0) => a
      | (n,1) => n
		     
		  
fun uflfact num =
    fold_l (fn (acc,x) => acc*x) 1 (unfold (fn n => if n = 0 then NONE else SOME (n,n-1)) num)

	
fun mapfr f xs =
    case xs of
	[] => []
     | y::ys => (fold_r (fn (1,y) => f y) 1 (y::[]))::(map f ys)
		     

fun filterfr f xs =
    case xs of
	[] => []
      | y::ys => let val temp = (fold_r (fn (NONE,y) => if f y then SOME y else NONE) NONE (y::[]))
		 in
		     case temp of
			 NONE =>  filterfr f ys
		      | SOME i => i::(filterfr f ys)
		 end
		     
fun fold_r f acc xs =
    case xs of
	[] => acc
     | x::[] => f(acc,x)
     | x::xs => f( (fold_r f acc xs), x );

fun fold_l f acc xs =
    case xs of
	[] => acc
      | x::xs => fold_l f (f(acc,x)) xs;

fun fold_l_with_fold_r f_new acc_new xs_new =
    case xs_new of
	[] => acc_new
      | x::[] => fold_r f_new (acc_new) (x::[])
      | x::xs => fold_l_with_fold_r f_new (fold_r f_new acc_new (x::[]))  xs

val flfr = fold_l_with_fold_r (fn (x,y) => y div x) 1 [1,4,1024]; 
val fl = fold_l (fn (x,y) => y div x) 1 [1,4,1024];

val tf = flfr = fl;

(*Define a (polymorphic) type for binary trees where data is at internal nodes but not at leaves. Define \verb|map|map and \verb|fold|fold functions over such trees. You can define \verb|filter|filter as well where we interpret a "false" as meaning the entire subtree rooted at the node with data that produced false should be replaced with a leaf.*)

datatype 'a mytree = Leaf | Node of 'a * 'a mytree * 'a mytree;

val a = Leaf;

val b = Node (4,Leaf,Leaf);
val c = Node (5,b,b);
val d = Node (7,b,c);

val e = Node ("hi",Node ("ok",Leaf,Leaf),Leaf);

val t1 = Node (200,c,d)

fun mtmap f mt =
    case mt of
	Leaf => Leaf
      | Node (i,a,b) => Node (f i,mtmap f a,mtmap f b);

fun mtfold f acc mt =
    let fun aux acc mt todo =
	    case (mt,todo) of
		(Leaf,[]) => acc
	      | (Leaf,(x::xs)) => aux acc x xs
	      | (Node (i,a,b),lst) => aux (f(acc,i)) a (b::lst) 
    in
	aux acc mt []
    end
	
fun mtfilter f mt =
    case mt of
	Leaf => Leaf
      | Node (i,a,b) => if f i
			then Node (i,mtfilter f a, mtfilter f b)
			else Leaf
				 
val cbs : (int -> unit) list ref = ref [];
fun onKeyEvent f = cbs := f::(!cbs);

fun onEvent i =
    let fun loop xs =
	    case xs of
		[] => ()
	      | x::xs => (x i ; loop xs)
    in loop (!cbs)
    end
				 
val timesPressed = ref 0
val _ = onKeyEvent (fn _ => timesPressed := (!timesPressed) + 1);

fun printIfPressed i =
    onKeyEvent (fn j => if i = j
			then print("pressed " ^ Int.toString i ^ "\n")
			else ());		
						
val _ = printIfPressed 2;
val _ = printIfPressed 3;
val _ = printIfPressed 4;



datatype set = S of {insert:int -> set, member:int -> bool, size:unit -> int};

val empty_set =
    let
	fun make_set xs =
	    let fun contains i = List.exists (fn j => i = j) xs
	    in
		S { insert = fn i => if contains i
				     then make_set xs
				     else make_set (i::xs),
		    member = contains,
		    size = fn () => length xs
		  }
	    end
    in
	make_set []
    end

fun use_sets () =
    let val S s1 = empty_set
	val S s2 = (#insert s1) 34
	val S s3 = (#insert s2) 34
	val S s4 = #insert s3 19
    in
	if (#member s4) 42
	then 99
	else if (#member s4) 19
	then 17 + (#size s3) ()
	else 0
    end
			
fun f x =
    let
	val g =
	    let
		val _ = x:= !x + 1
	    in
		x
	    end
    in
	g
    end
