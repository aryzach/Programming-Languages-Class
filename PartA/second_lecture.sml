datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp * exp;


val x = Add (Constant (10+9), Negate (Constant 4));

fun eval a =
    case a of
        Constant i => i
      | Negate e2 => ~ (eval e2)
      | Add(e1,e2) => (eval e1) + (eval e2)
      | Multiply(e1, e2) => (eval e1) + (eval e2);

eval(x);

fun max(e : exp) =
    let fun ok(e1,e2) =
	    Int.max(max(e1),max(e2))
    in
	    case e of
		Constant i => i
	       | Negate e2 => (max(e2))
	       | Add(e1,e2) => ok(e1,e2)
	       | Multiply(e1, e2) => ok(e1,e2)
    end
	



fun zip3 thing =
    case thing of
	([],[],[]) => []
      | (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1,hd2,hd3)::zip3(tl1,tl2,tl3)
(*      | _ => raise ListLengthMismatch *)
    
fun zip3rev thing =
    case thing of
	[] => ([],[],[])
      | (a,b,c)::tl => let val (tl1,tl2,tl3) = zip3rev tl
		       in
			   (a::tl1,b::tl2,c::tl3)
		       end
			   
fun nondec xs =
    case xs of
	[] => true
      | _::[] => true
      | x::y::tl => y >= x andalso nondec(y::tl)
		   
datatype sgn = P | N | Z;

fun multsign (x,y) =
    case (x,y) of
	(_,Z) => Z
      | (Z,_) => Z
      | (N,N) => P
      | (P,P) => P
      | _ => N

fun append (xs,ys) =
    case (xs,ys) of
	([],_) => ys
      | (x::xs',ys') => x::(append (xs',ys')) 

fun rev xs =
    case xs of
	[] => []
      | x::xs' =>  append((rev xs'),x::[])

fun rrev xs =
    let fun aux(xs , acc) =
	case xs of
	    [] => acc
	  | x::xs' => aux (xs', x::acc)
    in
	aux(xs,[])
    end

datatype 'a mylist = Empty | Value of 'a * 'a mylist

val ml = Value (5, Empty);

val ml2 = Value (5,Value(4,Empty));



type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

fun pass_fail {id=x, grade=y} =
    case y of
	NONE => fail
      | SOME i => if i >= 75 then pass
		  else fail


fun has_p {id=x, grade=y} =
    pass_fail {id = x, grade = y} = pass
		  
	      
fun num_p xs =
    case xs of
	[] => 0
      | x::xs' => if has_p x then 1 + num_p xs'
		  else num_p xs' 


val lofg = [{id = 5, grade = NONE}, {id = 5, grade = SOME 80},
	    {id = 5, grade = SOME 40}, {id = 5, grade = SOME 90}];

val mis = [(pass,{id = 5, grade = NONE}), (pass,{id = 5, grade = SOME 80}),
	    (pass, {id = 5, grade = SOME 40}), (pass,{id = 5, grade = SOME 90})];

fun num_m xs =
    case xs of
	[] => 0
      | (x,y)::xs' => if x <> pass_fail y
		      then 1 + num_m xs'
		      else num_m xs'

datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = lma | pm

val t1 = leaf;
val t2 = node {value = lma,
	       left = leaf,
	       right = node {value = lma,
			     left = node {value = pm,
					  left = leaf,
					  right = leaf},
			     right = node {value = pm,
					  left = leaf,
					  right = node {value = lma,
							left = leaf,
							right = leaf}}}}

fun tree_h tree =
    case tree of
	leaf => 0
     | node {value=a,left=b,right=c} => 1 + Int.max(tree_h b, tree_h c) 


fun tree_sum tree =
    case tree of
	leaf => 0
      | node {value=a,left=b,right=c} => a + tree_sum b + tree_sum c ;

fun garden tree =
    case tree of
	node {value=pm,left=b,right=c} => leaf
      | node {value=lma,left=b,right=c} =>  node {value=lma,left=(garden b),right=(garden c)}
      | x => x 

fun mylast xs =
    case xs of
	[] => 0
      | x::[] => x
      | x::xs' => mylast xs'
								 
fun mytake (xs,i) =
    let fun aux(xs,left) =
	case (xs,left) of
	    (_,0) => []
	  | ([],_) => [98]
	  | (x::xs',left) => x::aux(xs',left-1)
    in
	aux(xs,i)
    end;

mytake([1,3,2,4],0);
mytake([1,3,2,4],3);
mytake([1,3,2,4],4);
mytake([1,3,2,4],5);
mytake([],3);


fun mydrop (xs,i) =
    case (xs,i) of
	(xs,0) => xs
      | ([],_) => [99]
      | (x::xs',i) => mydrop(xs',i-1);

mydrop([1,3,2,4],0);
mydrop([1,3,2,4],3);
mydrop([1,3,2,4],4);
mydrop([1,3,2,4],5);
mydrop([],2);

fun myconcat lol =
    case lol of
	[] => []
     | x::xs => x @ myconcat xs

datatype nat = ZERO | SUCC of nat
			     
fun pos n =
    case n of
	ZERO => false
      | n => true;

pos ZERO;
pos (SUCC ZERO);
pos (SUCC (SUCC ZERO));

val s1 = ZERO;
exception Negative of string

fun pred n =
    case n of
	ZERO => raise Negative "negative"
     | SUCC x => x

val n0 = ZERO;
val n1 = SUCC ZERO;
val n2 = SUCC n1;
val n3 = SUCC n2;
		     
fun nat_to_int n =
    case n of
	ZERO => 0
     | SUCC x => 1 + nat_to_int x 

fun add (n, s) =
    case (n,s) of
	(ZERO, s) => s
      | (n,ZERO) => n
      | (SUCC x, s) => add(x,SUCC s)

fun sub (n, s) =
    case (n,s) of
	(n,ZERO) => n
     | _ => sub(pred n,pred s)

fun mult (n,s) =
    case (n,s) of
	(ZERO,_) => ZERO
      | (_,ZERO) => ZERO
      | (SUCC ZERO,s) => s
      | (n,SUCC ZERO) => n
      | (n,s) => mult(add(n,n),pred s)
fun less_than (n,s) =
    case (n,s) of
	(_,ZERO) => false
      | (ZERO,_) => true
      | (n,s) => less_than(pred n, pred s) 

datatype intSet = 
	 Elems of int list (*list of integers, possibly with duplicates to be ignored*)
	 | Range of { from : int, to : int }  (* integers from one number to another *)
	 | Union of intSet * intSet (* union of the two sets *)
	 | Intersection of intSet * intSet (* intersection of the two sets *)


fun listContains (i,lst) =
    case lst of
	[] => false
      | x::xs => if x = i then true
		 else listContains (i, xs)

fun interOfTwoLists (a,b) =
    case (a,b) of
	([],_) => []
      | (_,[]) => []
      | ((x::xs),ys) => if listContains(x,ys)
			then x::interOfTwoLists(xs,ys)
			else interOfTwoLists(xs,ys)
			       
fun toList is =
    case is of
	Elems [] => []
      | Elems (x::xs) => if listContains(x,xs) then toList(Elems xs) else x::(toList(Elems xs))
      | Range {from=x,to=y} => if x = y then y::[] else x::(toList (Range {from=(x+1),to=y}))
      | Union (x,y) => (toList x) @ (toList y)
      | Intersection (x,y) => interOfTwoLists(toList x, toList y)


				    



fun iscontains (item,is) =
    listContains(item, toList is)

fun isEmpty is =
    case toList(is) of
	[] => true
      | _ => false

fun filter (f, xs) =
    case xs of
	[] => []
      | x::xs' => if f x then x::filter(f,xs') else filter(f,xs')

fun all_even_tup xs = filter((fn (_,v) => v mod 2 = 0),xs)
					 
val ok = filter((fn x => not x), [true,false,true, true,false])


fun dt f =
    if f 7
    then fn x => x + 3
    else fn x => x + 4

val res = dt (fn x => x = 7)
val e = res 4


val x = Add (Constant (11+9), Negate (Constant 3));

fun isAll (f, exp) =
    case exp of
	Constant i => f i
      | Negate i => isAll (f, i)
      | Add (a,b) => isAll (f, a) andalso isAll (f,b)
      | Multiply (a,b) => isAll (f,a) andalso isAll (f,b)


val ans = isAll ((fn y =>y < 22), x)

fun f x = x + 1;
fun g x = x + 8;
		 

fun compose(f, g) = case x of
			(_,x) => fn x => f(g x)
		      | x => fn x => f x

val z = compose(f,g) (1,2)

fun g x = x + 9;

val z = compose(f,g) 1
