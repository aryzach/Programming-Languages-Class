fun max(xs : int list)=
    if null xs
    then NONE
    else
	let val max_rest = max(tl xs)
	in
	    if isSome max_rest andalso valOf max_rest > hd xs
	    then max_rest
	    else SOME (hd xs)
	end

	    

val g = [3,2,1,5,4,6];

fun max2(xs : int list)=
    if null xs
    then NONE
    else
	(* int list -> int *)
	let fun max_nonempty(xs : int list)=
		if null (tl xs)
		then hd xs
		else
		    let val max_tl = max_nonempty(tl xs)
		    in
			if max_tl > hd xs
			then max_tl
			else hd xs
		    end
	in
	    SOME (max_nonempty xs)
	   
	end
	    

fun alternate(xs : int list) =
    if null xs
    then 0
    else (hd xs) - alternate(tl xs)

fun min2(xs : int list) =
    if null xs
    then NONE
    else
	let fun min2_nonempty(xs : int list) =
		if null (tl xs)
		then hd xs
		else
		    let val min2_tl = min2_nonempty(tl xs)
		    in
			if min2_tl < hd xs
			then min2_tl
			else hd xs
		    end
	in
	    SOME (min2_nonempty xs)
	end
	    
			    
fun min_max(xs : int list) =
    (valOf (min2 xs),valOf (max2 xs))

fun sum_list(xs : int list) =
    if null xs
    then 0
    else (hd xs) + sum_list (tl xs)
			    
	    
fun cumsum(xs : int list) =
    if null xs
    then []
    else
	let fun cumsum_sofar(xs : int list, sofar : int) =
		if null xs
		then []
		else
		   (hd xs + sofar) :: cumsum_sofar((tl xs),sofar + (hd xs))
	in
	    cumsum_sofar(xs,0)
	end


fun greeting(s : string option) =
    if isSome s
    then "Hello " ^ valOf s ^ "!"
    else
	"Hello you"

fun append(xs : 'a list, ys : 'a list) =
    if null xs
    then ys
    else hd xs :: append(tl xs, ys)
	

fun repeat(vals : int list, occ : int list) =
    if null vals orelse null occ
    then []
    else
	let fun repeat_times(num : int, occ_left : int) =
		if occ_left = 0
		then []
		else
		    num :: repeat_times(num, occ_left - 1)
	in
	    append(repeat_times(hd vals, hd occ), repeat(tl vals, tl occ))
	end
	    
fun addOpt(a : int option, b : int option) =
    if isSome a andalso isSome b
    then SOME (valOf a+ valOf b)
    else NONE
	     
fun addAllOpt(a : int option list) =
    let fun add(a : int option list, allnone : bool) =
	       if null a andalso allnone
	       then NONE
	       else if null a then SOME 0
	       else
		   if isSome (hd a)
		   then SOME (valOf(hd a) + valOf(add(tl a, false)))
		   else (add(tl a, allnone))
    in
	add(a, true)
    end

val b = [NONE, SOME 1, NONE, SOME 2, SOME ~8]

val bool1 = [true, false, false, true];
val bool2 = [false, false];
val bool3 = [true, true];

fun any(b : bool list) =
    if null b then false
    else if not (hd b) then any(tl b)
    else hd b
	    
fun all(b : bool list) =
    if null b then true
    else if hd b then all(tl b)
    else hd b		

fun zipRecycle(first : int list, second : int list) =
    let fun zRi(second : int list, first_cur : int list) =
	    if null second then []
	    else if null first_cur then zRi(second, first)
	    else (hd first_cur, hd second) :: zRi(tl second, tl first_cur)
    in
	zRi(second, first)
    end

val a = [1,2,3];
val b = [1,2,3,4,5];

fun lookup(sip : (string * int) list, s : string) =
    if null sip then NONE
    else if #1 (hd sip) = s then SOME (#2 (hd sip))
    else lookup(tl sip, s)

val sip = [("hi", 2),("by",1),("lo",4)];

fun splitAt(xs : int list, at : int) =
    if null xs then []
    else
	let fun bs(xs : int list) =
		if null xs then []
		else if hd xs < at then hd xs :: bs(tl xs)
		else bs(tl xs)
	    fun bb(xs : int list) =
		if null xs then []
		else if hd xs > at then hd xs :: bb(tl xs)
		else bb(tl xs)
	    fun bi(xs : int list) =
		  if null xs then []
		  else if hd xs = at then hd xs :: bi(tl xs)
		  else bi(tl xs) 
	in
	    bs(xs) :: bi(xs) ::  bb(xs) :: []
	end

fun sortMerge(a : int list, b : int list) =
    if null a then b
    else if null b then a
    else if hd a < hd b then hd a :: sortMerge(tl a, b)
    else hd b :: sortMerge(a, tl b)
			  
fun qsort(a : int list) =
    if null a then []
    else
	let val sp = splitAt(a,hd a)
	in
	  append( append(qsort(hd sp), hd (tl sp)), qsort(hd (tl (tl sp))))
	end
	    
fun divide(xs : int list) =
    let fun d(xs : int list, state : bool) =
	    if null xs then []
	    else if state then hd xs :: d(tl xs, not state)
	    else d(tl xs, not state)
	fun e(xs : int list, state : bool) =
	    if null xs then []
	    else if not state then  hd xs :: e(tl xs, not state)
	    else e(tl xs, not state)
    in
	( d(xs,true), e(xs,true) )
    end
	
fun not_so_qsort(xs : int list) =
    if null xs then []
    else if null (tl xs) then xs
    else
	let val subd = divide xs
	in sortMerge(not_so_qsort(#1 subd), not_so_qsort(#2 subd))
	end

fun fulldiv(q : int, p : int) =
    let fun subfull(times : int, p : int) =
	    if p mod q = 0 then subfull(times + 1, p div q)
	    else (times, p)
    in
	subfull(0, p)
    end

fun estsqrt(x : int) =
    let fun suba(n : int) =
	if n * n > x then n
	else suba(n+1)
    in
	suba(1)
    end

fun factorize(x : int) =
    let fun ff(x : int, xcur: int, cur : int, sofar : (int * int) list) =
	    if cur > estsqrt x then sofar
	    else
		if #1 (fulldiv(cur, xcur)) <> 0
		then ff(x, #2 (fulldiv(cur, xcur)), cur + 1, append( sofar, [(cur, #1 (fulldiv(cur, x)))]))
		else
		    ff(x, #2 (fulldiv(cur, xcur)), cur + 1, sofar)
    in
	ff(x,x,2,[])
    end

fun powerdo(l : int, t :int) =
	if t = 1 then l
	else l * powerdo(l, t-1)

fun mult(xs : (int * int) list) =
    if null xs then 1
    else
	powerdo(#1 (hd xs), #2 (hd xs)) * mult(tl xs)


fun last_added(xs : int list) =
    if null (tl xs) then hd xs
    else last_added(tl xs)

fun insert(x : int, xs : int list) =
    let val split = splitAt(xs, x)
    in
	append(hd split, append( (x :: hd (tl split)), hd (tl (tl split))))
    end

fun mult_first_tuple_list(x : int, xs : (int * int) list) =
    if null xs then []
    else (x * (#1 (hd xs)), (#2 (hd xs))) :: mult_first_tuple_list(x, tl xs)

	
fun all_products(oxs : (int * int) list) =
    if null oxs then []
    else
	let fun all_products(xs : (int * int) list) =
		if null xs then []
		else
		    let val cn = powerdo(#1 (hd xs), #2 (hd xs));
		    in
			if cn > mult(oxs)
			then
			    if #2 (hd xs) = 1 then all_products(tl xs)
			    else all_products( (#1 (hd xs), (#2 (hd xs)) - 1) :: tl xs)
			else
			    if #2 (hd xs) = 1 then cn :: append(
						       (all_products(mult_first_tuple_list(cn, tl xs))) ,
						       all_products(tl xs) )
			    else cn :: append(
				     (all_products(mult_first_tuple_list(cn, tl xs) )) ,
				     all_products( (#1 (hd xs), (#2 (hd xs)) - 1) :: tl xs) )
		    end
	in
	    all_products(oxs)
	end
	    
	    
	    
		 
	



	
						 
	
    
					 
					      

	      
	
	
		       
	     
