(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(x, xs) = 
    let
        fun f (prev, cur) = 
            case cur of  [] => NONE
            | y::ys => if same_string(y, x) 
             then SOME (prev@ys) 
             else f (prev@[x], ys)
    in 
        f([], xs)
    end 

fun get_substitutions1(xs, str) = 
    case xs of [] => []
    | y::ys => case all_except_option(str, y) of NONE => get_substitutions1(ys, str)
               | SOME x => x@get_substitutions1(ys, str)

fun get_substitutions2(xs, str) = 
    let 
        fun f (str, ys, ans) = 
            case ys of [] => ans
            | y::ys' => case all_except_option(str, y)
            of NONE => f(str, ys', ans)
            | SOME x => f(str, ys', ans@x)
    in
        f(str, xs, [])
    end

fun similar_names(xs, {first=f, last=l, middle=m}) = 
    let 
        val first_names = f::get_substitutions2(xs, f)
      
        fun g(firsts, names) =
        case firsts of
            [] => names
        | x::xs' => g(xs', names@[{first = x, middle=m, last=l}])
    in
      g(first_names, [])
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(suit, _) = 
  case suit of
    (Clubs | Spades) => Black
    | _ => Red

fun card_value(_, value) = 
    case value of Num x => x
    | Ace => 11
    | _ => 10

fun remove_card(cs, c, e) = 
    case cs of [] => raise e 
    | x::xs => if x = c then xs else x::remove_card(xs, c, e)

fun all_same_color(cs) = 
    case cs of [] => true
    | x::[] => true
    | x::y::xs => if card_color(x) = card_color(y) 
                  then all_same_color(y::xs) 
                  else false

fun sum_cards(cs) = 
    let
        fun f (xs, sum) = 
            case xs of [] => sum 
            | x::xs' => f(xs', card_value(x) + sum)
             
    in 
        f(cs, 0)
    end

fun score(cs, goal) = 
    let 
        val cur = sum_cards(cs)
        fun f (sum) = 
            if all_same_color(cs) = true then sum div 2
            else sum
        
    in 
        if cur - goal > 0 then f(3 * (cur - goal))
        else f(goal - cur)
    end

    fun officiate(cs, ms, goal) =
  let
    fun run(cs, ms, hs) = 
      case ms of
        [] => score(hs, goal)
      | m::ms' => case m of
                      Discard d => run(cs, ms', remove_card(hs, d, IllegalMove))
                    | _         => case cs of
                                        []      => score(hs, goal)
                                      | c::cs'  => if sum_cards(hs) > goal
                                                   then score(c::hs, goal)
                                                   else run(cs', ms', c::hs)
  in
    run(cs, ms, [])
  end