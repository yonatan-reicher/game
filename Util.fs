module Util


let mapFst (f: 'a -> 'b) (pair: 'a * 'c) : 'b * 'c = (f (fst pair), snd pair)


let mapSnd (f: 'a -> 'b) (pair: 'c * 'a) : 'c * 'b = (fst pair, f (snd pair))


module List =
    /// partition but for zero or one elements for one of the lists.
    let tryFindAndRest (f: 'T -> bool) (list: 'T list) : ('T * 'T list) option =
        /// Just like the nesting function, but with an accumulator for visited
        /// elements in reverse order.
        let rec inner acc f list =
            match list with
            | [] -> None
            | x :: xs ->
                if f x then
                    Some(x, List.rev acc @ xs)
                else
                    inner (x :: acc) f xs

        inner [] f list


module Seq =
    let rec tryTail (s: 'T seq) : 'T seq option =
        if Seq.isEmpty s then None else Some(Seq.tail s)

(* Maybe this should be called truncSkip?
    let rec trySkip (n: int) (s: 'T seq) : 'T seq =
        if n <= 0 || Seq.isEmpty s then
            s
        else
            trySkip (n - 1) (Seq.tail s)
    *)
