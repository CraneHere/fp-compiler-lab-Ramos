let do_n_times n acc f =
    if (= n 0) then acc
    else (do_n_times (- n 1) (f acc) f);

(println "--- Определяем функции для работы со списком ---");

let list_empty = {};

let list_add lst elem =
    if ? lst.tail def 
    then {
        head : lst.head,
        tail : (list_add lst.tail elem)
    } 
    else {
        head : elem,
        tail : {}
    };

let list_len lst =
    if ? lst.tail def
    then (+ 1 (list_len lst.tail) )
    else 0;

let list_map lst f =
    if ? lst.tail def
    then {
        head : (f lst.head),
        tail : (list_map lst.tail f)
    }
    else {};

(print "Введите количество элементов в списке: ");
let n = read_int;

(println "Введите " n " чисел по одному на отдельной строке");
let lst = (do_n_times n list_empty lam acc -> (list_add acc read_int) );

let list_sort_iteration lst =
    if ? lst.tail def 
    then if ? lst.tail.tail def
         then 
            loclet fst = lst.head in
            loclet snd = lst.tail.head in
            if (> fst snd) 
            then {
                head : snd,
                tail : (list_sort_iteration { head : fst, tail : lst.tail.tail } )
            }
            else {
                head : fst,
                tail : (list_sort_iteration lst.tail)
            }
         else lst
    else {};

let list_sort lst =
    (do_n_times (list_len lst) lst list_sort_iteration);

let my_print x = (print x " ");

(list_map (list_sort lst) my_print);
