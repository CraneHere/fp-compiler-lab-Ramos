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

let list_sum lst = 
    if ? lst.tail def
    then (+ lst.head (list_sum lst.tail) )
    else 0;

let list_map lst f =
    if ? lst.tail def
    then {
        head : (f lst.head),
        tail : (list_map lst.tail f)
    }
    else {};

(print "Введите количество элементов в списке:");
let n = read_int;

(println "Введите " n " чисел по одному на отдельной строке");
let lst = (do_n_times n list_empty lam acc -> (list_add acc read_int) );

(println "Сумма чисел: " (list_sum lst));

let print_elem elem = (print elem " ");
(println "Список квадратов чисел: ");

(list_map (list_map lst lam x -> (* x x) ) print_elem);

(println "");
