let gcd a b = 
    if (= b 0) then a
    else if (> b a) then (gcd b a)
    else (gcd (- a b) b);

(print "Введите 1 число: ");
let a = read_int;

(print "Введите 2 число: ");
let b = read_int;

(println "gcd(" a ", " b ") = " (gcd a b) );
