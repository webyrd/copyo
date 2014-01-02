(load "mk.scm")
(load "test-check.scm")

(test "rv-1"
  (let ((x (var 'x))
        (y (var 'y)))
    (replace-vars `(5 (,x) #t (,y (,x)) ,y)))
  '(5 (#(z_0)) #t (#(z_1) (#(z_0))) #(z_1)))

(test "same-structure-1"
  (let ((a (var 'a))
        (b (var 'b)))
    (same-structure a b))
  '((#(b) . #(a))))

(test "same-structure-2"
  (let ((a (var 'a)))
    (same-structure a a))
  '((#(a) . #(a))))

(test "same-structure-3"
  (let ((a (var 'a)))
    (same-structure a 5))
  #f)

(test "same-structure-4"
  (let ((a (var 'a))
        (b (var 'b)))
    (same-structure `(,a ,a) `(,b ,b)))
  '((#(b) . #(a))))

(test "same-structure-5"
  (let ((a (var 'a))
        (b (var 'b))
        (c (var 'c)))
    (same-structure `(,a ,c) `(,b ,b)))
  #f)

(test "same-structure-6"
  (let ((a (var 'a))
        (b (var 'b))
        (c (var 'c))
        (d (var 'd)))
    (same-structure `(,a ,c) `(,b ,d)))
  '((#(d) . #(c)) (#(b) . #(a))))

(test "same-structure-7"
  (let ((a (var 'a))
        (b (var 'b))
        (c (var 'c))
        (d (var 'd)))
    (same-structure `(,a ,c ,a ,c ,c ,a) `(,b ,d ,b ,d ,d ,b)))
  '((#(d) . #(c)) (#(b) . #(a))))

(test "same-structure-8"
  (let ((a (var 'a))
        (b (var 'b))
        (c (var 'c))
        (d (var 'd)))
    (same-structure `(,a ,c ,a ,a ,c ,a) `(,b ,d ,b ,d ,d ,b)))
  #f)

(test "same-structure-9"
  (let ((a (var 'a))
        (b (var 'b))
        (c (var 'c))
        (d (var 'd)))
    (same-structure `(,a (,c ,a . ,c) #t 7 ,c ,a) `(,b (,d ,b . ,d) #t 7 ,d ,b)))
  '((#(d) . #(c)) (#(b) . #(a))))

(test "same-structure-10"
  (let ((a (var 'a))
        (b (var 'b))
        (c (var 'c))
        (d (var 'd)))
    (same-structure `(,a (,c ,a ,c) #t 7 ,c ,a) `(,b (,d ,b . ,d) #t 7 ,d ,b))) ;;; missing dot
  #f)

(test "same-structure-11"
  (let ((a (var 'a))
        (b (var 'b))
        (c (var 'c))
        (d (var 'd)))
    (same-structure `(,a (,c ,a . ,c) #t 8 ,c ,a) `(,b (,d ,b . ,d) #t 7 ,d ,b))) ;;; 7 =/= 8
  #f)

(test "same-structure-12"
  (let ((a (var 'a))
        (b (var 'b))
        (c (var 'c))
        (d (var 'd)))
    (same-structure `(,a (,c ,a . ,c) #t 7 ,c ,a ,a) `(,b (,d ,b . ,d) #t 7 ,d ,b))) ;;; extra a 
  #f)

;;; All of these seem to have obvious answers.
(test "more-general-than-1"
  (more-general-than 5 5 '())
  #f)

(test "more-general-than-2"
  (more-general-than 5 6 '())
  #f)

(test "more-general-than-3"
  (more-general-than 5 '(5) '())
  #f)

(test "more-general-than-4"
  (let ((x (var 'x)))
    (more-general-than 5 x '()))
  #f)

(test "more-general-than-5"
  (let ((x (var 'x)))
    (more-general-than x 5 '()))
  #t)

(test "more-general-than-6"
  (let ((x (var 'x))
        (y (var 'y)))
    (more-general-than x y '()))
  #f)

(test "more-general-than-7"
  (let ((x (var 'x)))
    (more-general-than x x '()))
  #f)

(test "more-general-than-8"
  (let ((x (var 'x)))
    (more-general-than x `(,x) '()))
  #f)

(test "more-general-than-9"
  (let ((x (var 'x))
        (y (var 'y)))
    (more-general-than x `(,y) '()))
  #t)

(test "more-general-than-10"
  (let ((x (var 'x))
        (y (var 'y))
        (z (var 'z)))
    (more-general-than x `(,y . ,z) '()))
  #t)

(test "more-general-than-11"
  (let ((w (var 'w))
        (x (var 'x))
        (y (var 'y))
        (z (var 'z)))
    (more-general-than `(,w . ,x) `(,y . ,z) '()))
  #f)

(test "more-general-than-12"
  (let ((w (var 'w))
        (x (var 'x))
        (y (var 'y)))
    (more-general-than `(,w . ,x) `(,y . 5) '()))
  #t)

(test "more-general-than-13"
  (let ((w (var 'w))
        (x (var 'x))
        (y (var 'y)))
    (more-general-than `(,y . 5) `(,w . ,x) '()))
  #f)

(test "more-general-than-14"
  (let ((x (var 'x))
        (y (var 'y))
        (z (var 'z)))
    (more-general-than `(,y . ,z) x '()))
  #f)


(test "0" (run* (q) (copyo 5 5)) '(_.0))
(test "1" (run* (q) (copyo q q)) '(_.0))
(test "2" (run* (q) (copyo 5 q)) '(5))

(test "3"
  (run* (q)
    (copyo q 5)
    (copyo q 6))
  ;; Interesting: in this case, _.0 must remain fresh.
  ;;
  ;; Could reify as
  ;;
  ;; ((_.0 (copy (_.0 _.1))))
  ;;
  ;; old busted answer
  ;;
  ;; ((_.0 (copy (_.0 5) (_.0 6))))
  ;;
  ;; Claire suggests using f.n to clarify when a variable on the rhs
  ;; must really be fresh:
  ;;
  ;; ((_.0 (copy (_.0 f.1))))
  '((_.0 (copy (_.0 f.1)))))

(test "4a" 
  (run* (q) 
    (fresh (x y)
      (copyo q 5)
      (== `(,x ,y) q)))
  '())

(test "4b" 
  (run* (q) 
    (fresh (x y)
      (== `(,x ,y) q)
      (copyo q 5)))
  '())

(test "5a"
  (run* (q)
    (fresh (x y)
      (copyo q `(1 2))
      (copyo q `(3 4 5 6 7))
      (== q `(,x . ,y))))
  ;; Interesting: in this case, _.0 must remain fresh.
  ;; _.1 can become a (z0 . z1)
  ;;
  ;; Could reify as
  ;;
  ;; (((_.0 . _.1) (copy (_.0 _.2) (_.1 (_.3 . _.4)))))
  ;;
  ;; old busted answer
  ;;
  ;; (((_.0 . _.1) (copy (_.0 1) (_.0 3) (_.1 (2)) (_.1 (4 5 6 7)))))  
  '(((_.0 . _.1) (copy (_.0 f.2) (_.1 (f.3 . f.4))))))

(test "5b"
  (run* (q)
    (fresh (x y)
      (copyo q `(3 4 5 6 7))
      (copyo q `(1 2))
      (== q `(,x . ,y)))) 
  '(((_.0 . _.1) (copy (_.0 f.2) (_.1 (f.3 . f.4))))))

(test "5c"
  (run* (q)
    (fresh (x y)
      (== q `(,x . ,y))
      (copyo q `(1 2))
      (copyo q `(3 4 5 6 7))))
  '(((_.0 . _.1) (copy (_.0 f.2) (_.1 (f.3 . f.4))))))

(test "5d"
  (run* (q)
    (fresh (x y)
      (copyo q `(1 2))
      (== q `(,x . ,y))
      (copyo q `(3 4 5 6 7))))
  '(((_.0 . _.1) (copy (_.0 f.2) (_.1 (f.3 . f.4))))))

(test "5e"
  (run* (q)
    (fresh (x y)
      (copyo q `(3 4 5 6 7))
      (== q `(,x . ,y))
      (copyo q `(1 2))))
  '(((_.0 . _.1) (copy (_.0 f.2) (_.1 (f.3 . f.4))))))

(test "5f"
  (run* (q)
    (fresh (x y)
      (== q `(,x . ,y))
      (copyo q `(3 4 5 6 7))
      (copyo q `(1 2))))
  '(((_.0 . _.1) (copy (_.0 f.2) (_.1 (f.3 . f.4))))))

(test "5g"
  (run* (q)
    (fresh (x y)
      (copyo q `(3 2))
      (copyo q `(3 4 5 6 7))
      (== q `(3 . ,y))))
  '(((3 . _.0) (copy (_.0 (f.1 . f.2))))))

(test "5g1"
  (run* (q)
    (fresh (x y)
      (== q `(3 . ,y))
      (copyo q `(3 2))
      (copyo q `(3 4 5 6 7))))
  '(((3 . _.0) (copy (_.0 (f.1 . f.2))))))

(test "5h"
  (run* (q) 
    (fresh (x y)
      (copyo q `(3))
      (copyo q `(3 4 5 6 7))
      (== q `(3 . ,y))))
  '(((3 . _.0) (copy (_.0 f.1)))))

(test "5h1"
  (run* (q) 
    (fresh (x y)
      (== q `(3 . ,y))
      (copyo q `(3))
      (copyo q `(3 4 5 6 7))))
  '(((3 . _.0) (copy (_.0 f.1)))))

(test "6a"  
  (run* (q)  ;; q has to be a pair by the time (== q 5) happens
    (fresh (x y)
      (copyo q `(1 2))
      (copyo q `(3 4 5 6 7))
      (== q 5)))
  '())

(test "6b"
  (run* (q)
    (fresh (x y)
      (copyo q `(3 4 5 6 7))
      (copyo q `(1 2))
      (== q 5)))
  '())

(test "6c"
  (run* (q)
    (fresh (x y)
      (copyo q `(3 4 5 6 7))
      (== q 5)
      (copyo q `(1 2))))
  '())

(test "6d"
  (run* (q)
    (fresh (x y)
      (== q 5)
      (copyo q `(3 4 5 6 7))
      (copyo q `(1 2))))
  '())

(test "6e"
  (run* (q)
    (fresh (x y)
      (== q 5)
      (copyo q `(1 2))
      (copyo q `(3 4 5 6 7))))
  '())

(test "7a"
  (run* (q)
    (fresh (x y)
      (copyo q `(1 2))
      (copyo q `(3 4))))
  ;; Interesting: in this case, _.0 must remain fresh, or can be
  ;; instantiated to a list of 2 fresh variables.
  ;;
  ;; Could reify as
  ;;
  ;; ((_.0 (copy (_.0 (_.1 _.2)))))
  ;;
  ;; old busted answer:
  ;;
  ;; ((_.0 (copy (_.0 (1 2)) (_.0 (3 4)))))
  '((_.0 (copy (_.0 (f.1 f.2))))))

(test "7b"
  (run* (q)
    (fresh (x y)
      (copyo q `(3 4))
      (copyo q `(1 2))))
  '((_.0 (copy (_.0 (f.1 f.2))))))

(test "8a"
  (run* (q)
    (fresh (x y z)
      (copyo x y)
      (copyo y z)
      (copyo z x)
      (== q `(,x ,y ,z))))
  ;; old busted answer:
  ;;
  ;; (((_.0 _.1 _.2) (copy (_.0 _.1) (_.1 _.2) (_.2 _.0))))  
  '((_.0 _.0 _.0)))

(test "8b"
  (run* (q)
    (fresh (x y z)
      (== q `(,x ,y ,z))
      (copyo x y)
      (copyo y z)
      (copyo z x)))
  '((_.0 _.0 _.0)))

(test "8c"
  (run* (q)
    (fresh (x y z)
      (copyo y z)
      (copyo x y)
      (copyo z x)
      (== q `(,x ,y ,z))))
  '((_.0 _.0 _.0)))

(test "8d"
  (run* (q)
    (fresh (x y z)
      (copyo y z)
      (copyo x y)
      (== q `(,x ,y ,z))
      (copyo z x)))
  '((_.0 _.0 _.0)))

(test "8e"
  (run* (q)
    (fresh (x y z)
      (copyo y z)
      (== q `(,x ,y ,z))
      (copyo z x)
      (copyo x y)))
  '((_.0 _.0 _.0)))

(test "8-2a"
  (run* (q)
    (fresh (x y)
      (copyo x y)
      (copyo y x)
      (== q `(,x ,y))))
  ;; old busted answer:
  ;;
  ;; (((_.0 _.1) (copy (_.0 _.1) (_.1 _.0))))
  '((_.0 _.0)))

(test "8-2b"
  (run* (q)
    (fresh (x y)
      (== q `(,x ,y))
      (copyo x y)
      (copyo y x)))
  '((_.0 _.0)))

(test "8-2c"
  (run* (q)
    (fresh (x y)
      (copyo x y)
      (== q `(,x ,y))
      (copyo y x)))
  '((_.0 _.0)))

(test "8-2d"
  (run* (q)
    (fresh (x y)
      (copyo y x)
      (== q `(,x ,y))
      (copyo x y)))
  '((_.0 _.0)))

(test "9a" (run* (q) (copyo `(,q) q)) '())
(test "9b" (run* (q x y) (copyo `(,x) y) (== x y)) '())
(test "9c" (run* (q x y) (== x y) (copyo `(,x) y)) '())

(test "10"
  (run* (q) (fresh (x) (copyo `(,x ,x) q)))
  '((_.0 _.0)))

(test "11"
  (run* (q) (fresh (x y) (copyo `((,x) (,x)) y) (== q `(,x ,y))))
  '(((_.0 ((_.1) (_.1))) (copy (_.0 _.1)))))

(test "12"
  (run 1 (q) (fresh (x y) (copyo `(lambda (,x) (,y ,x)) q)))
  '((lambda (_.0) (_.1 _.0))))

(test "13"
  (run* (q)
    (fresh (x y a b)
      (== x y)
      (copyo `(,x ,y) `(,a ,b))
      (== q `(,x ,y ,a ,b))))
  '(((_.0 _.0 _.1 _.1) (copy (_.0 _.1)))))

(test "14a"
  (run* (q)
    (fresh (x y a b)
      (copyo `(,x ,y) `(,a ,b))
      (== x y)
      (== q `(,x ,y ,a ,b))))
  ;; old busted answer
  ;; (((_.0 _.0 _.1 _.2) (copy (_.0 _.1) (_.0 _.2))))
  '(((_.0 _.0 _.1 _.1) (copy (_.0 _.1)))))

(test "14b"
  (run* (q)
    (fresh (x y a b)
      (== x y)
      (copyo `(,x ,y) `(,a ,b))
      (== q `(,x ,y ,a ,b))))
  '(((_.0 _.0 _.1 _.1) (copy (_.0 _.1)))))

(test "14c"
  (run* (q)
    (fresh (x y a b)
      (== x y)
      (== q `(,x ,y ,a ,b))
      (copyo `(,x ,y) `(,a ,b))))
  '(((_.0 _.0 _.1 _.1) (copy (_.0 _.1)))))

(test "14d"
  (run* (q)
    (fresh (x y a b)
      (== q `(,x ,y ,a ,b))
      (copyo `(,x ,y) `(,a ,b))
      (== x y)))
  '(((_.0 _.0 _.1 _.1) (copy (_.0 _.1)))))

(test "15"
  (run* (q)
    (fresh (t t^)
      (copyo `(,t ,t) `(,t ,t^))
      (== `(,t ,t^) q)))
  '((_.0 _.0)))

(test "17"
  (run* (x y) (copyo x y))
  '(((_.0 _.1) (copy (_.0 _.1))))) 

(test "18a"
  (run* (x y z) (=/= y z) (copyo `(,x ,x) `(,y ,z)))
  '()) 

(test "18b"
  (run* (x y z) (copyo `(,x ,x) `(,y ,z)) (=/= y z))
  '()) 

(test "19a"
  (run* (x y z t) (== `(,x ,x) t) (=/= y z) (copyo t `(,y ,z)))
  '()) 

(test "19b"
  (run* (x y z t) (copyo t `(,y ,z)) (== `(,x ,x) t) (=/= y z))
  '()) 

(test "19c"
  (run* (x y z t) (copyo t `(,y ,z)) (=/= y z) (== `(,x ,x) t))
  '()) 

(test "19d"
  (run* (x y z t) (copyo t `(,y ,z)) (== `(,x ,x) t) (=/= y z))
  '())

(test "20a"
  (run* (x y z t) (== `(,x ,x) t) (symbolo y) (numbero z) (copyo t `(,y ,z)))
  '())

(test "20b"
  (run* (x y z t) (copyo t `(,y ,z)) (== `(,x ,x) t) (symbolo y) (numbero z))
  '())

(test "20c"
  (run* (x y z t) (copyo t `(,y ,z)) (symbolo y) (numbero z) (== `(,x ,x) t))
  '())

(test "20d"
  (run* (x y z t) (symbolo y) (numbero z) (copyo t `(,y ,z)) (== `(,x ,x) t))
  '())

(test "20e"
  (run* (x y z t) (symbolo y) (copyo t `(,y ,z)) (== `(,x ,x) t) (numbero z))
  '())

(test "21a"
  (run* (x y z t) (== `(,x ,x) t) (absento y z) (copyo t `(,y ,z)))
  '())

(test "21b"
  (run* (x y z t) (copyo t `(,y ,z)) (== `(,x ,x) t) (absento y z))
  '())

(test "22a"
  (run* (x y z t) (== `(,x (,x)) t) (absento y z) (copyo t `(,y ,z)))
  '())

(test "22b"
  (run* (x y z t) (copyo t `(,y ,z)) (== `(,x (,x)) t) (absento y z))
  '())

(test "23a"
  (run* (x y z t) (== `(,x (,x)) t) (copyo t `(,y ,z)))
  ;; old busted answer:
  ;;
  ;; (((_.0 _.1 (_.1) (_.0 (_.0))) (copy ((_.0 (_.0)) (_.1 (_.1))))))
  '(((_.0 _.1 (_.1) (_.0 (_.0))) (copy (_.0 _.1)))))

(test "24a"
  (run* (x y z t1 t2) (=/= y z) (== `(,x ,x) t1) (copyo t1 t2) (copyo t2 `(,y ,z)))
  '())

(test "24a1"
  (run* (x y z t) (copyo t `(,y ,z)) (== `(,x (,x)) t) (=/= y z))
  '(((_.0 _.1 (_.1) (_.0 (_.0))) (copy (_.0 _.1)))))

(test "24a2"
  (run* (x y z) (copyo `(,x (,x)) `(,y ,z)) (=/= y z))
  '(((_.0 _.1 (_.1)) (copy (_.0 _.1)))))

(test "24a3"
  (run* (w x y z) (copyo `(,w (,x)) `(,y ,z)) (=/= y z))
  '(((_.0 _.1 _.2 (_.3)) (=/= ((_.2 (_.3)))) (copy (_.0 _.2) (_.1 _.3)))))

(test "24b"
  (run* (x y z t1 t2) (=/= y z) (== `(,x ,x) t1) (copyo t2 `(,y ,z)) (copyo t1 t2))
  '())

(test "24c"
  (run* (x y z t1 t2) (copyo t2 `(,y ,z)) (copyo t1 t2) (=/= y z) (== `(,x ,x) t1))
  '())

(test "24d"
  (run* (x y z t1 t2) (copyo t2 `(,y ,z)) (copyo t1 t2) (== `(,x ,x) t1) (=/= y z))
  '())

(test "24e"
  (run* (x y z t1 t2) (copyo t2 `(,y ,z)) (copyo t1 t2) (=/= y z) (== `(,x ,x) t1))
  '())

(test "24f"
  (run* (x y z t1 t2) (copyo t1 t2) (copyo t2 `(,y ,z)) (=/= y z) (== `(,x ,x) t1))
  '())

(test "24g"
  (run* (x y z t1 t2) (copyo t1 t2) (=/= y z) (copyo t2 `(,y ,z)) (== `(,x ,x) t1))
  '())

(test "25a" 
  (run* (x y z t1 t2 t3) (=/= y z) (== `(,x ,x) t1) (copyo t1 t2) (copyo t2 t3) (copyo t3 `(,y ,z)))
  '())

(test "25b"
  (run* (x y z t1 t2 t3) (copyo t1 t2) (copyo t2 t3) (copyo t3 `(,y ,z)) (=/= y z) (== `(,x ,x) t1))
  '())

(test "25c"
  (run* (x y z t1 t2 t3) (copyo t3 `(,y ,z)) (copyo t1 t2) (copyo t2 t3) (=/= y z) (== `(,x ,x) t1))
  '())

(test "25d"
  (run* (x y z t1 t2 t3) (copyo t3 `(,y ,z)) (copyo t2 t3) (copyo t1 t2) (=/= y z) (== `(,x ,x) t1))
  '())

(test "25e"
  (run* (x y z t1 t2 t3) (copyo t3 `(,y ,z)) (copyo t2 t3) (copyo t1 t2) (== `(,x ,x) t1) (=/= y z))
  '())

(test "26a"
  (run* (q)
    (fresh (x y)
      (== q 5)
      (copyo q `(1 2))
      (copyo q `(3 4))))
  '())

(test "26b"
  (run* (q)
    (fresh (x y)
      (copyo q `(1 2))
      (== q 5)
      (copyo q `(3 4))))
  '())

(test "26c"
  (run* (q)
    (fresh (x y)
      (copyo q `(1 2))
      (copyo q `(3 4))
      (== q 5)))
  '())

(test "26a"
  (run* (q)
    (fresh (x y z)
      (== x 5)
      (copyo x y)
      (copyo y z)
      (copyo z x)
      (== q `(,x ,y ,z))))
  '((5 5 5)))

(test "26b"
  (run* (q)
    (fresh (x y z)
      (copyo x y)
      (copyo y z)
      (copyo z x)
      (== q `(,x ,y ,z))
      (== x 5)))
  '((5 5 5)))

(test "26c"
  (run* (q)
    (fresh (x y z)
      (copyo z x)
      (copyo y z)
      (copyo x y)
      (== q `(,x ,y ,z))
      (== x 5)))
  '((5 5 5)))

(test "26d"
  (run* (q)
    (fresh (x y z)
      (copyo z x)
      (copyo y z)
      (copyo x y)
      (== x 5)
      (== q `(,x ,y ,z))))
  '((5 5 5)))

(test "27a"
  (run* (q)
    (fresh (x y z)
      (== x 5)
      (== y 6)
      (copyo x y)
      (copyo y z)
      (copyo z x)
      (== q `(,x ,y ,z))))
  '())

(test "27b"
  (run* (q)
    (fresh (x y z)
      (copyo x y)
      (copyo y z)
      (copyo z x)
      (== q `(,x ,y ,z))
      (== x 5)
      (== y 6)))
  '())

(test "27c"
  (run* (q)
    (fresh (x y z)
      (copyo x y)
      (copyo y z)
      (copyo z x)
      (== q `(,x ,y ,z))
      (== y 6)
      (== x 5)))
  '())

(test "27d"
  (run* (q)
    (fresh (x y z)
      (== q `(,x ,y ,z))
      (copyo x y)
      (copyo y z)
      (copyo z x)
      (== y 6)
      (== x 5)))
  '())

(test "27e"
  (run* (q)
    (fresh (x y z)
      (copyo z x)
      (copyo y z)
      (copyo x y)
      (== q `(,x ,y ,z))
      (== y 6)
      (== x 5)))
  '())

(test "28a"
  (run* (a b c d)
    (copyo a b)
    (copyo c d))
  '(((_.0 _.1 _.2 _.3) (copy (_.0 _.1) (_.2 _.3)))))

(test "28b"
  (run* (a b c d)
    (copyo c d)
    (copyo a b))
  '(((_.0 _.1 _.2 _.3) (copy (_.0 _.1) (_.2 _.3)))))



(test "30a"
  (run* (q)
    (fresh (g g^ t t^)
      (== `(,t) g^)
      (copyo `((,t) ,t) `(,g^ ,t^))
      (== `(,t ,t^) q)))
  '((_.0 _.0)))

(test "30b"
  (run* (q)
    (fresh (g g^ t t^)
      (== `(,t) g^)
      (== `(,t ,t^) q)
      (copyo `((,t) ,t) `(,g^ ,t^))))
  '((_.0 _.0)))

(test "30c"
  (run* (q)
    (fresh (g g^ t t^)
      (copyo `((,t) ,t) `(,g^ ,t^))
      (== `(,t) g^)
      (== `(,t ,t^) q)))
  '((_.0 _.0)))

(test "31a"
  (run* (q)
    (fresh (g g^ t t^)
      (== `(,t) g)
      (== g g^)
      (== `(,t ,t^) q)
      (copyo `(,g ,t) `(,g^ ,t^))))
  '((_.0 _.0)))

(test "31b"
  (run* (q)
    (fresh (g g^ t t^)
      (copyo `(,g ,t) `(,g^ ,t^))
      (== `(,t) g)
      (== g g^)
      (== `(,t ,t^) q)))
  '((_.0 _.0)))

(test "32a"
  (run* (q)
    (fresh (g t t^)
      (== `(,t) g)
      (copyo `(,g ,t) `(,g ,t^))
      (== `(,t ,t^) q)))
  '((_.0 _.0)))

(test "32b"
  (run* (q)
    (fresh (g t t^)
      (== `(,t) g)
      (== `(,t ,t^) q)
      (copyo `(,g ,t) `(,g ,t^))))
  '((_.0 _.0)))

(test "33a"
  (run* (q)
    (fresh (g g^ t t^ t1 t2)
      (== g g^)
      (== `(-> ,t1 ,t2) t)
      (== `((,t1)) g)
      (== `(,t ,t^) q)
      (copyo `(,g ,t) `(,g^ ,t^))))
  ;; old busted answer
  ;; ((((-> _.0 _.1) (-> _.0 _.2)) (copy ((((_.0)) (-> _.0 _.1)) (((_.0)) (-> _.0 _.2))))))  
  '((((-> _.0 _.1) (-> _.0 _.2)) (copy (_.1 _.2)))))

(test "33b"
  (run* (q)
    (fresh (g g^ t t^ t1 t2)
      (copyo `(,g ,t) `(,g^ ,t^))
      (== g g^)
      (== `(-> ,t1 ,t2) t)
      (== `((,t1)) g)
      (== `(,t ,t^) q)))
  '((((-> _.0 _.1) (-> _.0 _.2)) (copy (_.1 _.2)))))

(test "34a"
  (run* (q)
    (fresh (s t t^)
      (== `(,t) s)
      (== `(,t ,t^) q)
      (copyo `(,s ,t) `(,s ,t^))))
  '((_.0 _.0)))

(test "34b"
  (run* (q)
    (fresh (s t t^)
      (copyo `(,s ,t) `(,s ,t^))
      (== `(,t) s)
      (== `(,t ,t^) q)))
  '((_.0 _.0)))

(test "35a"
  (run* (q)
    (fresh (x g g^ t t^ t1 t2)
      (== g g^)
      (== `(-> ,t1 ,t2) t)
      (== `((x ,t1)) g)
      (== `(,x ,t ,t1 ,t2 ,t^ ,g ,g^) q)
      (copyo `(,g ,t) `(,g^ ,t^))))
  '(((_.0 (-> _.1 _.2) _.1 _.2 (-> _.1 _.3) ((x _.1)) ((x _.1))) (copy (_.2 _.3))))) 

(test "35b"
  (run* (q)
    (fresh (x g g^ t t^ t1 t2)
      (copyo `(,g ,t) `(,g^ ,t^))
      (== g g^)
      (== `(-> ,t1 ,t2) t)
      (== `((x ,t1)) g)
      (== `(,x ,t ,t1 ,t2 ,t^ ,g ,g^) q)))
  '(((_.0 (-> _.1 _.2) _.1 _.2 (-> _.1 _.3) ((x _.1)) ((x _.1))) (copy (_.2 _.3))))) 

(test "36a"
  (run* (q)
    (fresh (x y)
      (copyo q `(1 2 8))
      (copyo q `(3 4 5 6 7))
      (== q `(,x . ,y))))
  ;; Interesting: in this case, _.0 must remain fresh.
  ;; _.1 can become a (z0 z1 . z2)
  ;;
  ;; Could reify as
  ;;
  ;; (((_.0 . _.1) (copy (_.0 _.2) (_.1 (_.3 _.4 . _.5)))))
  ;;
  ;; old busted answer
  ;;
  ;; (((_.0 . _.1) (copy (_.0 1) (_.0 3) (_.1 (2 8)) (_.1 (4 5 6 7)))))
  '(((_.0 . _.1) (copy (_.0 f.2) (_.1 (f.3 f.4 . f.5))))))

(test "37a"
  (run* (q)
    (copyo q `(1 4))
    (copyo q `(3 4)))
  ;; old busted answer:
  ;;
  ;; ((_.0 (copy (_.0 (1 4)) (_.0 (3 4)))))
  '((_.0 (copy (_.0 (f.1 4))))))

(test "37b"
  (run* (q)
    (copyo q `(3 4))
    (copyo q `(1 4)))
  ;; old busted answer:
  ;;
  ;; ((_.0 (copy (_.0 (1 4)) (_.0 (3 4)))))
  '((_.0 (copy (_.0 (f.1 4))))))


#!eof

;; Interaction of eigen and copyo seems subtle, and I'm sure I don't have it right.
;;
;; Should the eigens in the template be replaced with new eigens?  Or kept as-is?
;;
;; When replacing fresh logic vars with new fresh vars, should the new
;; vars have the same scope as the vars?  I guess.  Need to make sure
;; this is happening (I suspect it isn't)
(test "eigen-1a"
  (run* (q) (eigen (a b) (fresh (x y z t) (== `(,x ,x) t) (== a y) (== b z))))
  '(_.0))

(test "eigen-2a"
  (run* (q) (eigen (a b) (fresh (x y z t) (== `(,x ,x) t) (== a y) (== b z) (== `(,y ,z) t))))
  '())

(test "eigen-23a"
  (run* (q) (eigen (a b) (fresh (x y z t) (== `(,x ,x) t) (== a y) (== b z) (copyo t `(,y ,z)))))
  '())

(test "eigen-4b"
  (run* (q) (eigen (a b) (fresh (x y z t) (copyo t `(,y ,z)) (== `(,x ,x) t) (== a y) (== b z))))
  '())

(test "eigen-5a"
  (run* (q) (eigen (a b) (fresh (x y z t) (copyo t `(,y ,z)) (== a y) (== b z))))
 ;;; should succeed with (_.0) and some constraint
  '???)
