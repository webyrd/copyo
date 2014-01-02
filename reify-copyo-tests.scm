(load "mk.scm")
(load "test-check.scm")

(test "rv-1"
  (let ((x (var 'x))
        (y (var 'y)))
    (replace-vars `(5 (,x) #t (,y (,x)) ,y)))
  '(5 (#(z_0)) #t (#(z_1) (#(z_0))) #(z_1)))

(test "0" (run* (q) (copyo 5 5)) '(_.0))
(test "1" (run* (q) (copyo q q)) '(_.0))
(test "2" (run* (q) (copyo 5 q)) '(5))

(test "a"
  (run* (x y) (copyo x y))
  '(((_.0 _.1) (copy (_.0 _.1)))))

(test "b"
  (run* (x y z)
    (copyo x y)
    (copyo x z))
  ;; is there a more succinct way to express the reified constraint?
  ;; I'm not sure there is.
  '(((_.0 _.1 _.2) (copy (_.0 _.1) (_.0 _.2)))))

(test "c"
  (run* (x y z a1 d1 a2 d2)
    (== `(,a1 . ,d1) y)
    (== `(,a2 . ,d2) z)
    (copyo x y)
    (copyo x z))
  ;; is there a more succinct way to express the reified constraint?
  ;; I'm not sure there is.
  '(((_.0 (_.1 . _.2) (_.3 . _.4) _.1 _.2 _.3 _.4)
     (copy (_.0 (_.1 . _.2)) (_.0 (_.3 . _.4))))))

(test "d-b"
  (run* (x y) (copyo x y) (== x 5))
  '((5 5)))

(test "d-a"
  (run* (x y) (== x 5) (copyo x y))
  '((5 5)))

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
  '((_.0 (copy (_.0 f.1)))))

(test "4a" 
  (run* (q) 
    (fresh (x y)
      (copyo q 5)
      (== `(,x ,y) q)))
  '())

(test "5b"
  (run* (q)
    (fresh (x y)
      (== q `(,x . ,y))
      (copyo q `(1 2))
      (copyo q `(3 4 5 6 7))))
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

(test "5a"
  (run* (q)
    (fresh (x y)
      (copyo q `(1 2))
      (copyo q `(3 4 5 6 7))
      (== q `(,x . ,y))))
  '(((_.0 . _.1) (copy (_.0 f.2) (_.1 (f.3 . f.4))))))

(test "6a"
  (run* (q)
    (fresh (x y)
      (copyo q `(1 2))
      (copyo q `(3 4 5 6 7))
      (== q 5)))
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

(test "9a" (run* (q) (copyo `(,q) q)) '())

(test "10"
  (run* (q) (fresh (x) (copyo `(,x ,x) q)))
  '((_.0 _.0)))

(test "11*"
  (run* (q) (fresh (x y) (copyo `((,x) (,x)) y) (== q `(,x ,y))))
  '(((_.0 ((_.1) (_.1))) (copy (_.0 _.1)))))

(test "11-easier-to-read*"
  (run* (q) (fresh (t x y) (== `((,x) (,x)) t) (copyo t y) (== q `(,t ,y))))
  '(((((_.0) (_.0)) ((_.1) (_.1))) (copy (_.0 _.1)))))

(test "12"
  (run 1 (q) (fresh (x y) (copyo `(lambda (,x) (,y ,x)) q)))
  '((lambda (_.0) (_.1 _.0))))

(test "13*"
  (run* (q)
    (fresh (x y a b)
      (== x y)
      (copyo `(,x ,y) `(,a ,b))
      (== q `(,x ,y ,a ,b))))
  '(((_.0 _.0 _.1 _.1) (copy (_.0 _.1)))))

(test "14a*"
  (run* (q)
    (fresh (x y a b)
      (copyo `(,x ,y) `(,a ,b))
      (== x y)
      (== q `(,x ,y ,a ,b))))
  ;; old, busted answer:
  ;; (((_.0 _.0 _.1 _.2) (copy (_.0 _.1) (_.0 _.2))))
  '(((_.0 _.0 _.1 _.1) (copy (_.0 _.1)))))

(test "15"
  (run* (q)
    (fresh (x g g^ t t^)
      (copyo `(,t ,t) `(,t ,t^))
      (== `(,t ,t^) q)))
  '((_.0 _.0)))

(test "17"
  (run* (x y) (copyo x y))
  '(((_.0 _.1) (copy (_.0 _.1))))) 

(test "18a"
  (run* (x y z) (=/= y z) (copyo `(,x ,x) `(,y ,z)))
  '()) 
 
(test "19a"
  (run* (x y z t) (== `(,x ,x) t) (=/= y z) (copyo t `(,y ,z)))
  '()) 

(test "20a"
  (run* (x y z t) (== `(,x ,x) t) (symbolo y) (numbero z) (copyo t `(,y ,z)))
  '())

(test "21a"
  (run* (x y z t) (== `(,x ,x) t) (absento y z) (copyo t `(,y ,z)))
  '())

(test "22a"
  (run* (x y z t) (== `(,x (,x)) t) (absento y z) (copyo t `(,y ,z)))
  '())

(test "23a*"
  (run* (x y z t) (== `(,x (,x)) t) (copyo t `(,y ,z)))
  '(((_.0 _.1 (_.1) (_.0 (_.0))) (copy (_.0 _.1)))))

(test "24a"
  (run* (x y z t1 t2) (=/= y z) (== `(,x ,x) t1) (copyo t1 t2) (copyo t2 `(,y ,z)))
  '())

(test "25a"
  (run* (x y z t1 t2 t3) (=/= y z) (== `(,x ,x) t1) (copyo t1 t2) (copyo t2 t3) (copyo t3 `(,y ,z)))
  '())

(test "26a"
  (run* (q)
    (fresh (x y)
      (== q 5)
      (copyo q `(1 2))
      (copyo q `(3 4))))
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

(test "28a"
  (run* (a b c d)
    (copyo a b)
    (copyo c d))
  '(((_.0 _.1 _.2 _.3) (copy (_.0 _.1) (_.2 _.3)))))

(test "30a"
  (run* (q)
    (fresh (g g^ t t^)
      (== `(,t) g^)
      (copyo `((,t) ,t) `(,g^ ,t^))
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

(test "32a"
  (run* (q)
    (fresh (g t t^)
      (== `(,t) g)
      (copyo `(,g ,t) `(,g ,t^))
      (== `(,t ,t^) q)))
  '((_.0 _.0)))

(test "33a*"
  (run* (q)
    (fresh (g g^ t t^ t1 t2)
      (== g g^)
      (== `(-> ,t1 ,t2) t)
      (== `((,t1)) g)
      (== `(,t ,t^) q)
      (copyo `(,g ,t) `(,g^ ,t^))))
  '((((-> _.0 _.1) (-> _.0 _.2)) (copy (_.1 _.2)))))

(test "34a"
  (run* (q)
    (fresh (s t t^)
      (== `(,t) s)
      (== `(,t ,t^) q)
      (copyo `(,s ,t) `(,s ,t^))))
  '((_.0 _.0)))

(test "35a*"
  (run* (q)
    (fresh (x g g^ t t^ t1 t2)
      (== g g^)
      (== `(-> ,t1 ,t2) t)
      (== `((x ,t1)) g)
      (== `(,x ,t ,t1 ,t2 ,t^ ,g ,g^) q)
      (copyo `(,g ,t) `(,g^ ,t^))))
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

(test "38a"
  (run* (x y z)
    (copyo x `(,y 3 5))
    (copyo x `(,z 4 5)))
  ;; can we do better than this answer?
  ;; maybe not
  ;;
  ;; *** important point ***
  ;;
  ;; It is tempting tp produce an answer like:
  ;;
  ;; ((((_.0 . _.1) _.2 _.3) (copy (_.0 _.2) (_.0 _.3) (_.1 (_.4 5)))))
  ;;
  ;; However, this would force x to be a pair, which isn't right:
  ;; x can remain a fresh variable, and remain at least as general as
  ;; either copy of x.
  ;;
  ;; Seems like the right-hand-sides need to be ground before
  ;; using the lgg of those sides is legitimate
  '(((_.0 _.1 _.2) (copy (_.0 (_.1 3 5)) (_.0 (_.2 4 5))))))

(test "39a"
  (run* (x y)
    (copyo x `(,y 3 5))
    (copyo x `(,y 4 5)))
  ;; old busted answer:
  ;;
  ;; (((_.0 _.1) (copy (_.0 (_.1 3 5)) (_.0 (_.1 4 5)))))
  '(((_.0 _.1) (copy (_.0 (_.1 f.2 5))))))

(test "39b"
  (run* (x y)
    (copyo x `(,y 4 5))
    (copyo x `(,y 3 5)))
  ;; old busted answer:
  ;;
  ;; (((_.0 _.1) (copy (_.0 (_.1 3 5)) (_.0 (_.1 4 5)))))
  '(((_.0 _.1) (copy (_.0 (_.1 f.2 5))))))

(test "39-2a"
  (run* (x y z)
    (copyo x `(,y 3 5 ,z))
    (copyo x `(,y 4 5 ,z)))
  ;; old busted answer:
  ;;
  ;; (((_.0 _.1 _.2) (copy (_.0 (_.1 3 5 _.2)) (_.0 (_.1 4 5 _.2)))))
  '(((_.0 _.1 _.2) (copy (_.0 (_.1 f.3 5 _.2))))))

(test "39-2b"
  (run* (x y z)
    (copyo x `(,y 4 5 ,z))
    (copyo x `(,y 3 5 ,z)))
  ;; old busted answer:
  ;;
  ;; (((_.0 _.1 _.2) (copy (_.0 (_.1 3 5 _.2)) (_.0 (_.1 4 5 _.2)))))
  '(((_.0 _.1 _.2) (copy (_.0 (_.1 f.3 5 _.2))))))

(test "39-3a"
  (run* (x y z)
    (copyo x `(,y 3 5 ,z))
    (copyo x `(,z 4 5 ,y)))
  '(((_.0 _.1 _.2) (copy (_.0 (_.1 3 5 _.2)) (_.0 (_.2 4 5 _.1))))))

(test "39-4a"
  (run* (x y z)
    (== y 7)
    (copyo x `(,y 3 5 ,z))
    (copyo x `(,z 4 5 ,y)))
  '(((_.0 7 _.1) (copy (_.0 (7 3 5 _.1)) (_.0 (_.1 4 5 7))))))

(test "39-5a"
  (run* (x y z)
    (== y 7)
    (== z 8)
    (copyo x `(,y 3 5 ,z))
    (copyo x `(,z 4 5 ,y)))
  '(((_.0 7 8) (copy (_.0 (f.1 f.2 5 f.3))))))

(test "39-5b"
  (run* (x y z)
    (copyo x `(,y 3 5 ,z))
    (copyo x `(,z 4 5 ,y))
    (== y 7)
    (== z 8))
  '(((_.0 7 8) (copy (_.0 (f.1 f.2 5 f.3))))))

(test "39-6a"
  (run* (x)
    (copyo x `(7 3 5 8))
    (copyo x `(8 4 5 7)))
  '((_.0 (copy (_.0 (f.1 f.2 5 f.3))))))

(test "40a"
  (run* (x)
    (copyo x `(3 5))
    (copyo x `(4 5)))
  ;; old busted answer:
  ;;
  ;; ((_.0 (copy (_.0 (3 5)) (_.0 (4 5)))))  
  '((_.0 (copy (_.0 (f.1 5))))))

(test "41a"
  (run* (x y)
    (copyo x 3)
    (copyo x y))
  ;; *** important point ***
  ;; can we produce a better answer?
  ;;
  ;; maybe not.  Can't produce  
  ;;
  ;; (((_.0 _.1) (copy (_.0 3))))
  ;;
  ;; since this would mean that,even if _.0 becomes instantiated to 3,
  ;; _.1 can be anything, which would be incorrect
  '(((_.0 _.1) (copy (_.0 3) (_.0 _.1)))))

(test "42a"
  (run* (x y z)
    (copyo x `(,y ,z))
    (copyo x `(,z ,y)))
  ;; can we do better with answer?
  ;;
  ;; I don't think so
  '(((_.0 _.1 _.2) (copy (_.0 (_.1 _.2)) (_.0 (_.2 _.1))))))

(test "43a"
  (run* (x y z)
    (== 5 y)
    (copyo x `(,y ,z))
    (copyo x `(,z ,y)))
  ;; can we do better with answer?
  ;;
  ;; I don't think so
  '(((_.0 5 _.1) (copy (_.0 (5 _.1)) (_.0 (_.1 5))))))

(test "44a"
  (run* (x y z)
    (== 5 y)
    (== 5 x)
    (copyo x `(,y ,z))
    (copyo x `(,z ,y)))
  '())

(test "44a"
  (run* (x a d y z)
    (== 5 y)
    (== `(,a . ,d) x)
    (copyo x `(,y ,z))
    (copyo x `(,z ,y)))
  '((((_.0 . _.1) _.0 _.1 5 _.2)
     (copy (_.0 5) (_.0 _.2) (_.1 (5)) (_.1 (_.2))))))

(test "45a"
  (run* (x a d y z)
    (== 5 y)
    (== `(5 . ,d) x)
    (copyo x `(,y ,z))
    (copyo x `(,z ,y)))
  '((((5 . _.0) _.1 _.0 5 5) (copy (_.0 (5))))))

(test "45b"
  (run* (x a d y z)
    (== 5 y)
    (== `(5 . ,d) x)
    (copyo x `(,z ,y))
    (copyo x `(,y ,z)))
  '((((5 . _.0) _.1 _.0 5 5) (copy (_.0 (5))))))

(test "45c"
  (run* (x a d y z)
    (== 5 y)
    (copyo x `(,z ,y))
    (copyo x `(,y ,z))
    (== `(5 . ,d) x))
  '((((5 . _.0) _.1 _.0 5 5) (copy (_.0 (5))))))

(test "45d"
  (run* (x a d y z)
    (copyo x `(,z ,y))
    (copyo x `(,y ,z))
    (== 5 y)
    (== `(5 . ,d) x))
  '((((5 . _.0) _.1 _.0 5 5) (copy (_.0 (5))))))

(test "46a"
  (run* (x a b y z)
    (== 5 y)
    (== `(,a ,b) x)
    (copyo x `(,y ,z))
    (copyo x `(,z ,y)))
  '((((_.0 _.1) _.0 _.1 5 _.2)
     (copy (_.0 5) (_.0 _.2) (_.1 5) (_.1 _.2)))))

(test "47a"
  (run* (x a b y z)
    (== 5 y)
    (== `(5 ,b) x)
    (copyo x `(,y ,z))
    (copyo x `(,z ,y)))
  '((((5 _.0) _.1 _.0 5 5) (copy (_.0 5)))))

(test "48a"
  (run* (x a b y z)
    (== 5 y)
    (== `(,a 5) x)
    (copyo x `(,y ,z))
    (copyo x `(,z ,y)))
  '((((_.0 5) _.0 _.1 5 5) (copy (_.0 5)))))

(test "49a"
  (run* (x y z)
    (== 5 y)
    (== 6 z)
    (copyo x `(,y ,z))
    (copyo x `(,z ,y)))
  '(((_.0 5 6) (copy (_.0 (f.1 f.2))))))

(test "50a"
  (run* (x y z)
    (== `(5) x)
    (copyo x `(,y . ,z))
    (copyo x `(,z . ,y)))
  '())

(test "51a"
  (run* (x a y z)
    (== `(,a 5) x)
    (copyo x `(,y ,z))
    (copyo x `(,z ,y)))
  '((((_.0 5) _.0 5 5) (copy (_.0 5)))))

(test "51b"
  (run* (x a y z)
    (== `(,a 5) x)
    (copyo x `(,z ,y))
    (copyo x `(,y ,z)))
  '((((_.0 5) _.0 5 5) (copy (_.0 5)))))

(test "51c"
  (run* (x a y z)
    (copyo x `(,z ,y))
    (copyo x `(,y ,z))
    (== `(,a 5) x))
  '((((_.0 5) _.0 5 5) (copy (_.0 5)))))

(test "51d"
  (run* (x a y z)
    (copyo x `(,y ,z))
    (copyo x `(,z ,y))
    (== `(,a 5) x))
  '((((_.0 5) _.0 5 5) (copy (_.0 5)))))

(test "51e"
  (run* (x a y z)
    (copyo x `(,y ,z))
    (== `(,a 5) x)
    (copyo x `(,z ,y)))
  '((((_.0 5) _.0 5 5) (copy (_.0 5)))))

(test "52a"
  (run* (x y z)
    (== `(5 5) x)
    (copyo x `(,y ,z))
    (copyo x `(,z ,y)))
  '(((5 5) 5 5)))

(test "52b"
  (run* (x y z)
    (copyo x `(,y ,z))
    (== `(5 5) x)
    (copyo x `(,z ,y)))
  '(((5 5) 5 5)))

(test "52c"
  (run* (x y z)
    (copyo x `(,y ,z))
    (copyo x `(,z ,y))
    (== `(5 5) x))
  '(((5 5) 5 5)))

(test "52d"
  (run* (x y z)
    (copyo x `(,z ,y))
    (copyo x `(,y ,z))
    (== `(5 5) x))
  '(((5 5) 5 5)))

(test "53a"
  (run* (x a y z)
    (== `(,a 5) x)
    (copyo x `(,y ,z)))
  '((((_.0 5) _.0 _.1 5) (copy (_.0 _.1)))))

(test "53b"
  (run* (x a y z)
    (copyo x `(,y ,z))
    (== `(,a 5) x))
  '((((_.0 5) _.0 _.1 5) (copy (_.0 _.1)))))

(test "54a"
  (run* (b a y z)
    (== `(,a 5) b)
    (copyo b `(,y ,z)))
  '((((_.0 5) _.0 _.1 5) (copy (_.0 _.1)))))

(test "54b"
  (run* (b a y z)
    (copyo b `(,y ,z))
    (== `(,a 5) b))
  '((((_.0 5) _.0 _.1 5) (copy (_.0 _.1)))))

(test "55a"
  (run* (b y z)
    (== `(5 6) b)
    (copyo b `(,y ,z)))
  '(((5 6) 5 6)))

(test "55b"
  (run* (b y z)
    (copyo b `(,y ,z))
    (== `(5 6) b))
  '(((5 6) 5 6)))

(test "56a"
  (run* (a y z)
    (copyo `(,a 5) `(,y ,z))
    (copyo `(,a 5) `(,z ,y)))
  '(((_.0 5 5) (copy (_.0 5)))))

(test "56b"
  (run* (a y z)
    (copyo `(,a 5) `(,z ,y))
    (copyo `(,a 5) `(,y ,z)))
  '(((_.0 5 5) (copy (_.0 5)))))

(test "57a"
  (run* (a y z)
    (copyo a y)
    (copyo a z)
    (copyo 5 a))
  '((5 5 5)))

(test "57b"
  (run* (a y z)
    (copyo a y)
    (copyo 5 a)
    (copyo a z))
  '((5 5 5)))

(test "57c"
  (run* (a y z)
    (copyo 5 a)
    (copyo a y)
    (copyo a z))
  '((5 5 5)))

(test "58a"
  (run* (a y z)
    (copyo a y)
    (copyo a z)
    (== 5 a))
  '((5 5 5)))

(test "58b"
  (run* (a y z)
    (copyo a z)
    (copyo a y)
    (== 5 a))
  '((5 5 5)))

(test "58c"
  (run* (a y z)
    (== 5 a)
    (copyo a z)
    (copyo a y))
  '((5 5 5)))

(test "59a"
  (run* (a x y z)
    (copyo `(,a 5) `(,x ,y))
    (copyo `(,a 5) `(,y ,z))
    (copyo `(,a 5) `(,z ,x)))
  '(((_.0 5 5 5) (copy (_.0 5)))))

(test "59b"
  (run* (a x y z)
    (copyo `(,a 5) `(,x ,y))
    (copyo `(,a 5) `(,z ,x))
    (copyo `(,a 5) `(,y ,z)))
  '(((_.0 5 5 5) (copy (_.0 5)))))

(test "60a"
  (run* (x a b c d e f)
    (copyo `(,x 5) `(,a ,b))
    (copyo `(,x 5) `(,b ,c))
    (copyo `(,x 5) `(,c ,d))
    (copyo `(,x 5) `(,d ,e))
    (copyo `(,x 5) `(,e ,f))
    (copyo `(,x 5) `(,f ,a)))
  '(((_.0 5 5 5 5 5 5) (copy (_.0 5)))))

(test "61a"
  (run* (x a b c d e f)
    (copyo `(,x 5) `(,a ,b))
    (copyo `(,x 5) `(,b ,c))
    (copyo `(,x 5) `(,c ,d))
    (copyo `(,x 5) `(,d ,e))
    (copyo `(,x 5) `(,e ,f)))
  '(((_.0 _.1 5 5 5 5 5) (copy (_.0 5) (_.0 _.1)))))

(test "61b"
  (run* (x a b c d e f)
    (copyo `(,x 5) `(,e ,f))
    (copyo `(,x 5) `(,d ,e))
    (copyo `(,x 5) `(,a ,b))
    (copyo `(,x 5) `(,b ,c))
    (copyo `(,x 5) `(,c ,d)))
  '(((_.0 _.1 5 5 5 5 5) (copy (_.0 5) (_.0 _.1)))))
