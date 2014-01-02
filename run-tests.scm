(load "mk.scm")
(load "test-check.scm")

(test "run-1"
  (run 5 (q) (== q 5))
  '(5))

(test "run-3"
  (run 5 (a b c) (== 5 5))
  '((_.0 _.1 _.2)))

(test "run-4"
  (run 5 (a b c) (== a 5) (== b 6) (== c 6))
  '((5 6 6)))


(test "run*-1"
  (run* (q) (== q 5))
  '(5))

(test "run*-3"
  (run* (a b c) (== 5 5))
  '((_.0 _.1 _.2)))

(test "run*-4"
  (run* (a b c) (== a 5) (== b 6) (== c 6))
  '((5 6 6)))

(test "run*-5"
  (run* (a b c) (== b 6) (== c 6))
  '((_.0 6 6)))

(test "run*-6"
  (run* (a b c d e f) (symbolo a) (numbero b) (=/= c d) (absento e f))
  '(((_.0 _.1 _.2 _.3 _.4 _.5)
     (=/= ((_.2 _.3)))
     (num _.1)
     (sym _.0)
     (absento (_.4 _.5)))))

(test "run*-7"
  (run* (a b c d e f)
    (conde
      [(symbolo a)]
      [(numbero b)]
      [(=/= c d)]
      [(absento e f)]))
  '(((_.0 _.1 _.2 _.3 _.4 _.5) (sym _.0))
    ((_.0 _.1 _.2 _.3 _.4 _.5) (num _.1))
    ((_.0 _.1 _.2 _.3 _.4 _.5) (=/= ((_.2 _.3))))
    ((_.0 _.1 _.2 _.3 _.4 _.5) (absento (_.4 _.5)))))
