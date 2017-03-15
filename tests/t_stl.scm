(include "lib/core.scm")

(cverbatim "
#include <list>
std::list<int> my_list;
")

(define (push-back n)
  (%%cexp (int -> undefined) "my_list.push_back(%0)" n))

(define (size)
  (%%cexp (-> int) "my_list.size()"))

(define (pop-front)
  (%%cexp (-> undefined) "my_list.pop_front()") #u)

(define (front)
  (%%cexp (-> int) "my_list.front()"))

(push-back 10)
(push-back 20)
(printn (size))
(printn (front))
(printn (pop-front))
(printn (front))
(printn (pop-front))
