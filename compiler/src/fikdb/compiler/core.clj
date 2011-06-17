(ns fikdm.compiler.core
  (:use matchure))

(defn compile-a [x expr]
  (cond-match expr

	      [?M ?N]
	      (list (list 'S (compile-a x M)) (compile-a x N))

	      ?y
	      (if (= x y)
		'I
		(list 'K y))))

(defn compile-lambda [expr]
  (cond-match expr

	      [?lambda [?x] ?M]
	      (if (= lambda 'fn)
		(compile-a x (compile-lambda M))
		(throw (Exception. (str "Not a valid expression: " expr))))

	      [?M ?N]
	      (list (compile-lambda M) (compile-lambda N))

	      ?x
	      x))

(defn- other-slot [s]
  (- 3 s))

(defn generate [ski s]
  (cond-match ski

	      [?x ?y]
	      (let [os (other-slot s)]
		(concat
		 (generate x s)
		 (generate y os)
		 (generate os 0)
		 [[:left 0 'get]
		  [:left s 'K]
		  [:left s 'S]
		  [:right s 'get]
		  [:right s 'zero]]))

	      (and (number? ?) (zero? ?))
	      [[:left s 'put]
	       [:right s 'zero]]

	      (number? ?)
	      (concat
	       (generate (dec ski) s)
	       [[:left s 'succ]])

	      ?x
	      [[:left s 'put]
	       [:right s x]]))
