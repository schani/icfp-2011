(ns fikdm.compiler.core
  (:use matchure
	clojure.contrib.def))

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

(defn- expand-if-lets [bindings consequent alternative]
  (if (empty? bindings)
    consequent
    (let [[n v & rest] bindings]
      `(if-let [~n ~v]
	 ~(expand-if-lets rest consequent alternative)
	 ~alternative))))

(defmacro- if-lets [bindings consequent alternative]
  (let [alt (gensym 'alt)]
    `(let [~alt (fn [] ~alternative)]
       ~(expand-if-lets bindings consequent (list alt)))))

(defn- other-slot [s]
  (- 3 s))

(defn- primitive-card? [x]
  (cond (symbol? x)
	x
	(= x 0)
	'zero))

(defn- gen-primitive? [x s]
  (if-let [card (primitive-card? x)]
    (if (= card 'I)
      [[:left s 'put]]
      [[:left s 'put]
       [:right s card]])))

(defn- gen-simple? [x s]
  (if-let [primitive (gen-primitive? x s)]
    primitive
    (if-match [[?l ?r] x]
	      (if-lets [l-card (primitive-card? l)
			r-gen (gen-simple? r s)]
		       (concat r-gen
			       [[:left s l-card]])
		       (if-lets [r-card (primitive-card? r)
				 l-gen (gen-simple? l s)]
				(concat l-gen
					[[:right s r-card]])
				nil)))))

(defn generate [ski s]
  (cond-match ski

	      [?x ?y]
	      (if-let [simple (gen-simple? ski s)]
		simple
		(let [os (other-slot s)]
		  (concat
		   (generate x s)
		   (generate y os)
		   (generate os 0)
		   [[:left 0 'get]
		    [:left s 'K]
		    [:left s 'S]
		    [:right s 'get]
		    [:right s 'zero]])))

	      (and (number? ?) (zero? ?))
	      [[:left s 'put]
	       [:right s 'zero]]

	      (number? ?)
	      (concat
	       (generate (dec ski) s)
	       [[:left s 'succ]])

	      ?x
	      (if (= x 'I)
		[[:left s 'put]]
		[[:left s 'put]
		 [:right s x]])))
