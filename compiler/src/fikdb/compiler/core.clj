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

(defn- generate-mn [s x-code y-code m-card n-card]
  (concat
   x-code
   y-code
   [[:left s 'K]
    [:left s 'S]
    [:right s m-card]
    [:right s n-card]]))

(declare generate)

(defn- generate-complex [s x-code y]
  (let [os (other-slot s)]
    (generate-mn s x-code
		 (concat (generate y os)
			 (gen-primitive? 0 os)
			 [[:left 0 'get]])
		 'get 'zero)))

(defn generate [ski s]
  (if-let [simple (gen-simple? ski s)]
    simple
    (cond-match ski

		[?x [?M ?N]]
		(let [x-code (generate x s)]
		  (if-lets [m-card (primitive-card? M)
			    n-card (primitive-card? N)]
			   (generate-mn s x-code [] m-card n-card)
			   (if-let [y-simple (gen-simple? [M N] 0)]
			     (generate-mn s x-code y-simple 'get 'zero)
			     (generate-complex s x-code [M N]))))

		[?x ?y]
		(generate-complex s (generate x s) y)

		(number? ?)
		(concat
		 (generate (dec ski) s)
		 [[:left s 'succ]])

		?x
		(throw (Exception. (str "Malformed SKI " x))))))

;; endless loop
;(compile-lambda '(((S I) I)
;		  (fn [f]
;		    ((fn [y]
;		       (((S I) I) f))
;		     :boes))))
