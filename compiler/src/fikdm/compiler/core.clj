(ns fikdm.compiler.core
  (:use matchure
	clojure.set
	clojure.contrib.def
	clojure.contrib.str-utils))

(if-match [[[?a ?b] & ?rest] [[1 2] 3 4]]
	  [a b rest])

(defmacro match-lambda [expr &{:keys [lambda apply primitive]}]
  (if-match [[[?M ?N] & ?apply-exprs] apply]
	    (if-match [[[?x ?y] & ?lambda-exprs] lambda]
		      (if-match [[?p & ?primitive-exprs] primitive]
				`(let [expr# ~expr]
				   (if (seq? expr#)
				     (if (= (first expr#) :fn)
				       (do
					 (assert (= (count expr#) 3))
					 (assert (= (count (second expr#)) 1))
					 (let [~x (first (second expr#))
					       ~y (nth expr# 2)]
					   (assert (symbol? ~x))
					   ~@lambda-exprs))
				       (do
					 (assert (= (count expr#) 2))
					 (let [~M (first expr#)
					       ~N (second expr#)]
					   ~@apply-exprs)))
				     (let [~p expr#]
				       (assert (or (keyword? expr#) (symbol? expr#) (number? expr#)))
				       ~@primitive-exprs)))
				(throw (Exception. (str "Invalid primitive argument " primitive))))
		      (throw (Exception. (str "Invalid lambda argument " lambda))))
	    (throw (Exception. (str "Invalid apply argument " apply)))))

(defn compile-a [x expr]
  (cond-match expr

	      [?M ?N]
	      (list (list :S (compile-a x M)) (compile-a x N))

	      ?y
	      (if (= x y)
		:I
		(list :K y))))

(defn compile-lambda [expr]
  (match-lambda expr
		:lambda ([x M]
			   (compile-a x (compile-lambda M)))
		:apply ([M N]
			  (list (compile-lambda M) (compile-lambda N)))
		:primitive (x
			    x)))

(defn- bound-in? [sym expr]
  (match-lambda expr
		:lambda ([x M]
			   (if (= sym x)
			     false
			     (bound-in? sym M)))
		:apply ([M N]
			  (or (bound-in? sym M)
			      (bound-in? sym N)))
		:primitive (x
			    (= sym x))))

(defn pre-optimize-lambda [expr]
  (match-lambda expr
		:lambda ([x M]
			   (if (bound-in? x M)
			     (list :fn [x] (pre-optimize-lambda M))
			     (match-lambda M
					   :lambda ([y N]
						      (list :K (pre-optimize-lambda M)))
					   :apply ([O P]
						     (list (list :S
								 (list :K (pre-optimize-lambda O)))
							   (list :K (pre-optimize-lambda P))))
					   :primitive (y
						       expr))))
		:apply ([M N]
			  (list (pre-optimize-lambda M) (pre-optimize-lambda N)))
		:primitive (x
			    x)))

(defn optimize-ski [ski]
  (if (seq? ski)
    (let [ski (map optimize-ski ski)]
      (or
       (if-match [[[?S [?K ?L]] [?M ?x]] ski]
		 (if (= [S K L M] [:S :K :K :K])
		   `(:K (:K ~x))))
       (if-match [[[?S [?K ?T]] [?L [?M ?U]]] ski]
		 (if (= [S T U K L M] [:S :S :S :K :K :K])
		   `(:K (:S (:K :S)))))
       ski))
    ski))

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

(defn- alloc-slot [free]
  (let [slot (first free)]
    (assert (number? slot))
    [slot (difference free #{slot})]))

(defn- primitive-card? [x]
  (cond (keyword? x)
	x
	(= x 0)
	:zero))

(defn- gen-primitive? [x s]
  (if-let [card (primitive-card? x)]
    (if (= card :I)
      [[:left s :put]]
      [[:left s :put]
       [:right s card]])))

(defn- gen-number [x s]
  (assert (and (number? x) (>= x 0)))
  (if-let [primitive (gen-primitive? x s)]
    primitive
    (let [smaller (bit-shift-right x 1)]
      (concat
       (gen-number smaller s)
       (if (zero? smaller)
	 []
	 [[:left s :dbl]])
       (if (bit-test x 0)
	 [[:left s :succ]]
	 [])))))

(defn- gen-simple? [x s]
  (if-let [primitive (gen-primitive? x s)]
    primitive
    (if (number? x)
      (gen-number x s)
      (if-match [[?l ?r] x]
		(if-lets [l-card (primitive-card? l)
			  r-gen (gen-simple? r s)]
			 (concat r-gen
				 [[:left s l-card]])
			 (if-lets [r-card (primitive-card? r)
				   l-gen (gen-simple? l s)]
				  (concat l-gen
					  [[:right s r-card]])
				  nil))))))

(defn- generate-mn [s x-code y-code m-card n-card]
  (concat
   x-code
   y-code
   [[:left s :K]
    [:left s :S]
    [:right s m-card]
    [:right s n-card]]))

(declare generate)

(defn- generate-complex [s free x-code y]
  (assert (not (contains? free s)))
  (let [[os os-free] (alloc-slot free)]
    (generate-mn s x-code
		 (concat (generate y os os-free)
			 (generate os 0 nil)
			 [[:left 0 :get]])
		 :get :zero)))

(defn generate [ski s free]
  (assert (not (contains? free s)))
  (if-let [simple (gen-simple? ski s)]
    simple
    (cond-match ski

		[?x [?M ?N]]
		(let [x-code (generate x s free)]
		  (if-lets [m-card (primitive-card? M)
			    n-card (primitive-card? N)]
			   (generate-mn s x-code [] m-card n-card)
			   (if-let [y-simple (gen-simple? [M N] 0)]
			     (generate-mn s x-code y-simple :get :zero)
			     (generate-complex s free x-code [M N]))))

		[?x ?y]
		(generate-complex s free (generate x s free) y)

		?x
		(throw (Exception. (str "Malformed SKI " x))))))

(defn- command-str [prefix command]
  (let [[side slot card] command]
    (case side
	  :left (str prefix "1\n" prefix (name card) "\n" prefix slot "\n")
	  :right (str prefix "2\n" prefix slot "\n" prefix (name card) "\n")
	  (throw (Exception. (str "Malformed command " command))))))

(defn shell-script [filename commands]
  (spit filename
	(str "#!/bin/bash\n"
	     (apply str (map (fn [command]
			       (str (command-str "echo " command)
				    "read ; read ; read\n"))
			     commands)))))

(defn command-script [filename commands]
  (spit filename
	(apply str (map #(command-str "" %) commands))))
