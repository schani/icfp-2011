(ns fikdm.compiler.core
  (:use matchure
	clojure.set
	clojure.contrib.def
	clojure.contrib.logging
	clojure.contrib.str-utils))

(defn realseq? [x]
  (or (seq? x) (vector? x)))

(defmacro match-lambda [expr &{:keys [lambda apply primitive]}]
  (if-match [[[?M ?N] & ?apply-exprs] apply]
	    (if-match [[[?x ?y] & ?lambda-exprs] lambda]
		      (if-match [[?p & ?primitive-exprs] primitive]
				`(let [expr# ~expr]
				   (if (realseq? expr#)
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

(defn compile-a [x expr]
  (if (bound-in? x expr)
    (cond-match expr

		[?M ?N]
		(list (list :S (compile-a x M)) (compile-a x N))

		?y
		(do
		  (assert (= x y))
		  :I))
    (list :K expr)))

(defn compile-lambda [expr]
  (match-lambda expr
		:lambda ([x M]
			   (compile-a x (compile-lambda M)))
		:apply ([M N]
			  (list (compile-lambda M) (compile-lambda N)))
		:primitive (x
			    x)))

(defn optimize-ski [ski]
  (if (realseq? ski)
    (let [ski (map optimize-ski ski)]
      (or
       (if-match [[[?S [?K ?L]] [?M ?x]] ski]
		 (if (= [S K L M] [:S :K :K :K])
		   `(:K (:K ~x))))
       (if-match [[[?S [?K ?T]] [?L [?M ?U]]] ski]
		 (if (= [S T U K L M] [:S :S :S :K :K :K])
		   `(:K (:S (:K :S)))))
       (if-match [[[
		    [?S1 [?K ?S2]]
		    [?S3 ?x]]
		   ?y] ski]
		 (if (= [S1 S2 S3 K] [:S :S :S :K])
		   (do
		     (info "optimize")
		     `(:S ((:S ~x) ~y)))))
       (if-match [[[?S [?K ?x]] ?I] ski]
		 (if (= [S K I] [:S :K :I])
		   x))
       (if-match [[?K ?I] ski]
		 (if (= [K I] [:K :I])
		   :put))
       (if-match [[?S1 [[?S2 [?K ?x]] ?I]] ski]
		 (if (= [S1 S2 K I] [:S :S :K :I])
		   `(:S ~x)))
       ski))
    ski))

(defn fixpoint [f max x]
  (loop [x x
	 i 0]
    (if (>= i max)
      (do
	(info "Max iterations")
	x)
      (let [nx (f x)]
	(if (= nx x)
	  (do
	    (info (str i " iterations"))
	    x)
	  (recur nx (inc i)))))))

(defn lambda->ski [program]
  (fixpoint optimize-ski 10 (compile-lambda program)))

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
    (do
      (info (str "alloced slot " slot))
      [slot (difference free #{slot})])))

(defn- primitive-card? [x]
  (cond (keyword? x)
	x
	(= x 0)
	:zero))

(declare *fields*)

(defn- gen-primitive? [x s]
  (if-let [card (primitive-card? x)]
    (let [put (if (= (*fields* s) :I)
		[]
		[[:left s :put]])]
      (if (= card :I)
	put
	(concat
	 put
	 [[:right s card]])))))

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

(defn- get-from-field? [ski]
  (some (fn [[s f]]
	  (if (= f ski)
	    s
	    false))
	*fields*))

(defn- set-field! [s ski]
  ;;(info (str "field " s " <- " ski))
  (set! *fields* (assoc *fields* s ski)))

(defn- generate-complex [s free x-code y]
  (assert (not (contains? free s)))
  ;;(info (str "generating complex for " y))
  (let [[os os-free] (alloc-slot free)]
    (generate-mn s x-code
		 (concat (generate y os os-free)
			 (generate os 0 nil)
			 [[:left 0 :get]])
		 :get :zero)))

(defn- generate [ski s free]
  (assert (not (contains? free s)))
  ;;(if-let [slot (get-from-field? ski)]
  ;;  (info (str "already in " slot ": " ski)))
  (let [gen (if-let [simple (gen-simple? ski s)]
	      simple
	      (cond-match ski

			  [?x [?M ?N]]
			  (if-let [x-card (primitive-card? x)]
			    (concat (generate [M N] s free)
				    [[:left s x-card]])
			    (let [x-code (generate x s free)]
			      (if-lets [m-card (primitive-card? M)
					n-card (primitive-card? N)]
				       (generate-mn s x-code [] m-card n-card)
				       (if-let [y-simple (gen-simple? [M N] 0)]
					 (do
					   (set-field! 0 [M N])
					   (generate-mn s x-code y-simple :get :zero))
					 (do
					   (info (str "complex " x " ||||||||||||||| " [M N]))
					   (generate-complex s free x-code [M N]))))))

			  [?x ?y]
			  (generate-complex s free (generate x s free) y)

			  ?x
			  (throw (Exception. (str "Malformed SKI " x)))))]
    (set-field! s ski)
    gen))

(defvar *regs* #{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27})

(defn ski->commands [ski s]
  (binding [*fields* (into {} (map (fn [r] [r :I]) *regs*))]
    (generate ski s *regs*)))

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
