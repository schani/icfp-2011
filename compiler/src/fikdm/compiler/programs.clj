(ns fikdm.compiler.programs
  (:use clojure.contrib.def
	clojure.set
	fikdm.compiler.core))

(defvar *SII* (compile-lambda '(:fn [x] (x x))))

;;; combinators

;; A
(defn make-se-fn [side-effect]
  (match-lambda side-effect
		:lambda ([x M]
			   (throw (Exception. (str "Side effect cannot be an fn: " side-effect))))
		:apply ([M N]
			  `((:S (:K ~M)) (:K ~N)))
		:primitive (x
			    (throw (Exception. (str "Side effect cannot be a primitive: " side-effect))))))

;; C
(defn make-se-combine-fn [a b]
  `((:S ~a) ~b))

;; P
(defn make-se-pass-fn [g e]
  `(:fn [d#]
	(~e (~g d#))))

(defvar *se-twice* '((:S :S) :I))
;; equivalent to, but shorter than:
;;  (let [f (gensym 'f)]
;;    (compile-lambda
;;     `(:fn [~f]
;;	   ~(make-se-combine-fn f f)))))

(defn make-loop [side-effect-fn]
  (let [f (gensym 'f)]
    `(~*SII*
      (:fn [~f]
	   (~(make-se-fn (list *SII* f))
	    (~side-effect-fn ~f))))))

(defn make-apply-self-return [side-effect-fn]
  (let [f (gensym 'f)]
    `(~*SII*
      (:fn [~f]
	   ~(make-se-combine-fn side-effect-fn
				(make-se-fn (list f f)))))))

(defn make-get-fn [slot]
  (make-se-fn `(:get ~slot)))

(defn make-help-attack-fn [help-field help-strength attack-field attack-strength]
  (let [attack-fn (make-se-fn `(((:attack ~help-field) ~attack-field) ~attack-strength))
	help-fn (make-se-fn `(((:help ~help-field) ~help-field) ~help-strength))]
    (make-se-combine-fn help-fn attack-fn)))

;; takes the attack field as an argument
(defn make-param-attack-fn [help-field attack-strength]
  (let [attack-field (gensym 'attack-field)]
    `(:fn [~attack-field]
	  (((:attack ~help-field) ~attack-field) ~attack-strength))))

;; takes the attack field from a slot
(defn make-help-get-attack-fn [help-field help-strength attack-field-slot attack-strength]
  (make-se-combine-fn (make-se-fn `(((:help ~help-field) ~help-field) ~help-strength))
		      (make-se-pass-fn (make-se-fn `(:get ~attack-field-slot))
				       (make-param-attack-fn help-field attack-strength))))

;; takes the attack field as an argument
(defn make-param-help-attack-fn [help-field help-strength attack-strength]
  (let [attack-field (gensym 'attack-field)]
    `(:fn [~attack-field]
	  ~(make-help-attack-fn help-field help-strength attack-field attack-strength))))

;; takes the revive field from a slot
(defn make-get-revive-fn [slot]
  (make-se-pass-fn (make-get-fn slot)
		   :revive))

(defvar *doubler*
  (let [sep (gensym 'sep)
	i (gensym 'i)]
    `(:fn [~sep]
	  (:fn [~i]
	       ~(make-se-combine-fn `(~sep (:dbl ~i))
				    `(~sep (:succ (:dbl ~i))))))))

(defn make-get-apply-fn [param-side-effect-fn slot]
  (make-se-pass-fn (make-se-fn `(:get ~slot))
		   param-side-effect-fn))

(defn make-repeat-effect-fn [n]
  (assert (>= n 2))
  (if (= n 2)
    *se-twice*
    (let [f (gensym 'f)]
      `(:fn [~f]
	    ~(loop [acc f
		    n n]
	       (if (= n 1)
		 acc
		 (do
		   (assert (zero? (rem n 2)))
		   (recur (list *se-twice* acc)
			  (quot n 2)))))))))

(defn- make-masr [help-slot register-slot]
  `(((:S :I) :I) ((:S (:K (:S (((:S :S) :I) (((:S :S) :I) (((:S :S) :I) (((:S :S) :I) ((:S ((:S (:K ((:help ~help-slot) ~help-slot))) (:K 8192))) ((:S ((:S (:K (:attack ~help-slot))) ((:S (:K :get)) (:K ~register-slot)))) (:K 768)))))))))) ((:S ((:S (:K :S)) :K)) :K))))

(defvar *masr* (make-masr 0 64))

(defvar *super-hand-optimized-masr*
  (let [M [[:S :I] :I]
	N [[:S :S] :I]
	P [[:S [[:S [:K :S]] :K]] :K]
	S1 [:S [[[:S [:K [:S [:K [[:help 0] 0]]]]] :K] 8192]]
	S2 [[:S [:K [:attack 0]]] [[[:S [:K [:S [:K :get]]]] :K] 64]]
	S3 [[[:S [:K [:S S2]]] :K] 768]]
    [M [[:S [:K [:S [N [N [N [N [S1 S3]]]]]]]] P]]))

(defvar *semi-hand-optimized-masr*
  (let [M [[:S :I] :I]
	N [[:S :S] :I]
	P [[:S [[:S [:K :S]] :K]] :K]
	S1 [:S [[[:S [:K [:S [:K [[:help 0] 0]]]]] :K] 8192]]
	S2 [[:S [:K [:attack 0]]] [[[:S [:K [:S [:K :get]]]] :K] 64]]
	S3 [[[:S [:K [:S S2]]] :K] 768]]
    [M [[:S [:K [:S [N [N [N [N [

				 [:S
				  [[[:S [:K [:S [:K [[:help 0] 0]]]]] :K] 8192]
				  ]

				 [[:S [[:S [:K [:attack 0]]] [[:S [:K :get]] [:K 64]]]] [:K 768]]

				 ]]]]]]]] P]]))

(defvar *masr4* (lambda->ski
		 ;;(make-get-apply-fn (make-param-help-attack-fn 0 8192 768) 64)

		 (make-apply-self-return
		  (let [d (gensym 'd)]
		    `(:fn [~d]
			  (
			   (~(make-repeat-effect-fn 16)
			    ((~*doubler*
			      (~*doubler*
			       ~(make-param-help-attack-fn 0 8192 768)))
			     (~(make-get-fn 64) ~d)))

			   ~d))))

		 ;;(list (make-repeat-effect-fn 16)
		 ;;	(make-se-pass-fn (make-get-fn 64)
		 ;;			 (make-param-help-attack-fn 0 8192 768)))


		 ))

(defvar *kill-255* (lambda->ski
		    (make-loop (make-help-attack-fn 0 8192 255 768))))

(defn spit-echoer [filename program]
  (shell-script filename
		(generate
		 (lambda->ski program)
		 1 *regs*)))

(shell-script "/tmp/empkillah.sh"
	      (concat
	       (apply concat (map #(ski->commands % 16)
					  (map (fn [x]
						 `(((:attack ~x) 255) 5556))
					       (range 4 256))))))

(shell-script "/tmp/empkillah2.sh"
	      (concat
	       (repeat 200 [:left 0 :I])
	       (apply concat (map #(ski->commands % 16)
					  (map (fn [x]
						 `(((:attack ~x) ~(- 255 8)) 5556))
					       (range 4 256))))))

(defn masr-shell-script [filename counter-slot masr-slot]
  (shell-script filename
		(concat (ski->commands (make-masr counter-slot counter-slot) masr-slot)
			(binding [*regs* nil]
			  (ski->commands 0 counter-slot))
			(apply concat (repeat 256
					      [[:right masr-slot :I]
					       [:left counter-slot :succ]])))))

(shell-script "/tmp/empkillah3.sh"
	      (let [r (java.util.Random.)
		    rl (map (fn [x]
			      [x (nth [0 2 4 8] (.nextInt r 4))])
			    (range 0 125))]
		(concat (repeat 100 [:left 0 :I])
			(apply concat (map (fn [[i s]]
					     (concat (ski->commands `(((:attack ~(+ 4 (* 2 i))) ~(- 255 s)) 6000) 16)
						     (ski->commands `(((:attack ~(+ 5 (* 2 i))) ~(- 255 s)) 6000) 16)
						     (repeat (.nextInt r 150) [:left 0 :I])))
					   rl)))))

(command-script "/tmp/masr.cmd"
		(ski->commands *masr* 65))
(command-script "/tmp/masr4.cmd"
		(ski->commands *masr4* 65))
(command-script "/tmp/kill255.cmd"
		(ski->commands *kill-255* 65))
(command-script "/tmp/revive32.cmd"
		(ski->commands (lambda->ski (make-apply-self-return (make-get-revive-fn 32))) 33))

(command-script "/tmp/masr-8-4-noinit.cmd"
		(ski->commands (make-masr 4 4) 8))
(command-script "/tmp/masr-8-4-init.cmd"
		(binding [*assume-inited* true]
		  (ski->commands (make-masr 4 4) 8)))

(doseq [s (range 9 256)]
  (let [commands (ski->commands `(((:help ~s) 4) 8000) 12)]
    (command-script (str "/tmp/help-" s "-to-4.cmd") commands)
    (shell-script (str "/tmp/help-" s "-to-4.sh") commands)))

(masr-shell-script "/tmp/masr-8-4.sh" 4 8)

;;(spit-echoer "/tmp/beidler.sh" (make-help-attack-loop 0 8192 0 768))
;;(spit-echoer "/tmp/beidler.sh" (make-dec-loop 0))
;;(spit-echoer "/tmp/beidler.sh" (make-repeat-effect 3 '(:dec :zero)))
;;(spit-echoer "/tmp/beidler.sh" (list (make-repeat-effect-fn 15 (make-help-attack-fn 0 8192 0 768)) :I)

;;(spit-echoer "/tmp/beidler.sh" (make-apply-self-return (make-help-attack-fn 0 8192 0 768)))

;;(spit-echoer "/tmp/beidler.sh" (make-help-attack-self-return 10 0 8192 0 768))

;;(lambda->ski (make-apply-self-return (make-repeat-effect-fn 15 (make-help-attack-fn 0 8192 0 768))))
