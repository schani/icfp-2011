(ns fikdm.compiler.programs
  (:use clojure.contrib.def
	clojure.set
	fikdm.compiler.core
	fikdm.compiler.eval))

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

(defn make-loop [side-effect]
  `(((:S :I) :I)
    (:fn [f#]
	 ((:fn [y#]
	    (((:S :I) :I) f#))
	  ~side-effect))))

(defn make-apply-self-return [side-effect-fn]
  (let [f (gensym 'f)]
    `(~*SII*
      (:fn [~f]
	   ~(make-se-combine-fn side-effect-fn
				`(:fn [d2#]
				      (~f ~f)))))))

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

(defn make-repeat-effect-fn [n side-effect-fn]
  (assert (>= n 2))
  (let [se-fn (gensym 'se-fn)]
    `((:fn [~se-fn]
	   ~(loop [n (dec n)
		   acc se-fn]
	      (if (zero? n)
		acc
		(recur (dec n)
		       (make-se-combine-fn se-fn acc)))))
      ~side-effect-fn)))

(defn lambda->ski [program]
  (fixpoint optimize-ski 10 (compile-lambda program)))

(defvar *regs* #{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27})

(defn spit-echoer [filename program]
  (shell-script filename
		(generate
		 (lambda->ski program)
		 1 *regs*)))

(shell-script "/tmp/beidler.sh"
	      (concat
	       (generate (lambda->ski
			  (make-apply-self-return
			   (make-repeat-effect-fn 8
						  (make-help-get-attack-fn 0 8192 64 768)))) 65 *regs*)
	       [[:left -1 :init-number]]
	       (generate 0 64 nil)
	       [[:left -1 :death-loop]
		[:right 65 :I]
		[:right 65 :I]
		[:left 64 :succ]]))

;;(spit-echoer "/tmp/beidler.sh" (make-help-attack-loop 0 8192 0 768))
;;(spit-echoer "/tmp/beidler.sh" (make-dec-loop 0))
;;(spit-echoer "/tmp/beidler.sh" (make-repeat-effect 3 '(:dec :zero)))
;;(spit-echoer "/tmp/beidler.sh" (list (make-repeat-effect-fn 15 (make-help-attack-fn 0 8192 0 768)) :I)

;;(spit-echoer "/tmp/beidler.sh" (make-apply-self-return (make-help-attack-fn 0 8192 0 768)))

;;(spit-echoer "/tmp/beidler.sh" (make-help-attack-self-return 10 0 8192 0 768))

;;(lambda->ski (make-apply-self-return (make-repeat-effect-fn 15 (make-help-attack-fn 0 8192 0 768))))
