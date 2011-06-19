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

(defvar *masr* '(((:S :I) :I) ((:S (:K (:S (((:S :S) :I) (((:S :S) :I) (((:S :S) :I) (((:S :S) :I) ((:S ((:S (:K ((:help 0) 0))) (:K 8192))) ((:S ((:S (:K (:attack 0))) ((:S (:K :get)) (:K 64)))) (:K 768)))))))))) ((:S ((:S (:K :S)) :K)) :K))))

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

(shell-script "/tmp/beidler.sh"
	      (concat
	       (ski->commands *masr* 65)
	       ;;[[:left -1 :init-255]]
	       ;;(generate 255 64 nil)
	       ;;[[:left -1 :kill-255]]
	       ;;(repeat 16 [:right 65 :I])
	       ;;[[:left -1 :init-0]]
	       (ski->commands 0 64)
	       ;;[[:right 65 :I]]
	       (apply concat (repeat 256
	       [;;[:left -1 :death-loop]
		[:right 65 :I]
		;;[:right 65 :I]
		[:left 64 :succ]
		]))))

(command-script "/tmp/masr.cmd"
		(ski->commands *masr* 65))
(command-script "/tmp/masr4.cmd"
		(ski->commands *masr4* 65))
(command-script "/tmp/kill255.cmd"
		(ski->commands *kill-255* 65))
(command-script "/tmp/revive32.cmd"
		(ski->commands (lambda->ski (make-apply-self-return (make-get-revive-fn 32))) 33))

;;(spit-echoer "/tmp/beidler.sh" (make-help-attack-loop 0 8192 0 768))
;;(spit-echoer "/tmp/beidler.sh" (make-dec-loop 0))
;;(spit-echoer "/tmp/beidler.sh" (make-repeat-effect 3 '(:dec :zero)))
;;(spit-echoer "/tmp/beidler.sh" (list (make-repeat-effect-fn 15 (make-help-attack-fn 0 8192 0 768)) :I)

;;(spit-echoer "/tmp/beidler.sh" (make-apply-self-return (make-help-attack-fn 0 8192 0 768)))

;;(spit-echoer "/tmp/beidler.sh" (make-help-attack-self-return 10 0 8192 0 768))

;;(lambda->ski (make-apply-self-return (make-repeat-effect-fn 15 (make-help-attack-fn 0 8192 0 768))))
