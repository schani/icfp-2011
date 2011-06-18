(ns fikdm.compiler.programs
  (:use fikdm.compiler.core))

(defn make-loop [side-effect]
  `(((:S :I) :I)
    (:fn [f#]
	 ((:fn [y#]
	    (((:S :I) :I) f#))
	  ~side-effect))))

(defn make-help-attack-loop [help-field help-strength attack-field attack-strength]
  (make-loop
   `((:fn [x]
	  (((:attack ~help-field) ~attack-field) ~attack-strength))
     (((:help ~help-field) ~help-field) ~help-strength))))

(defn spit-echoer [filename program]
  (shell-script filename
		(generate
		 (optimize-ski (compile-lambda (pre-optimize-lambda program)))
		 1 #{2 3 4 5 6 7 8 9 10 11 12 13 14 15})))

;(spit-echoer "/tmp/beidler.sh" (make-help-attack-loop 0 8192 0 768))
