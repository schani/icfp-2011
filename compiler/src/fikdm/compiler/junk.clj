(defvar *apply-succ-return*
  (let [s (gensym 's)
	i (gensym 'i)
	f (gensym 'f)]
    (lambda->ski
     `(~*SII*
       (:fn [~s]
	    (:fn [~f]
		 (:fn [~i]
		      ~(make-se-combine-fn (make-se-fn (list f i))
					   (make-se-fn `(((~s ~s) ~f) (:succ ~i)))))))))))


(fixpoint eval-ski 10 '(((((:S (:K (:S (:K (:S (:K x)))))) M) N) O) P))
(regen-ski (fixpoint eval-ski 10 '((((:S (:K ((:S :S) :I))) a) b) c)))
