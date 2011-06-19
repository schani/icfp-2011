(ns fikdm.compiler.eval
  (:use fikdm.compiler.core
	clojure.contrib.def
	clojure.contrib.logging))

(defvar- *primitives* #{:S :K :I})
(defvar- *composites* #{:Sx :Sxy :Kx})

(defvar *eval-fns*
  {:S (fn [x] [:Sx x])
   :Sx (fn [x y] [:Sxy x y])
   :Sxy (fn [x y z] [[x z] [y z]])
   :K (fn [x] [:Kx x])
   :Kx (fn [x y] x)
   :I (fn [x] x)})

(defvar *gen-fns*
  {:Sx (fn [x] [:S x])
   :Sxy (fn [x y] [[:S x] y])
   :Kx (fn [x] [:K x])})

(defn- eval-ski [ski]
  (if (realseq? ski)
    (if (*composites* (first ski))
      ski
      (do
	(assert (= (count ski) 2))
	;;(info (str "apply " ski))
	(let [[x y] ski
	      x (eval-ski x)
	      y (eval-ski y)]
	  ;;(info (str "x " x "  y " y))
	  (cond
	   (*primitives* x)
	   (do
	     ;;(info (str "primitive " x))
	     ((*eval-fns* x) y))

	   (and (realseq? x) (*composites* (first x)))
	   (let [comp (first x)
		 comp-args (rest x)]
	     ;;(info (str "composite " comp "  " comp-args "  " y))
	     (apply (*eval-fns* comp) (concat comp-args [y])))

	   :else
	   (do
	     ;;(info (str "else " x "  " y))
	     (list x y))))))
    ski))

(defn- regen-ski [ski]
  (if (realseq? ski)
    (if (*composites* (first ski))
      (apply (*gen-fns* (first ski)) (map regen-ski (rest ski)))
      (do
	(assert (= (count ski) 2))
	(map regen-ski ski)))
    (do
      (assert (not (*composites* ski)))
      ski)))
