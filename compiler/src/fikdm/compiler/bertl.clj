(ns fikdm.compiler.bertl
  (:use fikdm.compiler.core
	fikdm.compiler.programs))

(defn bertl-generate [ski]
  (generate ski 65 *regs*))
