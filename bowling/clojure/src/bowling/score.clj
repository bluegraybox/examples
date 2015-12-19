(ns bowling.score)

(defn sumn "sum the first N elements of a list" ([n l] (reduce + (take n l))))

(defn score
    "Calculate score for a bowling game"

    ([rolls] (score rolls 1 0))

    ([rolls frame total] (cond
        (= frame 11) total

        (= rolls []) total

        (> 0 (first rolls))   :error
        (> 0 (second rolls))  :error
        (< 10 (first rolls))  :error

        (= 10 (first rolls))
            (score (drop 1 rolls) (inc frame) (+ total (sumn 3 rolls)))

        (< 10 (sumn 2 rolls)) :error

        (= 10 (sumn 2 rolls))
            (score (drop 2 rolls) (inc frame) (+ total (sumn 3 rolls)))

        :else
            (score (drop 2 rolls) (inc frame) (+ total (sumn 2 rolls))))))
