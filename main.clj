;STONES:
;{ [X, Y] :b/:w ...}
;
; - Check if the group must be deleted:
;     Sum all the liberties of the group -> if =0 delete.
;
; - If new stones are in the border start the liberty count in -1 per border
; they touch.
;
; - Ko rule. Status can't be repeated. Save old statuses.

(defn in?
  "Gives true if el in coll. nil if not."
  [coll el]
  (some #(= el %) coll))

(defn get-neighbors
  [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (and (not= dx dy) (not= (- dx) dy))]
    [(+ x dx) (+ y dy)]))

(defn get-touching
  [position board]
  (select-keys board (get-neighbors position)))

(defn calc-liberties
  [pos touched size]
  (- 4
    (count touched)
    (count (filter #(or (= % (dec size)) (= % 0)) pos))))

(defn get-group ; FIXME input arguments separation stone to check vs acc
  "Gives the group the stones are in.
   Board and stones are hash-maps"
  [board stones]
  (let [stone      (first stones)
        pos        (key stone)
        color      (val stone)
        candidates (->> (get-touching pos board)         ; Get adjacent stones
                        (filter #(= color (val %)))      ; Get same color
                        (filter #(not (in? stones %))))] ; Don't process twice
   (if (empty? candidates)
     stones
     (into {} (reduce #(get-group board (conj %1 %2)) stones candidates)))))

(defn stone ; FIXME if 2 adjacent stones are in the same group they are processed twice
  [size color pos board]
  (let [board   (assoc board pos color)
        touched (get-touching pos board)]
    (if (empty? touched)
      board
      (->> touched
          (filter #(not= (val %) color))
          (map #(get-group board (into {} [%]))) ;FIXME NOT CHECKING SUICIDE
          (map keys)
          (filter #(= 0 (reduce (fn [libs pos]
                                  (let [touching (get-touching pos board)]
                                    (+ libs (calc-liberties pos touching size))))
                                0
                                %)))
          (apply concat)
          (apply dissoc board)))))

(defn create-go
  [get-size listen-user notify-ko]
  (let [ size        (get-size)
         put-stone   (partial stone size)
         listen-user (partial listen-user size)]
    (defn turn
      ([]
        (let [them {}                         ; Create {} as first position
             position (listen-user :b them)]  ; Listen user
          (if (nil? position)                 ; User passed, no position returned
            (turn (conj [them] them))         ; -> repeat empty
            (turn (conj [them] (put-stone :b position them))))))

      ([history]
        (let [ them  (last history)                              ; Use for the next
               me    (last (drop-last 1 history))                ; Check Ko
               color (if (= (rem (count history) 2) 0) :w :b )   ; Black starts NOTE: First is empty
               position (listen-user color them)]                ; Get move
          (if (nil? position)
            (if (= them me)
              history                        ; Both passed, game ends
              (recur (conj history them)))    ; I passed, repeat last step and go

            (do
              (let [this (put-stone color position them)]
                (if (= this me) ;CHECK THIS: If extended Ko rule check all history
                  (do
                    (notify-ko color); TODO basically tell the user there's a ko
                    (recur history)) ; and loop again in the same user
                  (recur (conj history this)))))))))))


(defn extract-coordinates
  [text]
  (read-string text)) ; FIXME ERROR HANDLING

(defn generate-board
  [size stones]
  (let [init-board (for [x (range 0 size)
                         y (range 0 size)]
                           [x y])]
    (loop [remaining (map #(let [found (get stones %)]
                             (if found
                               (if (= found :b)
                                 "B"
                                 "W")
                             " ")) init-board)
           result    (concat (repeat (+ 2 size) "+") ["\n"])]
      (if (empty? remaining)
        (concat result (repeat (+ 2 size) "+"))
        (recur (drop size remaining) (concat result ["+"] (take size remaining) ["+\n"]))))))

(defn get-input
  [text]
  (println text)
  (read-line))

(defn listen-user
  [size color board]
  (println (apply str (generate-board size board)))
  (println "Introduce your stone coordinates [X,Y] or write 'pass' to pass")
  (loop [input (get-input "What is your decision?")]
    (let [coords (extract-coordinates input)]
      (if (= "pass" (str coords))
        nil
        (if (or (<= size (first coords)) (<= size (second coords)))
           (recur (get-input "Coordinates outside the board. Try again."))
           (if (not= nil (get board coords))
             (recur (get-input "Coordinate is filled. Try another one."))
             coords))))))

(defn notify-ko ; TODO
  [color]
  (println "That stone makes a Ko, try another move")
  )

(defn get-size ; TODO
  []
  19)


((create-go get-size listen-user notify-ko))
