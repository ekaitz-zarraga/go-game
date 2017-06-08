; -----------------------------------------------------------------------------
; ENGINE:
; -----------------------------------------------------------------------------
; - Board data structure:
;     { [X, Y] :b/:w ...}
;
; - Check if the group must be deleted:
;     Sum all the liberties of the group -> if =0 delete.
;
; - If new stones are in the border start the liberty count in -1 per border
; they touch.
;
; - Ko rule. Status can't be repeated. Save old statuses.
; -----------------------------------------------------------------------------

(defn in?
  "Gives true if el in coll. nil if not."
  [coll el]
  (some #(= el %) coll))

(defn get-neighbors
  "Receives [X Y] position, returns all adjacent coordinates with no boundary
  checking"
  [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (and (not= dx dy) (not= (- dx) dy))]
    [(+ x dx) (+ y dy)]))

(defn get-touching
  "Receives a position and the board hash-map and returns all to positions of
  stones adjacent to the received position"
  [position board]
  (select-keys board (get-neighbors position)))

(defn calc-liberties
  "Receives a position, the adjacent stones collection and board size. Returns
  the liberties of the received position"
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
        candidates (->> (get-touching pos board)        ; Get adjacent stones
                        (filter #(= color (val %)))     ; Get same color
                        (filter #(not (in? stones %))))]; Don't process twice
   (if (empty? candidates)
     stones
     (into {} (reduce #(get-group board (conj %1 %2)) stones candidates)))))

(defn get-liberties-positions
  "Returns the sum of the liberties of a sequence of positions."
  [board size positions]
  (reduce
    (fn [libs pos]
      (let [touching (get-touching pos board)]
        (+ libs (calc-liberties pos touching size))))
    0
    positions)
  )

(defn put-stone
  ; FIXME if 2 adjacent stones are in the same group they are processed twice
  "Puts stone with the received color in the received position of the size
  sized board. Processes the board for conquest or suicide moves and returns
  the processed board."
  [size color pos board]
  (let [board        (assoc board pos color)
        touched      (get-touching pos board)
        b-after-conq (if (empty? touched)
                       board
                       (->> touched
                           (filter #(not= (val %) color))
                           (map #(keys (get-group board (into {} [%]))))
                           (filter #(= 0 (get-liberties-positions board size %)))
                           (apply concat)
                           (apply dissoc board)))
        this-group   (keys (get-group b-after-conq {pos color}))]

      (if (= 0 (get-liberties-positions b-after-conq size this-group))
        (apply dissoc b-after-conq this-group)
        b-after-conq)))

(defn create-go
  "Receives user interaction related functions and returns a function to call
  to play the game."
  [get-size listen-user notify-ko]
  (let [ size        (get-size)
         put-stone   (partial put-stone size)
         listen-user (partial listen-user size)]
    (defn turn
      ([]
        (let [them {}                       ; Create {} as first position
             position (listen-user :b them)]; Listen user
          (if (nil? position)               ; User passed, no position returned
            (turn (conj [them] them))       ; -> repeat empty
            (turn (conj [them] (put-stone :b position them))))))

      ([history]
        (let [ them  (last history)                           ; Use for the next
               me    (last (drop-last 1 history))             ; Check Ko
               color (if (= (rem (count history) 2) 0) :w :b ); Black starts NOTE: First is empty
               position (listen-user color them)]             ; Get move
          (if (nil? position)
            (if (= them me)
              history                     ; Both passed, game ends
              (recur (conj history them))); I passed, repeat last step and go

            (do
              (let [this (put-stone color position them)]
                (if (= this me) ;CHECK THIS: If extended Ko rule check all history
                  (do
                    (notify-ko color); Basically tell the user there's a ko
                    (recur history)) ; and loop again in the same user
                  (recur (conj history this)))))))))))




; -----------------------------------------------------------------------------
; This functions below are made to test the engine.
; The engine works the same way for any functions with the same interface
; -----------------------------------------------------------------------------
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

(defn notify-ko
  [color]
  (println "That stone makes a Ko, try another move")
  )

(defn get-size
  []
  19)


((create-go get-size listen-user notify-ko))
