; Board shape: a list of stones
;{
;  [X, Y] {
;   :group    N
;   :liberties N
;   :color    :b/:w
;   }
;  ...
;}
;
; - Check if the group must be deleted:
;     Sum all the liberties of the group -> if =0 delete.
;
; - Add a new stone to the board:
;     find adjacent stones and check options:
;
;       1. No stones:
;          Create new group with only this stone
;
;       2. Only of the other color
;          Make 1 and decrease liberties of adjacent.
;          Also compute touched groups and check if they have to be deleted.
;
;       3. Stones of it's color
;          Add to the group of the others.
;          WARNING: if more than 1 stone is in the adj merge groups!
;
; - If new stones are in the border start the liberty count in -1 per border
; they touch.
;   WARNING: liberties are always <= initial value.
;
; - Ko rule. Status can't be repeated. Save old statuses.
;   COOL! Intersting for repetitions.

(defn in?
  "Gives true if el in coll. nil if not."
  [coll el]
  (some #(= el %) coll))

(defn turn
  ([]
    (let [them [{}]                 ; Create {} as first position
         position (listen-user)]     ; Listen user
      (if (= position nil)          ; User passed, no position returned
        (recur (cons them them))    ; -> repeat empty
        (recur (cons (put-stone :b position them) them)))))

  ([history]
    (let [ them  (first history)                             ; Use for the next
           me    (second history)                            ; Check Ko
           color (if (= (rem (count history) 2) 0) :b :w )   ; Black starts NOTE: First is empty
           position (listen-user color)                       ; Get move
         ]
      (if (= position nil)
        (if (= them me)
          history                         ; Both passed, game ends
          (recur (cons them history)))    ; I passed, repeat last step and go

        (do
          (let [this (put-stone color position)]
            (if (= this me) ;CHECK THIS: If extender Ko rule check all history
              (do
                (notify-ko)       ; TODO basically tell the user there's a ko
                (recur history)) ; and loop again in the same user
              (recur (cons this history)))))))))

(defn get-size ; TODO
  []
  19
  )

(defn get-neighbors
  [x y]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (and (not= dx dy) (not= (- dx) dy))]
    [(+ x dx) (+ y dy)]))

(defn process-position ;TODO
  [position board]
  (let [ neighbors (get-neighbors pos) ]
    ()
    ))

(defn generic-stone
  [size color pos status]
    (let [ neighbors  (get-neighbors pos)
           touched    (select-keys status neighbors)]
      (->> { pos
              { :color color
                :liberties
                  ; Liberties: -1 per touched border -1 per neighbor
                  (- 4
                    (count touched)
                    (count (filter #(or (= % (dec size)) (= % 0)) pos)))
                :group
                  ; GroupID is max GroupID +1 others will be updated
                  (->> status
                    (map #(or (:pos (second %)) 0))
                    (apply max)
                    inc) }}
        (into status)
        (process-position pos))))

(defn generic-listen-user ;TODO
  [size]
)

(def put-stone (partial generic-stone (get-size)))
(def listen-user (partial generic-listen-user (get-size)))
