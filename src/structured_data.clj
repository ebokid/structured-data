(ns structured-data)

(defn do-a-thing [x]
    (let [xx (+ x x)]
      (Math/pow xx xx)))

(defn spiff [v]
  (if (and (vector? v)(< 2 (count v))) (+ (get v 0)(get v 2)) "?")
)

(defn cutify [v]
  (if (vector? v) (conj v "<3")) 
)

(defn spiff-destructuring [v]
  (let [a (spiff v)]
    a)
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right]

)

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1))
)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1))
  )

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (- x1 x2)(- y1 y2)) true false ))
)

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (width rectangle)(height rectangle))
  ))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x y] point]
      (if (and (<= x1 x x2)(<= y1 y y2)) true false)
  ))
)

(defn contains-rectangle? [outer inner]
    (let [[x y] inner]
      (if (and (contains-point? outer x)(contains-point? outer y)) true false)  
    )
  )

(defn title-length [book]
  (count(:title book))
)

(defn author-count [book]
  (count(:authors book))
  )

(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false)
  )

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))
  )
)

(defn alive? [author]
  (if (contains? author :death-year) false true)
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [second (fn [col] (get col 1))]
    (map second collection))
  )

(defn titles [books]
  (let [get-title(fn [book] (:title book))]
    (map get-title books)
  )
)

(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true 
    (apply >= a-seq) true
    :else         false)
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
  (if (>(count a-seq)(count(set a-seq))) true false)
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
)

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false)
  )

(defn authors [books]
  (let [all-authors
    (fn [book] (map :authors book))]
      (apply clojure.set/union (all-authors books)))
  )

(defn all-author-names [books]
  (let [author-names
    (fn [author] (map :name author))]
      (set(author-names (authors books)))
  ))

(defn author->string [author]
  (str (str(:name author)) 
    (if (contains? author :birth-year) 
      (str " (" (:birth-year author) " - "
        (if (alive? author)(str ")")(str(:death-year author) ")"))
      )
    )
  )
)

(defn authors->string [authors]
  (let [author-string
    (fn [author] (author->string author))]
      (apply str (interpose ", " (map author-string authors)))
    )
  )

(defn book->string [book]
  (str (str (:title book))
    (if (contains? book :authors)
        (str ", written by " (authors->string(:authors book)))
      )
    )
  )

(defn books->string [books]
  (if (empty? books) (str "No books.")
    (str 
      (if (= 1(count books)) (str "1 book. ")(str (count books) " books. "))
      (let [book-string
        (fn [book] (str (book->string book)"."))]
        (apply str (interpose " " (map book-string books)))
      )
    )
  )
)

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
)

(defn author-by-name [name authors]
  (let [on-list 
    (fn [author] (if (= name (:name author)) true false))]
      (first (filter on-list authors))
    )
  )
 
(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (if (< 0 (count(living-authors(:authors book)))) true false)
  )

(defn books-by-living-authors [books]
    (let [living
      (fn [book] (if (has-a-living-author? book) true false))]
        (filter living books)
    )
  )

; %________%
