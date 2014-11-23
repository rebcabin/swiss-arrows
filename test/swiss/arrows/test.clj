(ns swiss.arrows.test
  (:require [clojure.string      :as str]
            [swiss.arrows        :refer :all]
            [clojure.test        :refer :all]
            [clojure.algo.monads :refer :all]))

(deftest diamond
  (testing "the diamond wand"
    (is (= (-<> (first [1]))
           1))
    (is (= (-<> 0
                (* <> 5)
                (vector 1 2 <> 3 4))
           [1 2 0 3 4]))
    (is (= (-<> [1 2 3]
                (concat [-1 0] <> [4 5]
                        (-<> 10
                             [7 8 9 <> 11 12]
                             (cons 6 <>))))
           (range -1 13))))

  (testing "vector"
    (is (= (-<> [])
           []))
    (is (= (-<> 10 [1 2 3 <> 4 5])
           [1 2 3 10 4 5]))
    (is (vector? (-<> 10 [1 2 3 <> 4 5])))
    (is (= (-<> 0 [<>])
           [0]))
    (is (= (-<> (+ 2 (* 2 3))
                [6 7 <> 9 10]
                (vector <>))
           [[6 7 8 9 10]]))
    (is (= (-<> 10 [1 2 'a <> 4 5])
           [1 2 'a 10 4 5]))
    (is (= (-<> 0 (vector 1 2 3))
           [0 1 2 3]))
    (is (= (-<>> 0 (vector 1 2 3))
           [1 2 3 0]))
    (is (= (-<> 0 [1 2 3])
           [0 1 2 3]))
    (is (= (-<>> 0 [1 2 3])
           [1 2 3 0])))

  (testing "seqs"
    (is (= (-<> 0 (list 1 2 3))
           '(0 1 2 3)))
    (is (= (-<>> 0 (list 1 2 3))
           '(1 2 3 0))))

  (testing "map"
    (is (= (-<> 'foo {:a <> :b 'bar})
           {:a 'foo :b 'bar}))
    (is (= (-<> :a {<> 'foo :b 'bar})
           {:a 'foo :b 'bar})))

  (testing "symbol = (symbol <>)"
    (is (= (-<> :a (map <> [{:a 1} {:a 2}]) vector)
           (-<> :a (map <> [{:a 1} {:a 2}]) (vector <>))))
    (is (= (-<> {:a 1 :b 2} :a inc (vector 1 <> 3))
           [1 2 3]))
    (is (= (-<> :a
                (map <> [{:a 1} {:a 2}])
                (map (partial + 2) <>)
                reverse)
           [4 3])))

  (testing "exception for more than one diamond"
    (is (thrown? Exception (eval '(-<> 0 [1 <> <>])))))

  (testing "default position behavior"
    (is (= (-<> 0 [1 2 3])
           [0 1 2 3]))
    (is (= (-<>> 0 [1 2 3])
           [1 2 3 0]))
    (is (= (-<> 0 (list 1 2 3))
           '(0 1 2 3)))
    (is (= (-<>> 0 (list 1 2 3))
           (list 1 2 3 0)))
    (is (= (-<>> 4 (conj [1 2 3]))
           [1 2 3 4]))
    (is (= (-<> 4 (cons [1 2 3]))
           [4 1 2 3]))
    (is (= (-<>> 4 (conj [1 2 3]) reverse (map inc <>))
           [5 4 3 2]))
    (is (= (-<> 4 (cons [1 2 3]) reverse (map inc <>))
           [4 3 2 5]))))

(deftest back-arrow
  (testing "back arrow"
    (is (= (<<-
            (let [x 'nonsense])
            (if-not x 'foo)
            (let [more 'blah] more))
           (->>
            (let [more 'blah] more)
            (if-not x 'foo)
            (let [x 'nonsense]))))
    (is (= (<<-
            (let [x 'nonsense])
            (if-not x 'foo)
            (let [more 'blah] more))
           (let [x 'nonsense]
             (if-not x 'foo
                     (let [more 'blah] more)))))
    (is (= (<<-
            (let [x 'nonsense])
            (if-not x 'foo)
            (let [more 'blah] more))
           'blah))))

(deftest furculi
  (testing "furculi"
    (is (= (-< (+ 1 2)
               (list 2)
               (list 3)
               (list 4))
           '[(3 2) (3 3) (3 4)]))
    (is (= (-< (+ 1 2)
               (->> vector (repeat 3))
               (-> (* 2) list)
               (list 4))
           '[([3] [3] [3]) (6) (3 4)]))
    (is (= (-<:p (+ 1 2)
                 (list 2)
                 (list 3)
                 (list 4))
           '[(3 2) (3 3) (3 4)]))
    (is (= (-<< (+ 1 2)
                (list 2 1)
                (list 5 7)
                (list 9 4))
           '[(2 1 3) (5 7 3) (9 4 3)]))
    (is (= (-<<:p (+ 1 2)
                  (list 2 1)
                  (list 5 7)
                  (list 9 4))
           '[(2 1 3) (5 7 3) (9 4 3)]))
    (is (= (-<>< (+ 1 2)
                 (list <> 2 1)
                 (list 5 <> 7)
                 (list 9 4 <>))
           '[(3 2 1) (5 3 7) (9 4 3)]))
    (is (= (-<><:p (+ 1 2)
                   (list <> 2 1)
                   (list 5 <> 7)
                   (list 9 4 <>))
           '[(3 2 1) (5 3 7) (9 4 3)]))
    (is (= (-<>>< (+ 1 2)
                  (list <> 2 1)
                  (list 5 <> 7)
                  (list 9 4 <>)
                  (list 10 11))
           '[(3 2 1) (5 3 7) (9 4 3) (10 11 3)]))
    (is (= (-<>><:p (+ 1 2)
                    (list <> 2 1)
                    (list 5 <> 7)
                    (list 9 4 <>)
                    (list 10 11))
           '[(3 2 1) (5 3 7) (9 4 3) (10 11 3)])))

  (testing "parallel time"
    (let [time (Float.
                (str/replace
                 (with-out-str
                   (time (doall
                          (-<<:p
                           "<3 Discordia"
                           (do (Thread/sleep 1000))
                           (do (Thread/sleep 1000))
                           (do (Thread/sleep 1000))))))
                 #"[^\d\.]" ""))]
      (is (< 1000 time 1005))))

  (testing "sequential time"
    (let [time (Float.
                (str/replace
                 (with-out-str
                   (time (doall
                          (-<<
                           "<5 Eris"
                           (do (Thread/sleep 1000))
                           (do (Thread/sleep 1000))
                           (do (Thread/sleep 1000))))))
                 #"[^\d\.]" ""))]
      (is (< 3000 time 3005)))))

(deftest maybe-arrows
  (testing "null-safe swiss arrows"
    (is (nil? (some-<> "abc"
                       (if (string? "adf") nil <>)
                       (str <> " + more"))))
    (is (= (some-<> "abc"
                    (if (string? "adf") "some" <>)
                    (str <> " + more"))
           "some + more"))

    (is (nil? (some-<> "abc"
                       (if (string? "adf") nil <>)
                       (str " + more"))))
    (is (= (some-<> "abc"
                    (if (string? "adf") "some" <>)
                    (str " + more"))
           "some + more"))

    (is (nil? (some-<>> "abc"
                        (if (string? "adf") nil)
                        (str <> "+ more"))))
    (is (= (some-<>> "abc"
                     (if (string? "adf") "some")
                     (str <> "+ more"))
           "some+ more"))

    (is (= "abc" (some-<> "abc" identity)))

    (is (= "abc" (some-<>> "abc" identity)))

    (is (nil? (some-<> "abc" ((constantly nil)))))

    (is (nil? (some-<>> "abc" ((constantly nil)))))

    (is (nil? (some-<> "abc" ((constantly nil)) identity)))

    (is (nil? (some-<>> "abc" ((constantly nil)) identity)))



    (is (= "abc" (=<> maybe-m "abc" identity)))

    (is (= "abc" (=<> maybe-m "abc" identity identity)))

    (is (nil? (=<> maybe-m "abc" (constantly nil) identity)))

    (is (nil? (=<> maybe-m "abc" identity (constantly nil) identity)))

    (is (nil?
         (domonad maybe-m
                  [s "abc"
                   t0 (if (string? "adf") nil s)
                   t1 (str t0 " + more")]
                  t1)))

    (is nil? (=<> maybe-m "abc"
                  (fn [s] s)
                  (fn [s] (if (string? "adf") nil s))
                  (fn [s] (str s " + more"))
                  ))

    (is (nil?
         (with-monad maybe-m
           ((m-chain
             [(fn [s] s)
              (fn [s] (if (string? "adf") nil s))
              (fn [s] (str s " + more"))
              ])
            "abc"))))

    (is (= "some + more"
           (domonad maybe-m
                    [s "abc"
                     t0 (if (string? "adf") "some" s)
                     t1 (str t0 " + more")]
                    t1))))
  )

(deftest applicative
  (testing "applicative arrows"
    (is (= (apply->> [] +)
           0))
    (is (= (apply->> [1] +)
           1))
    (is (= (apply->> [1 2] +)
           3))
    (is (= (apply->> [[]] concat +)
           0))
    (is (= (apply->> [[1]] concat +)
           1))
    (is (= (apply->> [[1 2]] concat +)
           3))
    (is (= (apply->> [[1 2] [3 4]] concat +)
           10))
    (is (= (apply->> [[1 2] [3 4]] (concat [5 6]))
           [5 6 1 2 3 4]))
    (is (= (apply->> [[1 2] [3 4]] (concat [[5 6]]))
           [[5 6] 1 2 3 4]))
    (is (= (apply->> [[1 2] [3 4]] (concat [5 6]) (+))
           21))
    (is (= (apply->> [[1 2] [3 4]] (concat [5 6]) +)
           21))
    (is (= (apply-> [[1 2] [3 4]] concat +)
           10))
    (is (= (apply-> [1 2 3 4] (concat [[5 6]]))
           [1 2 3 4 5 6]))
    (is (= (apply-> [1 2 3 4] (concat [[5 6]]) (+))
           21))
    (is (= '()
           (apply-> [])))
    (is (= '()
           (apply-> [] concat)))
    (is (= '()
           (apply-> '())))
    (is (= '()
           (apply-> [] concat)))
    ))

(defn wostr* [f]            ; [<results of f>, <side effects to *out* from f>]
  [(f) (with-out-str (f))])

(defmacro wostr [& body]    ; syntactic sugar on wostr*
  `(wostr* (fn [] ~@body)))

(deftest non-updating
  (testing "non-updating arrows"
    (with-out-str
      (is (= (wostr (-!> {:foo "bar"} :foo prn))
             [{:foo "bar"}
              "\"bar\"\n"]))
      (is (= (wostr (-!>> {:foo "bar"} :foo (prn "foo")))
             [{:foo "bar"}
              "\"foo\" \"bar\"\n"]))
      (is (= (wostr (-!<> {:foo "bar"} :foo (prn "got" <> "here")))
             [{:foo "bar"}
              "\"got\" \"bar\" \"here\"\n"]))
      (is (= (wostr (-!<>> {:foo "bar"} :foo (prn "got" "here")))
             [{:foo "bar"}
              "\"got\" \"here\" \"bar\"\n"]))
      (is (= (wostr (-> {:foo "bar"}
                        (assoc :baz ["quux" "you"])
                        (-!> :baz second (prn "got here"))
                        (-!>> :baz second (prn "got here"))
                        (-!<> :baz second (prn "got" <> "here"))
                        (assoc :bar "foo")))
             [{:foo "bar"
               :baz ["quux" "you"]
               :bar "foo"}
              (str "\"you\" \"got here\"\n"
                   "\"got here\" \"you\"\n"
                   "\"got\" \"you\" \"here\"\n")])))))

;;;  __  __                       _ _
;;; |  \/  | ___  _ __   __ _  __| (_) ___
;;; | |\/| |/ _ \| '_ \ / _` |/ _` | |/ __|
;;; | |  | | (_) | | | | (_| | (_| | | (__
;;; |_|  |_|\___/|_| |_|\__,_|\__,_|_|\___|
;;;
;;;   ____                                _ _   _
;;;  / ___|___  _ __ ___  _ __   ___  ___(_) |_(_) ___  _ __
;;; | |   / _ \| '_ ` _ \| '_ \ / _ \/ __| | __| |/ _ \| '_ \
;;; | |__| (_) | | | | | | |_) | (_) \__ \ | |_| | (_) | | | |
;;;  \____\___/|_| |_| |_| .__/ \___/|___/_|\__|_|\___/|_| |_|
;;;                      |_|

;;; via http://bit.ly/18DrEKB

(defn- rng [seed]
  (let [m 259200
        v (/ (float seed) (float m))
        n (rem (+ 54773 (* 7141 seed)) m)]
    [v n]))

(defn- val-seq [f seed]
  (lazy-seq
   (let [[val next] (f seed)]
     (cons val (val-seq f next)))))

(defn- seq-sum [xs] (apply + xs))

(defn- seq-mean [xs] (/ (float (seq-sum xs)) (count xs)))

(defn- seq-variance [xs]
  (let [m (seq-mean xs)
        sq #(* % %)]
    (seq-mean (for [x xs] (sq (- x m))))))

;;; via http://bit.ly/1i7rvSP

(defn- roughly [v t slop] (and (>= v (- t slop)) (<= v (+ t slop))))

(defn- bumper  [v] (fn [s] [v (inc s)]))

(defn- summer  [v] (fn [s] [v (+ s v)]))

(defn- welford [v] (fn [s] [v (let [count    (inc (:count s))
                                   sum      (+ v (:sum s))
                                   old-mean (:mean s)
                                   ssq      (:sum-squared-residuals s)
                                   new-mean (/ (float sum) count)]
                               {:count count
                                :sum   sum
                                :mean  new-mean
                                :sum-squared-residuals
                                (+ ssq (* (- v old-mean) (- v new-mean)))
                                })]))

(def ^:private welford-zero
  {:count 0, :sum 0, :mean 0, :sum-squared-residuals 0})

(defn- run [initial-state fv2s2vs coll]
  (with-monad state-m
    (reduce
     (fn [acc val]
       (let [vs
             ((m-bind
               (m-result val) ; generate a state-monad item (fs2vs) from the val
               fv2s2vs)  ; shove it through a monadic function (fv2s2vs)
              acc)]      ; call the state-monad item (fs2vs) on acc
         (vs 1)          ; fetch the state portion out of the result
         ))              ; that becomes the new value of acc
     initial-state
     coll)))

;;; A faster and simpler alternative is to fold state extractor over a
;;; sequence of monadic values.

(defn- state-extractor [s fs2vs] (get (fs2vs s) 1))

(defn- run2 [zero monadic-statistic-constructor data]
  (reduce state-extractor
          zero
          (map monadic-statistic-constructor data)))

;;;   ___                              _    _      __   __          _
;;;  / __|___ _ __  _ __  ___ ___ __ _| |__| |___  \ \ / /__ _ _ __(_)___ _ _
;;; | (__/ _ \ '  \| '_ \/ _ (_-</ _` | '_ \ / -_)  \ V / -_) '_(_-< / _ \ ' \
;;;  \___\___/_|_|_| .__/\___/__/\__,_|_.__/_\___|   \_/\___|_| /__/_\___/_||_|
;;;                |_|

(defn- rng-c [state-map]
  (let [s (:seed state-map)
        m 259200
        v (/ (float s) (float m))
        n (rem (+ 54773 (* 7141 s)) m)]
    [v (assoc state-map :seed n)]))

(defn- summer-c [v] (fn [s] [v (assoc s :sum (+ v (:sum s)))]))

(defn- welford-c [v] (fn [s] [v (let [count    (inc (:count s))
                                   sum      (+ v (:sum s))
                                   old-mean (:mean s)
                                   ssq      (:sum-squared-residuals s)
                                   new-mean (/ (float sum) count)]
                               ;; "into" allows s to have other
                               ;; key-value pairs, thus to be be
                               ;; composable with other state-monadic
                               ;; values; it is MUCH slower than the
                               ;; non-composable version above.
                               (into s {:count count
                                        :sum   sum
                                        :mean  new-mean
                                        :sum-squared-residuals
                                        (+ ssq (* (- v old-mean) (- v new-mean)))
                                        }))]))

(def ^:private welford-zero-c
  {:count 0, :sum 0, :mean 0, :sum-squared-residuals 0, :seed 123456})

;;;  __  __                  _  __      __
;;; |  \/  |___ _ _  __ _ __| | \ \    / /_ _ _ _ _ __ _  _ _ __
;;; | |\/| / _ \ ' \/ _` / _` |  \ \/\/ / _` | '_| '  \ || | '_ \
;;; |_|  |_\___/_||_\__,_\__,_|   \_/\_/\__,_|_| |_|_|_\_,_| .__/
;;;                                                        |_|

(deftest monad-warmup
  (testing "basic monadic operators"
    (is (= (domonad sequence-m
                    [a (range 5)
                     b (range a)]
                    (* a b))
           (for [a (range 5), b (range a)] (* a b))))
    (with-monad set-m
      (is (= ((m-chain [(fn [v] #{v (* 2 v)})
                        (fn [v] #{v (* 3 v)})])
              1)
             #{1 2 3 6})))

    (let [data [42 43 44]
          scount   3
          smean   43.0
          ssqres   2.0
          ssum   129
          swelf  {:count scount
                  ,:sum ssum
                  ,:mean smean
                  ,:sum-squared-residuals ssqres}]

      (is (= scount (run 0 bumper data)))
      (is (= ssum   (run 0 summer data)))
      (is (= swelf  (run welford-zero welford data)))

      (is (= scount (run2 0 bumper data)))
      (is (= ssum   (run2 0 summer data)))
      (is (= swelf  (run2 welford-zero welford data)))
      )

    (is (roughly 1   1   0  ))
    (is (roughly 1.0 1.0 0.1))

;;; __   __          _                    __   _   _
;;; \ \ / /__ _ _ __(_)___ _ _  ___  ___ / _| | |_| |_  ___
;;;  \ V / -_) '_(_-< / _ \ ' \(_-< / _ \  _| |  _| ' \/ -_)
;;;   \_/\___|_| /__/_\___/_||_/__/ \___/_|    \__|_||_\___|
;;;
;;;   ___                 _
;;;  / __|__ _ _  _ _____(_)__ _ _ _
;;; | (_ / _` | || (_-<_-< / _` | ' \
;;;  \___\__,_|\_,_/__/__/_\__,_|_||_|

    (let ;; These all emulate a gaussian with an 11-th-order polynomial
         ;; via the central-limit theorem.

        [
         ;; fast
         gaussian1
         (domonad state-m [x1 rng,  x2 rng,  x3 rng,
                           x4 rng,  x5 rng,  x6 rng,
                           x7 rng,  x8 rng,  x9 rng,
                           x10 rng, x11 rng, x12 rng ]
                  (- (+ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) 6.0))

         ;; faster
         gaussian2
         (with-monad state-m
           ((m-lift 1 #(- % 6.0))
            (m-reduce + (repeat 12 rng))))

         ;; slow (about 3x slower than the above)
         gaussian3
         (fn [initial-seed]
           (let [s (-<> ((domonad state-m
                                  [_ (m-bind rng-c summer-c)
                                   _ (m-bind rng-c summer-c)
                                   _ (m-bind rng-c summer-c)

                                   _ (m-bind rng-c summer-c)
                                   _ (m-bind rng-c summer-c)
                                   _ (m-bind rng-c summer-c)

                                   _ (m-bind rng-c summer-c)
                                   _ (m-bind rng-c summer-c)
                                   _ (m-bind rng-c summer-c)

                                   _ (m-bind rng-c summer-c)
                                   _ (m-bind rng-c summer-c)
                                   _ (m-bind rng-c summer-c)
                                   ]
                                  _)
                         {:sum 0 :seed initial-seed})
                        (get 1))]
             [(- (:sum s) 6.0) (:seed s)]
             ))

         ;; slow (about 3x slower than gaussian1 and gaussian2
         gaussian4
         (fn [initial-seed]
           (let [mc (with-monad state-m
                      (m-chain
                       (repeat 12 (fn [_] (m-bind rng-c summer-c)))))
                 s (-<> ((mc nil) {:sum 0 :seed initial-seed})
                        (get 1))]
             [(- (:sum s) 6.0) (:seed s)]
             ))

         ]

      (let [g1 (time (nth (val-seq gaussian1 123456) 1000))
            g2 (time (nth (val-seq gaussian2 123456) 1000))
            g3 (time (nth (val-seq gaussian3 123456) 1000))
            g4 (time (nth (val-seq gaussian4 123456) 1000))]
        (is (= g1 g2))
        (is (= g1 g3))
        (is (= g1 g4))))

    (let [xs (with-monad state-m
               (let [gaussian3
                     ((m-lift 1 #(- % 6.0))
                      (m-reduce + (repeat 12 rng)))]
                 (take 1000 (val-seq gaussian3 123456))))]
      (is (roughly (seq-mean xs) 0 0.1))
      (is (roughly (seq-variance xs) 1 0.05))
      (let [s (time (run welford-zero welford xs))]
        (is (= 1000 (:count s)))
        (is (roughly (:mean s) 0 0.1))
        (is (roughly (/ (:sum-squared-residuals s) (dec (:count s))) 1 0.05)))
      (let [s (time (run2 welford-zero welford xs))]
        (is (= 1000 (:count s)))
        (is (roughly (:mean s) 0 0.1))
        (is (roughly (/ (:sum-squared-residuals s) (dec (:count s))) 1 0.05))))))
