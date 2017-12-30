(ns diffy.core
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [reagent.core :as r]
            [clojure.string :as str]
            [cljs.reader :as reader]
            ))
(comment
  "
  This follows the implementation presented in the SICP book:
    https://mitpress.mit.edu/sicp/full-text/sicp/book/node39.html
  dc/dx = 0
  dx/dx = 1
  d(u + v)/dx = du/dx + dv/dx
  d(uv)/dx = u(dv/dx) + v(dx/dx)
  ")

(enable-console-print!)
(defn log [s]
  (.log js/console s)
  )

(def variable? symbol?)

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

;; Helpful getters
(defn third [e]
  (nth e 2))

(defn get-op [e]
  (first e))

(defn get-arg1 [e]
  (second e))

(defn get-arg2 [e]
  (third e))

(defn make-sum [a b]
  ['+ a b])

(defn make-product [a b]
  ['* a b])

(def addend get-arg1)
(def augend get-arg2)
(def multiplier get-arg1)
(def multiplicand get-arg2)

(defn is-op? [e op]
  (and (list? e) (= (get-op e) op)))

(defn sum? [e] (is-op? e '+))
(defn product? [e] (is-op? e '*))

(defn deriv [exp var]
  "Take derivative of exp w.r.t. var. Makes some assumptions about exp being well formed."
  (cond
    (number? exp) 0

    (variable? exp)
    (if (same-variable? exp var) 1 0)

    (sum? exp)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var))

    (product? exp)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp)))

    :else nil))

(deftest test-deriv
  (is (= 0 (deriv 1 'x)))
  (is (= 1 (deriv 'x 'x)))
  (is (= 0 (deriv 'y 'x)))

  ;; Add
  (is (= '(+ 1 0) (deriv '(+ x 3) 'x)))
  (is (= '(+ 1 1) (deriv '(+ x x) 'x)))
  (is (= '(+ 0 0) (deriv '(+ x 3) 'y)))

  ;; Product
  (is (= '(+ (* x 0) (* 1 y)) (deriv '(* x y) 'x)))

  ;; Nested
  (is (= '(+ (* (* x y) (+ 1 0))
             (* (+ (* x 0) (* 1 y))
                (+  x 3)))
         (deriv '(* (* x y)
                    (+ x 3))
                'x))))

(deftest test-same-variable
  (is (= true (same-variable? 'x 'x)))
  (is (= false (same-variable? 'x 'y)))
  (is (= false (same-variable? '(1 + x) 'y)))
  (is (= false (same-variable? '1 '1))))

(deftest test-getters
  (is (= '+ (get-op '(+ 1 2))))
  (is (= 1 (get-arg1 '(+ 1 2))))
  (is (= 2 (get-arg2 '(+ 1 2)))))

(deftest test-sum-product?
  (is (= true (sum? '(+ 1 1))))
  (is (= false (sum? '+)))
  (is (= false (sum? '(* x 1))))
  (is (= false (sum? 'x)))

  (is (= true (product? '(* x 1)))))

;; (cljs.test/run-tests)
(println "We're running!")

(defn simple-component []
  [:div
   [:p "I am a component!"]
   [:p.someclass
    "I have " [:strong "bold"]
    [:span {:style {:color "red"}} " and red "] "text."]])

(def equation (r/atom "+ x (* x y)"))
(def variable (r/atom "x"))
(def derivative (r/atom "1"))


(defn simplify [e]
  (do
    (cond
      (= (count e) 1)
      (first e)

      (= (count e) 3)
      e

      :else
      (do
        (log "Simplify error?")
        e)
      )))

(defn parse-infix [s]
  (let [wrapped (str "(" s ")")
        parsed (simplify (reader/read-string wrapped))]
    parsed
    )
  )

(defn derivative-block [s]
                  [:span (str (deriv (parse-infix @equation) (symbol @variable)))])

(defn calculator []
  [:div
   [:span " Derivative of " ]
   [:input {:type "text"
            :value @equation
            ;; :on-change #(swap! equation %)
            :on-change #(reset! equation (-> % .-target .-value))
            }]
   [:span " w.r.t. " ]
   [:input {:type "text"
            :value @variable
            ;; :on-change #(swap! equation %)
            :on-change #(reset! variable (-> % .-target .-value))
            }]
   [:p [:span " = "]
    ;; Parse the equation and take the derivative
    [derivative-block]
   ]])

(r/render [calculator]
          (.getElementById js/document "app"))



;; 1. Input
;; 2. Parse string to infix notation
;; 3. Apply derivative
;; 4. Show derivative
