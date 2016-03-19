(ns project-euler.core
  (:require [clojure.math.numeric-tower :as math]))

(defn multiple-of?
  "Return true if x is a multiple of y"
  [x y]
  (= 0 (rem y x)))

(def multiple-of3? #(multiple-of? 3 %))
(def multiple-of5? #(multiple-of? 5 %))

(defn problem1
  "Return sum of multiples of 3 and 5 less than x"
  [x]
  (reduce + (filter #(or (multiple-of3? %) (multiple-of5? %)) (range x))))

(def square #(math/expt % 2))

(defn sum-of-squares
  "Return the sum of all squares from 1 through x"
  [x]
  (reduce + (map square (range (inc x)))))

(defn square-of-sums
  "Return the sum of all square from 1 through x"
  [x]
  (square (reduce + (range (inc x)))))

(defn problem6
  "Return the difference between the square of sums and the sum of squares from 1 through x"
  [x]
  (- (square-of-sums x) (sum-of-squares x)))

(defn is-factor?
  "Return whether x is a factor of y"
  [x y]
  (= 0 (rem y x)))

(defn unmemoized-factor-list
  "Return an unmemoized lazy seq of factors of x"
  [x]
  (let [low-factors (lazy-seq
                     (filter
                      #(is-factor? % x)
                      (range 1 (inc (math/sqrt (inc x))))))
        high-factors (lazy-seq
                      (map
                       #(/ x %)
                       low-factors))]
    (lazy-cat low-factors high-factors)))

(def factor-list (memoize unmemoized-factor-list))

(defn is-prime?
  "Return true if x is prime"
  [x]
  (let [a (first (factor-list x))
        b (second (factor-list x))]
       (= (* a b) x)))

(defn problem3
  "Return the largest prime factor of x"
  [x]
  (first (reverse (filter is-prime? (factor-list x)))))

(defn fib-seq
  "Return a lazy Fibonacci sequence"
  []
  ((fn rfib [a b] 
     (cons a (lazy-seq (rfib b (+ a b)))))
   1 2))

(defn problem2
  "Return the sum of even Fibonacci numbers less than x. Note that the Fibonacci can only be 90 numbers."
  [x]
  (reduce +
          (filter even?
                  (filter #(< % x)
                          (take 90 (fib-seq))))))

(defn palindrome?
  "Determine if a number is a palindrome"
  [x]
  (= (seq (str x)) (reverse (seq (str x)))))

(defn problem4
  "Return the biggest palindrome created by multiplying all numbers between x and y"
  [x y]
  (apply max (filter palindrome?
               (set
                (for [x1 (range x y)
                      x2 (range x y)]
                  (* x1 x2))))))

(defn divisible?
  "Determine if y is divisible by x"
  [x y]
  (= 0 (rem x y)))

(defn evenly-divisible?
  "Determine if y is evenly divisible by x"
  [x y]
  (and (divisible? x y)
       (even? (quot x y))))

(defn evenly-divisible-by-range?
  "Determine if x is evenly divisible by all numbers in range min to max (inclusive)"
  [x min max]
  (= (range min (inc max))
     (for [y (range min (inc max))
           :when (evenly-divisible? x y)] y)))

(defn problem5-wrong
  "Find the smallest number evenly divisible by all numbers in the range of x to y (inclusive)"
  [x y]
  (second (filter
          #(evenly-divisible-by-range? % x y)
          (range))))

(defn gcd
  "Return the greatest common divisor of x and y"
  [x y]
  (if (zero? y) x
      (recur y, (rem x y))))

(defn lcm
  "Return the least common multiple of x and y"
  [x y]
  (/ (* x y) (gcd x y)))

(defn lcmv
  "Return the least common multiple of a list"
  [& v]
  (reduce lcm v))

(defn problem5
  "Find the smallest number evenly divisible by all numbers in the range of x to y (inclusive)"
  [x y]
  (apply lcmv (range x (inc y))))

(defn problem7
  "Find the nth prime"
  [n]
  (nth (filter is-prime? (range)) (inc n)))

(def problem8-string "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")

(defn str->intseq
  "Turn a string of numbers into a seq of single digit ints"
  [str]
  (map #(- (int %) (int \0)) (seq str)))

(defn multiply-nextn
  "Multiply the next n numbers in a collection"
  [n coll]
  (reduce * (take n coll)))

(defn problem8
  "Find the maximum product of n digits in a long string of numbers."
  [n]
  (apply max
         (loop [num (str->intseq problem8-string)
                maxes []]
           (if (< (count num) n) maxes
               (recur (rest num)
                      (conj maxes
                            (multiply-nextn n num)))))))

(defn pythagorean-triplet?
  "Determine if a, b and c form a Pythagorean triplet"
  [a b c]
  (and (< a b c)
       (pos? a)
       (pos? b)
       (pos? c)
       (= (+ (square a) (square b)) (square c))))

(defn pythagorean-triplets-with-sum
  "Find Pythagorean triplets with a specific sum"
  [x]
  (for [a (range 1 (inc x))
        b (range 1 (inc x))
        c (range 1 (inc x))
        :when (and (= x (+ a b c))
                   (pythagorean-triplet? a b c))]
    [a b c]))

(defn problem9
  "Find a Pythagorean triplet that sums to x and return its product"
  [x]
  (apply * (first (pythagorean-triplets-with-sum x))))

(defn primes-below-n
  "Return a collection of primes below n"
  [n]
  (filter is-prime? (range 2 (inc n))))

(defn problem10
  "Return the sum of all primes below n"
  [n]
  (reduce + (primes-below-n n)))
