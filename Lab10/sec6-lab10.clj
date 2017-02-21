(defn error [msg] (throw (Exception. msg)))

(def queue
  (fn []
    (letfn
      [(make-queue [head]
         (fn [method & args]
           (letfn
             [(empty? [] (= head '()))
              (enqueue [e]
                (make-queue (concat head (list e))))
              (dequeue []
                (if (empty?) (error "Queue empty") (make-queue (rest head))))
              (front []
                (if (empty?) (error "Queue empty") (first head)))]
              (cond
                (= method :empty?) (empty?)
                (= method :enqueue) (enqueue (first args))
                (= method :dequeue) (dequeue)
                (= method :front) (front)))))]
      (make-queue '()))))
