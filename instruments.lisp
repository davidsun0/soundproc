;;; Instruments

(defun bell (base-freq &optional (hcount 20))
  (apply #'stream-add
	 (loop for i from 1 to hcount
	       collect
	       (let ((freq (->> (+ 0.8 (random 0.4))
				(* i base-freq))))
		 (amplify (->> (/ (* *sample-rate* 800) freq)
				 (exp-decay)
				 (amplify 10))
			    (sine-stream freq))))))

(defmacro partials ()
  "Compile time series of random partial frequencies"
  (cons 'list
	(loop for i from 2 to 20
	      collect
					;(+ i (- (random 0.1) 0.05)
	      (* i (+ 0.8 (random 0.4)))
		 )))

(defun bell-2 (base-freq)
  (->> (cons (sine-stream base-freq)
	     (loop for p in (partials)
		   collect (sine-stream (* p base-freq))))
       (apply #'stream-add)
       (amplify 8)
       (low-sweep (exp-decay 6000))))

(defun glass-harp (base-freq)
  (amplify 20
  (amplify (stream-add (const-stream 1.0)
		       (sine-stream 2))
	   (sine-stream base-freq))))

(defun pluck (freq)
  (effect-delay (lambda (stream) (low-sweep (exp-decay 500)
					    stream))
		100
		(envelope '(0 100 30)
			  (square-stream freq))))

(defun pluck-1 (freq)
  (->> (saw-stream freq)
       ;; TODO: base constants on sample rate (8000)
       (low-sweep (exp-decay 600))
       ;; low-sweep doesn't reset to zero amplitude
       (envelope '(0 100 30 6900 30 1000 0))))

(defun pluck-2 (freq)
  (->> (square-stream freq)
       (envelope (list 0 (* *sample-rate* 0.01) 10
		       (* *sample-rate* 0.24) 10 1 0))))
