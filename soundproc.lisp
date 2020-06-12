;;; Utilities

(defmacro ->> (form1 &rest forms)
  "Clojure's thread last macro: pipes values by nesting expressions.
  For example, (->> r (square) (* pi)) calculates the area of a circle."
  (loop for f in (cons form1 forms)
	for acc = f then (append f (list acc))
	finally (return acc)))

(defmacro -> (form1 &rest forms)
  "Clojure's thread first macro: pipes values by nesting expressions."
  (loop for f in (cons form1 forms)
	for acc = f then (cons (first f)
			       (cons acc (rest f)))
	finally (return acc)))

(defun write-int (value bytes file)
  "Writes the lower n bytes of an integer to file in little endian."
  (loop for x from 0 to (- bytes 1)
        do (write-byte (logand (ash value (* x -8)) #xFF) file)))

;;; Synthesis settings

(defparameter *num-channels* 1) ; streams currently only support one channel
(defparameter *sample-rate* 8000)
(defparameter *sample-depth-bytes* 1)

(defun write-wave (sound-stream sample-count
		   &optional (filename "./out.wav"))
  "Writes n samples of a sound stream to WAVE file.
   Writes a lazy stream to file as a .WAV file in PCM format.
   Does not currently write metadata - this can be extended by including an
   INFO chunk.
   http://soundfile.sapp.org/doc/WaveFormat/"
  (let ((sample-size (* *num-channels* *sample-depth-bytes*)))
    (with-open-file (out filename
                         :direction :output
                         :element-type 'unsigned-byte
                         :if-exists :supersede)
      ;; RIFF HEADER
      (loop for i across "RIFF"
            do (write-byte (char-code i) out))
      ;; file size in bytes - 8
      (write-int (+ 44 (* sample-size sample-count)) 4 out)
      (loop for i across "WAVE"
            do (write-byte (char-code i) out))
      ;; WAVE HEADER
      (loop for i across "fmt "
            do (write-byte (char-code i) out))
      (write-int 16 4 out)                ; size of wave header
      (write-int 1 2 out)                 ; 1 = PCM
      (write-int *num-channels* 2 out)
      (write-int *sample-rate* 4 out)
      (write-int (* *sample-rate* sample-size) 4 out) ; byte rate
      (write-int sample-size 2 out)
      (write-int (* 8 *sample-depth-bytes*) 2 out)      ; bits per sample
      ;; DATA CHUNK
      (loop for i across "data"
            do (write-byte (char-code i) out))
      ;; size of following data
      (write-int (* sample-size sample-count) 4 out)
      ;; dump samples to disk
      (loop for i from 1 to sample-count
            for s = sound-stream then (l-cdr s)
            do (write-int (->>
			   ;; center on middle PCM value to prevent popping
			   (* *sample-depth-bytes* #x100 0.5)
			   (+ (car s))	; add stream sample value
			   (round)) ; quantize to integer
                          sample-size
                          out)))))

(defun l-cdr (stream)
  "Lazily evaluates the cdr of a lazy stream."
  (funcall (cdr stream)))

(defmacro l-cons (l-car l-cdr)
  "Creates a cons cell where the cdr is lazily realized via l-cdr."
  `(cons ,l-car (lambda () ,l-cdr)))

(defmacro l-stream (arg-binds body)
  "Unhygenic macro that creates a lazy stream: exposes 'self.
  Similar to the alambda anaphoric macro.
  Creates a lazy stream where arguments are bound to initial values
  and anonymous recursion is available through the self symbol."
  (let ((arg-syms (mapcar #'first  arg-binds))
	(arg-vals (mapcar #'second arg-binds)))
    `(labels ((self ,arg-syms ,body))
       (self ,@arg-vals))))

;; TODO: replace l-stream with nlet?
(defmacro nlet (name arg-binds &rest body)
  "Scheme's named let: defines a function with names, arguments and
  initial values, and a function body.
  Named recursion is allowed within the function body."
  (let ((arg-syms (mapcar #'first  arg-binds))
	(arg-vals (mapcar #'second arg-binds)))
    `(labels ((,name ,arg-syms ,@body))
       (,name ,arg-vals))))

(defun const-stream (&optional (value 0))
  "Generates a sample stream of silence."
  ;; cache the cons cell to save on CPU and memory
  (let ((l-cell (cons value nil)))
    ;; create self-referential lazy cons cell
    (setf (cdr l-cell) (lambda () l-cell))
    l-cell))

(defun as-stream (value)
  "Converts constant values into constant streams.
  Assumes that all cons cell inputs are lazy streams."
  (if (consp value)
      value
      (const-stream value)))

;;; Basic Soundwaves
;; All basic waveforms have range [-1, 1]

(defun sine-stream (freq)
  "Generates a lazy stream of sine wave samples."
  (l-stream ((freq-s (as-stream freq)) (value 0))
    (l-cons (sin value)
	    (self (l-cdr freq-s)
		  (-> (car freq-s)
		      (* 2 pi)
		      (/ *sample-rate*)
		      (+ value))))))

(defun square-stream (freq &optional (duty-cycle 0.5))
  "Generates a lazy stream of square wave samples."
  (l-stream ((freq-s (as-stream freq))
	     (duty-s (as-stream duty-cycle))
	     (value 0))
    (l-cons (if (< value (car duty-s)) -1 1)
	    (self (l-cdr freq-s)
		  (l-cdr duty-s)
		  (let ((incr (/ (car freq-s) *sample-rate*)))
		    (if (<= (+ value incr) 1)
			(+ value incr)
			incr))))))

(defun saw-stream (freq)
  "Generates a lazy stream of rising sawtooth wave samples."
  (l-stream ((freq-s (as-stream freq)) (value 0))
    (l-cons value
	    (self (l-cdr freq-s)
		  (let ((incr (-> (car freq-s)
				  (/ *sample-rate*)
				  (* 2))))
		    (if (< (+ value incr) 1)
			(+ value incr)
			(+ -1 incr)))))))

(defun white-noise (&optional (granularity 16))
  "Generates an approximation of white noise.
  Granularity is the n parameter to the Bates distribution:
  higher values better approximate white noise.
  The defining property of white noise is that it contains all frequencies,
  all with the same amplitude. This is computationally equivalent to
  generating Gaussian-distributed random samples. The Bates Distributtion
  is used as a replacement because not all properties of the Gaussian
  Distribution are desired or needed e.g. infinite range."
  ;; Average N uniformly random variables
  ;; By the Central Limit Theorem, this approximates the Gaussian
  (l-cons (-> (loop for i from 1 to granularity
		    sum (random 2.0))
	      (/ granularity)
	      ;; center range on 0 and nomralize variance
	      (- 1)
	      (* (sqrt granularity)))
	  (white-noise granularity)))

;;; Non-auditory simple streams

(defun exp-decay (half-life)
  "Creates an exponential decay stream whose value starts at one and has
  a half life equal to the argument (in samples)."
  (let ((rate (exp (/ (log 0.5) half-life))))
    (l-stream ((value 1))
      (l-cons value (self (* value rate))))))

(defun sine-range (low high freq)
  "Convenience function for defining a sine wave with range from low to high
  (inclusive) and the given frequency."
  (stream-add (const-stream (+ low (/ (- high low) 2)))
	      (amplify (/ (- high low) 2)
		       (sine-stream freq))))

;;; Stream Filters

(defun stream-add (&rest stream-list)
  "Mixes several streams together by sample addition."
  (l-stream ((streams stream-list))
    (l-cons (reduce #'+ (mapcar #'car streams))
	    (self (mapcar #'l-cdr streams)))))

(defun amplify (gain stream)
  "Amplifies the input stream by the gain factor."
  (l-stream ((gain-s (as-stream gain)) (stream stream))
	    (l-cons (* (car gain-s) (car stream))
		    (self (l-cdr gain-s)
			  (l-cdr stream)))))

(defun stream-delay (delay stream)
  "Delays a stream for the given number of samples.
  The value zero is returned until the wait time is over."
  (if (> delay 0)
      (l-cons 0 (stream-delay (- delay 1) stream))
      stream))

(defun low-pass (alpha stream)
  "Applies a low pass filter on the stream with dampening factor alpha.
  An alpha value of 1 does not change the input stream and a value of 0
  discards all frequencies. Alpha values outside of [0, 1] are not well
  defined."
  ;; https://en.wikipedia.org/wiki/Low-pass_filter#Simple_infinite_impulse_response_filter
  (l-stream ((value 0) (stream stream))
    (let ((new-value (+ (* alpha (car stream))
			(* (- 1 alpha) value))))
      (l-cons new-value (self new-value (l-cdr stream))))))

(defun low-sweep (alpha stream)
  (l-stream ((alpha (as-stream alpha)) (stream stream) (value 0))
    (let ((new-value (+ (* (car alpha) (car stream))
			(* (- 1 (car alpha)) value))))
	(l-cons new-value (self (l-cdr alpha)
				(l-cdr stream)
				new-value)))))

(defun high-pass (alpha stream)
  "Applies a high pass filter on the stream with factor alpha."
  ;; https://en.wikipedia.org/wiki/High-pass_filter#Algorithmic_implementation
  (l-stream ((value 0)
	     (prev-sample (car stream))
	     (stream (l-cdr stream)))
    (let ((new-value (* alpha (+ value (- (car stream) prev-sample)))))
	(l-cons new-value (self new-value
				(car stream)
				(l-cdr stream))))))

(defun piecewise (&rest vals-and-delays)
  "Creates a linear peicewise function from alternating value and delay
  values.
  For example, (piecewise 0 1000 200 500 40) creates a stream that starts
  at value 0, rises to 200 over 1000 samples, falls to 40 over 500 samples,
  and finally holds constant at 40 indefinitely."
  (assert (oddp (length vals-and-delays)))
  (let ((vals   (loop for v on vals-and-delays by #'cddr
		      collect (car v)))
	(delays (loop for p on (cdr vals-and-delays) by #'cddr
		      collect (car p))))
    (l-stream ((vals vals) (delays delays) (value 0))
      (if delays
	  (l-cons (-> (- (second vals) (first vals))
		      (/ (first delays))
		      (* value)
		      (+ (first vals)))
		  (if (>= value (first delays))
		      (self (rest vals) (rest delays) 0)
		      (self vals delays (+ value 1))))
	  (const-stream (first vals))))))

(defun envelope (amps-and-timers stream)
  "Creates an envelope from alternating gain and timing values.
  See piecewise on how to specify the gain and timings."
  (amplify (apply #'piecewise amps-and-timers)
	   stream))

;; Convolutions

(defun convolve (convolution stream)
  (l-stream ((conv-list convolution)
	     ;; create a circular buffer of samples
	     (samp-list (let ((circular-list
				(make-list
				 (length convolution)
				 :initial-element 0)))
			  (setf (cdr (last circular-list))
				circular-list)
			  circular-list))
	     (stream stream))
    ;; value could be optimized by subtracting
    ;; (* (first conv-list) (first samp-list))
    ;; and adding
    ;; (* (last conv-list) (last samp-list))
    ;; at each step
    (l-cons (loop for i in conv-list
		  for j in samp-list
		  sum (* i j))
	    (progn (setf (car samp-list) (car stream))
		   (self conv-list
			 (cdr samp-list)
			 (l-cdr stream))))))

(defun sinc (theta)
  (if (= theta 0)
      1
      (/ (sin theta) theta)))

;;; Transducer functionality (currently unused)

(defmacro deffilter (name args body)
  "Defines a filter as a transducer if a stream argument is not provided."
  `(defun ,name (,@ args &optional stream)
    (if stream
       ,body
       (lambda (stream)
         (,name ,@args stream)))))

(defun compose (filter &rest filters)
  "Composes several transducers into one."
  (lambda (stream)
    (loop for f in (cons filter filters)
	  for s = stream then (funcall f s)
	  finally (return s))))

(defun effect-delay (effect delay stream)
  "Waits a number of samples before applying an effect to the stream."
  (if (> delay 0)
      (l-cons (car stream)
	      (effect-delay effect (- delay 1) (l-cdr stream)))
      (funcall effect stream)))
