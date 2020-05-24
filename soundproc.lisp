;;; Utilities

(defmacro ->> (form1 &rest forms)
  "Clojure's thread last macro: pipes values by nesting expressions.
  For example, (->> r (square) (* pi)) calculates the area of a circle."
  (loop for f in (cons form1 forms)
	for acc = f then (append f (list acc))
	finally (return acc)))

(defun write-int (value bytes file)
  "Writes the lower n bytes of an integer to file in little endian."
  (loop for x from 0 to (- bytes 1)
        do (write-byte (logand (ash value (* x -8)) #xFF) file)))

;;; Synthesis settings

(defparameter *num-channels* 1) ; streams currently only support one channel
(defparameter *sample-rate* 8000)
(defparameter *sample-depth-bytes* 1)

;; Writes a lazy stream to file as a .WAV file in PCM format.
;; Does not currently write metadata - this can be extended by including an
;; INFO chunk.
;; http://soundfile.sapp.org/doc/WaveFormat/
(defun write-wave (sound-stream sample-count
		   &optional (filename "./out.wav"))
  "Writes n samples of a sound stream to WAVE file."
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

;; The defining property of white noise is that it contains all frequencies,
;; all with the same amplitude. This is computationally equivalent to
;; generating Gaussian-distributed random samples. The Bates Distributtion
;; is used as a replacement because not all properties of the Gaussian
;; Distribution are desired or needed e.g. infinite range.
(defun white-noise (&optional (granularity 16))
  "Generates an approximation of white noise.
  Granularity is the n parameter to the Bates distribution:
  higher values better approximate white noise."
  (l-cons (->>
	   ;; Average N uniformly random variables
	   ;; By the Central Limit Theorem, this approximates the Gaussian
	   (/ (loop for i from 1 to granularity
		    sum (random 2.0))
	      granularity)
	   (* (sqrt granularity))   ; normalize variance (i.e. volume)
	   (+ -1))	            ; center range on 0 to match other waves
	  (white-noise granularity)))

(defun saw-stream (freq)
  "Generates a lazy stream of rising sawtooth wave samples."
  (let ((incr (/ freq *sample-rate*)))
    (l-stream ((value 0))
       (l-cons (- value 1)
	       (self (if (< (+ value incr) 2)
			 (+ value incr)
			 incr))))))

(defun square-stream (freq &optional (duty-cycle 0.5))
  "Generates a lazy stream of square wave samples."
  (let ((incr (/ freq *sample-rate*)))
    (l-stream ((value 0))
      (l-cons (if (< value duty-cycle) -1 1)
	      (self (if (< (+ value incr) 1)
		      (+ value incr)
		      incr))))))

(defun d-square (freq duty-cycle)
  (l-stream ((freq freq) (duty duty-cycle) (value 0))
    (l-cons (if (< value (car duty)) -1 1)
	    (self (l-cdr freq)
		  (l-cdr duty)
		  (let ((next-value (+ value (/ (car freq)
						*sample-rate*))))
		    (if (< next-value 1)
			next-value
			0))))))

(defun pseudo-write (stream samples)
  (loop for i from 1 to samples
	for s = stream then (l-cdr s)))

(defun sine-stream (freq)
  "Generates a lazy stream of sine wave samples."
  (let ((incr (/ (* freq 2 pi) *sample-rate*)))
    (l-stream ((value 0))
      (l-cons (sin value) (self (+ value incr))))))

;;; Non-auditory simple streams

(defun exp-decay (half-life)
  (let ((rate (exp (/ (log 0.5) half-life))))
    (l-stream ((value 1))
      (l-cons value (self (* value rate))))))

;;; Frequency modulated streams

(defun fm-sine (stream)
  (l-stream ((value 0) (stream stream))
    (l-cons (sin value)
	    (self (+ value (/ (* (car stream) 2 pi) *sample-rate*))
		  (l-cdr stream)))))

(defun fm-saw (stream)
  (l-stream ((value 0) (stream stream))
    (l-cons (+ value -0.5)
	    (let ((next-value (+ value (/ (car stream)
					    *sample-rate*))))
		(self (if (<= next-value 1) next-value 0)
		      (l-cdr stream))))))

(defun fm-square (duty-cycle stream)
  (l-stream ((value 0) (stream stream))
	    (l-cons (if (> value duty-cycle) 1 -1)
		    (let ((next-value (+ value (/ (car stream)
						  *sample-rate*))))
		      (self (if (<= next-value 1) next-value 0)
			    (l-cdr stream))))))

;;; Stream Filters

(defun stream-add (&rest stream-list)
  "Mixes several streams together by sample addition."
  (l-stream ((streams stream-list))
    (l-cons (reduce #'+ (mapcar #'car streams))
	    (self (mapcar #'l-cdr streams)))))

(defun amplify (gain stream)
  "Amplifies the input stream by the gain factor."
  (l-cons (* gain (car stream))
	  (amplify gain (l-cdr stream))))

(defun d-amplify (gain-stream stream)
  "Amplifies the input stream by a factor of the gain stream."
  (l-cons (* (car gain-stream) (car stream))
	  (d-amplify (l-cdr gain-stream)
		     (l-cdr stream))))

(defun stream-delay (delay stream)
  "Delays a stream for the given number of samples.
  The value zero is returned until the wait time is over."
  (if (> delay 0)
      (l-cons 0 (stream-delay (- delay 1) stream))
      stream))

(defun effect-delay (effect delay stream)
  "Waits a number of samples before applying an effect to the stream."
  (if (> delay 0)
      (l-cons (car stream)
	      (effect-delay effect (- delay 1) (l-cdr stream)))
      (funcall effect stream)))

(defun low-pass (alpha stream)
  "Applies a low pass filter on the stream with dampening factor alpha."
  ;; https://en.wikipedia.org/wiki/Low-pass_filter#Simple_infinite_impulse_response_filter
  (l-stream ((value 0) (stream stream))
    (let ((new-value (+ (* alpha (car stream))
			(* (- 1 alpha) value))))
	(l-cons new-value (self new-value (l-cdr stream))))))

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

(defun envelope (amps-and-timers stream)
  "creates an envelope from alternating gain and timing values
  for example, (envelope '(0 100 3 400 0) stream) starts off silent,
  then gradually amplifies the stream to 3x in 100 samples,
  and finally gradually decreases the gain to 0 over another 400 samples"
  (assert (oddp (length amps-and-timers)))
  (destructuring-bind
      (amps timers)
      (loop for (l r) on amps-and-timers by #'cddr
	    collect l into left
	    when r
	      collect r into right
	    finally (return (list left right)))
    ;; value keeps track of samples passed (compared to timers)
    (l-stream ((amps amps) (timers timers) (value 0) (stream stream))
      (if timers
	  (l-cons (let ((amp1 (first amps))
		      (amp2 (second amps)))
		  (->> (/ (- amp2 amp1) (first timers))
			  (* value)
			  (+ amp1)
			  (* (car stream))))
		  (if (>= value (first timers))
		      (self (rest amps) (rest timers) 0 (l-cdr stream))
		      (self amps timers (+ 1 value) (l-cdr stream))))
	  (amplify (first amps) stream)))))

(defmacro def-filter (name args body)
  "Defines a filter as a transducer if a stream argument is not provided."
  (let ((args-stream (append args '(&optional stream))))
    `(defun ,name ,args-stream
       (if stream
	   ,body
	   (lambda (stream)
	     (,name ,@args stream))))))

(defun compose (filter &rest filters)
  "Composes several transducers into one."
  (lambda (stream)
    (loop for f in (cons filter filters)
	  for s = stream then (funcall f s)
	  finally (return s))))

(defun bell (base-freq &optional (hcount 20))
  (apply #'stream-add
	 (loop for i from 1 to hcount
	       collect
	       (let ((freq (->> (+ 0.95 (random 0.1))
				(* i base-freq))))
		 (envelope (list (/ hcount i)
				 ;; fade out based on frequency
				 (/ (* *sample-rate* 5 440) freq)
				 0)
			   (sine-stream freq))))))

(defun bell (base-freq &optional (hcount 20))
  (apply #'stream-add
	 (loop for i from 1 to hcount
	       collect
	       (let ((freq (->> (+ 0.90 (random 0.2))
				(* i base-freq))))
		 (d-amplify (->> (/ (* *sample-rate* 800) freq)
				 (exp-decay)
				 (amplify 10))
			    (sine-stream freq))))))

(defun simple-bell (base-freq)
  (->> (loop for freq in '(0.56 0.92 1.19 1.71 2.0 2.74 3 3.76 4.07)
	     collect (sine-stream (* base-freq freq)))
       (mapcar (lambda (s) (envelope '(8 48000 0) s)))
       (apply #'stream-add)))

(defun glass-harp (base-freq)
  (amplify 20
  (d-amplify (stream-add (const-stream 1.0)
			 (sine-stream 2))
	     (sine-stream base-freq))))
