(defvar *num-channels* 1)
(defvar *sample-rate* 8000) ; samples per second
(defvar *bytes-per-sample* 1) ; change to sample-depth-bytes ?

(defun write-int (value bytes file)
  "writes the lower n bytes of an integer to file in little endian"
  (loop for x from 0 to (- bytes 1)
        do (write-byte (logand (ash value (* x -8)) #xFF) file)))

(defun write-wave (sound-stream sample-count &optional (filename "./out.wav"))
  "writes n samples of a sound stream to WAVE file"
  (let ((sample-size (* *num-channels* *bytes-per-sample*)))
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
      (write-int (* 8 *bytes-per-sample*) 2 out)      ; bits per sample
      ;; DATA CHUNK
      (loop for i across "data"
            do (write-byte (char-code i) out))
      ;; size of following data
      (write-int (* sample-size sample-count) 4 out)
      ;; dump samples to disk
      (loop for i from 1 to sample-count
            for s = sound-stream then (l-cdr s)
            do (write-int (round (+ (car s)
                                    (* *bytes-per-sample* #x100 0.5)))
                          sample-size
                          out)))))

(defun take (n stream)
  "realizes n values of a lazy stream as a list"
  (loop for i from 1 to n
        for s = stream then (l-cdr s)
        collect (car s)))

(defun l-cdr (stream)
  "lazily evaluates the cdr of a lazy stream"
  (funcall (cdr stream)))

(defmacro l-cons (l-car l-cdr)
  "creates a cons cell where the cdr is lazily evaluated via l-cdr"
  `(cons ,l-car (lambda () ,l-cdr)))

(defmacro l-stream (arg-binds body)
  "creates a lazy stream
  acts like lambda, but arguments are bound to initial values
  and anonymous recursion is available through the self symbol"
  (let ((arg-syms (mapcar #'first  arg-binds))
	(arg-vals (mapcar #'second arg-binds)))
    `(labels ((self ,arg-syms ,body))
       (self ,@arg-vals))))

;; BASE STREAMS

(defun silent-stream ()
  "generates a sample stream of silence"
  (cons 0 #'silent-stream))

(defun saw-stream (freq)
  "generates a lazy stream of rising sawtooth wave samples"
  (let ((incr (/ freq *sample-rate*)))
    (l-stream ((value 0))
       (l-cons (- value 0.5)
	       (self (if (< (+ value incr) 1)
			 (+ value incr)
			 incr))))))

(defun square-stream (freq &optional (duty-cycle 0.5))
  "generates a lazy stream of square wave samples"
  (let ((incr (/ freq *sample-rate*)))
    (l-stream ((value 0))
      (l-cons (if (< value duty-cycle) -0.5 0.5)
	      (self (if (< (+ value incr) 1)
		      (+ value incr)
		      incr))))))

(defun sine-stream (freq)
  "generates a lazy stream of sine wave samples"
  (let ((incr (/ (* freq 2 pi) *sample-rate*)))
    (l-stream ((value 0))
      (l-cons (sin value) (self (+ value incr))))))

(defun noise-stream (&optional (granularity 16))
  "generates an approximation of white noise
  granularity is the n parameter to the Bates distribution - 
  higher values better approximate white noise"
  (l-cons (- (/ (loop for i from 1 to granularity
		      sum (random 1.0))
		granularity)
	     0.5)
	  (noise-stream granularity)))

;; HIGHER ORDER OPERATIONS

(defun amplify (gain stream)
  "amplifies the input stream by the gain factor"
  (l-cons (* gain (car stream))
	  (amplify gain (l-cdr stream))))

;; this function is in dire need of a better name
(defun change-volume-linearly (prev-gain next-gain timer stream)
  (let ((incr (/ (- next-gain prev-gain) timer)))
    (l-stream ((gain prev-gain) (stream stream))
	      (if (< (+ gain incr) next-gain)
		  (l-cons (* gain (car stream))
			  (self (+ gain incr) (l-cdr stream)))
		  (amplify next-gain stream)))))

(defun stream-add (&rest stream-list)
  "mixes several streams together by sample addition"
  (l-stream ((streams stream-list))
    (l-cons (reduce #'+ (mapcar #'car streams))
	    (self (mapcar #'l-cdr streams)))))

;; TODO: delay in seconds, not samples?
(defun stream-delay (delay stream)
  (if (> delay 0)
      (l-cons 0 (stream-delay (- delay 1) stream))))

(defun low-pass (alpha stream)
  "applies a low pass filter on the stream with dampening factor alpha"
  ;; https://en.wikipedia.org/wiki/Low-pass_filter#Simple_infinite_impulse_response_filter
  (l-stream ((value 0) (stream stream))
    (let ((new-value (+ (* alpha (car stream))
			 (* (- 1 alpha) value))))
	(l-cons new-value (self new-value (l-cdr stream))))))

(defun high-pass (alpha stream)
  "applies a high pass filter on the stream with factor alpha"
  ;; https://en.wikipedia.org/wiki/High-pass_filter#Algorithmic_implementation
  (l-stream ((value 0)
	     (prev-sample (car stream))
	     (stream (l-cdr stream)))
    (let ((new-value (* alpha (+ value (- (car stream) prev-sample)))))
	(l-cons new-value (self new-value
				(car stream)
				(l-cdr stream))))))
