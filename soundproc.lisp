(defvar *num-channels* 1)
(defvar *sample-rate* 8000) ; samples per second
(defvar *bytes-per-sample* 1) ; change to sample-depth-bytes ?

(defun l-cdr (stream)
  "lazily evaluates the cdr of a lazy sample stream"
  (funcall (cdr stream)))

(defun take (n stream)
  "realizes n values of a stream as a list"
  (loop for i from 1 to n
        for s = stream then (l-cdr s)
        collect (car s)))

(defun silent-stream ()
  (cons 0 (lambda () (silent-stream))))

(defun john-cage (filename)
  (write-wave (silent-stream) (* (+ (* 4 60) 33) *sample-rate*) filename))

(defun saw-stream (freq)
  "generates a sample stream of a rising sawtooth wave"
  (let ((incr (/ freq *sample-rate*)))
    (labels ((saw (value)
             (let ((next-val (if (> value 1) 0 (+ value incr))))
               (cons (- value 0.5)
                     (lambda () (saw next-val))))))
      (saw 0))))

(defun sine-stream (freq)
  "generates a sample stream of a sine wave"
  (let ((incr (/ (* freq 2 pi) *sample-rate*)))
    (labels ((sine (value)
               (cons (sin value)
                     (lambda () (sine (+ value incr))))))
      (sine 0))))

(defun stream-amp (gain stream)
  "amplifies the input stream by the gain factor"
  (cons (* gain (car stream))
        (lambda () (amp gain (l-cdr stream)))))

(defun stream-add (&rest stream-list)
  "mixes several streams together by sample addition"
  (labels ((add (streams)
             (cons (reduce #'+ (mapcar #'car streams))
                   (lambda () (add (mapcar #'l-cdr streams))))))
    (add stream-list)))

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
