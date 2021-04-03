(require 'pjb-cl)

(handler-case
    (play-sound
     `(sound :volume 0.5
             :data ,(coerce (loop
                              with freq = 440.0
                              with rate = 8000
                              repeat 16000
                              for theta from 0
                              for s = (+ 128 (* 128 (sin (* freq (/ theta rate)))))
                              collect (round s))
                            'string)) )
  (error (err)
    (message "%s" err)))



