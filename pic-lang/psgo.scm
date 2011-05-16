;;setup PS4

(load "pic-lang/picture")
(load "pic-lang/pic-ops")
(load "pic-lang/pic-reco")
(load "pic-lang/pic-read")
(load "pic-lang/pic-imag")

(define screen)

(if (lexical-unreferenceable? user-initial-environment 'screen)
    (set! screen false))

(define (setup)
  (load "pic-lang/prmpnt")
  (load "pic-lang/hutils")
  (load "pic-lang/hend")
  (setup-windows))

(define (setup-windows)
  (if (and screen (graphics-device? screen))
      (graphics-close screen))
  (begin (set! screen (make-window 512 512 -10 +10))
	 (graphics-set-coordinate-limits screen 0.0 0.0 1.0 1.0)
	 (graphics-operation screen 'set-window-name "scheme sandbox")))

(setup)


