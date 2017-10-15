#lang racket
; point scale
(define (coord-map rect)
  (lambda(point)
    (+vect
      (+vect (scale (xcord point)
                    (horiz rect))
             (scale (ycord point)
                    (vert rect)))
      (origin rect))))

; constructing primitive picture
; from a list of segments
(define (make-picture seglist)
  (lambda(rect)
    (for-each
      (lambda(s)
        (draw-line ((coord-map rect) (seg-start s))
                   ((coord-map rect) (seg-end s))))
        seglist)))

; usage
(define G (make-rect RECT_PROPERTIES))
(define P (make-picture PICTURE_SEG_LIST))
; draw a picture P using rectangle G
(P G)

; draw two pictures beside
(define (beside p1 p2 a)
  (lambda(rect)
    (p1 (make-rect
		  (origin rect)
		  (scale a (horiz rect))
		  (vert rect)))
    (p2 (make-rect
		  (+vect (origin rect)
				 (scale a (horiz rect)))
		  (scale (- 1 a) (horiz rect))
		  (vert rect)))))

; rotate picture
(define (rotate90 pict)
  (lambda(rect)
    (pict (make-rect
		    (+vect (origin rect)
				   (horiz rect))
			(vert rect)
			(scale -1 (horiz rect))))))
