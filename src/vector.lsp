;;; ***************************************************************************
;;;        vector.lsp
;;; 
;;;        This file is part of the program torad.lsp to export 
;;;        RADIANCE scene description files from Autocad. 
;;;
;;;        Copyright (C) 1993 by Georg Mischler / Lehrstuhl
;;;                              fuer Bauphysik ETH Zurich.
;;;  
;;;        Permission to use, copy, modify, and distribute this software  
;;;        for any purpose and without fee is hereby granted, provided  
;;;        that the above copyright notice appears in all copies and that  
;;;        both that copyright notice and this permission notice appear in  
;;;        all supporting documentation.  
;;;  
;;;        THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED  
;;;        WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR  
;;;        PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.  
;;;  
;;;        Acknowlegdements:   
;;;        Final developement of this program has been sponsored by Prof. Dr.   
;;;        B. Keller, Building Physics, Dep. for Architekture ETH Zurich.   
;;;        The developement environment has been provided by Prof. Dr. 
;;;        G. Schmitt, Architecture & CAAD ETH Zurich. 
;;;  
;;; ***************************************************************************

;;; VECTORS *******************************************************************

(defun vector (p1 p2 / len)
 ;; return: vector of length 1 from p1 towards p2.
 ;;         null-vector if points are identical.
  (setq len (distance p1 p2))
  (if (equal 0 len 0.0000001)
      '(0.0 0.0 0.0)
      (mapcar '(lambda (x1 x2)
                 (/ (- x2 x1)
                    len ) )
            p1 p2 ) ) )


(defun d-vector (p1 p2)
 ;; return: vector of resulting length from p1 towards p2.
 ;;         null-vector if points are identical.
      (mapcar '(lambda (x1 x2)
                 (- x2 x1) )
            p1 p2 ) )


(defun vectorize-l (line) 
 ;; return: list of first point and vector of length 1 from first
 ;;         towards second point of 'line'.
  (list (car line) (vector (car line) (cadr line)) ) )


(defun d-vectorize-l (line) 
 ;; return: list of first point and vector of length 1 from first
 ;;         towards second point of 'line'.
  (list (car line) (d-vector (car line) (cadr line)) ) )



(defun normalize (vect / len)
 ;; return: vector of length 1 in direction of vector 'vect'.
  (setq len (distance '(0 0 0) vect))
  (if (equal 0 len 0.0000001)
      vect
      (mapcar '(lambda (co)
                 (/ co len) )
         vect ) ) )


(defun trans-vl (vl from to)
  (list (trans (car vl) from to)
        (trans (cadr vl) from to T) ) )


(defun transl-p (point vector dist)
 ;; return: point translated in direction of 'vector' for the
 ;;         distance 'dist'.
  (mapcar '(lambda (p v)
             (+ p (* v dist)) )
        point vector ) )


(defun transl-l (pts dir dist)
  (mapcar '(lambda (pt)
              (transl-p pt dir dist) )
     pts ) )


(defun transl-vl (vl dir dist)
  (if (equal '(0.0 0.0 0.0) dist)
      vl
      (list (mapcar '(lambda (p v)
                      (+ p (* v dist)) )
              (car vl) dir )
            (cadr vl) ) ) )


(defun extend-vect (vect factor)
 ;; return: vector extended according to 'factor' relative to its
 ;;         original length.
  (mapcar '(lambda (xv)
              (* xv factor) )
     vect ) )


(defun extend-vline (vline dist)
 ;; return: point on vectorized line 'vline' in distance 'dist' from its
 ;;         origin-point.
  (mapcar '(lambda (xp xv)
              (+ xp (* xv dist)) )
     (car vline)(cadr vline) ) )


;;; PRODUKTS --------------------------------------------------***

(defun dot-prod (v1 v2)
  (apply '+ (mapcar '* v1 v2)) )


(defun vect-prod (v1 v2 / yzx)
  ;; vector-product
  ;;
  ;;    x = y1 z2 - y2 z1
  ;;    y = z1 x2 - z2 x1
  ;;    z = x1 y2 - x2 y1
  ;;
  (setq yzx (shift (mapcar 'list v1 v2)))
  (normalize (mapcar '(lambda (yl zl)
                        (- (* (car yl)(cadr zl))
                           (* (cadr yl)(car zl)) ) )
                yzx (shift yzx) )) )


;;; INQUIERY ---------------------------------------------------***

(defun vlen (vect)
 ;; return: length of vector 'vect'.
  (sqrt (apply '+ (mapcar '(lambda (xx)
                             (* xx xx) )
                      vect ))) )


(defun vlinters (vl1 vl2)
  ;; return : intersection-point of vectorized lines 'vl1' 'vl2' if
  ;;          existing. else NIL.
   (inters (car vl1) (extend-vline vl1 1.0)
           (car vl2) (extend-vline vl2 1.0) NIL) )



(defun vl-dist (vl1 vl2 / p1 v1 v2 vd sup sub)
 ;;
 ;; smallest distance between two vectorized lines.
 ;;
 ;;   p1 = vr + t1 va
 ;;   p2 = vs + t2 vb
 ;;
 ;;        | [va, vb, vs - vr] |
 ;;   d = -----------------------
 ;;             | va X vb |
 ;;
  (setq p1 (car vl1)
        v1 (cadr vl1)
        v2 (cadr vl2) )
  (cond ( (v-parallelp v1 v2)
          (distance p1 (orthonvline p1 vl2)) )
        ( T
          (setq vd (mapcar '- (car vl2)(car vl1))
                sup (abs (3det v1 v2 vd))
                sub (vlen (vect-prod v1 v2)) )
          (/ sup
             sub ) ) ) )



(defun interang (v1 v2 / ot pp d1 ang)
  ;; return : angle between vectors 'v1' and 'v2'  ---  0 <= ang <= pi.
  (cond ( (or (equal v2 '(0 0 0))(equal v1 '(0 0 0)))
		  (setq ang -1) )
		(T 
		 (setq ot (orthparamt v2 (list '(0 0 0) v1))
			   pp (extend-vline  (list '(0 0 0) v1) ot)
			   d1 (distance '(0 0 0) pp) )
		 (cond ( (= 0 d1)
				 (setq ang (/ pi 2)) )
			   ( T
				(setq ang (atan (/ (distance v2 pp) d1 )))
				(if (> 0 ot) 
					(setq ang (- pi ang)) ) ) ) ) )
  ang )

;;; PREDICATES ---------------------------------------------------***

(defun v-parallelp (v1 v2)
 ;; for normalized vectors only!
 ;; return: T if vectors 'v1' and 'v2' are parallel, else NIL.
  (or (apply 'and (mapcar '(lambda (x1 x2)
                             (equal x1 x2 0.000001) )
                    v1 v2 ))
      (apply 'and (mapcar '(lambda (x1 x2)
                             (equal x1 x2 0.000001) )
                    v1 (mapcar '- v2) )) ) )


(defun vl-samep (vl1 vl2)
 ;; return: T if the vectorized lines 'vl1' and 'vl2' are identical, else NIL.
  (and (v-parallelp (cadr vl1)(cadr vl2))
       (= 0.0 (vl-dist vl1 vl2)) ) )


(defun planarp (ptlist / p0 v1 v2)
  (cond ( (or (null ptlist) (> 3 (length ptlist))) NIL)
		( (= 3 (length ptlist)) T)
		( T
		  (setq p0 (car ptlist)
				v1 (vector p0 (cadr ptlist))
				v2 (vector p0 (caddr ptlist))
				ptlist (cdddr ptlist) )
		  (while (and ptlist
					  (< (abs (3det (vector p0 (car ptlist)) v1 v2)) 0.001) )
				 (setq ptlist (cdr ptlist)) )
		  (not ptlist) ) ) )



;;; RELATED VECTORS ----------------------------------------------***

(defun 3angvector (av bv cv coslist / xyz sxyz rxyz vallist)
 ;; parameters: three normalized vectors and
 ;;             coslist: (cos-a cos-b cos-c)
 ;; return: vector spanning certain angles with three other vectors.
 ;;         all vectors must be normalized.
 ;;         the result will only be correct if the angles fit possibluy
 ;;         together with the three vectors (ain't it strange?).
 ;; 
 ;;
 ;;    x xa + y ya + z za - cos-a = 0  
 ;;    x xb + y yb + z zb - cos-b = 0  
 ;;    x xc + y yc + z zc - cos-c = 0  
 ;;
 ;;
 ;;      cos-a (yc zb - yb zc) + cos-b (ya zc - yc za) + cos-c (yb za - ya zb)
 ;; x = -----------------------------------------------------------------------
 ;;          xa (yc zb - yb zc) + xb (ya zc - yc za) + xc (yb za - ya zb)
 ;;
 ;;      cos-a (zc xb - zb xc) + cos-b (za xc - zc xa) + cos-c (zb xa - za xb)
 ;; y = -----------------------------------------------------------------------
 ;;          ya (zc xb - zb xc) + yb (za xc - zc xa) + yc (zb xa - za xb)
 ;;
 ;;      cos-a (xc yb - xb yc) + cos-b (xa yc - xc ya) + cos-c (xb ya - xa yb)
 ;; z = -----------------------------------------------------------------------
 ;;          za (xc yb - xb yc) + zb (xa yc - xc ya) + zc (xb ya - xa yb)
 ;;
 ;;
  (setq xyz  (mapcar 'list av bv cv)
        sxyz (mapcar 'shift xyz)
        rxyz (mapcar 'shift sxyz)
        vallist (mapcar '(lambda (ry sz sy rz)
                           (mapcar '(lambda (yc zb yb zc)
                                      (- (* yc zb)
                                         (* yb zc) ) )
                              ry sz sy rz ) )
                  (shift rxyz)
                  (shift (shift sxyz))
                  (shift sxyz)
                  (shift (shift rxyz)) ) )
  (mapcar '(lambda (xlist vals)
             (/ (apply '+ (mapcar '* coslist vals))
                (apply '+ (mapcar '* xlist vals)) ) )
    xyz vallist ) )

;;; POINTS -------------------------------------------------------***

(defun pointthru (vl p0 vv vw)
  (extend-vline vl (pointthru-t vl p0 vv vw)) )

(defun orthonvline (pt vl)
  ;; return: point perpendiculer on vectorized line 'vl' from point 'pt'.
    (extend-vline vl (orthparamt pt vl) ) )


;;; PARAMETERS ---------------------------------------------------***

(defun pointthru-t (vl p0 vv vw / svv svw abc o-p   )
 ;; 
 ;;  return : parameter t extending vectorized line 'vl' to the common
 ;;           point of vector 'vl' and plane 'r = p0 + vv = vw'.
 ;;
 ;;       ( xp )     ( xq )              ( xo )     ( xv )     ( xw )
 ;;  vl = ( yp ) + t ( yq )      E : r = ( yo ) + s ( yv ) + u ( yw )
 ;;       ( zp )     ( zq )              ( zo )     ( zv )     ( zw )
 ;;
 ;;     x = xp + t xq
 ;;     y = yp + t yq
 ;;     z = zp + t zq
 ;; 
 ;; 
 ;;        A (xo - xp) + B (yo - yp) + C (zo - zp)
 ;;  t = -------------------------------------------
 ;;        A xq        + B yq        + C zq
 ;; 
 ;;  A = yv zw - yw zv
 ;;  A = zv xw - zw xv
 ;;  A = xv yw - xw yv
 ;;
 ;; 
  (setq svv (shift vv)
        svw (shift vw)
        abc (mapcar '(lambda (yv zw yw zv)
                        (- (* yv zw)
                           (* yw zv) ) )
               svv (shift svw) svw (shift svv) )
        o-p (mapcar '- p0 (car vl)) )
  (/ (apply '+ (mapcar '* abc o-p))
     (apply '+ (mapcar '* abc (cadr vl))) ) )


(defun orthparamt (pt vl / tc tn tval)
  ;;  return : parameter t extending vectorized line 'vl' to
  ;;           the point perpendicular on 'vl' from point 'pt'.
  ;;
  ;;      (  xp  )              (  xq  )     (  xv  )
  ;;  p = (  yp  )      g : r = (  yq  ) + t (  yv  )
  ;;      (  zp  )              (  zq  )     (  zv  )
  ;;
  ;; plane with orthogonal vector v:
  ;;  E :  xv x + yv y + zv z - xv xp - yv yp - zv zp = 0
  ;;
  ;; insert g:       x + xq + t xv
  ;;                 y + yq + t yv
  ;;                 z + zq + t zv
  ;;
  ;;
  ;;       tc      xv (xp - xr) + yv (yp - yr) + zv (zp - zr)
  ;;  t = ---- = --------------------------------------------
  ;;       tn                   xv2 + yv2 + zv2
  ;;
   (setq tc (apply '+ (mapcar '(lambda (xv xp xr)
                                 (* xv (- xp xr)) )
                        (cadr vl) pt (car vl) ))
         tn (apply '+ (mapcar '(lambda (xv)
                                 (* xv xv) )
                        (cadr vl) )) )
   (if (= 0 tn)
       (prompt "\ndivbyzero in orthparamt!\007 ");(/ pi 2)
       (/ tc tn) ) )



(defun ihlparam-t (vl1 vl2 / va vb vr vs aa bb ab subval as-r br-s)
  ;; return: list of the two parameters extending the vectorized lines
  ;;         'vl1'  and 'vl2' to the points of their smallest distance, if
  ;;         they are parallel, starting out from the startpoint of 'vl1'.
  ;;
  ;;   p1 = vr + t1 va
  ;;   p2 = vs + t2 vb
  ;;
  ;;   (vs + t2 vb - vr - t1 va).va = 0
  ;;   (vs + t2 vb - vr - t1 va).vb = 0
  ;;
  ;;
  ;;         (va.vb) ((vb.vr) - (vb.vs)) + (vb.vb) ((va.vs) - (va.vr))
  ;;   t1 = -----------------------------------------------------------
  ;;                      (va.va)(vb.vb) - (va.vb)(va.vb)
  ;;
  ;;         (va.vb) ((va.vs) - (va.vr)) + (vb.vb) ((vb.vr) - (va.vs))
  ;;   t2 = -----------------------------------------------------------
  ;;                      (va.va)(vb.vb) - (va.vb)(va.vb)
  ;;
  (setq va (cadr vl1)
        vb (cadr vl2)
        vr (car vl1)
        vs (car vl2)
        aa (dot-prod va va)
        bb (dot-prod vb vb)
        ab (dot-prod va vb)
        as-r (- (dot-prod va vs) (dot-prod va vr))
        br-s (- (dot-prod vb vr) (dot-prod vb vs))
        subval (- (* aa bb) (* ab ab)) )
  (if (= 0 subval)
      (list 0.0 (orthparamt (car vl1) vl2))
      (list (/ (+ (* ab br-s)(* bb as-r))
               subval )
            (/ (+ (* ab as-r)(* bb br-s))
               subval ) ) ) )

;;; DETERMINANTS ------------------------------------------------***

(defun 2det (va vb)
 ;; 2-dimensional determinant
 ;;
 ;;                 ( xa xb )
 ;;  [va, vb] = det ( ya yb ) 
 ;;
 ;;           = xa yb - xb ya
 ;;
  (- (* (car va)(cadr vb))
     (* (cadr va)(car vb)) ) )


(defun 3det (va vb vc)
 ;; 3-dimensional determinant
 ;;
 ;;                    ( xa xb xc )
 ;; [va, vb, vc] = det ( ya yb yc )
 ;;                    ( za zb zc )
 ;;
 ;;              =   xa yb zc - za yb xc
 ;;                + xb yc za - zb yc xa
 ;;                + xc ya zb - zc ya xb
 ;;
  (apply '+ (mapcar '(lambda (a1 a3 b2 c1 c3)
                       (- (* a1 b2 c3)
                          (* a3 b2 c1) ) )
              va (shift (shift va))
              (shift vb)
              vc (shift (shift vc)) )) )


;;;   --------------------------------------------------------***


(defun intervectors (v1 v2 vn ai num / astep a1 a2 vect vlist)
 ;; return: a list of one or more vectors interpolated regularly between
 ;;         the vectors 'v2' and 'v2'.
 ;;    'vn' must be a vector perpendicular to the other two.
 ;;    'ai' must be the angle between the two vectors.
 ;;    'num' is the number of vectors to return.
  (setq astep (/ ai (1+ num))
        a1 0 )
  (repeat num
     (setq a1 (+ a1 astep)
           a2 (- ai a1)
           vect (if (or (equal v1 v2)
                        (equal v1 (mapcar '- v2)) )
                    v1
                    (3angvector v1 v2 vn (list (cos a1) (cos a2) 0)) )
           vlist (append vlist (list vect)) ) )
  vlist )


(defun interpoints (dplist num / p1 p2 tstep vect plist)
 ;; return: a list of one or more points interpolated regularly between
 ;;         the points 'p2' and 'p2'.
 ;;    'num' is the number of points to return.
  (setq p1 (car dplist)
        p2 (cadr dplist)
        tstep (/ (distance p1 p2) (1+ num))
        vect (vector p1 p2) )
  (repeat num
     (setq p1 (transl-p p1 vect tstep)
           plist (append plist (list p1)) ) )
  plist )



;;; MATH *********************************************************

(defun mod (modulator moduland / res)
 ;; return: 'moduland' mod 'modulator'.
  (cond ( (<= modulator 0)
          (prompt "\n\007negative modulator! ") )
        ( T (setq res (- moduland (* modulator (fix (/ moduland modulator)))))
            (if (>= 0 res)
                (setq res (+ res modulator)) ) ) )
  res )

(defun fact (N / r)      ; n!
  (setq r 1)
  (while (< 1 (abs n))
    (setq r (* r n)
          n (1- n) ) )
  r )

(defun grad (ang)       ; radians to grad ( pi -> 180 )
  (* 180 (/ ang pi)) )


;;; MATRIX CALCULATIONS ************************************************

(defun matrix-to (vx vy vz)
  (mapcar 'list vx vy vz) )


(defun transf-p (vect matrix)
  (mapcar '(lambda (mline)
				   (apply '+ (mapcar '* vect mline)) )
		  matrix ) )


(defun transf-vl (vl matrix)
  (mapcar 'transf-p vl (list matrix matrix)) )


(defun reverse-matrix ( matrix / xyz sxyz rxyz vallist det-a)
  (setq xyz  (mapcar 'list (car matrix) (cadr matrix) (caddr matrix))
        sxyz (mapcar 'shift xyz)
        rxyz (mapcar 'shift sxyz)
        vallist (mapcar '(lambda (ry sz sy rz)
								 (mapcar '(lambda (yc zb yb zc)
												  (- (* yc zb)
													 (* yb zc) ) )
										 ry sz sy rz ) )
						(shift rxyz)
						(shift (shift sxyz))
						(shift sxyz)
						(shift (shift rxyz)) )
		det-a (apply '+ (mapcar '* (car vallist) (car xyz))) )
  (mapcar '(lambda (line)
				   (mapcar '/ line (list det-a det-a det-a)) )
		  vallist) )


(defun rot-xy (vect rotang / cosa sina)
  (setq cosa (cos rotang)
        sina (sin rotang) )
  (transf-p vect (list (list cosa (- sina)  0)
                       (list sina     cosa  0)
                          '(    0        0  1) )) )

(defun rot-3d-matrix (dir rotang / sina 1-cosa arot
						  u v w uu vv ww uv vw wu te ta ta2)
 ;; return: transformation-matrix rotating any point around the
 ;;         axis with the direction 'dir' that goes through the 
 ;;         origin of the coordinate-system.
 ;; the rotation-angle 'rotang' is given in radians.
  (setq sina   (sin rotang)
        1-cosa (- 1 (cos rotang))
        u (car dir)
        v (cadr dir)
        w (caddr dir)
        uu (* u u)      uv (* u v)
        vv (* v v)      vw (* v w)
        ww (* w w)      wu (* w u)
        TE '((1.0  0.0  0.0 )
             (0.0  1.0  0.0 )
             (0.0  0.0  1.0 ) )
        TA  (list (list  0.0   (- w) v     )
                  (list  w     0.0   (- u) )
                  (list  (- v) u     0.0   ) )
        TA2 (list (list  (- (+ vv ww))  uv            wu            )
                  (list  uv             (- (+ ww uu)) vw            )
                  (list  wu             vw            (- (+ uu vv)) ) ) )
  (mapcar '(lambda (elin alin a2lin)
				   (mapcar '(lambda (ex ax a2x)
									(+ ex (* sina ax) (* 1-cosa a2x)) )
						   elin alin a2lin ) )
		  TE TA TA2 ) )



(defun matmul (mx1 mx2)
   (mapcar '(lambda (l1 )
               (mapcar '(lambda (l2r)
                           (apply '+ (mapcar '* l1 l2r)) )
                   (mapcar 'list (car mx2)(cadr mx2)(caddr mx2)) ) )
      mx1  ) )



;;; TRANSFORMATION-MATRIX FROM WORLD-UCS TO ENTITY-CS.
(defun ent_xform (data / xtilt ytilt ztilt mx1 inspt
						 rotang mxrot xrot yrot zrot
						 xstr ystr zstr )

  ;; tilted coordinates in bl-cs
  (setq ztilt (cdr (assoc 210 data)) )   ; extrusion relative to world-z.
  (cond ( (equal '(0.0 0.0 1.0) ztilt)
		  (setq xtilt '(1.0 0.0 0.0)
				ytilt '(0.0 1.0 0.0) ) )
		( (and (< (abs (car ztilt)) (/ 1.0 64.0))
			   (< (abs (cadr ztilt)) (/ 1.0 64.0)) )
		  (setq xtilt (vect-prod '(0.0 1.0 0.0) ztilt)
				ytilt (vect-prod  ztilt xtilt) ) )
		(T (setq xtilt (vect-prod '(0.0 0.0 1.0) ztilt)
				 ytilt (vect-prod  ztilt xtilt) ) ) )

  (setq mx1 (mapcar 'list xtilt ytilt ztilt)
		inspt (transf-p (cdr (assoc 10 data)) mx1 ) )

  (cond ( (= "INSERT" (cdr (assoc 0 data)))
	   ;; rotated coordinates in bl-cs ucs
	   (setq rotang (cdr (assoc 50 data))
			 zrot ztilt)   ;; in radians!
	   (cond ( (and rotang (/= 0.0 rotang))
			   (if (equal '(0.0 0.0 1.0) ztilt)
				   (setq xrot (rot-xy xtilt rotang)
						 yrot (rot-xy ytilt rotang) )
				   (setq mxrot (rot-3d-matrix zrot rotang)
						 xrot (transf-p xtilt mxrot)
						 yrot (transf-p ytilt mxrot) ) ) )
			 (T (setq xrot xtilt
					  yrot ytilt )) )
	   
	   ;; stretched coorinates in bl-ucs
	   (setq xstr (extend-vect xrot (cdr (assoc 41 data)))
			 ystr (extend-vect yrot (cdr (assoc 42 data)))
			 zstr (extend-vect zrot (cdr (assoc 43 data))) )
	   
	   ;; transformation-matrix to world from if c-s of the entity.
	   (list (mapcar 'list xstr ystr zstr) inspt) )
	  (T (list  mx1 inspt)) ) )


;;; LIST MANIPULATIONS ********************************************************

(defun shift (alist)
  (append (cdr alist) (list (car alist))) )

;;;*****************************************************************************
;;; end of vector.lsp.
;;;*****************************************************************************
