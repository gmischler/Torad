;;; **************************************************************************
;;;        esample.lsp
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
;;; **************************************************************************

;;; VISIBLE LAYERS ***********************************************************

(defun vislaylist ( / layer laylist )
  ;; generate list of layernames currently visible.
  (setq layer (tblnext "LAYER" T))
  (if (and (< 0 (cdr (assoc 62 layer)))
          (= 0 (logand 1 (cdr (assoc 70 layer)))) )
		 (setq laylist (list (cdr (assoc 2 layer)))) )
  (while (setq layer (tblnext "LAYER"))
		 (if (and (< 0 (cdr (assoc 62 layer)))
				  (= 0 (logand 1 (cdr (assoc 70 layer)))) )
			 (setq laylist (cons (cdr (assoc 2 layer)) laylist)) ) )
  laylist )



;;; STORAGE INTERFACE ********************************************************

(defun valuablepoly (typ)
  ;; test an entity type for compliance with setup.
  (car (member typ *valuablepolylist*) ))



(defun makeentlist ( / num)
  ;; initialize entity list and create sublists according to sampling mode.
  (cond ( (= *exportsmode* "Color")
          (setq num 1)
          (while (> 256 num)            ; list colors instead of layers.
                 (setq *exportentlist* (cons (list (itoa num)) *exportentlist*)
                       num (1+ num) ) ) )
        ( T
          (setq *exportentlist* (mapcar 'list *exporttruelays*)) ) ) )



(defun addlayentlist (c-ent elay / oldlist newlist)
  ;; update entity list with new element added to the correct sublist.
  (setq oldlist (assoc elay *exportentlist*)
        newlist (append oldlist (list c-ent))
        *exportentlist* (subst newlist oldlist *exportentlist*) ) )



(defun addpolblocklist (blent)
  ;; update blocklist with a new element.
  (setq *exportblocklist* (append *exportblocklist* (list blent))) )



;;; TOP LEVEL ENTITIES ****************************************************

(defun sampleents (selset / num etype c-ent c-data elay numtot numstep)
  ;; extract entities out of a selection set and store to the apropriate list.
  (terpri)(terpri)
  (setq num 0
        numtot (sslength selset)
        numstep 0 )
  (while (> numtot numstep)
         (prompt (strcat "      sampling entities level 0:  "
                         (itoa numstep) "/" (itoa numtot) "  \r"))
         (setq numstep (min (+ numstep 25) numtot))
         (while (< num  numstep)
                (setq c-ent (ssname selset num))
                (setq num (1+ num)
                      c-data (entget c-ent)
                      elay (get_laystring c-data nil)
                      etype (getetype c-data) )
                (cond ( (= "INSERT" etype)
                        (addpolblocklist (list c-ent)) )
                      ( (and (valuablepoly etype)
                             (member elay *exporttruelays*) )
                        (if (equal *exportsmode* "Color")
                            (addlayentlist c-ent (get_colstring c-data nil))
                            (addlayentlist c-ent elay) ) )
                      (T NIL) ) ) )
  (prompt (strcat "      sampling entities level 0:  "
                  (itoa numstep ) "       \n")) )




;;; BLOCKS ****************************************************************
 
(defun sampleblocks (level / num numstep numtot xdata block blockl ename)
  ;; extract entities of a block stored previously on the blocks list.
  ;; pass the result on for proper storage.
  (setq blockl *exportblocklist*
        *exportblocklist* nil
        num 0
        numtot (length blockl)
        numstep 0 )
  (while (> numtot numstep)
         (prompt (strcat "        sampling blocks level " (itoa level)
                         ":  " (itoa numstep) "/" (itoa numtot) "  \r"))
         (setq numstep (min (+ numstep 10) numtot) )
         (while (< num  numstep)
                (setq num (1+ num)
                      block (car blockl)
                      blockl (cdr blockl) )
				(addblockentslist block) ) )
  (prompt (strcat "        sampling blocks level "
                  (itoa level) ":  " (itoa numstep) "       \n")) )



(defun addblockentslist (bllist / blent notend data ent typ elay)
  ;; store the entities out of blocklist in the correct place:
  ;; - if a block back to blocklist.
  ;; - if an entity on a valid layer on the entity list sorted according
  ;;   to samplemode.
  (setq notend T
		blent (car bllist)
        ent (cdr (assoc -2 (tblsearch "BLOCK" (cdr (assoc 2 (entget blent)))))) )  (while (and notend ent)
         (setq data (entget ent)
               typ (cdr (assoc 0 data)) )
         (cond ( (= typ "INSERT")
                 (addpolblocklist (cons ent bllist) ) )
               ( (= typ "ENDBLK")
                 (setq notend nil) )
               ( (valuablepoly (getetype data))
                 (setq elay (get_laystring data bllist))
                 (if (member elay *exporttruelays*)
                     (cond ( (equal *exportsmode* "Color")
                             (addlayentlist (cons ent bllist)
											(get_colstring data bllist) ) )
                           ( (equal *exportsmode* "Toplayer")
                             (addlayentlist (cons ent bllist)
								(get_laystring (entget (if bllist
														   (last bllist)
														   blent ) ) NIL)) )
                           ( T
                             (addlayentlist (cons ent bllist) elay) ) ) )   )
               ( T NIL) )
         (setq ent (entnext ent)) )
  NIL )



;;; ***************************************************

(defun noprefix (str / num)
  ;; strip a filename of its path components.
  (setq num (strlen str))
  (while (and (< 0 num) (/= "/" (substr str num 1)) )
         (setq num (1- num)) )
  (substr str (1+ num)) )



(defun shortnumstr (num prec / fnum)
  ;; create an integer string representation of a number if possible,
  ;; else a real.
  (cond ( (equal num (setq fnum (fix num)) 0.00000001)
          (itoa fnum) )
        ( (equal num (setq fnum (if (< 0 num)(1+ fnum)(1- fnum))) 0.00000001)
          (itoa fnum) )
        (T (rtos num 2 prec)) ) )


(defun get_laystring (data contele / lay)
  ;; return the name string of the layer used to display an entity.
  ;; search blocklist for floatinglayer entities.
  (setq lay (cdr (assoc 8 data)))
  (if (and (= "0" lay) contele)
	  (get_laystring (entget (car contele)) (cdr contele))
	  lay) )


(defun get_colstring (data contele / color)
  ;; create a string out of the name of the displayed color of an entity.
  ;; search blocklist for floating-color entities.
  (if (or (null (setq color (cdr (assoc 62 data))))
		  (= -1 color) ) ; bylayer
      (setq color (cdr (assoc 62 (tblsearch "LAYER"
											(get_laystring data contele) )))) )
  (if (= 0 color) ; byblock
      (if contele
          (get_colstring (entget (car contele))(cdr contele))
          "7" )
      (itoa color) ) )



(defun datestring (/ str mon)
  ;; create a string out of the current date and time.
  (setq str (rtos (GETVAR "cdate") 2 4)
        mon '(("01"" Jan.")("02"" Feb.")("03"" Mar.")("04"" Apr.")
              ("05"" May.")("06"" Jun.")("07"" Jul.")("08"" Aug.")
              ("09"" Sep.")("10"" Oct.")("11"" Nov.")("12"" Dec.") ) )
  (strcat (substr str 1 4)
          (cadr (assoc (substr str 5 2) mon))
          (substr str 7 3) " "
          (substr str 10 2) ":"
          (substr str 12) ) )



;;; BLOCK TRANSFORMATIONS ******************************************************
 
 
(defun trans_back (polylist blocks / xform matrix insp data )
  ;; transform a list of pointlists from entity- or block-cs to wcs.
  ;; blocks is a list containing -either the entity names of hierarchical blocks.  ;;                             -or a single entity name of a planar entity.
  (setq blocks (get_backtrans blocks)
        xform  (car blocks)
        data   (caddr blocks)
        blocks (cadr blocks)
        matrix (car xform)
        insp   (cadr xform) )
  (if blocks
      (setq polylist (trans_back polylist blocks)) )
  (if (= "INSERT" (cdr (assoc 0 data)))
      (if (or (/= 0.0 (cdr (assoc 70 data))) ; test for multiple-inserts.
              (/= 0.0 (cdr (assoc 71 data))))
          (setq polylist (shiftminsert polylist data )) )
      (setq insp '(0.0 0.0 0.0)) )
  (mapcar '(lambda (poly)
                   (mapcar '(lambda (pt)
                                    (transl-p (transf-p pt matrix) insp 1.0) )
                           poly ) )
          polylist ) )



(defun get_backtrans (bllist / data xform nxform txform)
  ;; evaluate the transformation out of a hierarchy of blocks.
  ;; stop if a multiple insert is encountered and include rest of blocklist.
  (setq data (entget (car bllist))
        xform (ent_xform data)
        bllist (cdr bllist) )
  (if (and bllist
           (= 0.0 (cdr (assoc 70 data)))
           (= 0.0 (cdr (assoc 71 data)) ))
      (setq bllist (get_backtrans bllist)
            nxform (car bllist)
            data   (caddr bllist)
            bllist (cadr bllist)
            txform (list (matmul (car xform)(car nxform))
                         (mapcar '+ (transf-p (cadr nxform) (car xform))
                                 (cadr xform) ) ) )
      (setq txform xform) )
  (list txform bllist data) )



(defun shiftminsert (polylist data / npolylist
                  xnum xxnum xdist xdir ynum yynum ydist ydir xfactor yfactor)
  ;; calculate the offset of the parts of a multiple insert.
  ;; make copies of polylist shifted apropriately.
  (setq xdir  '(1.0 0.0 0.0)
        xnum  (cdr (assoc 70 data))
        xdist (/ (cdr (assoc 44 data))(cdr (assoc 41 data)))
        ydir  '(0.0 1.0 0.0)
        ynum  (cdr (assoc 71 data))
        ydist (/ (cdr (assoc 45 data))(cdr (assoc 42 data))) )
  (foreach poly polylist
           (setq xxnum xnum )
           (while (< 0 xxnum)
                  (setq xxnum (1- xxnum)
                        yynum ynum
                        xfactor (* xxnum xdist) )
                  (while (< 0 yynum)
                         (setq yynum (1- yynum)
                               yfactor (* yynum ydist) )
                         (setq npolylist (cons (mapcar '(lambda (pt)
                                           (transl-p (transl-p pt ydir yfactor)
                                                     xdir
                                                     xfactor) )
                                                       poly)
                                               npolylist) ) ) ) ) )




(defun showpolylist (polylist)
  ;; graphical debugging.
  (mapcar '(lambda (ptl)
                   (mapcar '(lambda (p1 p2)
                                    (grdraw p1 p2 *col*) )
                           ptl (shift ptl) ) )
          polylist)
  (setq *col* (max 10 (rem (+ 2 *col*) 250))) )


;;; ANALYSYS *********************************************************

(defun getetype (data / typ flag)
  ;; extract the type of an entity.
  (setq typ (cdr (assoc 0 data)) )
  (if (= typ "POLYLINE")
      (setq flag (cdr (assoc 70 data))
            typ (cond ( (= 64 (logand flag 64))
                         "PFACE" )
                       ( (= 16 (logand flag 16))
                         "PMESH" )
                       ( (= 8 (logand flag 8))
                         "3DPOLY" )
                       ( (and (< 0.0 (cdr (assoc 40 data)))
                              (valuablepoly "WPLINE") )
                         "WPLINE")
                       ( (and (= 1 (logand flag 1))
                              (= 0 (logand flag 8))
                              (valuablepoly "POLYGON") )
                         "POLYGON" )
                       ( T "PLINE") ) )
      typ ) )




;;;-----------------------------------------------------------------------------

(defun pfacetopoly (data / ename xdata a b num face plist nodel pt)
  ;; make a polygon list describing a polyface mesh depending on
  ;; entity data 'data'
  (setq ename (cdr (assoc -1 data))
        data (entget ename '("MKVOL_LSP_01"))
        xdata (cdadr (assoc -3 data))
        b (entget (entnext (cdar data)))
        nodel NIL
        num 1 )
  (while (= 64 (logand 64 (cdr (assoc 70 b))))
         (setq nodel (cons (cons num (cdr (assoc 10 b ))) nodel )
               b (entget (entnext (cdar b)))
               num (1+ num)) )
  (cond ( (and xdata (= "flat" (cdr (assoc 1000 xdata))))
          (setq plist (list (reverse (mapcar 'cdr nodel)))) )
        ( T
         (while (/= "SEQEND" (cdr (assoc 0 b)))
                (foreach ptcode '(71 72 73 74)
                         (if (/= 0 (setq pt (abs (cdr (assoc ptcode b)))))
                             (setq face (cons (cdr (assoc pt nodel)) face)) ) )
                (setq plist (append plist
									(planarize4 (elimstraights (reverse face))))
                      face nil
                      b (entget (entnext (cdar b))) ) ) ) )
  plist )
 
 
 
;;;-----------------------------------------------------------------------------

(defun linetopoly (data / dir dist p1 p2)
  ;; make a polygon list describing a line depending on entity data 'data'
  ;; if thickness > 0 else nil.
  (cond ( (setq dist (cdr (assoc 39 data)))
          (setq dir (cdr (assoc 210 data))
                p1  (cdr (assoc 10 data))
                p2  (cdr (assoc 11 data)) )
          (if (< 0.0 (distance p1 p2))
              (list (list p1 p2
                          (transl-p p2 dir dist)
                          (transl-p p1 dir dist) )) ) )
        (T NIL) ) )


;;;-----------------------------------------------------------------------------

(defun plinetopoly (data typ segs / dist b plist p0 p1 p2 bulge flag uplist)
  ;; make a polygon list describing a polyline depending on entity data 'data'
  ;; the number of segments for arcs and the type:
  ;; 1 - pline with thickness.
  ;; 2 - closed pline as polygon.
  ;; 3 - pline with constant width.
  (setq flag (cdr (assoc 70 data))  ;polyline flags
        dist (cdr (assoc 39 data))
        b (entget (entnext (cdar data)))
        p0 (cdr (assoc 10 b))
        plist NIL )
  (cond ( (and (= 0 (logand flag (+ 16 32 64))) ; 0 1 2 4 8 are allowed
               (or (= 1 (logand flag 1)) dist ) )
          (while (/= "SEQEND" (cdr (assoc 0 b)))
                 (setq p1 (cdr (assoc 10 b ))
                       plist (cons p1 plist )
                       bulge (cdr (assoc 42 b))
                       b (entget (entnext (cdar b))) )
                 (if (and (/= 0.0 bulge)
                          (or (/= "SEQEND" (cdr (assoc 0 b)))
                              (= 1 (logand flag 1)) ) )
                     (setq p2 (cdr (assoc 10 b ))
                           p2 (if p2 p2 p0)
                           plist (append (segment_arc
                                          (bulgetoarc p1 p2 bulge segs))
                                         plist ) ) ) )
          (setq plist (reverse (elimstraights plist)))
		  (cond ( (< 1 (length plist))
				  (if (= 3 typ)
					  (setq plist (wideplist plist
											 (/ (cdr (assoc 40 data)) 2.0)
											 (= 1 (logand flag 1)))) )
				  (cond ( (and dist (< 0.0 dist))
						  (setq uplist (transup-l plist  dist)
								plist (append (interfaces plist uplist
													  (or (= 3 typ)
														  (= 1 (logand flag 1))))
											  (if (or (= 3 typ)
													  (and (= 2 typ)
														   (= 1(logand flag 1))))
												  (list plist (reverse uplist))
												  nil ) ) )
		  )
                ( T (setq plist (list plist))) ) )
		  (T NIL) ) )
        ( T NIL) )
  (trans_back plist (list (cdar data))) ) ; end plinetolist



;;;-----------------------------------------------------------------------------

(defun meshtopoly (data / a b flag m n mclose nclose plist pplist nplist  )
  ;; make a polygon list describing a 3dpolygon mesh depending on
  ;; entity data 'data'
  (setq a data
        b (entget (entnext (cdar a)))
        flag (cdr (assoc 70 data))  ;polyline flags
        m (cdr (assoc 71 data))
        n (cdr (assoc 72 data))
        mclose (= 1 (logand 1 flag ))
        nclose (= 32 (logand 32 flag))
        pplist NIL )
  (cond ( (= 0 (logand flag (+ 2 4 8 64))) ; 0, 1, 16, 32 are allowed
          (repeat m
                  (setq plist nil)
                  (repeat n
                          (setq plist (cons(cdr (assoc 10 b )) plist)
                                b (entget (entnext (cdar b))) ) )
                  (setq pplist (cons (reverse plist) pplist)) )
          (mapcar '(lambda (pl1 pl2)
                           (setq nplist (append (interfaces pl1 pl2 nclose)
                                                nplist)) )
                  pplist (if mclose (shift pplist) (cdr pplist)) ) )
        ( T NIL ) )
  nplist )
 
 
;;;-----------------------------------------------------------------------------

(defun facetopoly (data)
  ;; make a polygon list describing a 3dface depending on entity data 'data'
  (planarize4 (elimstraights (mapcar '(lambda (code)
                         (cdr (assoc code data)) )
                '(10 11 12 13) )) ) )
 
 

;;;-----------------------------------------------------------------------------

(defun tracetopoly (data / dist plist uplist)
  ;; make a polygon list describing a trace depending on entity data 'data'
  (setq dist (cdr (assoc 39 data))
        plist (elimstraights (mapcar '(lambda (code)
                                              (cdr (assoc code data)) )
                                     '(10 11 13 12) )) )
  (cond ( (< 2 (length plist))
		  (if (and dist (/= 0.0 dist))
			  (setq uplist (transup-l plist  dist)
					plist (append (interfaces plist uplist T)
								  (list plist (reverse uplist))) )
			  (setq plist (list plist)) )
		  (trans_back plist (list (cdr (assoc -1 data)))) )
		(T NIL) ) )


   
 
;;;-----------------------------------------------------------------------------

(defun arctopoly (data segments / center radius dist a1 a2 plist uplist)
  ;; make a polygon list describing an arc depending on entity data 'data'
  ;; and the number of segments.
  (setq center (cdr (assoc 10 data))
        radius (cdr (assoc 40 data))
        dist (cdr (assoc 39 data))
        a1 (cdr (assoc 50 data))
        a2 (cdr (assoc 51 data))
        plist (cons (polar center a1 radius)
                (append (segment_arc (list center radius a1 a2 segments))
                            (list (polar center a2 radius)) ) ) )
  (cond ( (and dist (/= 0.0 dist))
          (setq uplist (transup-l plist  dist)
                plist (interfaces plist uplist NIL) )
          (trans_back plist (list (cdr (assoc -1 data)))) )
        (T NIL) ) )


;;;-----------------------------------------------------------------------------

(defun circletopoly (data segments / center radius dist plist uplist)
  ;; make a polygon list describing a circle depending on entity data 'data'
  ;; and the number of segments.
  (setq center (cdr (assoc 10 data))
        radius (cdr (assoc 40 data))
        dist (cdr (assoc 39 data))
        plist (cons (polar center 0.0 radius)
               (segment_arc (list center radius 0.0 (* 2 pi) segments)) ) )
  (if (and dist (/= 0.0 dist))
      (setq uplist (transup-l plist  dist)
            plist (append (interfaces plist uplist T)
                          (list plist (reverse uplist))) )
      (setq plist (list plist)) )
  (trans_back plist (list (cdr (assoc -1 data)))) )



;;; UTILITY **********************************************************

(defun transup-l (plist dist)
  ;; move a list of points along z-axis.
  (mapcar '(lambda (pt) (list (car pt)(cadr pt)(+ (caddr pt) dist)) ) plist) )



(defun interfaces (poly1 poly2 key / npolylist)
  ;; return : list of planar polygons connecting input polys.
  ;; key=T  : closed Polygon
  ;; key=nil: open Polygon
  ;; polygons should have equal length.
  (mapcar '(lambda (p1 p2 p3 p4)
                   (setq npolylist (append (planarize4 (elimstraights
                                                        (list p1 p2 p3 p4)))
                                           npolylist)) )
          poly1 poly2
          (if key (shift poly2) (cdr poly2))
          (if key (shift poly1) (cdr poly1)) )
  npolylist )   ; end interfaces
 
 

(defun planarize4 (ptlist / len)
  ;; test a three or four sided polygon for planarity and make a list.
  ;; if nonplanar, split in two while preserving orientation.
  (cond ( (or (null ptlist) (> 3 (length ptlist))) NIL)
        ( (= 3 (length ptlist)) (list ptlist))
        ( (> 0.0000001 (abs (3det (vector (car ptlist)(cadr ptlist))
                                     (vector (car ptlist)(caddr ptlist))
                                     (vector (car ptlist)(cadddr ptlist)) )))
          (list ptlist) )
        ( T
          (list (list (car ptlist)(cadr ptlist)(caddr ptlist))
                (cons (car ptlist)(cddr ptlist)) ) ) ) )
 


(defun elimstraights (pl / npl lastp)
  ;; eliminate the points of a list that do not modify the shape of
  ;; the polygon.
  ;; eg.: - within straight segments.
  ;;      - identical to previous.
  ;;      - extending a segment the next one turns back on.
  (setq npl (list (car pl))
		lastp (last pl) )
  (mapcar '(lambda (p1 p2)
				   (if (and (not (equal p1 (car npl)))
							(or (< 0.0 (interang (vector (car npl) p1)
												 (vector (car npl) p2)))
								(equal lastp p1) ) )
					   (setq npl (cons p1 npl)) ) )
		  (cdr pl) (cdr (shift pl)) )
  (reverse npl) )


(defun wideplist (pl hwidth closed / a1 a2 am adif doff rlist llist off1 off2)
  ;; generates a polygon contouring the surface of pointlist 'pl' offset
  ;; at both sides for the distance 'hw' (halfwidth).
  (mapcar '(lambda (p1 p2 p3)
                   (setq a1 (angle p1 p2)
                         a2 (angle p2 p3)
                         am (/ (- (+ a1 a2) pi) 2.0)
                         adif (/ (- a2 a1) 2.0)
                         doff (/ hwidth (cos adif))
                         rlist (cons (polar p2 am doff) rlist)
                         llist (cons (polar p2 am (- doff)) llist) ) )
          (cons (last pl) pl) pl (shift pl)  )
  (if closed
      (append (reverse rlist) (list (last rlist)(last llist)) llist)
      (setq off1 (offset pl hwidth)
            off2 (offset (reverse pl) hwidth)
            llist (cons (cadr off1) (cdr (reverse llist)))
            llist (cons (car off2)(cdr (reverse llist)))
            rlist (cons (cadr off2)(cdr rlist))
            rlist (cons (car off1)(cdr (reverse rlist)))
            rlist (append rlist llist) ) ) )
 
 
 
(defun offset (pl hw / ao)
  ;; offset a pointlist to the right for a constant distance.
  (setq ao (- (angle (car pl)(cadr pl)) (/ pi 2.0)))
  (list (polar (car pl) ao hw)
		(polar (car pl) ao (- hw)) ) )



(defun segment_arc (arc / flag ai num astep acur plist)
  ;; generates the inner segment points of an arc.
  ;; starting and end point are not included.
  ;; 'arc' '(centerpoint radius starting_angle ending_angle num_of_segs)'.
  ;; the arc goes couterclockwise if 'num_of_segs' is positive.
  (setq ai (- (cadddr arc)(caddr arc)))
  (if (< ai 0) (setq ai (+ (* 2.0 PI) ai)))
  (setq num (fix (1+ (/ ai (/ (* 2 pi) (abs (last arc))))))
        astep (/ ai (1- (max 3 num)))
        acur (caddr arc) )
  (repeat (- num 2)
          (setq acur (+ acur astep)
                plist (cons (polar (car arc) acur (cadr arc)) plist) ) )
  (if (< 0 (last arc))
      (reverse plist)
       plist ) )
 
 
 
(defun bulgetoarc (p1 p2 bulge segs / x1 y1 x2 y2 cotbce a1 a2 center radius)
  ;; make an arc description out of the AutoCad bulge form.
  ;; 'arc' '(centerpoint radius starting_angle ending_angle num_of_segs)'.
  (setq x1 (car p1) y1 (cadr p1)
        x2 (car p2) y2 (cadr p2)
        cotbce (/ (- (/ 1.0 bulge) bulge) 2.0)
        center (list (/ (+ x1 x2 (- (* (- y2 y1) cotbce))) 2.0)
                     (/ (+ y1 y2    (* (- x2 x1) cotbce) ) 2.0)
                     (caddr p1) )
        radius (distance p1 center)
        a1 (atan (- y1 (cadr center)) (- x1 (car center)))
        a2 (atan (- y2 (cadr center)) (- x2 (car center))) )
  (if (< a1 0.0) (setq a1 (+ a1 pi pi)))
  (if (< a2 0.0) (setq a2 (+ a2 pi pi)))
  (if (< bulge 0.0)
      (list center radius a2 a1 segs)
      (list center radius a1 a2 (- segs)) ) )



(defun shift (alist)
  (append (cdr alist) (list (car alist))) )
 

;;; end of esample.lsp ********************************************************
