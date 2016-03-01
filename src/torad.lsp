;;; ***************************************************************************
;;;        torad.lsp
;;;        export RADIANCE scene description files from Autocad.
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
;;;
;;; changes:
;;; 04.11.1997 mod gm - make sure all numbers are output as decimals.
;;;
;;; ***************************************************************************

;;; general environment setup.
;;; load compiled files if possible or else sources.

(progn
  (setq *torad_preverr* *error*
		*error* '((msg)
				  (setq *error* *torad_preverr*)
				  (prompt "Load failed for torad.lsp!\n")
				  (if (null BD4A)
		  (prompt "Extended Lisp compiler not supported on this platform!\n"))
				  (princ) ) )
  (if (null *col*) (setq *col* 10))
  (if (null  *toradtypelist*) (setq *toradtypelist*
	'("3DFACE""TRACE""SOLID""LINE""PLINE""WPLINE""CIRCLE""ARC""PMESH""PFACE")) )
  (if (null *exportsmode*) (setq *exportsmode* "Color" ))
  (if (null *exportnsegs*) (setq *exportnsegs*  16     ))
  (if (null *toraddlgpos*) (setq *toraddlgpos* '(-1 -1)))
  (cond ( (null (or (and BD4A (load "esample.bi4" NIL)) (load "esample" NIL)))
		  (prompt "Can't load sampling functions from \"esample.lsp\"!\007\n")
		  (exit) )
		(T NIL) )
  (cond ( (null (or (and BD4A (load "vector.bi4"  NIL)) (load "vector"  NIL)))
		  (prompt "Can't load vector functions from \"vector.lsp\"!\007\n")
		  (exit) )
		(T NIL) )
  )


;;; ***************************************************************************

(defun *torad_error* (msg)
  ;; error handling for torad.lsp.
  (cond ( (and (/= "console break" msg)
               (/= "Function cancelled" msg))
          (terpri)
          (princ msg) ))
  (torad_reset) )


(defun torad_setup ()
  ;; global setup for torad.lsp.
  (regapp "MKVOL_LSP_01")
  (setq *Exportentlist* NIL
        *exportblocklist* NIL
        *FILE* NIL
        *torad_preverr* *error*
        *error* *torad_error*
))

(defun torad_reset ()
  ;; global reset for torad.lsp.
  (if *FILE* (close *FILE*))
  (setq *error* *torad_preverr*
        *FILE* NIL
        *exportentlist* NIL
        *exportblocklist* NIL
        *valuablepolylist* NIL )
  (princ) )


;;; ***************************************************************************
(defun c:torad (/ stat fname selset blocklevel home dwg
				   filelist matlist erot sun view)
  ;; main control. 
  (torad_setup)
  (setq *exporttruelays* (vislaylist)) ; collect names of visible layers.
  ;(if (and (wcmatch (getvar "acadver") "12*")
  (if (and (> (atoi (substr (getvar "acadver") 1 2)) 11)
		  (findfile "torad.dcl") )
	  (setq stat (torad_dlg)
			filelist *toradfilelist* )
	  (setq stat -1) )
  (if (> 0 stat)
	  (setq filelist (setradparams)
			*toradfilelist* filelist
			stat 1) )
  (cond ( (< 0 stat)
		  (setq  blocklevel 1)
		  (setq fname (strcase (cdr (assoc "prefix" filelist)) T))
		  (cond ( (and (assoc "files" filelist)
					   (setq selset (ssget)) )
				  (makeentlist )
				  (setq *valuablepolylist* *toradtypelist*)
				  (sampleents selset )
				  (while *exportblocklist*
						 (sampleblocks blocklevel )
						 (setq blocklevel (1+ blocklevel)) )
				  (setq matlist (writerad fname))
				  (if (assoc "mat" filelist)
					  (writeradmatlist fname matlist) )
				  (if (setq erot (cdr (assoc "master" filelist)))
					  (writeradtot fname erot matlist) )
				  (if (assoc "make" filelist)
					  (writeradmake fname matlist) ) )
				(T NIL) )
		  (if (setq view (cdr (assoc "view" filelist)))
			  (writeradview fname view) )
		  (if (setq sun (cdr (assoc "light" filelist)))
			  (writeradsun fname sun) )
		  )
		(T NIL) )
  (torad_reset ) )



;;; GENERAL SETUP **********************************************************

;;; currently supported entity types for torad.
(setq *toradetypes* '(
                ("3DFACE"   "\n    Planarized faces of 3DFACEs" )
                ("TRACE"    "\n       Extruded and flat TRACEs" )
                ("SOLID"    "\n       Extruded and flat SOLIDs" )
                ("CIRCLE"   "\n      Extruded and flat CIRCLEs" )
                ("ARC"      "\n         Extruded faces of ARCs" )
                ("LINE"     "\n        Extruded faces of LINEs" )
                ("PLINE"    "\n    Extruded faces of 2D-PLINEs" )
                ("WPLINE"   "\n    Constant width of 2D-PLINES" )
                ("POLYGON"  "\nClosed 2d-polylines as POLYGONs" )
                ("PMESH"    "\n             Faces of 3D-MESHes" )
                ("PFACE"    "\n             Faces if POLYFACEs" )
                ("POINT"    "\n   Points as SPHEREs or BUBBLEs" )
                ))



(defun setradparams (/ filelist typelist types wcsrot dwg home fname)
  ;; setup on older versions than 12.
  (toradshowitems nil)
  (prompt                 "\n\n       Entity data collected by:  ")
  (princ *exportsmode*)
  (prompt                   "\n Number of segments for circles:  ")
  (princ *exportnsegs*)
  (initget "Yes No")
  (cond ( (= "Yes"
			 (getkword "\n\n        Do you want to change anything? <No>: "))
		  (terpri)
		  (foreach item *toradetypes*
				   (toradsetitem (car item) nil))
		  (setsamplemode nil)
		  (setnumsegs nil)
		  )
		( T NIL) )
  (initget "Yes No")
  (cond ( (/= "No" (getkword   "\n       Write geometry data to file <Yes>?: "))
		  (setq filelist '(("files")))
		  (initget "Yes No")
		  (cond ( (= "Yes"
					 (getkword "\n      Write organizing control-file <No>?: "))
				  (setq wcsrot
					  (getreal "\n WCS rotation from East to X <0.0>: ")
					  filelist (cons (cons "master" (if wcsrot wcsrot 0.0))
									 filelist) )
				  (initget "Yes No")
				  (if (= "Yes"
					 (getkword "\n  Write execution rules to makefile <No>?: "))
					  (setq filelist (cons '("make") filelist)) ) ) )
		  (initget "Yes No")
		  (if (= "Yes"
				 (getkword "\n Write materials (all same) to file <No>?: "))
			  (setq filelist (cons '("mat") filelist)) ) )
		(T NIL) )
  (initget "Yes No")
  (if (= "Yes"
		 (getkword "\n            Write view to view-file <No>?: "))
	  (setq filelist (cons (cons "view" (askview)) filelist)) )
  (initget "Yes No")
  (if (= "Yes"
		 (getkword "\n       Write sun definition to file <No>?: "))
	  (setq filelist (cons (cons "light" (asksun)) filelist)) )
  (setq dwg (getvar "DWGNAME")
		fname (getstring
			   (strcat "\n\nprefix for output-file <" dwg ">: ")))
  (if (= 0 (strlen fname)) (setq fname dwg))
  (if (and (= "~" (substr fname 1 1))
		   (setq home (getenv "HOME")) )
	  (setq fname (strcat home (substr fname 2) )) )
  (cons (cons "prefix" fname) filelist) )



(defun toradshowitems (stdalone / types typelist)
  ;; display setting of sampled entity types.
  (textpage)
  (if stdalone (torad_setup))
  (setq types *toradetypes*
		typelist *toradtypelist* )
          (prompt         "\n\n           TORAD sampling modes:")
  (prompt                   "\n -------------------------------")
				  (prompt "\n\n             Collected entities:\n")
  (foreach item  types
           (princ (strcat (cadr item) ":  "))
           (princ (if (member (car item) typelist) "Y" "N")) )
  (if stdalone (torad_reset)) )



(defun toradsetitem (item stdalone / types old new tstr oldl newl)
  ;; set sampled entity types.
  (if stdalone (torad_setup))
  (initget "Yes No")
  (setq oldl *toradtypelist*
        types *toradetypes*
        tstr (assoc item types)
        old (if (member (car tstr) oldl) "Y" "N")
        new (getkword (strcat (cadr tstr) " <" old ">: ")) )
  (cond ( (and new (/= 0 (strlen new))
               (/= old (setq new (substr new 1 1))))
          (setq newl (if (= New "Y")
                         (cons item oldl)
                         (append (cdr (member item oldl))
                                 (cdr (member item (reverse oldl))) ) ) )
		  (setq *toradtypelist* newl) )              
        (T NIL) )        
  (if stdalone (torad_reset)) )


(defun askview (/ vlist num view res)
  ;; set view number to export.
  (setq vlist (list (cons 0 "Current"))
		num 0
		res -1)
  (while (setq view (tblnext "VIEW" (not view)))
		 (setq num (1+ num)
			   vlist (cons (cons num (cdr (assoc 2 view))) vlist) ) )
  (prompt "\nNUMBER  VIEW")
  (prompt "\n------------\n")
  (foreach item (reverse vlist)
		   (princ (car item))(princ (strcat "    " (cdr item) "\n")) )
  (while (and res (or (> 0 res)(< num res)))
		 (setq res (getint "\n View Number <0>: ")) )
  (if res res 0) )


(defun asksun (/ vlist val)
  ;; set lighting parameters.
  (foreach item '(("\n     Hour <16.5>: " 16.5 T   )
				  ("\n      Day   <01>: " 01   NIL )
				  ("\n    Month   <08>: " 08   NIL )
				  ("\n Timezone   <-1>: " -1   NIL )
				  ("\n Latitude <47.5>: " 47.5 T   )
				  ("\nLongitude <-8.5>: " -8.5 T   ) )
		   (if (null (setq val (if (last item)
								   (getreal (car item))
								   (getint (car item)) )))
			   (setq val (cadr item)) )
		   (setq vlist (cons (if (last item)(rtos val 2)(itoa val)) vlist)) )
  vlist )


;;; SAMPLING SETUP ***********************************************************

(defun setradsamplemode (stdalone / samplemode)
  ;; set sorting criteria.
  (if stdalone (torad_setup))
  (initget "Layer Toplayer Color")
  (setq samplemode *exportsmode*
        samplemode (getkword (strcat "\n\ncollect data by Color/Layer/Toplayer <"                                     samplemode ">: ") ) )
  (if samplemode (setq *exportsmode* samplemode))
  (if stdalone (torad_reset)) )



(defun setradnumsegs (stdalone / numsegs)
  ;; set arc smoothing.
  (if stdalone (torad_setup))
  (setq numsegs *exportnsegs*
        numsegs (getint (strcat "\nNumber of segments for circles (arcs) <"
                                     (itoa numsegs) ">: ") ) )
  (if numsegs (setq *exportnsegs* numsegs))
  (if stdalone (torad_reset)) )


;;; DIALOG BOX CALL FOR TORAD *************************************************

(defun torad_dlg (/ dcl_id typelist dwgname dwgprefix num view viewlist stat)
  ;; dialog box control for Autocad 12 and later (?).
  (setq dwgname (getvar "dwgname")
		dwgprefix (strcat (getvar "dwgprefix") "*")
		num 0 )
  (if (wcmatch dwgname dwgprefix)
	  (setq dwgname (strcat "./" (substr dwgname (strlen dwgprefix)))) )
  ;; load and execute dialog if possible.
  (setq dcl_id (load_dialog "torad.dcl"))
  (cond ( (> 0 dcl_id)
		  (alert "\nCouldn't load dialog!")
		  (setq stat -1))
		( (not (new_dialog "radiance" dcl_id "" *toraddlgpos*))
		  (alert "\nCouldn't open dialog!")
		  (setq stat -1) )
		(T
		 ;; setup view list.
		 (start_list "viewlist" 3)
		 (add_list "current")
		 (while (setq view (tblnext "VIEW" (not view)))
				(setq viewlist (cons (cons num view) viewlist)
					  num (1+ num) )
				(add_list (cdadr view)) )
		 (end_list)
		 ;; setup entity types
		 (mapcar '(lambda (item)
						  (set_tile (car item)
									(if (member (car item) *toradtypelist*)
										"1" "0" ) ) )
				 *toradetypes*)
		 ;; setup filetypes section.
		 (mode_tile "viewlist"    1)
		 (mode_tile "sunvals"     1)
		 (mode_tile "masterblock" 1)
		 (mode_tile "prefix"      2)
		 ;; setup default values.
		 (set_tile "prefix"  dwgname)
		 (set_tile "make"    "1")
		 (set_tile "nsegs"   (itoa *exportnsegs*))
		 (set_tile "sample"  *exportsmode*)
		 ;; initialize callback functions.
		 (action_tile "files"  "(toggle_files)")
		 (action_tile "master" "(toggle_master)")
		 (action_tile "light"  "(toggle_light)")
		 (action_tile "view"   "(toggle_view)")
		 (action_tile "prefix" "(torad_enddlg)")
		 (action_tile "accept" "(torad_enddlg)")
		 (action_tile "cancel" "(torad_candlg)")
		 ;; go for it.
		 (setq stat (start_dialog))
		 (unload_dialog dcl_id)
		 ))
  stat )



(defun toggle_light ()
  ;; callback for sunlight toggle.
  (cond ( (= "1" (get_tile "light"))
		  (mode_tile "sunvals" 0)
		  (mode_tile "long" 2) )
		(T  (mode_tile "sunvals" 1)
			(mode_tile "prefix" 2) ) ) )



(defun toggle_master ()
  ;; calback for masterfile toggle.
  (if (= "1" (get_tile "master"))
	  (mode_tile "masterblock" 0)
	  (mode_tile "masterblock" 1) ) )



(defun toggle_view ()
  ;; callback for viewfile toggle.
  (if (= "1" (get_tile "view"))
	  (mode_tile "viewlist" 0)
	  (mode_tile "viewlist" 1) ) )



(defun toggle_files ()
  ;; callback for geometry files toggle.
  (cond ( (= "1" (get_tile "files"))
		  (mode_tile "filelist" 0)
		  (mode_tile "modes" 0)
		  (mode_tile "auxf" 0)
		  (toggle_master) )
		(T
		 (mode_tile "filelist" 1)
		 (mode_tile "modes" 1)
		 (mode_tile "auxf" 1) ) ) )



(defun torad_enddlg ()
  ;; callback for accepting dialog.
  ;; accepted if 'ok' or return in prefix field.
  (cond ( (= 2 $reason) nil)
		( T (getraddlgvalues)) ) )



(defun getraddlgvalues (/ home filelist lightlist typelist nmodl samplebase
			  prefix errval make lightval radtypelist samplemode east numsegs)
  ;; extract data if possible and close dialog box.
  ;; else give alert and stay.
  (cond ( (= "1" (get_tile "files"))
		  (setq typelist *toradetypes*)
		  (mapcar '(lambda (item)
						   (if (= "1" (get_tile (car item)))
							   (setq nmodl (cons (car item) nmodl)) ) )
				  typelist )
		  (setq radtypelist nmodl
				samplemode (get_tile "sample")
				numsegs (read (get_tile "nsegs"))
				filelist '(("files"))
				samplebase '(("mat")("master")("make")("view")("light") ) ) )
		(T (setq samplebase '(("view")("light"))) ) )
  (mapcar '(lambda (item)
				   (if (= "1" (get_tile (car item)))
					   (setq filelist (cons item filelist)) ) )
		  samplebase )
  (cond ( (assoc "master" filelist)
		  (setq east (read (get_tile "WCS rotation")))
		  (if (numberp east)
			  (setq filelist (subst (cons "master" east) '("master") filelist))
			  (setq errval "WCS rotation") ) )
		(T (if (setq make (member '("make") filelist))
			   (setq filelist (append (cdr make)
									  (cdr (member '("make")
												   (reverse filelist))))) ) ) )
  (cond ( (assoc "light" filelist)
		  (mapcar '(lambda (item)
						   (setq lightval (read (get_tile item)))
						   (if (numberp lightval)
							   (setq lightlist (cons (get_tile item) lightlist))
							   (setq errval item) ) )
				  '("Hour""Day""Month""TZ""Latitude""Longitude") )
		  (setq filelist (subst (cons "light" lightlist) '("light") filelist)) )
		(T NIL) )
  (if (assoc "view" filelist)
	  (setq filelist (subst (cons "view" (read (get_tile "viewlist")))
							'("view") filelist) ) )
  (setq prefix (get_tile "prefix"))
  (if (and (= "~" (substr prefix 1 1))
		   (setq home (getenv "HOME")) )
	  (setq prefix (strcat home (substr prefix 2))) )
  (setq filelist (cons (cons "prefix" prefix) filelist))
  (cond ( (and numsegs (not (numberp numsegs)))
		  (mode_tile "nsegs" 2)
		  (mode_tile "nsegs" 3)
		  (alert "Please enter a NUMBER for \"Number of Segments\" !") )
		( errval
		 (mode_tile errval 2)
		 (mode_tile errval 3)
		 (alert (strcat "Please enter a NUMBER for \"" errval "\" !")) )
		(T (if numsegs (setq *exportnsegs*  numsegs))
		   (if samplemode (setq *exportsmode* samplemode))
		   (if filelist (setq *toradfilelist* filelist))
		   (if radtypelist (setq *toradtypelist* radtypelist))
		   (setq *toraddlgpos* (done_dialog 1)) ) ) )



(defun torad_candlg ()
  ;; cancel button selected.
  (setq *toraddlgpos* (done_dialog 0)) )



;;; WRITES ******************************************************************

(defun writerad (fname / lplist lay radname radfname radfile ename matlist)
  ;; open files for radiance geometry description.
  (prompt "\nwriting out radiance-files:\n")
  (foreach lplist *exportentlist*
           (cond ( (cdr lplist)
               (setq lay (strcase (strcat (if (= "Color" *exportsmode*)
                                     "c_" "l_") (regulatename (car lplist)) ) T )
                     radname (strcat (noprefix fname) "_" lay)
                     radfname (strcat fname "_" lay ".rad") )
               (cond ( (setq radfile (setq *FILE* (open radfname "w")))
                       (writeradlist fname lplist lay radname radfname radfile)
                       (setq matlist (cons (list lay radname radfname) matlist))
                       (close radfile)
                       (setq *FILE* NIL) )
                     ( T (prompt "\nCan't open file \"" radfname
                                 "\" for write! ") ) ) )
                 (T NIL) ) )
  matlist )



(defun writeradlist (fname lplist lay radname radfname radfile
                           / ename contele num numstep numtot polylist)
  ;; write radiance geometry description.
  (princ (strcat "### Radiance scene-file:  " radfname) radfile)
  (princ (strcat "\n### Created: " (datestring)) radfile)
  (princ "\n### TORAD.LSP  by Georg Mischler\n\n" radfile)
  (princ "### make sure material " radfile) (princ radname radfile)
  (princ " is defined in a previous file!\n" radfile)
  (princ "\n### polygons for object " radfile)
  (princ  radname radfile) (princ "\n" radfile)
  (setq num 0
        numtot (length lplist)
        numstep 0 )
  (while (> numtot numstep)
         (prompt (strcat "  file: " radfname "   "
                         (itoa numstep) "/" (itoa numtot) " \r"))
         (setq numstep (min (+ numstep 10) numtot))
         (while (< num  numstep)
                (setq lplist (cdr lplist)
                      ename (car lplist)
                      num (1+ num) )
                (if (listp ename)
                    (setq contele (reverse (cdr ename))
                          ename (car ename))
                    (setq contele nil) )
                (writeradents ename contele radfile radname num) ) )
  (prompt (strcat "  file: " radfname "   " (itoa numstep) "       \n")) )



(defun writeradents (ename conte rfile radname num / typ data)
  ;; dispatch entities to extraction and write functions.
  (if ename (setq data (entget ename)
                  TYP (getetype data) ))
  (cond ( (valuablepoly typ)
          (cond ( (equal typ    "LINE")
                  (writeradpoly conte rfile radname num (linetopoly data)) )
                ( (equal typ   "PLINE")
                  (writeradpoly conte rfile radname num
								(plinetopoly data 1 *exportnsegs*)) )
                ( (equal typ "POLYGON")
                  (writeradpoly conte rfile radname num
								(plinetopoly data 2 *exportnsegs*)) )
                ( (equal typ  "WPLINE")
                  (writeradpoly conte rfile radname num
								(plinetopoly data 3 *exportnsegs*)) )
                ( (equal typ   "PMESH")
                  (writeradpoly conte rfile radname num (meshtopoly data)) )
                ( (equal typ   "PFACE")
                  (writeradpoly conte rfile radname num (pfacetopoly data)) )
                ( (equal typ  "3DFACE")
                  (writeradpoly conte rfile radname num (facetopoly  data)) )
                ( (equal typ   "TRACE")
                  (writeradpoly conte rfile radname num (tracetopoly data)) )
                ( (equal typ   "SOLID")
                  (writeradpoly conte rfile radname num (tracetopoly data)) )
                ( (equal typ  "CIRCLE")
                  (writeradcircle conte rfile radname num (circletorad data)) )
                ( (equal typ     "ARC")
                  (writeradpoly conte rfile radname num
								(arctopoly data *exportnsegs*)) )
                ( (equal typ   "POINT")
                  (writeradpoint conte rfile radname num (pointtorad data)) )
                (T NIL) ) )
        ( T NIL) ) )



(defun writeradpoly (contele radfile radname num polylist / len polnum)
  ;; write polygon lists to file.
  (if contele (setq polylist (trans_back polylist contele)))
  ;(showpolylist polylist) ; visual debugging.
  (setq polnum 0)
  (foreach poly polylist
           (cond ( (and poly (< 2 (setq len (length poly))))
                   (setq polnum (1+ polnum))
                   (princ (strcat "\n" radname " polygon " radname "."
                                  (itoa num) "." (itoa polnum)) radfile )
                   (princ "\n0\n0\n" radfile)
                   (princ (* len 3) radfile)
                   (foreach pt poly (printradpoint pt radfile))
                   (princ "\n" radfile) )
                 (T nil) ) ) )



(defun writeradcircle (contele radfile radname num polylist / len rad typ xname)
  ;; write circles as rings cylinders or tubes.
  (setq len (car polylist)
        rad (cadr polylist)
        xname (strcat radname "." (itoa num))
        polylist (if contele (car (trans_back (caddr polylist) contele))
                     (caaddr polylist) ) )
  (cond ( (= 0.0 len)
          (princ (strcat "\n" radname " ring " xname "\n0\n0\n8") radfile)
          (printradpoint (car polylist) radfile)
          (printradpoint (vector (car polylist)(cadr polylist)) radfile)
          (princ (strcat "     0     " (rtos rad 2) "\n" ) radfile) )
        ( T
         (cond ( (> 0.0 len) (setq typ "tube"))
               ( T (setq typ "cylinder")) )
         (princ (strcat "\n" radname " " typ " " xname ".1\n0\n0\n7") radfile)
          (printradpoint (car polylist) radfile)
          (printradpoint (cadr polylist) radfile)
          (princ (strcat "     " (rtos rad 2) "\n") radfile)
          (princ (strcat "\n" radname " ring " xname ".2\n0\n0\n8") radfile)
          (printradpoint (cadr polylist) radfile)
          (printradpoint (vector (car polylist)(cadr polylist)) radfile)
          (princ (strcat "     0     " (rtos rad 2) "\n" ) radfile)
          (princ (strcat "\n" radname " ring " xname ".3\n0\n0\n8") radfile)
          (printradpoint (car polylist) radfile)
          (printradpoint (vector (cadr polylist)(car polylist)) radfile)
          (princ (strcat "     0     " (rtos rad 2) "\n" ) radfile) ) ) )



(defun writeradpoint (conte rfile rname num polylist / center radius typ xname)
  ;; write point entities to file as spheres or bubbles.
  (setq radius (car polylist))
  (if (= 0.0 radius) (setq radius (getvar "PDSIZE")))
  (cond ( (= 0.0 radius) NIL)
        ( (< 0.0 radius) (setq typ "sphere"))
        ( (> 0.0 radius) (setq typ "bubble")) )
  (cond ( typ
         (setq xname (strcat rname "." (itoa num))
               center (caar (if conte
                                (trans_back (cadr polylist) conte)
                                (cadr polylist) ))
               )
         (princ (strcat "\n" rname " " typ " " xname "\n0\n0\n4") rfile)
         (printradpoint center rfile)
         (princ (strcat "     " (rtos radius 2) "\n") rfile) ) ) )



(defun printradpoint (point radfile)
  ;; write a single vertex to file.
  (foreach number point
           (princ "     " radfile)
           (princ (shortnumstr number 11) radfile) )
           (princ "\n" radfile) )



;;; WRITE ADDITIONAL CONTROL INFORMATION ************************************

(defun writeradsun (fname sun / sunfname sfname sunfile)
  ;; write a file containing a description of natural lighting.
  ;; generate a call to gensky and the source for the sky for time and place.
  (setq sunfname (strcat fname ".sun")
		sfname (noprefix sunfname) )
  (cond ( (setq sunfile (setq *FILE* (open sunfname "w")))
		  (princ (strcat "\nCreating sun-file: " sunfname))
		  (princ (strcat "### Radiance Sun-definition-file: " sfname) sunfile)
		  (princ (strcat "\n### Created: " (datestring)) sunfile)
		  (princ "\n### TORAD.LSP  by Georg Mischler\n" sunfile)
		  (princ "\n### Sun and sky definition at:" sunfile)
		  (princ (strcat "\n###     Longitude: " (nth 0 sun)) sunfile)
		  (princ (strcat "\n###      Latitude: " (nth 1 sun)) sunfile)
		  (princ (strcat "\n###      Timezone: " (nth 2 sun)) sunfile)
		  (princ (strcat "\n###         Month: " (nth 3 sun)) sunfile)
		  (princ (strcat "\n###           Day: " (nth 4 sun)) sunfile)
		  (princ (strcat "\n###          Hour: " (nth 5 sun)) sunfile)
		  (princ "\n\n!gensky " sunfile)
		  (princ (strcat (nth 3 sun) " " (nth 4 sun) " " (nth 5 sun)) sunfile)
		  (princ (strcat " -o " (car sun) " -a " (cadr sun)) sunfile)
		  (princ (strcat " -m " (rtos (* 15 (read (caddr sun))) 2) "\n") sunfile)
          (princ "\nskyfunc glow skyglow\n0\n0\n4 0.9 0.9 1 0\n" sunfile)
          (princ "\nskyglow source sky\n0\n0\n4 0 0 1 180\n" sunfile) )
		(T (princ (strcat "\nCan't open material-file " sunfname
						  " for write."))) ) )



(defun writeradmatlist (fname matlist / matfname matfile sfname)
  ;; write a list of materials from the used modifier names.
  ;; materials are all plastic of a constant grey.
  (setq matfname (strcat fname ".mat")
        sfname (noprefix fname) )
  (cond ( (setq matfile (setq *FILE* (open matfname "w")))
          (princ (strcat "\nCreating material-file: " matfname))
          (princ (strcat "### Radiance material-file:  " sfname ".mat") matfile)
          (princ (strcat "\n### Created: " (datestring)) matfile)
          (princ "\n### TORAD.LSP  by Georg Mischler\n\n" matfile)

          (foreach mat matlist
                   (princ (strcat "\nvoid plastic " (cadr mat)) matfile)
                   (princ "\n0\n0\n5 0.65 0.65 0.65 0.0 0.0\n" matfile)
          )
          (close matfile)
          (setq *FILE* NIL) )
        (T (princ (strcat "\nCan't open material-file " matfname
						  " for write." ))) ) )



(defun writeradtot (fname erot matlist / totfname totfile sfname infunc)
  ;; write a controlling master file to combine all the written data
  ;; into a complete RADIANCE scene description.
  (setq totfname (strcat fname ".rad")
        sfname (noprefix fname)
		infunc (if (/= 0.0 erot)
				   (strcat "\n!xform -rz " (rtos erot 2) " ")
				   "\n!cat " ) )
  (cond ( (setq totfile (setq *FILE* (open totfname "w")))
          (princ (strcat "\nCreating Master-file: " totfname))
          (princ (strcat "### Radiance Master-file: " sfname ".rad") totfile)
          (princ (strcat "\n### Created: " (datestring)) totfile)
          (princ "\n### TORAD.LSP  by Georg Mischler\n\n" totfile)
		  (if (assoc "light" *toradfilelist*)
			  (princ (strcat infunc sfname ".sun\n\n") totfile) )
		  (if (assoc "mat" *toradfilelist*)
			  (princ (strcat "!cat " sfname ".mat\n\n") totfile) )
          (foreach mat matlist
                   (princ (strcat "!cat " (cadr mat) ".rad\n" ) totfile) )
          (close totfile)
          (setq *FILE* NIL) )
        (T (princ (strcat "\nCan't open Master-file "
                          totfname " for write.") )) ) )



(defun writeradview (fname viewnum / viewfname vdir vpoint vmode target
						   lensl twist zvect vsize vlist viewfile)
  ;; write a RADIANCE viewfile either from the current view or
  ;; from a named view from the view table.
  (setq viewfname (strcat fname ".view"))
  (cond ( (= 0 viewnum)
		  (setq vdir (trans (getvar "VIEWDIR") 1 0 T)
				vmode (getvar "VIEWMODE")
				target (if (= 0 vmode)(getvar "VIEWCTR")(getvar "TARGET"))
				vpoint (transl-p (trans target 1 0 T)  vdir 1.0)
				lensl (getvar "LENSLENGTH")
				twist (getvar "VIEWTWIST")
				zvect (trans '(0.0 1.0 0.0) 2 0 T) ) )
		(T
		 (repeat viewnum (setq vlist (tblnext "VIEW" (not vlist))) )
		 (setq vdir (cdr (assoc 11 vlist))
			   vmode (cdr (assoc 71 vlist))
			   target (cdr (if (= 0 vmode) ; keep it simple...
							   (append (mapcar '+ (assoc 10 vlist)
									   (assoc 12 vlist) )'(0.0))
							   (assoc 12 vlist) ))
			   vpoint (transl-p target vdir 1.0)
			   lensl (cdr (assoc 42 vlist))
			   twist (cdr (assoc 50 vlist))
			   zvect (vect-prod '(0.0 0.0 1.0) vdir)
			   zvect (if (equal '(0.0 0.0 0.0) zvect 0.0000001)
						 '(0.0 0.1 0.0)
						 (vect-prod vdir zvect) ) ) ) )
  (if (= 0 vmode)
	  (setq vsize (rtos (getvar "VIEWSIZE") 2))
	  (setq vsize (rtos (/ (* 360 (atan (/ 12.0 lensl))) pi) 2)) )
  (setq vdir (mapcar '- vdir))
  (if (and (< 0 viewnum) (/= 0.0 twist))
	  (setq zvect (transf-p zvect (rot-3d-matrix (normalize vdir) twist))) )
  (if (and (< 0.7 (caddr zvect))(= 0.0 twist))
	  (setq zvect '(0.0 0.0 1.0)) )
  (cond ( (setq viewfile (setq *FILE* (open viewfname "w")))
          (princ (strcat "\nCreating View-file: " fname ".view"))
          (princ "rview -vt" viewfile)
          (princ (if (= 1 vmode) "v -vp " "l -vp ") viewfile)
          (mapcar '(lambda (pt) (princ (strcat (rtos pt 2) " ") viewfile)) vpoint )
		  (princ " -vd " viewfile)
          (mapcar '(lambda (pt) (princ (strcat (rtos pt 2) " ") viewfile)) vdir)
          (princ " -vu " viewfile)
          (mapcar '(lambda (pt) (princ (strcat (rtos pt 2) " ") viewfile)) zvect )
		  (princ (strcat " -vh " vsize " -vv " vsize " -vs 0 -vl 0\n") viewfile)
          (close viewfile)
          (setq *FILE* NIL) )
        (T (princ (strcat "\nCan't open view-file "
                                    viewfname " for write." ))) ) )



(defun writeradmake (fname matlist / makefname makefile sfname)
  ;; write a makefile for the UNIX utility make containing rules for
  ;; octree conversion, previewing with rview and batch rendering with rpict.
  (setq sfname (noprefix fname)
		makefname (strcat (substr fname 1 (- (strlen fname)
                                             (strlen sfname))) "makefile" ) )
  (cond ( (setq makefile (setq *FILE* (open makefname "w")))
          (princ (strcat "\nCreating makefile: " makefname))
          (princ (strcat "### makefile for Radiance-file: "sfname".rad")makefile)          (princ (strcat "\n### Created: " (datestring)) makefile)
          (princ "\n### TORAD.LSP  by Georg Mischler\n\n" makefile)
		  (princ "\nall:\n\t@echo \"  make what?\"" makefile)
		  (princ "\n\t@echo \"  enter \\\"make view\\\" or \\\"make pict\\\"\"\n" makefile)
          (princ (strcat "\nview:" sfname ".oct") makefile)
          (princ (strcat "\n\trview -ab 2 -vf " sfname".view "
                         sfname".oct &\n")makefile)
          (princ (strcat "\npict:" sfname ".oct") makefile)
          (princ (strcat "\n\trpict -ab 2 -vf " sfname".view "
                         sfname".oct > " sfname ".pic &\n")makefile)
          (princ (strcat "\n" sfname ".oct: ") makefile)
          (princ (strcat " \\\n         " sfname ".rad ") makefile)
          (foreach mat matlist
                   (princ (strcat " \\\n         " (cadr mat) ".rad") makefile))
          (princ (strcat "\n\toconv "sfname".rad > "sfname".oct\n") makefile)
          (princ (strcat "\nclean:\n\t @rm " sfname".oct\n") makefile)
          (close makefile)
          (setq *FILE* NIL) )
        (T (princ (strcat "\nCan't open makefile "
                          makefname " for write." ))) ) )


;;; ***************************************************************************
(defun regulatename (name / pos char)
  ;; eliminate illegal characters in filenames.
  (setq pos 1)
  (repeat (strlen name)
		  (setq char (substr name pos 1))
		  (if (or (= char "|")(= char "$"))
			  (setq name (strcat (substr name 1 (1- pos))
								 "_"
								 (substr name (1+ pos)))))
		  (setq pos (1+ pos)) )
  name )


;;;-----------------------------------------------------------------------------
(defun circletorad (data / center1 center2 radius dist plist)
  ;; extract a description of a circle for 'writeradcircle'.
  (setq center1 (cdr (assoc 10 data))
        radius (cdr (assoc 40 data))
        dist (cdr (assoc 39 data))
        center2 (list (car center1)(cadr center1)
                      (+ (caddr center1) (if dist dist 1.0)) )
        plist (trans_back (list (List center2 center1))
                          (list (cdr (assoc -1 data)))) )
  (list (if dist dist 0.0) radius plist) )
 
 
;;;-----------------------------------------------------------------------------
(defun pointtorad (data / center rad)
  ;; extract a description of a point for 'writeradpoint'.
  (setq center (cdr (assoc 10 data))
        rad (cdr (assoc 39 data))
        rad (if (and rad (/= 0.0 rad)) rad 0.0) )
  (list rad (list (list center))) )


;;; ***************************************************************************

(progn
  (prompt   "\n-- TORAD.LSP  -  1993 by Georg Mischler --\n")
  (prompt   "\n Enter \"TORAD\" for writing Radiance files.")
  (torad_reset) )

;;; ***************************************************************************
;;; end of torad.lsp.
;;; ***************************************************************************
