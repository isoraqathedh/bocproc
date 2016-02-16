((:tags
  ;;; Tags
  ;; A property list that follows two strings.
  ;; The two strings represent, in turn:
  ;; - a SYMBOL: a short code for the object that this thing names.
  ;; - a NAME: The full name of the object as it is most commonly known as.
  ;; All properties in the plist alist are optional
  ;; and represent alternate forms of the name of the object
  ;; such as ASCII form, true form, etc.

  (:languages
   ;; Main four
   ("EP" "Egonyota Pasaru")
   ("SX" "Serakafph Xaxex")
   ("EQ" "yukũa|Elaga ütæk|Qvaḻsa"
         :ascii-name "yukua|Elaga utak|Qvalsa")
   ("Ct" "Cipogrtesaj")

   ;; PSD
   ("Yk" "Yoskrai")
   ("Ag" "Âagenzbèe" :ascii-name "Aahqgenzbeeq")
   ("Cd" "Cindri")
   ("Rs" "Rattssaw")
   ("GE" "Gwa-elohba")
   ("NH" "Nnn Heeel")
   ;; Reserved for future use
   ;; ("Gb" "Genbarg")
   ;; ("Or" "Oraṕ" :ascii-name "Oraf")

   ;; XAX
   ("Sk" "Sekapon")
   ("Ya" "Ya-kĕnaj" :ascii-name "Ya-kewnaj")
   ("OF" "Ouduec Fxelw")

   ;; Others
   ("Ub" "Uvbraot")
   ("MGY" "Man Gog Yuu")
   ("AKF" "Altrvukaif")
   ("RDWA" "Rďƿa" :ascii-name "Rdhwqa"))

  (:worlds
   ("PSD" "Pseudo" :true-name "Pasaru")
   ("XAX" "Xaxex")
   ("CIR" "Circular" :true-name "Cipog")
   ("QUX" "Quaxtion")
   ("LEB" "Leħbraot" :ascii-name "Lehqhbraot")
   ("BIG" "Bigencenryin"))

  (:special-tags
   ("RSGN" "Road signs")
   ("OVEX" "Octovexillology")))

;;; Ignore list
 ;; For each book (whose code is indicated by the key),
 ;; a list of objects that indicate its ignorance.
 ;; Lists indicate that the specified pages are ignored in that particular book,
 ;; whereas T indicates the entire book is ignored.
 ;; For readability, it is advised to write the first cons in dotted form,
 ;; even if another cons follows.

 (:ignore-list
  ("boc"
   (1  . T)
   (2  . T)
   (3  . T)
   (4  . T)
   (5  . T)
   (6  . (2 3))
   (7  . (38 39 49))
   (8  . (21 27 28))
   (9  . (34 37))
   (10 . (41))
   (11 . (2))
   (13 . (47))
   (14 . (29 40))
   (18 . (15)))
  ("purple"
   (1 . (2)))))
