((:tag-props
  (:language :metadata "Langs" :tumblr "conlangs" :filename "Trl")
  (:world :metadata "Worlds" :tumblr "conworlds" :filename "Tpl")
  (t :metadata "Other/Misc" :tumblr "misc" :filename "Mis"))
 (:tags
  ;;; Tags
  ;; A property list that follows a string:
  ;; a SYMBOL: a short code for the object that this thing names.
  ;; All properties in the plist alist are optional except two:
  ;; NAME: The full name of the object as it is most commonly known as, and
  ;; TYPE: the type of tag that it belongs to.
  ;; Other keys and values
  ;; represent alternate forms of the name of the object
  ;; such as ASCII form, true form, etc.

  ;; Main four
  ("EP" :name "Egonyota Pasaru" :type :language)
  ("SX" :name "Serakafph Xaxex" :type :language)
  ("EQ" :name "yukũa|Elaga ütæk|Qvaḻsa"
        :type :language
        :ascii-name "yukua|Elaga utak|Qvalsa")
  ("Ct" :name "Cipogrtesaj" :type :language)

  ;; PSD
  ("Yk" :name "Yoskrai" :type :language)
  ("Ag" :name "Âagenzbèe" :type :language :ascii-name "Aahqgenzbeeq")
  ("Cd" :name "Cindri" :type :language)
  ("Rs" :name "Rattssaw" :type :language)
  ("GE" :name "Gwa-elohba" :type :language)
  ("NH" :name "Nnn Heeel" :type :language)
  ;; Reserved for future use
  ;;R ("Gb" :name "Genbarg" :type :language)
  ;;R ("Or" :name "Oraṕ" :type :language :ascii-name "Oraf")

  ;; XAX
  ("Sk" :name "Sekapon" :type :language)
  ("Ya" :name "Ya-kĕnaj" :type :language :ascii-name "Ya-kewnaj")
  ("OF" :name "Ouduec Fxelw" :type :language)

  ;; Others
  ("Ub" :name "Uvbraot" :type :language)
  ("MGY" :name "Man Gog Yuu" :type :language)
  ("AKF" :name "Altrvukaif" :type :language)
  ;;R ("RDWA" :type :language "Rďƿa" :ascii-name "Rdhwqa")
  ("GMF" :name "g'Mòdyfäjq" :type :language :ascii-name "gModifajq")

  ;; Worlds
  ("PSD" :name "Pseudo" :type :world :true-name "Pasaru")
  ("XAX" :name "Xaxex" :type :world)
  ("CIR" :name "Circular" :type :world :true-name "Cipog")
  ("QUX" :name "Quaxtion" :type :world)
  ("LEB" :name "Leħbraot" :type :world :ascii-name "Lehqhbraot")
  ("BIG" :name "Bigencenryin" :type :world)

  ;;; Special tags
  ;; These are thematic tags, tags that describe a running theme;
  ;; they can be used in any combination with the tags above.
  ;; They are usually mutually exclusive with each other and
  ;; are in English rather than in their own language.
  ("RSGN" :type :special :name "Road signs")
  ("OVEX" :type :special :name "Octovexillology"))

 (:ignore-list
  ;;; Ignore list
  ;; For each book (whose code is indicated by the key),
  ;; a list of objects that indicate its ignorance.
  ;; Lists indicate that the specified pages are ignored in that particular book,
  ;; whereas T indicates the entire book is ignored.
  ;; For readability, it is advised to write the first cons in dotted form,
  ;; even if another cons follows.
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
