((:tag-props
  (:language :metadata "Langs" :tumblr "conlangs" :filename "Trl")
  (:world :metadata "Worlds" :tumblr "conworlds" :filename "Tpl")
  (t :metadata "Other/Misc" :type "misc" :filename "Mis"))
 (:tags
  ;;; Tags
  ;; A property list that follows two strings.
  ;; The two strings represent, in turn:
  ;; - a SYMBOL: a short code for the object that this thing names.
  ;; - a NAME: The full name of the object as it is most commonly known as.
  ;; The first key is always a tag
  ;; All properties in the plist alist are optional
  ;; and represent alternate forms of the name of the object
  ;; such as ASCII form, true form, etc.
  ;; as well as :TYPE, the type of tag that it belongs to.

  ;; Main four
  ("EP" "Egonyota Pasaru" :type :language)
  ("SX" "Serakafph Xaxex" :type :language)
  ("EQ" "yukũa|Elaga ütæk|Qvaḻsa"
        :type :language
        :ascii-name "yukua|Elaga utak|Qvalsa")
  ("Ct" "Cipogrtesaj" :type :language)

  ;; PSD
  ("Yk" "Yoskrai" :type :language)
  ("Ag" "Âagenzbèe" :type :language :ascii-name "Aahqgenzbeeq")
  ("Cd" "Cindri" :type :language)
  ("Rs" "Rattssaw" :type :language)
  ("GE" "Gwa-elohba" :type :language)
  ("NH" "Nnn Heeel" :type :language)
  ;; Reserved for future use
  ;;R ("Gb" "Genbarg" :type :language)
  ;;R ("Or" "Oraṕ" :type :language :ascii-name "Oraf")

  ;; XAX
  ("Sk" "Sekapon" :type :language)
  ("Ya" "Ya-kĕnaj" :type :language :ascii-name "Ya-kewnaj")
  ("OF" "Ouduec Fxelw" :type :language)

  ;; Others
  ("Ub" "Uvbraot" :type :language)
  ("MGY" "Man Gog Yuu" :type :language)
  ("AKF" "Altrvukaif" :type :language)
  ;;R ("RDWA" :type :language "Rďƿa" :ascii-name "Rdhwqa")
  ("GMF" "g'Mòdyfäjq" :type :language :ascii-name "gModifajq")

  ;; Worlds
  ("PSD" "Pseudo" :type :world :true-name "Pasaru")
  ("XAX" "Xaxex" :type :world)
  ("CIR" "Circular" :type :world :true-name "Cipog")
  ("QUX" "Quaxtion" :type :world)
  ("LEB" "Leħbraot" :type :world :ascii-name "Lehqhbraot")
  ("BIG" "Bigencenryin" :type :world))
 (:special-tags
  ;;; Special tags
  ;; These are thematic tags, tags that describe a running theme;
  ;; they can be used in any combination with the tags above.
  ;; They are usually mutually exclusive with each other and
  ;; are in English rather than in their own language.
  ("RSGN" "Road signs")
  ("OVEX" "Octovexillology"))

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
