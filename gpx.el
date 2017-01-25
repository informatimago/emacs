(progn
  ;; http://www.topografix.com/GPX/1/1/#type_wptType
  ;; http://www.topografix.com/gpx.asp

  (java-defstruct gpx
    (provider String)
    (parsed boolean)
    (waypoints ArrayList<Point> )
    (routes ArrayList<Route> )
    (tracks ArrayList<Track> ))


  (java-defstruct Point
    (latitude Double)
    (longitude Double)
    (elevation Double)
    (time longMillis)
    (magneticVariation Double)
    (geoidHeight Double)
    (name String)
    (comment String)
    (description String)
    (source String)
    (link String)
    (symbol String)
    (type String)
    (fix String)
    (satellite Integer)
    (horizontalDilutionOfPrecision Double)
    (verticalDilutionOfPrecision Double)
    (ageOfGpsData Double)
    (dGpsId Integer))

  (java-defstruct Segment
    (points ArrayList<Point>))

  (java-defstruct Track
    (name String)
    (comment String)
    (description String)
    (source String)
    (link String)
    (number Integer)
    (type String)
    (segments ArrayList<Segment>))

  (java-defstruct Route
    (name String)
    (comment String)
    (description String)
    (source String)
    (link String)
    (number Integer)
    (type String)
    (points ArrayList<Point>)))





(defun java-generate-structure-class (class-name slots)
  (insert (format "\npublic class %s {\n" class-name))
  (insert (format "  public %s(){}\n" class-name))
  (dolist (slot slots)
    (destructuring-bind (name type &rest options) slot
      (let ((came (java-capitalize-first-letter (etypecase name
                                                  (string name)
                                                  (symbol (symbol-name name)))))
            (read-only (member :read-only options)))
        (insert (format "  protected %s m%s;\n" type came))
        (insert (format "  public %s get%s(){return m%s;}\n" type came came))
        (unless read-only
          (insert (format "  public void set%s(%s a%s){m%s=a%s;}\n" came type came came came))))))
  (insert "}\n"))


(defun generate-gpx-bundle (name slots)
  (let* ((cname (java-capitalize-first-letter name))
         (attributes (remove-if-not (lambda (slot) (member :attribute slot)) slots))
         (elements   (remove-if     (lambda (slot) (member :attribute slot)) slots)))
    (insert (format "\n// %s\n" name)
            "public Bundle asBundle(){\n"
            "  Bundle bundle=new Bundle();\n")
    (dolist (slot slots)
      (destructuring-bind (jname jtype xname &rest options) slot
        (declare (ignore options))
        (let* ((cname  (java-capitalize-first-letter jname))
               (sjtype (symbol-name jtype))
               (get    (format "get%s()" cname))
               (put    (case jtype
                         ((String)              "putString")
                         ((Integer longMillis)  "putLong")
                         ((Double)              "putDouble")
                         (t                     nil))))
          (when put
           (insert (format "  if(%s!=%s){\n" get (if (eq 'longMillis jtype) 0 "null"))
                   (format "    bundle.%s(\"%s\",%s);\n" put jname get )
                   "  }\n")))))
    (insert "  return bundle;\n"
            "}\n\n")))

(defun generate-gpx-parse (name slots)
  (let* ((cname (java-capitalize-first-letter name))
         (jname (format "parse%s" cname))
         (attributes (remove-if-not (lambda (slot) (member :attribute slot)) slots))
         (elements   (remove-if     (lambda (slot) (member :attribute slot)) slots)))
    (insert (format "\nprotected %s %s(XmlResourceParser parser) throws XmlPullParserException,IOException {\n" name jname)
            (format "  %s object=new %s();\n" name name)
            "  String value;\n")
    (dolist (attribute attributes)
      (destructuring-bind (jname jtype xname &rest options) attribute
        (declare (ignore options))
        (let ((cname (java-capitalize-first-letter jname)))
          (insert (format "  value=getValueOfAttributeNamed(parser,\"%s\");\n" xname)
                  "  if(value!=null){\n"
                  (format "  object.set%s(%s);\n"
                          cname
                          (case jtype
                            ((String)              "value")
                            ((Integer longMillis)  "Integer.parseInt(value)")
                            ((Double)              "Double.parseDouble(value)")
                            (t                     (format "(%s)value" jtype))))
                  "  }\n"))))
    (insert "  parser.next();\n"
            "  int eventType=parser.getEventType();\n"
            "  while(eventType!=XmlPullParser.END_TAG){\n"
            "    if(eventType==XmlPullParser.START_TAG){\n"
            "     ")
    (dolist (element elements)
      (destructuring-bind (jname jtype xname &rest options) element
        (declare (ignore options))
        (let ((cname (java-capitalize-first-letter jname))
              (sjtype (symbol-name jtype)))
          (insert (format " if(parser.getName().equals(\"%s\")){\n" xname))
          (if (prefixp "ArrayList" sjtype)
              (insert (format "      object.add%s(parse%s(parser));\n"
                              cname (substring sjtype 10 (1- (length sjtype)))))
              (insert
               "        value=parser.getText();\n"
               "        if(value!=null){\n"
               (format "        object.set%s(%s);\n"
                       cname
                       (case jtype
                         ((String)              "value")
                         ((Integer)             "Integer.parseInt(value)")
                         ((Double)              "Double.parseDouble(value)")
                         ((longMillis)          "parseISO8601Date(value)")
                         (t                     (format "(%s)value" jtype))))
               "        }\n"))
          (insert "      }else"))))
    (insert "{\n"
            " // ignore unknown tags\n"
            "      }\n"
            "    }\n"
            "    parser.next();\n"
            "  }\n"
            "  return object;\n"
            "}\n\n")))


(defmacro gpx-parse (name &rest slots)
  (declare (indent 1))
  (generate-gpx-bundle name slots)
  ;; (generate-gpx-parse name slots)
  )


(progn
  ;; http://www.topografix.com/GPX/1/1/#type_wptType
  ;; http://www.topografix.com/gpx.asp


  (gpx-parse Gpx
    (metadata Metadata metadata)
    (waypoints ArrayList<Point> wpt)
    (routes ArrayList<Route> rte)
    (tracks ArrayList<Track> trk))

  (gpx-parse Metadata
    (name String name)
    (description String desc)
    (author Person author)
    (copyright Copyright copyright)
    (link Link link)
    (time String time)
    (keywords String keywords)
    (bounds Bounds bounds)
    (extensions String extensions))

  (gpx-parse Copyright
    (author String author)
    (year Integer year)
    (license String license))

  (gpx-parse Person
    (name String name)
    (email Email email)
    (link Link link))

  (gpx-parse Bounds
    (minimum-latitude Double minlat)
    (minimum-longitude Double minlon)
    (maximum-latitude Double maxlat)
    (maximum-latitude Double maxlat))

  (gpx-parse Email
    (id String id)
    (domain String domain))

  (gpx-parse Link
    (href String href)
    (text String text)
    (type String type))


  (gpx-parse Point
    (latitude Double lat :attribute)
    (longitude Double lon :attribute)
    (elevation Double ele)
    (time longMillis time)
    (magneticVariation Double magvar)
    (geoidHeight Double geoidheight)
    (name String name)
    (comment String cmt)
    (description String desc)
    (source String src)
    (link String link)
    (symbol String sym)
    (type String type)
    (fix String fix)
    (satellite Integer sat)
    (horizontalDilutionOfPrecision  Double hdop)
    (verticalDilutionOfPrecision Double vdop)
    (ageOfGpsData Double ageofdgpsdata)
    (dGpsId Integer dgpsid))

  (gpx-parse Segment
    (points ArrayList<Point> trkpt))

  (gpx-parse Track
    (name String name)
    (comment String cmt)
    (description String desc)
    (source String src)
    (link String link)
    (number Integer number)
    (type String type)
    (segments ArrayList<Segment> trkseg))

  (gpx-parse Route
    (name String name)
    (comment String cmt)
    (description String desc)
    (source String src)
    (link String link)
    (number Integer number)
    (type String type)
    (points ArrayList<Point> rtept)))

