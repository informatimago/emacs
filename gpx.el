(progn
  ;; http://www.topografix.com/GPX/1/1/#type_wptType
  ;; http://www.topografix.com/gpx.asp

  (java-defstruct x
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
public class x {
  public x(){}
  protected String mProvider;
  public String getProvider(){return mProvider;}
  public void setProvider(String aProvider){mProvider=aProvider;}
  protected boolean mParsed;
  public boolean getParsed(){return mParsed;}
  public void setParsed(boolean aParsed){mParsed=aParsed;}
  protected ArrayList<Point> mWaypoints;
  public ArrayList<Point> getWaypoints(){return mWaypoints;}
  public void setWaypoints(ArrayList<Point> aWaypoints){mWaypoints=aWaypoints;}
  protected ArrayList<Route> mRoutes;
  public ArrayList<Route> getRoutes(){return mRoutes;}
  public void setRoutes(ArrayList<Route> aRoutes){mRoutes=aRoutes;}
  protected ArrayList<Track> mTracks;
  public ArrayList<Track> getTracks(){return mTracks;}
  public void setTracks(ArrayList<Track> aTracks){mTracks=aTracks;}
}

public class Point {
  public Point(){}
  protected Double mLatitude;
  public Double getLatitude(){return mLatitude;}
  public void setLatitude(Double aLatitude){mLatitude=aLatitude;}
  protected Double mLongitude;
  public Double getLongitude(){return mLongitude;}
  public void setLongitude(Double aLongitude){mLongitude=aLongitude;}
  protected Double mElevation;
  public Double getElevation(){return mElevation;}
  public void setElevation(Double aElevation){mElevation=aElevation;}
  protected longMillis mTime;
  public longMillis getTime(){return mTime;}
  public void setTime(longMillis aTime){mTime=aTime;}
  protected Double mMagneticVariation;
  public Double getMagneticVariation(){return mMagneticVariation;}
  public void setMagneticVariation(Double aMagneticVariation){mMagneticVariation=aMagneticVariation;}
  protected Double mGeoidHeight;
  public Double getGeoidHeight(){return mGeoidHeight;}
  public void setGeoidHeight(Double aGeoidHeight){mGeoidHeight=aGeoidHeight;}
  protected String mName;
  public String getName(){return mName;}
  public void setName(String aName){mName=aName;}
  protected String mComment;
  public String getComment(){return mComment;}
  public void setComment(String aComment){mComment=aComment;}
  protected String mDescription;
  public String getDescription(){return mDescription;}
  public void setDescription(String aDescription){mDescription=aDescription;}
  protected String mSource;
  public String getSource(){return mSource;}
  public void setSource(String aSource){mSource=aSource;}
  protected String mLink;
  public String getLink(){return mLink;}
  public void setLink(String aLink){mLink=aLink;}
  protected String mSymbol;
  public String getSymbol(){return mSymbol;}
  public void setSymbol(String aSymbol){mSymbol=aSymbol;}
  protected String mType;
  public String getType(){return mType;}
  public void setType(String aType){mType=aType;}
  protected String mFix;
  public String getFix(){return mFix;}
  public void setFix(String aFix){mFix=aFix;}
  protected Integer mSatellite;
  public Integer getSatellite(){return mSatellite;}
  public void setSatellite(Integer aSatellite){mSatellite=aSatellite;}
  protected Double mHorizontalDilutionOfPrecision;
  public Double getHorizontalDilutionOfPrecision(){return mHorizontalDilutionOfPrecision;}
  public void setHorizontalDilutionOfPrecision(Double aHorizontalDilutionOfPrecision){mHorizontalDilutionOfPrecision=aHorizontalDilutionOfPrecision;}
  protected Double mVerticalDilutionOfPrecision;
  public Double getVerticalDilutionOfPrecision(){return mVerticalDilutionOfPrecision;}
  public void setVerticalDilutionOfPrecision(Double aVerticalDilutionOfPrecision){mVerticalDilutionOfPrecision=aVerticalDilutionOfPrecision;}
  protected Double mAgeOfGpsData;
  public Double getAgeOfGpsData(){return mAgeOfGpsData;}
  public void setAgeOfGpsData(Double aAgeOfGpsData){mAgeOfGpsData=aAgeOfGpsData;}
  protected Integer mDGpsId;
  public Integer getDGpsId(){return mDGpsId;}
  public void setDGpsId(Integer aDGpsId){mDGpsId=aDGpsId;}
}

public class Segment {
  public Segment(){}
  protected ArrayList<Point> mPoints;
  public ArrayList<Point> getPoints(){return mPoints;}
  public void setPoints(ArrayList<Point> aPoints){mPoints=aPoints;}
}

public class Track {
  public Track(){}
  protected String mName;
  public String getName(){return mName;}
  public void setName(String aName){mName=aName;}
  protected String mComment;
  public String getComment(){return mComment;}
  public void setComment(String aComment){mComment=aComment;}
  protected String mDescription;
  public String getDescription(){return mDescription;}
  public void setDescription(String aDescription){mDescription=aDescription;}
  protected String mSource;
  public String getSource(){return mSource;}
  public void setSource(String aSource){mSource=aSource;}
  protected String mLink;
  public String getLink(){return mLink;}
  public void setLink(String aLink){mLink=aLink;}
  protected Integer mNumber;
  public Integer getNumber(){return mNumber;}
  public void setNumber(Integer aNumber){mNumber=aNumber;}
  protected String mType;
  public String getType(){return mType;}
  public void setType(String aType){mType=aType;}
  protected ArrayList<Segment> mSegments;
  public ArrayList<Segment> getSegments(){return mSegments;}
  public void setSegments(ArrayList<Segment> aSegments){mSegments=aSegments;}
}

public class Route {
  public Route(){}
  protected String mName;
  public String getName(){return mName;}
  public void setName(String aName){mName=aName;}
  protected String mComment;
  public String getComment(){return mComment;}
  public void setComment(String aComment){mComment=aComment;}
  protected String mDescription;
  public String getDescription(){return mDescription;}
  public void setDescription(String aDescription){mDescription=aDescription;}
  protected String mSource;
  public String getSource(){return mSource;}
  public void setSource(String aSource){mSource=aSource;}
  protected String mLink;
  public String getLink(){return mLink;}
  public void setLink(String aLink){mLink=aLink;}
  protected Integer mNumber;
  public Integer getNumber(){return mNumber;}
  public void setNumber(Integer aNumber){mNumber=aNumber;}
  protected String mType;
  public String getType(){return mType;}
  public void setType(String aType){mType=aType;}
  protected ArrayList<Point> mPoints;
  public ArrayList<Point> getPoints(){return mPoints;}
  public void setPoints(ArrayList<Point> aPoints){mPoints=aPoints;}
}



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
    (waypoints ArrayList<Point> wpt)
    (routes ArrayList<Route> rte)
    (tracks ArrayList<Track> trk))
  
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

