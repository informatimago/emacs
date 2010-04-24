;;;;****************************************************************************
;;;;FILE:               constantes.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines constants of physics.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-12-04 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2003 - 2003
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************
(require 'cl)
(require 'pjb-cl)
(require 'pjb-strings)


;; http://lx50.fmridc.org/dmt/units_j.dat

(eval-when (compile load eval)
  (defvar *constantes* '()
    "List of constantes definies par def-phys-const.") ;;*constantes*
  )


(defmacro def-phys-const (name value unit &optional doc-string)
  (pushnew name *constantes*)
  (setq doc-string (or doc-string ""))
  `(progn
     (defconst ,name ,value ,doc-string)
     (put  ',name 'physical-constant t)
     (put  ',name 'unit ',unit)
     ,name)
  ) ;;def-phys-const


(defun physical-constant-p (name)
  (get name 'physical-constant)
  ) ;;physical-constant-p


(defun unit (name)
  (if (physical-constant-p name)
      (get name 'unit)
      (error "%s is not a physical constant (to my knowledge)." name))
  ) ;;unit


;; (get 'c 'unit)

(def-phys-const  N       6.022098e23           (^ mol -1)
                 "Avogadro's number") ;;N

(def-phys-const  H_0      1.01325e5            (* kg (^ m -1) (^ s -2))
                 "pression atmosphérique normale") ;;H_0

(def-phys-const  V_0      22.414e-3            (* (^ m 3) (^ mol -1))
                 "volume molaire du gaz parfait (CN)") ;;V_0

(def-phys-const  T_0       273.15              K
                 "zéro de l'échelle Celsius") ;;T_0

(def-phys-const  n_0 (/ N V_0)                 (^ m -3)
                 "Loschmidt's number") ;;n_0



(def-phys-const  R       8.3143                (* J (^ K -1) (^ mol -1))
                 "constante des gaz parfaits") ;;R

(def-phys-const  G       6.672e-11             (* (^ kg -1) (^ m 3) (^ s -2))
                 "constante de la gravitation") ;;G

(def-phys-const  e       1.602189e-19          C
                 "charge élémentaire") ;;e

(def-phys-const  c       299792458.0           (* m (^ s -1))
                 "célérité de la lumière") ;;c

(def-phys-const  k       1.38066e-23           (* J (^ K -1))
                 "Boltzmann's constant") ;;k

(def-phys-const  h       6.626176e-34          (* J s)
                 "Planck's constant") ;;h

(def-phys-const  m_p     1.67265e-27            kg
                 "Mass of proton") ;;m_p

(def-phys-const  m_n     1.67496e-27            kg
                 "Mass of neutron") ;;m_n

(def-phys-const  m_e     9.10953e-31            kg
                 "Mass of electron") ;;m_e

(def-phys-const  mu_0       (* 16e-7 (atan 1))  (* H (^ m -1))
                 "perméabilité du vide") ;;mu_0

(def-phys-const  epsilon_0  (/ 1 (* mu_0 c c))  (* F (^ m -1))
                 "permittivité du vide") ;;epsilon_0

(def-phys-const  g_n        9.80665             (* m (^ s -2))
                 "pesanteur normale") ;;g_n


;;----------------------------------------------------------------------
;; Solar System
;;----------------------------------------------------------------------


;; Sun

(def-phys-const sun-mass      1.989e+30            kg
                "Mass of Sun") ;;sun-mass

(def-phys-const sun-equatorial-radius 695e6        m
                "Equatorial Radius of Sun")

(def-phys-const sun-luminosity   3.827e26  J/s)

(def-phys-const sun-mean-surface-temperature 6274   K)

;; Earth

(def-phys-const earth-mass     5.9736e24           kg
                "Mass of Earth") ;;earth-mass

(def-phys-const earth-volume   108.321e19          (^ m 3)
                "Volume of Earth") ;;earth-volume

(def-phys-const earth-radius-equatorial   6378.1e3        m
                "Equatorial radius of Earth") ;;earth-radius-equatorial

(def-phys-const earth-radius-polar        6356.8e3        m
                "Polar radius of Earth") ;;earth-radius-polar

(def-phys-const earth-radius-mean         6371.0e3        m
                "Volumetric Mean Radius of Earth") ;;earth-radius-mean

(def-phys-const earth-core-radius         3485e3          m
                "Radius of core of Earth") ;;earth-core-radius

(def-phys-const earth-ellipticity          0.00335       1
                "Ellipticity of Earth") ;;earth-ellipticity

(def-phys-const earth-density             5515           (* kg (^ m -3))
                "Ellipticity of Earth") ;;earth-density


(def-phys-const earth-GM             .3986e15         (* (^ m 3) (^ s -2))
                "G × Mass of Earth") ;;earth-GM

(def-phys-const earth-albedo-bond                  0.306           1
                "Bond Albedo of Earth") ;;earth-albedo-bond

(def-phys-const earth-albedo-visual-geometric      0.367           1
                "Visual Geometric Albedo of Earth") ;;earth-albedo-visual-geometric

(def-phys-const earth-visual-magnitude            -3.86            1
                "Visual Magnitude (V(1,0)) of Earth") ;;earth-visual-magnitude


;;; Solar Irradiance          1367.6 W m-2
;;; Black-Body Temperature    254.3 K
;;; Moment of Inertia         0.3308 I MR-2
;;; J2                        1082.63 × 10-6
;;; Topographic Range         20 km



;;;; Orbital Parameters
;;;; Semimajor Axis           149.60 × 106 km
;;;; Sidereal Orbit Period    365.256 day
;;;; Tropical Orbit Period    365.242 day

(def-phys-const earth-perihelion           147.09e9       m
                "Perihelion of Earth") ;;earth-perihelion

(def-phys-const earth-aphelion             152.10e9       m
                "Aphelion of Earth") ;;earth-aphelion


(def-phys-const earth-mass                      5.9736e24  kg)
(def-phys-const earth-volume                    108.321e10  (^ km 3))
(def-phys-const earth-equatorial-radius         6378.1  km)
(def-phys-const earth-polar-radius              6356.8  km)
(def-phys-const earth-volumetric-mean-radius    6371.0  km)
(def-phys-const earth-core-radius               3485  km)
(def-phys-const earth-ellipticity               0.00335  1)
(def-phys-const earth-mean-density              5515 (* kg (^ m -3)))
(def-phys-const earth-surface-gravity           9.78 (* m (^ s -2)))
(def-phys-const earth-escape-velocity           11.186 (* km (^ s -1)))
(def-phys-const earth-gm                        0.3986e6 (* (^ km 3) (^ s -2)))
(def-phys-const earth-bond-albedo               0.306  1)
(def-phys-const earth-visual-geometric-albedo   0.367  1)
(def-phys-const earth-albedo-visual-magnitude   -3.86  1)
(def-phys-const earth-solar-irradiance          1367.6 (* W (^ m -2)))
(def-phys-const earth-black-body-temperature    254.3  K)
(def-phys-const earth-moment-of-inertia         0.3308 (* I (^ MR -2)))
(def-phys-const earth-j2                        1082.63e-6  1)
(def-phys-const earth-topographic-range         20  km)
(def-phys-const earth-semimajor-axis            149.60e6  km)
(def-phys-const earth-sidereal-orbit-period     365.256  day)
(def-phys-const earth-tropical-orbit-period     365.242  day)
(def-phys-const earth-pericenter                147.09e6  km)
(def-phys-const earth-apocenter                 152.10e6  km)
(def-phys-const earth-mean-orbital-velocity     29.78 (* km (^ s -1)))
(def-phys-const earth-maximum-orbital-velocity  30.29 (* km (^ s -1)))
(def-phys-const earth-minimum-orbital-velocity  29.29 (* km (^ s -1)))
(def-phys-const earth-sidereal-rotation-period  23.9345  hours)
(def-phys-const earth-length-of-day             24.0000  hours)
(def-phys-const earth-obliquity-to-orbit        23.45  1)
(def-phys-const earth-orbital-eccentricity      0.01671022  1)
(def-phys-const earth-orbital-inclination       0.00005  °)
(def-phys-const earth-longitude-of-ascending-node  -11.26064  °)
(def-phys-const earth-longitude-of-pericenter   102.94719  °)
(def-phys-const earth-mean-longitude            100.46435  °)
(def-phys-const earth-dipole-field-strength     0.3076 (* gauss (^ Re 3)))
(def-phys-const earth-latitude-of-dipole-north  78.6  N)
(def-phys-const earth-longitude-of-dipole-north  70.1  W)
(def-phys-const earth-dipole-offset-distance    0.0725  Re)
(def-phys-const earth-latitude-of-offset-vector  18.3  N)
(def-phys-const earth-longitude-of-offset-vector  147.8  E)
(def-phys-const Re 6378.1 km "Earth Radii")
(def-phys-const earth-surface-pressure          1014  mbar)
(def-phys-const earth-surface-density           1.217 (* kg (^ m -3)))
(def-phys-const earth-scale-height              8.5  km)
(def-phys-const earth-average-temperature       288  K)
(def-phys-const earth-diurnal-temperature-minimum 283 K)
(def-phys-const earth-diurnal-temperature-maximum 293 K)
(def-phys-const earth-surface-wind-speeds-minimum  0 (* m (^ s -1)))
(def-phys-const earth-surface-wind-speeds-maximum  100 (* m (^ s -1)))
(def-phys-const earth-mean-molecular-weight     28.97 (* g (^ mol -1)))
(def-phys-const earth-nitrogen-n2               0.78084  1)
(def-phys-const earth-oxygen-o2                 0.20946  1)
(def-phys-const earth-water-h2o                 0.01  1)
(def-phys-const earth-argon-ar                  0.00934  1)
(def-phys-const earth-carbon                    0.00035  1)
(def-phys-const earth-neon-ne                   0.00001818  1)
(def-phys-const earth-helium-he                 0.00000524  1)
(def-phys-const earth-ch4                       0.0000017  1)
(def-phys-const earth-krypton-kr                0.00000114  1)
(def-phys-const earth-hydrogen-h2               0.00000055  1)






(def-phys-const moon-mass                       0.07349e24  kg)
(def-phys-const moon-volume                     2.1973e10  (^ km 3))
(def-phys-const moon-radius-equatorial          1738  km)
(def-phys-const moon-polar-radius               1735  km)
(def-phys-const moon-volumetric-mean-radius     1737.5  1)
(def-phys-const moon-ellipticity                0.002  1)
(def-phys-const moon-density                    3340 (* kg (^ m -3)))
(def-phys-const moon-surface-gravity            1.62 (* m (^ s -2)))
(def-phys-const moon-escape-velocity            2.38 (* km (^ s -1)))
(def-phys-const moon-gm                         0.0049e6 (* (^ km 3) (^ s -2)))
(def-phys-const moon-albedo-bind                0.067  1)
(def-phys-const moon-albedo-visual-geometric    0.12  1)
(def-phys-const moon-albedo-visual-magnitude    +0.21  1)
(def-phys-const moon-solar-irradiance           1380 (* W (^ m -2)))
(def-phys-const moon-black-body-temperature     274.5  K)
(def-phys-const moon-moment-of-inertia          0.394 (* I (^ MR -2)))
(def-phys-const moon-topographic-range          16  km)
(def-phys-const moon-semimajor-axis             0.3844e6  km)
(def-phys-const moon-pericenter                 0.3633e6  km)
(def-phys-const moon-apocenter                  0.4055e6  km)
(def-phys-const moon-revolution-period          27.322  day)
(def-phys-const moon-synodic-period             29.53  day)
(def-phys-const moon-mean-orbital-velocity      1.023 (* km (^ s -1)))
(def-phys-const moon-orbital-inclination        5.145  °)
(def-phys-const moon-orbital-excentricity       0.0549  1)
(def-phys-const moon-sidereal-rotation-period   655.728  hours)
(def-phys-const moon-equatorial-inclination     6.68  °)
(def-phys-const moon-recession-rate-from-earth  3.8 (* cm (^ yr -1)))




(def-phys-const mars-aphelion                   249.3e6  km)
(def-phys-const mars-argon-ar                   1.6  %)
(def-phys-const mars-average-temperature        210  K)
(def-phys-const mars-black-body-temperature     216.6  K)
(def-phys-const mars-bond-albedo                0.16  1)
(def-phys-const mars-carbon-dioxide-co2  95.32 %)
(def-phys-const mars-carbon-monoxide-co  0.08 %)
(def-phys-const mars-core-radius                1700  km)
(def-phys-const mars-ellipticity                0.0065  1)
(def-phys-const mars-equatorial-radius          3397  km)
(def-phys-const mars-escape-velocity            5.03 (* km (^ s -1)))
(def-phys-const mars-gm                         0.04283e6 (* (^ km 3) (^ s -2)))
(def-phys-const mars-krypton-kr                 0.3  ppm)
(def-phys-const mars-mass                       0.6419e24  kg)
(def-phys-const mars-mean-density               3933 (* kg (^ m -3)))
(def-phys-const mars-mean-molecular-weight      43.34 (* g (^ mol -1)))
(def-phys-const mars-mean-orbital-velocity      24.13 (* km (^ s -1)))
(def-phys-const mars-moment-of-inertia          0.366 (* I (^ MR -2)))
(def-phys-const mars-neon-ne                    2.5  ppm)
(def-phys-const mars-nitrogen-n2                2.7  %)
(def-phys-const mars-obliquity-to-orbit         25.19  °)
(def-phys-const mars-orbital-eccentricity       0.0934  1)
(def-phys-const mars-orbital-inclination        1.85  °)
(def-phys-const mars-oxygen-o2                  0.13  %)
(def-phys-const mars-perihelion                 206.6e6  km)
(def-phys-const mars-polar-radius               3375  km)
(def-phys-const mars-scale-height               11.1  km)
(def-phys-const mars-semimajor-axis             227.9e6  km)
(def-phys-const mars-sidereal-orbit-period      686.980  day)
(def-phys-const mars-sidereal-rotation-period   24.6229  hours)
(def-phys-const mars-solar-irradiance           595 (* W (^ m -2)))
(def-phys-const mars-surface-density            0.020 (* kg (^ m -3)))
(def-phys-const mars-surface-gravity            3.69 (* m (^ s -2)))
(def-phys-const mars-surface-pressure           6.9 (* mbars to 9 mbars))
(def-phys-const mars-surface-wind-speeds-range-autumn     '(2 4) (* m (^ s -1)))
(def-phys-const mars-surface-wind-speeds-range-dust-storm '(17 30) (* m (^ s -1)))
(def-phys-const mars-surface-wind-speeds-range-summer     '(5  10) (* m (^ s -1)))
(def-phys-const mars-synodic-period             779.94  day)
(def-phys-const mars-topographic-range          36  km)
(def-phys-const mars-tropical-orbit-period      686.930  day)
(def-phys-const mars-visual-geometric-albedo    0.150  1)
(def-phys-const mars-volume                     16.318e10  (^ km 3))
(def-phys-const mars-volumetric-mean-radius     3390  km)
(def-phys-const mars-water-h2o                  210  ppm)
(def-phys-const mars-apparent-magnitude-near-opposition  -2.0  1)
(def-phys-const mars-diurnal-temperatunre-minimum  184 (* K to 242 K))
(def-phys-const mars-heavy-water-hdo            0.85  ppm)
(def-phys-const mars-apparent-diameter-from-earth-maximum  25.7 (* arc second))
(def-phys-const mars-distance-from-earth-maximum  401.3e6  km)
(def-phys-const mars-apparent-diameter-from-earth-minimum  3.5 (* arc second))
(def-phys-const mars-distance-from-earth-minimum  54.5e6  km)
(def-phys-const mars-nitrogen-oxide-no          100  ppm)
(def-phys-const mars-visual-magnitude           -1.52  1)
(def-phys-const mars-xenon-xe                   0.08  ppm)




(defconstant year (* 365 24 60 60)
  "Value of a year in second.") ;;year

(defconstant light-year (* c year)
  "Value of a light-year in meter.") ;;light-year


(defun square (x) (* x x))
(defun sqrt (x) (expt x 0.5))
(defun lorentz ( x t_ v )
  "
RETURN: x' ; t' ; dx' ; dt' such as
    gamma =  sqrt( 1 - ( v / c ) ^ 2 )
    x' = ( x + v t ) / gamma
    t' = ( t + x v / c ^ 2 ) /  gamma
    dx' = x / gamma
    dt' = t / gamma
"
  (let ((gamma (/ 1.0 (sqrt (- 1.0 (square (/ v c)))))))
    (values
     gamma
     (/ (- x (* v t_)) gamma)
     (/ (- t_ (/ (* x v) (* c c))) gamma)
     (/ x  gamma)
     (/ t_ gamma)))) ;;lorentz


;;; (setq d (* 4.3  light-year))
;;; (setq v (* 0.90 c))
;;; (setq t_ (/ d v))
;;; (setq gamma (car (lorentz d t_ v)))

;; Luego,  por efecto  de  la compresión  del  tiempo cuando  se viaja  a
;; velocidades cerca de  c, la sonda no tarda tanto  tiempo en llegar, y,
;; por lo contrario, la gente que  se queda en la Tierra debe esparar más
;; tiempo en antes de saber que la sonda a llegado.
;;
;; Cómo se  podra verificar en cualquier  página web sobre  el tema (para
;; decir que ¡esta clase de información  esta a accesible por todo con un
;; par de clics!):
;;
;; Aplicando las transformadas de Lorentz:
;;
;;     gamma = 1 / sqrt( 1 - ( v / c ) ^ 2 )
;; 	x' = ( x + v t ) * gamma
;; 	t' = ( t + x v / c ^ 2 ) * gamma
;;
;; que dan las coordenadas (x',t')  dentro de un referencial que se mueve
;; a  la  velocidad  v con  respecto  a  un  referencial fijo,  dadas  la
;; coordenadas  (x,t)  dentro  del  referencial fijo,  se  puede  deducir
;; facilmente  la dilatación  y la  contracción de  la longitud  y  de la
;; duración:
;;
;;     l' = l / gamma
;;     d' = d / gamma
;;
;; (o si se es vago, se puede tanbien buscar en el web con un par de clic
;; de más).
;;
;;
;; Así que,  por ejemplo, si  la sonda viaja  a v/c =  0.90 or 90%  de la
;; velocidad de  la luz, a una distancia  de 4.3 años-luz, lo  hara en un
;; tiempo superior a 4.3 años (visto desde el referencial fijo, es decir,
;; desde la Tierra porque va siempre a una velocidad menor que c:
;;
;;       d = 4.3 * año * c
;;       v = 0.9 * c
;;       t = d / v
;;
;;       t = ( 4.3 * año * c )  / ( 0.9 * c )
;;         = (  4.3 / 0.9 ) * año
;;         = 4.777 año
;;
;; Pero, visto de la sonda, la duración del viaje es:
;;
;;       d' = d / gamma
;; con:
;;       gamma = 1 / sqrt( 1 - ( v / c ) ^ 2 )
;;       gamma = 1 / sqrt( 1 - 0.9^2 )
;;       gamma = 2.294
;;
;; asi que:
;;
;;       d' = 4.777 año / 2.294
;;       d' = 2.082 año
;;
;;
;; En  el caso  que la  sonda viaje  a  99% de  la velocidad  de la  luz,
;; obtendremos:
;;
;;       gamma = 1 / sqrt( 1 - 0.99^2 )
;;       gamma = 7.089
;; y:
;;       d' = 4.777 año / 7.089
;;       d' = 0.673 año, o 8 meses,
;;
;; ¡a comparar con los 3 meses que Cristobal Colomb tomó para ir a America!
;;
;; (Esto para decir  que se podria salir a explorar  las estrellas con la
;; tecnologia que tenemos  desde hace 50 años :  los reactores nucleares,
;; salvo que los  que se quedan atras no verian  volver a los astronautas
;; que van más lejos).
;;
;;
;;
;;
;; (En el caso del photon que viaja a la velocidad de la luz, él toca los
;; dos puntos  al mismo  tiempo, de  su punto de  vista. Cuando  quita un
;; electron y cuando llega a un  otro electro, que sea dentro de la misma
;; molecula o al  otro lado de la  galaxia o del universo, para  él es al
;; mismo tiempo.  Las  interacciónes electro-magneticas son efectivamente
;; instantaneas, para los photones).
;;
;;
;;
;; http://scienceworld.wolfram.com/physics/TimeDilation.html
;; http://www.geocities.com/angelto.geo/bhole/lorentz.html
;; y miles de páginas sobre el tema...
;;

;;; (show constantes)


(defun make-value (mant expo)
  (setq expo (remove (character " ")  expo))
  (if (= 0 (length expo))
      (remove (character " ")  mant)
      (concatenate 'string (remove (character " ")  mant) "e" expo)))


(defun make-unit-part (part)
  (cond
    ((string-match "\\([a-z][a-z]*\\)\\([-+]?[0-9][0-9]*\\)*" part)
     (let ((unit (match-string 1 part))
           (expo (match-string 2 part)))
       (if (= 0 (length expo))
           unit
           (format "(^ %s %s)" unit expo))))
    (t part)))



(defun make-unit (unit)
  (if (null unit)
      " 1"
      (let ((parts (remove "" (split-string unit " "))))
        (cond
          ((null parts)         " 1")
          ((= 1 (length parts))
           (concatenate 'string " " (make-unit-part (car parts))))
          (t (format "(* %s)"
               (unsplit-string
                (mapcar (function make-unit-part) parts)))))))
  ) ;;make-unit


(defun astre-to-constantes (astre)
  "Edit a file of data on a planet into a set of def-phys-const declarations."
  (interactive "*sPlanet Name: ")
  (dolist
      (sub (sort
            '(
              ("Dipole Offset (planet centre to dipole centre) distance"
               "dipole-offset-distance" t t)
              ("Longitude of Ascending Node" "longitude-of-ascending-node" t t)
              ("Diurnal Temperature Minimum" "diurnal-temperature-minimum" t t)
              ("Diurnal Temperature Maximum" "diurnal-temperature-maximum" t t)
              ("Visual Magnitude (V.?1,0.?)" "albedo-visual-magnitude" t t)
              ("Longitude of Offset Vector" "longitude-of-offset-vector" t t)
              ("Visual Magnitude (V(1,0))" "visual-magnitude-(v(1,0))" t t)
              ("Longitude of Dipole North" "longitude-of-dipole-north" t t)
              ("Latitude of Offset Vector" "latitude-of-offset-vector" t t)
              ("Recession Rate from Earth" "recession-rate" t t)
              ("Maximum Orbital Velocity" "maximum-orbital-velocity" t t)
              ("Minimum Orbital Velocity" "minimum-orbital-velocity" t t)
              ("Latitude of Dipole North" "latitude-of-dipole-north" t t)
              ("Sidereal Rotation Period" "sidereal-rotation-period" t t)
              ("Visual Geometric Albedo" "visual-geometric-albedo" t t)
              ("Longitude of Perihelion" "longitude-of-pericenter" t t)
              ("Visual Geometric Albedo" "albedo-visual-geometric" t t)
              ("Volumetric Mean Radius" "volumetric-mean-radius" t t)
              ("Black-Body Temperature" "black-body-temperature" t t)
              ("Equatorial Inclination" "equatorial-inclination" t t)
              ("Sidereal Orbit Period" "sidereal-orbit-period" t t)
              ("Tropical Orbit Period" "tropical-orbit-period" t t)
              ("Dipole Field Strength" "dipole-field-strength" t t)
              ("Mean Molecular Weight" "mean-molecular-weight" t t)
              ("Mean Orbital Velocity" "mean-orbital-velocity" t t)
              ("Orbital Eccentricity" "orbital-eccentricity" t t)
              ("Orbital Eccentricity" "orbital-excentricity" t t)
              ("Average Temperature" "average-temperature" t t)
              ("Surface Wind Speeds" "surface-wind-speeds" t t)
              ("Orbital Inclination" "orbital-inclination" t t)
              ("Obliquity to Orbit" "obliquity-to-orbit" t t)
              ("Equatorial Radius" "equatorial-radius" t t)
              ("Equatorial Radius" "radius-equatorial" t t)
              ("Moment of Inertia" "moment-of-inertia" t t)
              ("Topographic Range" "topographic-range" t t)
              ("Revolution Period" "revolution-period" t t)
              ("Surface Pressure" "surface-pressure" t t)
              ("Solar Irradiance" "solar-irradiance" t t)
              ("Surface Density" "surface-density" t t)
              ("Surface Gravity" "surface-gravity" t t)
              ("Escape Velocity" "escape-velocity" t t)
              ("Mean Longitude" "mean-longitude" t t)
              ("Semimajor Axis" "semimajor-axis" t t)
              ("Synodic Period" "synodic-period" t t)
              ("Length of Day" "length-of-day" t t)
              ("Mean Density" "mean-density" t t)
              ("Scale Height" "scale-height" t t)
              ("Polar Radius" "polar-radius" t t)
              ("Mean Density" "density" t t)
              ("Core Radius" "core-radius" t t)
              ("Bond Albedo" "bond-albedo" t t)
              ("Nitrogen N2" "nitrogen-n2" t t)
              ("Hydrogen H2" "hydrogen-h2" t t)
              ("Ellipticity" "ellipticity" t t)
              ("Bond Albedo" "albedo-bind" t t)
              ("Perihelion" "pericenter" t t)
              ("Krypton Kr" "krypton-kr" t t)
              ("Oxygen O2" "oxygen-o2" t t)
              ("Water H2O" "water-h2o" t t)
              ("Helium He" "helium-he" t t)
              ("Aphelion" "apocenter" t t)
              ("Argon Ar" "argon-ar" t t)
              ("Neon Ne" "neon-ne" t t)
              ("Perigee" "pericenter" t t)
              ("Carbon" "carbon" t t)
              ("Volume" "volume" t t)
              ("Apogee" "apocenter" t t)
              ("Mass" "mass" t t)
              ("CH4" "ch4" t t)
              ("J2" "j2" t t)
              ("GM" "gm" t t)
              ("Visual Magnitude" "visual-magnitude" t t)
              ("Maximum Apparent Diameter from Earth" "apparent-diameter-from-earth-maximum" t t)
              ("Minimum Apparent Diameter from Earth" "apparent-diameter-from-earth-minimum" t t)
              ("Maximum Distance from Earth" "distance-from-earth-maximum" t t)
              ("Minimum Distance from Earth" "distance-from-earth-minimum" t t)
              ("Apparent Magnitude near Opposition" "apparent-magnitude-near-opposition" t t)
              ("Diurnal Temperature Range" "diurnal-temperatunre-minimum" t t)
              ("Nitrogen Oside NO" "nitrogen-oxide-no" t t)
              ("Xenon Xe" "xenon-xe" t t)
              ("HDO" "heavy-water-hdo" t t)
              )
            (lambda (a b) (> (length (car a)) (length (car b))))))
    (let ((regexp     (elt sub 0))
          (to-string  (format "(def-phys-const %-30s |"
                        (concatenate 'string astre "-" (elt sub 1))))
          (fixed-case (elt sub 2))
          (literal    (elt sub 3))
          (case-fold-search nil))
      (goto-char (point-min))
      (message "sub = %S" sub)
      (while (re-search-forward regexp nil t)
        (replace-match to-string fixed-case literal))))
  (goto-char (point-min))
  (while (re-search-forward "| *\\([-+]*[0-9][ 0-9]*\\(\\.[0-9][ 0-9]*\\)*\\)\\( × 10\\([-+]*[0-9][0-9]*\\)\\)?\\( .*\\)?$" nil t)
    (let* ((mantissa-str (match-string-no-properties 1))
           (exponent-str (match-string-no-properties 4))
           (unit-str     (match-string-no-properties 5))
           (data         (match-data))
           (clean-mant   (make-value mantissa-str exponent-str))
           (clean-unit (make-unit unit-str))
           )
      (store-match-data data)
      (replace-match (format " %s %s)" clean-mant clean-unit) t t)))
  ) ;;astre-to-constantes





;;; (mapcar
;;;  (lambda (n) (list n (substitute 
;;;                       (character "-") (character " ") (downcase n)
;;;                       :test (function char=)) t t))
;;;  (remove-duplicates
;;;    (sort 
;;;     '(
;;;       )
;;;     (lambda (a b) (> (length (car a)) (length (car b)))))
;;;    :test (function equalp)))



;;;; pjb-constants.el                 --                     --          ;;;;
