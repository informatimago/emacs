;; -*- mode:emacs-lisp -*-

(require 'cl)

(defvar *make-conf-path* "/etc/make.conf")
(setf *make-conf-path* "/tmp/test-make.conf")


(defmacro with-conf-file (path &rest body)
  `(progn
     (find-file ,path)
     (erase-buffer)
     (unwind-protect (progn ,@body)
       (save-buffer)
       (kill-buffer (current-buffer)))))


(defun generate-list-in-string (variable value)
  (loop
     with first = t
     with items = value
     while items
     do (let* ((line-cnt (loop
                            for cnt from 0
                            for item in items
                            for width = (- 56 (length (symbol-name item)))
                            then (- width (length (symbol-name item)))
                            while (<= 0 width)
                            finally (return (if (< width 0)
                                                (1- cnt)
                                                cnt))))
               (line (subseq items 0 line-cnt)))
          (setf items (nthcdr line-cnt items))
          (if first
              (progn (setf first nil)
                     (insert (format "%s=\"" variable))))
          (dolist (item line) (insert (format " %s" item)))
          (insert " \\\n     "))
     finally (insert "\"\n")))


(defun insert-conf-item (items)
  (dolist (item items)
    (destructuring-bind (op arg . rest) item
      (when rest (error "Too many arguments in %S" item))
      (case op
        ((comment)
         (dolist (line (split-string arg "\n"))
           (insert (format "# %s\n" line))))
        (otherwise ; a variable
         (etypecase arg
           ((or string number)
            (insert (format "%s=%S\n" op arg)))
           (list (generate-list-in-string op arg))))))))


(defmacro conf (&rest items) `(insert-conf-item ',items))

(defun configure-make-conf ()
  (with-conf-file  *make-conf-path*
    (conf
     (comment "-*- mode: shell-script -*- ")
     (comment "DO NOT EDIT!  THE SOURCE IS /etc/make.conf.el")
     (comment "Consult /etc/make.conf.example for a more detailed example.")
     (CFLAGS           "-O2 -march=athlon-xp")
     (CHOST            "i686-pc-linux-gnu")
     (CXXFLAGS         "${CFLAGS}")
     (PORTDIR_OVERLAY  "/usr/local/portage")
     (ACCEPT_KEYWORDS  "~x86")
     (GENTOO_MIRRORS   "http://mirror.gentoo.es http://mirror.gentoo.com")
     (INPUT_DEVICES    "keyboard mouse")
     (VIDEO_CARDS      "sis dummy fbdev vesa vga")
     (comment " PORTAGE_ELOG_SYSTEM=\"mail\"")
     (comment "      ~arch version of portage and read /etc/make.conf.example")
     (comment "package.keywords could look like this: ")
     (comment "      http://rafb.net/paste/results/SWPxKb71.txt")
     (USE (objc -apache apache2 cli cgi dri nptl nptlonly
            -oracle 
            ;; doc gd-external oracle
            ;; NOT AT ALL: fdftk
            3dnow X Xaw3d a52 aac aalib accessibility acpi aim
            alsa apm audiofile avi bash-completion bcmath berkdb
            bidi blas bluetooth bmp bzip2 calendar cdb cdparanoia
            cdr cpdflib crypt cscope ctype cups curl curlwrappers
            dba dbase dbm dbmaker dbx dedicated dga dio directfb
            divx4linux dv dvb dvd dvdr  dvdread emacs emacs-w3
            encode esd examples exif expat fam fastcgi fbcon
            ffmpeg fftw flac flatfile fortran ftp gcj gd
            gdbm gif  ginac glut gmp gnustep gnutls
            gphoto2 gpm gstreamer guile hal iconv icq  ieee1394
            imagemagick imap imlib inifile innodb iodbc ipv6
            jabber jack  java javascript jikes jpeg junit ladcca
            lapack lcms ldap leim lesstif libcaca libedit libg++
            libgda libwww lirc lm_sensors m17n-lib mad maildir
            mailwrapper mcal mhash mime mmap mmx mng mnogosearch
            motif  mozilla mp3 mpeg mpi msession msn mule mysql
            mysqli nas ncurses netboot  netcdf nneXt nls nocd
            oci8 odbc offensive ofx ogg oggvorbis openal
            opengl osc oscar oss pam pcmcia pcntl pcre pda
            pdflib perl php pic  plotutils png portaudio posix
            postgres ppds prelude profile python qdbm  qt
            quicktime readline recode ruby samba sasl sdl session
            sharedext sharedmem  shorten simplexml skey slang slp
            smartcard sndfile snmp soap sockets source sox speex
            spell spl sse ssl svg svga symlink sysvipc szip tcltk
            tcpd test  tetex theora threads tidy tiff tokenizer
            truetype unicode usb v4l vcd  vhosts videos vorbis
            wifi win32codecs wmf wxwindows xface xine xinerama
            xml xml2 xmlrpc xmms xosd xpm xprint xsl xv xvid
            yahoo zlib)))))


(configure-make-conf)
