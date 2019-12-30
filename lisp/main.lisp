(in-package #:youtube-comments)

(defparameter *version* "SNAPSHOT")

(defparameter +service-option-spec+
  '(
    (("port" #\p) :type integer :optional t :documentation "service port")
    ;; TODO actually pass this down
    (("google-secrets" #\s) :type string :optional t :documentation
     "path to google oauth web client json"
     :initial-value "/usr/local/share/tetris/shapes.in")
    ;; TODO stack exchange secrets
    (("verbose" #\v) :type boolean :optional t :documentation "verbose logging")
    (("help" #\h) :type boolean :optional t :documentation "display help")
    (("version" #\V) :type boolean :optional t :documentation "display version"))
  "CLI arguments spec for launching content-online service.")


(defun main-parse-args (&rest args &key positional verbose dims help version port
                              &allow-other-keys)
  "parse command-line arguments and start the service if applicable"
  (declare (ignore positional))
  ;; destructure any argument that need to be handled before proxying to make-config
  (cond
   (help (command-line-arguments:show-option-help +service-option-spec+ :sort-names t))
   (version (format t "~A~%" *version*))
   (t
    (when verbose
      (vom:config t :debug)
      (vom:debug "verbose enabled"))
    (start :port port)
    ;; prevent lisp from exiting while the server is running
    ;; https://stackoverflow.com/questions/30422451/
    (sb-thread:join-thread
     (find-if
      (lambda (th)
        (cl-ppcre:scan "hunchentoot-listener-" (sb-thread:thread-name th)))
      (sb-thread:list-all-threads))))))

(defun main (args)
  "main entry point"
  (command-line-arguments:handle-command-line
   +service-option-spec+ 'main-parse-args
   :command-line (cdr args) ;;the first is the executable path
   :name "content-online-service"
   :rest-arity :positional))

;; '(main '("./script" "-p" "1234" "-h" "-v" "pmlkasdf"))
