(in-package :hamurabi)

;;; io functions
(declaim (inline input-str))
(defun input-str (prompt)
  (format *query-io* "~A " prompt)
  (force-output *query-io*)
  (read-line *query-io* nil ""))

(defun input-num (prompt)
  "Read numerical input, makes sure input is a positive number. Fractional number are FLOORed"
  (input-value prompt 
               :validator (lambda (num) (and (numberp num) (>= num 0)))
               :transformer #'floor 
               :error-prompt "Hamurabi, I do not understand that number. Please try again."))

(defun input-value (prompt &key (validator #'stringp)
                                (transformer nil)
                                (error-prompt "Invalid value, try again."))
  (let ((*read-eval* nil)
        (value nil))
    (do () ((funcall validator value))
      (when (not (null value)) (format *query-io* "~A~%" error-prompt ))
      (setf value (read-from-string (input-str prompt) nil "")))
    (if (not (null transformer))
      (funcall transformer value)
      value)))
;;;

(defparameter *plague-percent* 15) ; 15% possibility for plague
(defparameter *bushels-per-person* 20)
(defparameter *people-per-acre* 10)
(defparameter *bushels-per-acre* 2)

(defclass game-data ()
  ((population :accessor population :initarg :population :initform 0)
   (deaths :accessor deaths :initarg :deaths :initform 0)
   (percent-starved :accessor percent-starved :initarg :percent-starved :initform 0)
   (store :accessor store :initarg :store :initform 0)
   (acreage :accessor acreage :initarg :acreage :initform 0)
   (impeached :accessor impeached :initarg :impeached :initform nil)
   (years :accessor years :initarg :years :initform 0)))

(defclass year-data ()
  ((year :accessor year :initarg :year :initform 0)
   (births :accessor births :initarg :births :initform 0)
   (starved :accessor starved :initarg :starved :initform 0)
   (plague? :accessor plague? :initarg :plague? :initform nil)
   (rats :accessor rats :initarg :rats :initform 0)
   (yield :accessor yield :initarg :yield :initform 0)
   (feed :accessor feed :initarg :feed :initform 0)
   (plant :accessor plant :initarg :plant :initform 0)))

(defun intro (game-data)
  (declare (game-data game-data))
  (with-slots
    (population store acreage) game-data
    (format *query-io* 
            "Try your hand at governing ancient Sumeria for a ~R term office.~%~%You are starting with a population of ~D, ~D acres and ~D bushels in store.~%" 
            *years-to-rule* population acreage store)))

(defun year-review (year-data game-data)
  (declare (year-data year-data)
           (game-data game-data))
  (when (not (impeached game-data))
    (with-slots
      (year births starved plague? rats yield plant) year-data
      (with-slots
        (population store acreage) game-data 
        (format *query-io* "~%In year ~D, ~D people starved, ~D came to the city.~%"
              year starved births)
        (when plague?
          (format *query-io* "A horrible plague struck! Half the population died!~%"))
        (format *query-io* "Population is now ~D.~%The City owns ~D acres~%You cultivated ~D acres and harvested ~D bushels per acre.~%Rats ate ~D bushels.~%You now have ~D bushels in store.~%"
                population acreage plant yield rats store))))
  (values))

(defun game-review (game-data)
  (declare (game-data game-data))
  (when (not (impeached game-data))
    (with-slots
       (population deaths percent-starved acreage years) game-data
       (let ((land (float (/ acreage population)))
             (start-land (floor (/ *start-acreage* *start-population*))))
         (format *query-io* "~%In your ~R year term in office, on average ~,2F percent of the population starved per year.~%A total of ~D people died.~%You started with ~D acres per person and ended with ~,2F acres per person.~%~%" 
                 years percent-starved deaths start-land land)
         (cond
           ((or (< 33 percent-starved) (< land (* .7 start-land))) (fink))
           ((or (< 10 percent-starved) (< land (* .9 start-land))) (nero))
           ((or (< 3  percent-starved) (< land start-land)) (not-bad population))
           (t (fantastic))))))
  (values))

(declaim (inline fink nero not-bad fantastic impeach))

(defun fink ()
  (format *query-io* "Due to this extreme mismanagement, you have not only been impeached and thrown out of office but you have also been declared 'National Fink'!~%"))

(defun nero ()
  (format *query-io* "Your heavy-handed performance smacks of Nero and Ivan IV. The People (remaining) find you an unpleasant ruler and, frankly, hate your guts!~%"))

(defun not-bad (population)
  (declare (fixnum population))
  (let ((haters (1+ (random (floor (* population 4/5))))))
    (format *query-io* "Your performance could have been somewhat better, but really wasn't too bad at all. ~D people would dearly like to see you assassinated... but we all have our trivial problems.~%" haters)))

(defun fantastic ()
  (format *query-io* "A fantastic performance!!! Charlemagne, Disraeli and Jefferson combined could not have done better!~%"))

(defun impeach (deaths)
  (format *query-io* "~%You starved ~D people in one year!!!~%" deaths)
  (fink))

(defun plague-check! (year-data game-data)
  (declare (year-data year-data)
           (game-data game-data))
  (let ((plague (> *plague-percent* (random 100))))
    (setf (plague? year-data) plague)
    (when plague
      (with-slots (population deaths) game-data
        (let ((died (ceiling (/ population 2))))
          (setf deaths (+ deaths died))
          (setf population (- population died)))))))

(defun buy-sell! (game-data)
  (declare (game-data game-data))
  (let ((price (+ (random 10) 17)))
    (with-slots
      (store acreage) game-data
      (labels ((do-buy-sell ()
                            (format *query-io* "Land is trading at ~D bushels per acre.~%" price)
                            (let* ((buy-num (input-num "How many acres do you wish to buy?"))
                                   (sell-num (if (eq 0 buy-num) (input-num "How many acres do you wish to sell?") 0))
                                   (buy-rate (* buy-num price))
                                   (sell-rate (* sell-num price)))
                              (cond
                                ((> buy-rate store)
                                 (format *query-io* "Hamurabi, think again. You only have ~D bushels of grain and that many acres will cost you ~:D.~%" store buy-rate)
                                 (do-buy-sell))
                                ((> sell-num acreage)
                                 (format *query-io* "Hamurabi, think again. You only own ~D acres.~%" acreage)
                                 (do-buy-sell))
                                (t
                                  (setf acreage (- (+ acreage buy-num) sell-num))
                                  (setf store (- (+ store sell-rate) buy-rate)))))))
        (do-buy-sell)))))

(defun feed! (year-data game-data)
  (declare (year-data year-data)
           (game-data game-data))
  (with-slots
    (store) game-data
    (labels ((do-feed ()
                      (let ((food-num (input-num "How many bushels do you wish to feed your people?")))
                        (cond
                          ((> food-num store)
                           (format *query-io* "Hamurabi, think again. You only have ~D bushels available.~%" store)
                           (do-feed))
                          (t
                            (setf store (- store food-num))
                            (setf (feed year-data) food-num))))))
      (do-feed))))

(defun harvest! (year-data game-data)
  (declare (year-data year-data)
           (game-data game-data))
  (with-slots
    (store acreage population) game-data
    (labels ((do-harvest ()
                         (let* ((planted (input-num "How many acres do you wish to plant with seed?"))
                                (bushel-cost (ceiling (/ planted *bushels-per-acre*)))
                                (pop-cost (ceiling (/ planted *people-per-acre*))))
                           (cond
                             ((> planted acreage)
                              (format *query-io* "Hamurabi, think again. You only have ~D acres available for planting.~%" acreage)
                              (do-harvest))
                             ((> bushel-cost store)
                              (format *query-io* "Hamurabi, think again. Planting that many acres requires ~D bushels and you only have ~D.~%" bushel-cost store)
                              (do-harvest))
                             ((> pop-cost population)
                              (format *query-io* "Hamurabi, think again. It takes ~D people to harvest that many acres, but you only have ~D.~%" pop-cost population)
                              (do-harvest))
                             (t
                               (let ((work-store (- store bushel-cost)))
                                 (with-slots
                                   (yield rats plant) year-data
                                   (setf plant planted)
                                   (setf yield (1+ (random 5)))
                                   (setf rats (let ((ran (1+ (random 5))))
                                                (if (oddp ran) 0
                                                  (floor (/ work-store ran)))))
                                   (setf store (- (+ work-store (* yield planted)) rats)))))))))
      (do-harvest))))

(defun birth-n-death! (year-data game-data)
  (declare (year-data year-data)
           (game-data game-data))
  (with-slots
    (births starved feed year) year-data
    (with-slots
      (deaths population store acreage percent-starved) game-data
      (let ((tmp-death (- population (floor (/ feed *bushels-per-person*)))))
        (when (plusp tmp-death) 
          (setf starved tmp-death)
          (setf percent-starved
                (/ (+ (* (1- year) percent-starved)
                      (floor (/ (* starved 100) population)))
                   year))
          (setf deaths (+ deaths starved))
          (setf population (- population starved))
          (when (> starved (* .45 population))
            (setf (impeached game-data) t)
            (impeach starved))))
      (when (> population 0)
        (setf births (ceiling (/ (* (1+ (random 5)) (+ (* 20 acreage) store)) population 100)))
        (setf population (+ population births))))))

(defun do-hamurabi (game-data)
  (declare (game-data game-data))
  (intro game-data)
  (labels ((do-game (year-data)
                  (buy-sell! game-data)
                  (feed! year-data game-data)
                  (harvest! year-data game-data)
                  (birth-n-death! year-data game-data)
                  (plague-check! year-data game-data)
                  (year-review year-data game-data)
                  (when (and (not (impeached game-data))
                             (> (years game-data) (year year-data)))
                    (do-game (make-instance 'year-data :year (1+ (year year-data)))))))
  (do-game (make-instance 'year-data :year 1)))
  (game-review game-data))
  
(defun hamurabi-cust (&key (years 10)
                           (acres 1000)
                           (population 100)
                           (bushels 2800)
                           (plague-percent 15))
  (let ((game-data (make-instance 'game-data
                                  :acreage acres
                                  :population population
                                  :store bushels
                                  :years years))
        (*plague-percent* plague-percent))
    (do-hamurabi game-data)))

(defun hamurabi ()
  (hamurabi-cust))
