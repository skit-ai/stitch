(in-package #:stitch)

(defstruct audio-resource
  "An audio resource represents slicing information and filepath for various
symbols present in the file."
  (values nil :type list)
  (filepath nil :type string))

(defun parse-label-line (line)
  "Parse a single audacity style label line. A line has three components.
Start time (in seconds), end time and a label. We assume that the sequence of
labels is in order."
  (let ((splits (-> line
                   clean
                   (replace-all (make-string 1 :initial-element #\tab) (make-string 1 :initial-element #\ ))
                   (split #\ ))))
    (cons (nth 2 splits) (cons (parse-number:parse-positive-real-number (nth 0 splits))
                               (parse-number:parse-positive-real-number (nth 1 splits))))))

(defun parse-labels (filepath)
  "Parse audacity style label file `filepath'."
  (let ((lines (-<> filepath
                   alexandria:read-file-into-string
                   clean
                   (split #\newline)
                   (remove-if (lambda (line) (zerop (length line))) <>))))
    (make-audio-resource :values (mapcar #'parse-label-line lines) :filepath filepath)))
