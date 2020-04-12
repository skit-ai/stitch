(in-package #:stitch)

;; Label reading

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

;; Wav interaction

(defun slice-wav-data (data start-time end-time)
  "Slice wav data (read from cl-wav) from start-time to end-time.")

(defun merge-wav-data (data-a data-b)
  "Merge two wav data pieces in a one data piece.")

(defun swap-extension (filepath new-ext)
  "Primitive extension swapping for files with single extension component."
  (let ((splits (split filepath ".")))
    (join (append (butlast splits) (list new-ext)) :separator ".")))

(defun read-wav-for-labels-file (label-filepath)
  "Read wav file corresponding to the label file. We assume that that wav file
has same name as the labels file with extension changed."
  (wav:read-wav-file (swap-extension label-filepath "wav")))

(defun read-audio-resouce-slices (resource)
  "Read slices listed in `resource'."
  (let ((wav-data (read-wav-for-labels-file (audio-resource-filepath resource))))
    (mapcar (lambda (label-info) (slice-wav-data wav-data (cadr label-info) (cddr label-info)))
            (audio-resource-values resource))))

;; Planning

(defun find-plan (sequence audio-resources)
  "For the given list of sequence of symbols, look up audio resources.")

(defun stitch-plan (plan output-filepath)
  "Take the list of audio-resources stitch the final audio output and put in.")
