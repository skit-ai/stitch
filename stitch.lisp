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
  "Slice wav data (read from cl-wav) from start-time to end-time."
  (error 'error))

(defun plist-set (plist key new-value)
  "Modify value of `key' in `plist' to be `new-value'."
  (append (alexandria:remove-from-plist plist key) (list key new-value)))

(defun plist-get (plist key)
  (getf plist key))

(defun concat-wav-data (data-a data-b)
  "Concatenate two wav data pieces in one. We assume same general structural
components."
  (let* ((riff-chunk-a (car data-a))
         (riff-chunk-b (car data-b))
         (fmt-chunk (cadr data-a))
         (data-size-a (plist-get (nth 2 data-a) :chunk-data-size))
         (data-size-b (plist-get (nth 2 data-b) :chunk-data-size))
         (array-a (plist-get (nth 2 data-a) :chunk-data))
         (array-b (plist-get (nth 2 data-b) :chunk-data))
         (header-size (- (plist-get riff-chunk-a :chunk-data-size) data-size-a)))
    (list (plist-set riff-chunk-a :chunk-data-size (+ data-size-a data-size-b header-size))
          fmt-chunk
          (plist-set (plist-set (nth 2 data-a) :chunk-data-size (+ data-size-a data-size-b))
                     :chunk-data (concatenate '(vector (unsigned-byte 8)) array-a array-b)))))

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

(defun split-audio-resource (resource)
  "Split audio resource in multiple where each has "
  (let ((filepath (audio-resource-filepath resource)))
    (mapcar (lambda (label-info) (make-audio-resource :values (list label-info) :filepath filepath))
            (audio-resource-values resource))))

(defun find-plan-basic (sequence resources)
  "Very basic 1 by 1 stitching planner."
  (let ((solo-resources (alexandria:flatten #'split-audio-resource resources))
        (table (make-hash-table :test 'equal)))
    (loop for res in solo-resouces
          do (setf (gethash (caar (audio-resource-values res)) table) res))
    (mapcar (lambda (label) (gethash label table)) sequence)))

(defun find-plan (sequence resources)
  "For the given list of sequence of symbols, look up audio resources."
  (find-plan-basic sequence resources))

(defun stitch-plan (plan output-filepath)
  "Take the list of audio-resources (`plan'), stitch the final audio output and
write to `output-filepath'."
  (let ((slices (alexandria:flatten (mapcar (lambda (resource) (read-audio-resouce-slices resource)) plan))))
    (wav:write-wav-file (reduce #'concat-wav-data slices) output-filepath)))
