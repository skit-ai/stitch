(in-package #:stitch)

;; General utils

(defun plist-set (plist key new-value)
  "Modify value of `key' in `plist' to be `new-value'."
  (append (alexandria:remove-from-plist plist key) (list key new-value)))

(defun plist-get (plist key)
  (getf plist key))

(defun swap-extension (filepath new-ext)
  "Primitive extension swapping for files with single extension component."
  (let ((splits (split filepath ".")))
    (join (append (butlast splits) (list new-ext)) :separator ".")))

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

(defun audio-resource-from-label-file (filepath)
  "Parse audacity style label file `filepath'."
  (let ((lines (-<> filepath
                   alexandria:read-file-into-string
                   clean
                   (split #\newline)
                   (remove-if (lambda (line) (zerop (length line))) <>))))
    (make-audio-resource :values (mapcar #'parse-label-line lines) :filepath filepath)))

(defun split-audio-resource (resource)
  "Split audio resource in multiple where each has single symbol."
  (let ((filepath (audio-resource-filepath resource)))
    (mapcar (lambda (label-info) (make-audio-resource :values (list label-info) :filepath filepath))
            (audio-resource-values resource))))

;; Wav interaction

(defun data-size (data)
  (plist-get (caddr data) :chunk-data-size))

(defun header-size (data)
  "Return header size for the wave data."
  (- (plist-get (car data) :chunk-data-size) (data-size data)))

(defun seconds-to-bytes (seconds fmt-chunk)
  "Convert a second value to bytes offset using info from fmt-chunk."
  (let ((sample-rate (plist-get (plist-get fmt-chunk :chunk-data) :sample-rate))
        (bytes-per-sample (/ (plist-get (plist-get fmt-chunk :chunk-data) :significant-bits-per-sample) 8)))
    (* bytes-per-sample (floor (* seconds sample-rate)))))

(defun slice-wav-data (data start-time end-time)
  "Slice wav data (read from cl-wav) from start-time to end-time. Sample edges
might not be very precise."
  (let* ((start-pos (seconds-to-bytes start-time (cadr data)))
         (end-pos (seconds-to-bytes end-time (cadr data)))
         (slice-size (- end-pos start-pos)))
    (list (plist-set (car data) :chunk-data-size (+ slice-size (header-size data)))
          (cadr data)
          (plist-set (plist-set (caddr data) :chunk-data-size slice-size)
                     :chunk-data (cl-slice:slice (plist-get (caddr data) :chunk-data) (cons start-pos end-pos))))))

(defun concat-wav-data (data-a data-b)
  "Concatenate two wav data pieces in one. We assume same general structural
components."
  (let* ((riff-chunk-a (car data-a))
         (riff-chunk-b (car data-b))
         (fmt-chunk (cadr data-a))
         (data-size-a (data-size data-a))
         (data-size-b (data-size data-b))
         (array-a (plist-get (caddr data-a) :chunk-data))
         (array-b (plist-get (caddr data-b) :chunk-data)))
    (list (plist-set riff-chunk-a :chunk-data-size (+ data-size-a data-size-b (header-size data-a)))
          fmt-chunk
          (plist-set (plist-set (caddr data-a) :chunk-data-size (+ data-size-a data-size-b))
                     :chunk-data (concatenate '(vector (unsigned-byte 8)) array-a array-b)))))

(defun read-audio-resource-slices (resource)
  "Read slices listed in `resource'."
  (let ((wav-data (wav:read-wav-file (swap-extension (audio-resource-filepath resource) "wav"))))
    (mapcar (lambda (label-info) (slice-wav-data wav-data (cadr label-info) (cddr label-info)))
            (audio-resource-values resource))))

;; Planning

(defun find-plan-basic (sequence resources)
  "Very basic 1 by 1 stitching planner."
  (let ((solo-resources (alexandria:flatten (mapcar #'split-audio-resource resources)))
        (table (make-hash-table :test 'equal)))
    (loop for res in solo-resources
          do (let* ((label (caar (audio-resource-values res)))
                    (value (gethash label table)))
               (setf (gethash label table) (cons res value))))
    (mapcar (lambda (label)
              ;; We assume everything is present
              (let ((ress (gethash label table)))
                (nth (random (length ress)) ress)))
            sequence)))

(defun find-plan (sequence resources)
  "For the given list of sequence of symbols, look up audio resources."
  (find-plan-basic sequence resources))

(defun stitch-plan (plan output-filepath)
  "Take the list of audio-resources (`plan'), stitch the final audio output and
write to `output-filepath'."
  (let ((slices (reduce #'append (mapcar (lambda (resource) (read-audio-resource-slices resource)) plan))))
    (wav:write-wav-file (reduce #'concat-wav-data slices) output-filepath)))
