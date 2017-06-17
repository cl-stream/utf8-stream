
(in-package :babel-stream)

(defclass babel-stream (stream)
  ((underlying-stream :initarg :stream
		      :accessor stream-underlying-stream
		      :type stream)
   (external-format :initarg :external-format
		    :initform :utf-8
		    :accessor stream-external-format
		    :type symbol)))

(defmethod initialize-instance :after ((stream babel-stream)
				       &rest initargs
				       &key &allow-other-keys)
  (declare (ignore initargs))
  (assert (equal '(unsigned-byte 8)
		 (stream-element-type (stream-underlying-stream stream)))))

(defclass babel-input-stream (babel-stream input-stream)
  ())

(defclass babel-output-stream (babel-stream output-stream)
  ())

(defmethod write ((stream babel-output-stream) (element fixnum))
  (assert (typep element '(unsigned-byte 8)))
  (write (stream-underlying-stream stream) element))

(defmethod write ((stream babel-output-stream) (element character))
  (let ((seq (babel:string-to-octets (make-string 1 :initial-element element)
				     :encoding (stream-external-format stream)
				     :use-bom nil)))
    (write-sequence (stream-underlying-stream stream)
		    seq)))

(defmethod flush ((stream babel-output-stream))
  (flush (stream-underlying-stream stream)))

(let ((s (make-instance 'babel-output-stream
			:stream (fd-stream:fd-output-stream 1))))
  (write s #\é)
  (flush s))
