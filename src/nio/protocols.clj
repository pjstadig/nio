(ns nio.protocols)

(defprotocol IBuffer
  (make-buffer [x])
  (buffer-seq [x start])
  (buffer-nth [x n not-found])
  (buffer-to-array [x]))
(defprotocol IByteBuffer
  (make-byte-buffer [x]))
(defprotocol ICharBuffer
  (make-char-buffer [x]))
(defprotocol IDoubleBuffer
  (make-double-buffer [x]))
(defprotocol IFloatBuffer
  (make-float-buffer [x]))
(defprotocol IIntBuffer
  (make-int-buffer [x]))
(defprotocol ILongBuffer
  (make-long-buffer [x]))
(defprotocol IShortBuffer
  (make-short-buffer [x]))

(defprotocol IMmap
  (do-mmap [x position size opts]))

(defprotocol NIOFactory
  "Factory functions that create ready-to-use, versions of
   the various Java NIO channel types, on top of anything that can
   be unequivocally converted to the requested kind of channel.

   Callers should generally prefer the higher level API provided by
   readable-channel, writable-channel, and channel."
  (make-readable-channel [x]
    "Creates a ReadableByteChannel. See also NIOFactory docs.")
  (make-writable-channel [x]
    "Creates a WritableByteChannel. See also NIOFactory docs.")
  (make-channel [x]
    "Creates a ByteChannel. See also NIOFactory docs."))
