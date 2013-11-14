(ns nio.core
  (:require [clojure.java.io :as jio]
            [nio.protocols :as proto])
  (:import (clojure.lang MultiFn)
           (java.io ByteArrayInputStream File FileInputStream FileOutputStream
                    InputStream OutputStream RandomAccessFile)
           (java.net DatagramSocket MalformedURLException Socket URL)
           (java.nio ByteBuffer ByteOrder CharBuffer DoubleBuffer FloatBuffer
                     IntBuffer LongBuffer ShortBuffer)
           (java.nio.channels ByteChannel Channels FileChannel
                              FileChannel$MapMode Pipe ReadableByteChannel
                              WritableByteChannel)))

;; Extend existing clojure.java.io functions to java.nio
(declare buffer-to-array)
(extend ByteBuffer
  jio/IOFactory
  (assoc jio/default-streams-impl
    :make-input-stream
    (fn [^ByteBuffer x opts]
      (jio/make-input-stream
       (ByteArrayInputStream. (buffer-to-array x)
                              (.position x)
                              (.remaining x))
       opts))))

(extend ReadableByteChannel
  jio/IOFactory
  (assoc jio/default-streams-impl
    :make-input-stream (fn [^ReadableByteChannel x opts]
                         (Channels/newInputStream x))))

(extend WritableByteChannel
  jio/IOFactory
  (assoc jio/default-streams-impl
    :make-output-stream (fn [^WritableByteChannel x opts]
                          (Channels/newOutputStream x))))

(extend ByteChannel
  jio/IOFactory
  (assoc jio/default-streams-impl
    :make-input-stream (fn [^ByteChannel x opts] (Channels/newInputStream x))
    :make-output-stream (fn [^ByteChannel x opts]
                          (Channels/newOutputStream x))))

;;;; some funny stuff going on because the things we want are private
(.addMethod
 ^MultiFn
 @#'jio/do-copy
 [ByteBuffer WritableByteChannel]
 (fn [^ByteBuffer input ^WritableByteChannel output opts]
   (.write output input)))

(.addMethod
 ^MultiFn
 @#'jio/do-copy
 [ReadableByteChannel ByteBuffer]
 (fn [^ReadableByteChannel input ^ByteBuffer output opts]
   (.read input output)))

(.addMethod
 ^MultiFn
 @#'jio/do-copy
 [FileChannel WritableByteChannel]
 (fn [^FileChannel input ^WritableByteChannel output opts]
   (let [buffer-size (@#'jio/buffer-size opts)]
     (loop [position (.position input)
            read (.transferTo input
                              position
                              buffer-size
                              output)]
       (if (pos? read)
         (let [position (+ position read)]
           (recur position
                  (.transferTo input
                               position
                               buffer-size
                               output))))))))

(.addMethod
 ^MultiFn
 @#'jio/do-copy
 [ReadableByteChannel FileChannel]
 (fn [^ReadableByteChannel input ^FileChannel output opts]
   (let [buffer-size (@#'jio/buffer-size opts)]
     (loop [position (.position output)
            read (.transferFrom output
                                input
                                position
                                buffer-size)]
       (if (pos? read)
         (let [position (+ position read)]
           (recur position
                  (.transferFrom output
                                 input
                                 position
                                 buffer-size))))))))

(.addMethod
 ^MultiFn
 @#'jio/do-copy
 [ReadableByteChannel WritableByteChannel]
 (fn [^ReadableByteChannel input ^WritableByteChannel output opts]
   (let [buf (ByteBuffer/allocate (@#'jio/buffer-size opts))]
     (loop [read (.read input buf)]
       (when (or (> read 0)
                 (not= (.position buf) 0))
         (.write output (.flip buf))
         (recur (.read input (.compact buf))))))))

(declare channel)
(.addMethod
 ^MultiFn
 @#'jio/do-copy
 [File File]
 (fn [^File input ^File output opts]
   (with-open [input ^FileChannel (channel input)
               output ^FileChannel (channel output)]
     (@#'jio/do-copy input output))))

(prefer-method @#'jio/do-copy
               [FileChannel WritableByteChannel]
               [ReadableByteChannel FileChannel])

;; New java.nio coercion functions
(defn buffer
  "Coerces its argument into a java.nio.Buffer. If the argument is
  already a Buffer this is a no-op. Otherwise, return an appropriate
  buffer type. Implemented for byte array, char array, double array,
  float array, int array, long array, and short array."
  ^java.nio.Buffer
  [x] (proto/make-buffer x))
(defn buffer-seq
  "Returns a seq that iterates over the elements in a
  java.nio.Buffer. Does not change the position of the Buffer."
  ^clojure.lang.ISeq
  [^java.nio.Buffer x] (proto/buffer-seq x 0))
(defn buffer-nth
  "Returns element n of a java.nio.Buffer. Does not change the
  position of the Buffer."
  ([x n] (buffer-nth x n nil))
  ([x n not-found] (proto/buffer-nth x n not-found)))
(defn buffer-to-array
  "Returns an array representation of a java.nio.Buffer. Creates a new
  array (even for an array backed Buffer), and copies each element of
  the Buffer into the array, respecting the Buffer's limit. Does not
  change the position of the Buffer."
  [x] (proto/buffer-to-array x))
(defn byte-buffer
  "Coerces its argument into a java.nio.ByteBuffer. If the argument
  that is already a ByteBuffer this is a no-op. Implemented for byte
  array."
  ^java.nio.ByteBuffer
  [x] (proto/make-byte-buffer x))
(defn char-buffer
  "Coerces its argument into a java.nio.CharBuffer. If the argument
  that is already a CharBuffer this is a no-op. Implemented for byte
  array, ByteBuffer, char array, and CharSequence (which includes
  String)."
  ^java.nio.CharBuffer
  [x] (proto/make-char-buffer x))
(defn double-buffer
  "Coerces its argument into a java.nio.DoubleBuffer. If the argument
  that is already a DoubleBuffer this is a no-op. Implemented for byte
  array, ByteBuffer, and double array."
  ^java.nio.DoubleBuffer
  [x] (proto/make-double-buffer x))
(defn float-buffer
  "Coerces its argument into a java.nio.FloatBuffer. If the argument
  that is already a FloatBuffer this is a no-op. Implemented for byte
  array, ByteBuffer, and float array."
  ^java.nio.FloatBuffer
  [x] (proto/make-float-buffer x))
(defn int-buffer
  "Coerces its argument into a java.nio.IntBuffer. If the argument
  that is already a IntBuffer this is a no-op. Implemented for byte
  array, ByteBuffer, and int array."
  ^java.nio.IntBuffer
  [x] (proto/make-int-buffer x))
(defn long-buffer
  "Coerces its argument into a java.nio.LongBuffer. If the argument
  that is already a LongBuffer this is a no-op. Implemented for byte
  array, ByteBuffer, and long array."
  ^java.nio.LongBuffer
  [x] (proto/make-long-buffer x))
(defn short-buffer
  "Coerces its argument into a java.nio.ShortBuffer. If the argument
  that is already a Short this is a no-op. Implemented for byte
  array, ByteBuffer, and short array."
  ^java.nio.ShortBuffer
  [x] (proto/make-short-buffer x))

(def byte-array-type (Class/forName "[B"))
(def char-array-type (Class/forName "[C"))
(def double-array-type (Class/forName "[D"))
(def float-array-type (Class/forName "[F"))
(def int-array-type (Class/forName "[I"))
(def long-array-type (Class/forName "[J"))
(def short-array-type (Class/forName "[S"))

(extend byte-array-type
  proto/IBuffer
  {:make-buffer
   (fn [x] (proto/make-byte-buffer x))}
  proto/IByteBuffer
  {:make-byte-buffer
   (fn [x]
     (ByteBuffer/wrap x))}
  proto/ICharBuffer
  {:make-char-buffer
   (fn [x]
     (proto/make-char-buffer (proto/make-byte-buffer x)))}
  proto/IDoubleBuffer
  {:make-double-buffer
   (fn [x]
     (proto/make-double-buffer (proto/make-byte-buffer x)))}
  proto/IFloatBuffer
  {:make-float-buffer
   (fn [x]
     (proto/make-float-buffer (proto/make-byte-buffer x)))}
  proto/IIntBuffer
  {:make-int-buffer
   (fn [x]
     (proto/make-int-buffer (proto/make-byte-buffer x)))}
  proto/ILongBuffer
  {:make-long-buffer
   (fn [x]
     (proto/make-long-buffer (proto/make-byte-buffer x)))}
  proto/IShortBuffer
  {:make-short-buffer
   (fn [x]
     (proto/make-short-buffer (proto/make-byte-buffer x)))})
(extend char-array-type
  proto/IBuffer
  {:make-buffer
   (fn [x] (proto/make-char-buffer x))}
  proto/ICharBuffer
  {:make-char-buffer
   (fn [^chars x]
     (CharBuffer/wrap x))})
(extend double-array-type
  proto/IBuffer
  {:make-buffer
   (fn [x] (proto/make-double-buffer x))}
  proto/IDoubleBuffer
  {:make-double-buffer
   (fn [x]
     (DoubleBuffer/wrap x))})
(extend float-array-type
  proto/IBuffer
  {:make-buffer
   (fn [x] (proto/make-float-buffer x))}
  proto/IFloatBuffer
  {:make-float-buffer
   (fn [x]
     (FloatBuffer/wrap x))})
(extend int-array-type
  proto/IBuffer
  {:make-buffer
   (fn [x] (proto/make-int-buffer x))}
  proto/IIntBuffer
  {:make-int-buffer
   (fn [x]
     (IntBuffer/wrap x))})
(extend long-array-type
  proto/IBuffer
  {:make-buffer
   (fn [x] (proto/make-long-buffer x))}
  proto/ILongBuffer
  {:make-long-buffer
   (fn [x]
     (LongBuffer/wrap x))})
(extend short-array-type
  proto/IBuffer
  {:make-buffer
   (fn [x] (proto/make-short-buffer x))}
  proto/IShortBuffer
  {:make-short-buffer
   (fn [x]
     (ShortBuffer/wrap x))})

(extend-type ByteBuffer
  proto/IBuffer
  (make-buffer [x] x)
  (buffer-seq [x ^long start]
    (lazy-seq
     (if (< start (.limit x))
       (cons (.get x start) (proto/buffer-seq x (inc start))))))
  (buffer-nth [x ^Number n not-found]
    (try
      (.get x (.intValue n))
      (catch IndexOutOfBoundsException e
        not-found)))
  (buffer-to-array [x] (into-array Byte/TYPE (buffer-seq x)))
  proto/IByteBuffer
  (make-byte-buffer [x] x)
  proto/ICharBuffer
  (make-char-buffer [x] (.asCharBuffer x))
  proto/IDoubleBuffer
  (make-double-buffer [x] (.asDoubleBuffer x))
  proto/IFloatBuffer
  (make-float-buffer [x] (.asFloatBuffer x))
  proto/IIntBuffer
  (make-int-buffer [x] (.asIntBuffer x))
  proto/ILongBuffer
  (make-long-buffer [x] (.asLongBuffer x))
  proto/IShortBuffer
  (make-short-buffer [x] (.asShortBuffer x)))
(extend-type CharBuffer
  proto/IBuffer
  (make-buffer [x] x)
  (buffer-seq [x ^long start]
    (lazy-seq
     (if (< start (.limit x))
       (cons (.get x start) (proto/buffer-seq x (inc start))))))
  (buffer-nth [x ^Number n not-found]
    (try
      (.get x (.intValue n))
      (catch IndexOutOfBoundsException e
        not-found)))
  (buffer-to-array [x] (into-array Character/TYPE (buffer-seq x)))
  proto/ICharBuffer
  (make-char-buffer [x] x))
(extend-type CharSequence
  proto/IBuffer
  (make-buffer [x] (char-buffer x))
  (buffer-seq [x]
    (throw (Exception. "CharSequence is not a Buffer")))
  (buffer-nth [x ^Number n not-found]
    (throw (Exception. "CharSequence is not a Buffer")))
  (buffer-to-array [x]
    (throw (Exception. "CharSequence is not a Buffer")))
  proto/ICharBuffer
  (make-char-buffer [x] (CharBuffer/wrap x)))
(extend-type DoubleBuffer
  proto/IBuffer
  (make-buffer [x] x)
  (buffer-seq [x ^long start]
    (lazy-seq
     (if (< start (.limit x))
       (cons (.get x start) (proto/buffer-seq x (inc start))))))
  (buffer-nth [x ^Number n not-found]
    (try
      (.get x (.intValue n))
      (catch IndexOutOfBoundsException e
        not-found)))
  (buffer-to-array [x] (into-array Double/TYPE (buffer-seq x)))
  proto/IDoubleBuffer
  (make-double-buffer [x] x))
(extend-type FloatBuffer
  proto/IBuffer
  (make-buffer [x] x)
  (buffer-seq [x ^long start]
    (lazy-seq
     (if (< start (.limit x))
       (cons (.get x start) (proto/buffer-seq x (inc start))))))
  (buffer-nth [x ^Number n not-found]
    (try
      (.get x (.intValue n))
      (catch IndexOutOfBoundsException e
        not-found)))
  (buffer-to-array [x] (into-array Float/TYPE (buffer-seq x)))
  proto/IFloatBuffer
  (make-float-buffer [x] x))
(extend-type IntBuffer
  proto/IBuffer
  (make-buffer [x] x)
  (buffer-seq [x ^long start]
    (lazy-seq
     (if (< start (.limit x))
       (cons (.get x start) (proto/buffer-seq x (inc start))))))
  (buffer-nth [x ^Number n not-found]
    (try
      (.get x (.intValue n))
      (catch IndexOutOfBoundsException e
        not-found)))
  (buffer-to-array [x] (into-array Integer/TYPE (buffer-seq x)))
  proto/IIntBuffer
  (make-int-buffer [x] x))
(extend-type LongBuffer
  proto/IBuffer
  (make-buffer [x] x)
  (buffer-seq [x ^long start]
    (lazy-seq
     (if (< start (.limit x))
       (cons (.get x start) (proto/buffer-seq x (inc start))))))
  (buffer-nth [x ^Number n not-found]
    (try
      (.get x (.intValue n))
      (catch IndexOutOfBoundsException e
        not-found)))
  (buffer-to-array [x] (into-array Long/TYPE (buffer-seq x)))
  proto/ILongBuffer
  (make-long-buffer [x] x))
(extend-type ShortBuffer
  proto/IBuffer
  (make-buffer [x] x)
  (buffer-seq [x ^long start]
    (lazy-seq
     (if (< start (.limit x))
       (cons (.get x start) (proto/buffer-seq x (inc start))))))
  (buffer-nth [x ^Number n not-found]
    (try
      (.get x (.intValue n))
      (catch IndexOutOfBoundsException e
        not-found)))
  (buffer-to-array [x] (into-array Short/TYPE (buffer-seq x)))
  proto/IShortBuffer
  (make-short-buffer [x] x))

(extend-protocol proto/IMmap
  String
  (do-mmap [file position size {:keys [mode] :as opts}]
    (with-open [file-channel ^FileChannel (channel file)]
      (proto/do-mmap file-channel position size opts)))
  File
  (do-mmap [file position size {:keys [mode] :as opts}]
    (with-open [file-channel ^FileChannel (channel file)]
      (proto/do-mmap file-channel position size opts)))
  FileChannel
  (do-mmap [file-channel position size {:keys [mode] :as opts}]
    (let [position (or position 0)
          size (or size (.size file-channel))
          mode (or mode :read-write)]
      (.map file-channel
            (get {:read-write FileChannel$MapMode/READ_WRITE
                  :read-only FileChannel$MapMode/READ_ONLY
                  :private FileChannel$MapMode/PRIVATE}
                 mode)
            position
            size))))

(defn mmap
  "Memory maps a file with optional offset and length arguments to map
  only a portion of the file. Accepts keyword :mode option for the
  mapping mode. Valid options are: :read-write, :read-only, :private.
  Implemented for String, File and FileChannel."
  ^MappedByteBuffer
  [file & [offset length & {:as opts}]]
  (proto/do-mmap file offset length opts))

(defn readable-channel
  "Attempts to coerce its argument into an open
  java.nio.ReadableByteChannel.

  Default implementations are defined for ReadableByteChannel,
  ByteChannel, Pipe, InputStream, File, RandomAccessFile, Socket, and
  DatagramSocket arguments.

  Should be used inside with-open to ensure the ReadableByteChannel is
  properly closed."
  ^java.nio.channels.ReadableByteChannel
  [x]
  (proto/make-readable-channel x))

(defn writable-channel
  "Attempts to coerce its argument into an open
  java.nio.WritableByteChannel.

  Default implementations are defined for WritableByteChannel,
  ByteChannel, Pipe, OutputStream, File, RandomAccessFile, Socket, and
  DatagramSocket arguments.

  Should be used inside with-open to ensure the WritableByteChannel is
  properly closed."
  ^java.nio.channels.WritableByteChannel
  [x]
  (proto/make-writable-channel x))

(defn channel
  "Attempts to coerce its argument into an open java.nio.ByteChannel.

  Default implementations are defined for ByteChannel, File,
  RandomAccessFile, Socket, and DatagramSocket arguments.

  Should be used inside with-open to ensure the WritableByteChannel is
  properly closed."
  ^java.nio.channels.ByteChannel
  [x]
  (proto/make-channel x))

(def default-channels-impl
  {:make-channel
   (fn [x]
     (throw
      (IllegalArgumentException.
       (str "Cannot open <" (pr-str x) "> as an ByteChannel."))))
   :make-readable-channel
   (fn [x] (proto/make-readable-channel (proto/make-channel x)))
   :make-writable-channel
   (fn [x] (proto/make-writable-channel (proto/make-channel x)))})

(extend ReadableByteChannel
  proto/NIOFactory
  (assoc default-channels-impl
    :make-readable-channel (fn [x] x)))

(extend WritableByteChannel
  proto/NIOFactory
  (assoc default-channels-impl
    :make-writable-channel (fn [x] x)))

(extend ByteChannel
  proto/NIOFactory
  (assoc default-channels-impl
    :make-channel (fn [x] x)
    :make-readable-channel (fn [x] x)
    :make-writable-channel (fn [x] x)))

(extend Pipe
  proto/NIOFactory
  (assoc default-channels-impl
    :make-readable-channel (fn [^Pipe x] (.source x))
    :make-writable-channel (fn [^Pipe x] (.sink x))))

(extend InputStream
  proto/NIOFactory
  (assoc default-channels-impl
    :make-readable-channel
    (fn [^InputStream x] (Channels/newChannel x))))

(extend ByteBuffer
  proto/NIOFactory
  (assoc default-channels-impl
    :make-readable-channel
    (fn [^ByteBuffer x]
      (proto/make-readable-channel (jio/input-stream x)))))

(extend OutputStream
  proto/NIOFactory
  (assoc default-channels-impl
    :make-writable-channel
    (fn [^OutputStream x] (Channels/newChannel x))))

(extend File
  proto/NIOFactory
  (assoc default-channels-impl
    :make-channel
    (fn [^File x] (proto/make-channel (RandomAccessFile. x "rw")))
    :make-readable-channel
    (fn [^File x]
      (proto/make-readable-channel (FileInputStream. x)))
    :make-writable-channel
    (fn [^File x]
      (proto/make-writable-channel (FileOutputStream. x)))))

(extend Socket
  proto/NIOFactory
  (assoc default-channels-impl
    :make-channel (fn [^Socket x] (.getChannel x))))

(extend DatagramSocket
  proto/NIOFactory
  (assoc default-channels-impl
    :make-channel (fn [^DatagramSocket x] (.getChannel x))))

(extend String
  proto/NIOFactory
  (assoc default-channels-impl
    :make-channel
    (fn [^String x] (proto/make-channel (jio/file x)))
    :make-readable-channel
    (fn [^String x]
      (try
        (proto/make-readable-channel (jio/input-stream (URL. x)))
        (catch MalformedURLException e
          (proto/make-readable-channel (jio/file x)))))
    :make-writable-channel
    (fn [^String x]
      (try
        (proto/make-writable-channel (jio/output-stream (URL. x)))
        (catch MalformedURLException e
          (proto/make-writable-channel (jio/file x)))))))

(extend RandomAccessFile
  proto/NIOFactory
  (assoc default-channels-impl
    :make-channel (fn [^RandomAccessFile x] (.getChannel x))))

(def byte-order-to-keyword {ByteOrder/LITTLE_ENDIAN :little-endian
                            ByteOrder/BIG_ENDIAN :big-endian})

(defn byte-order
  "Returns the byte order for buf as a keyword, either :big-endian
  or :little-endian."
  [^ByteBuffer buf]
  (byte-order-to-keyword (.order buf)))

(def keyword-to-byte-order {:little-endian ByteOrder/LITTLE_ENDIAN
                            :big-endian ByteOrder/BIG_ENDIAN})

(defn set-byte-order!
  "Sets the byte order for buf, order should be either :big-endian
  or :little-endian."
  [^ByteBuffer buf order]
  (.order buf (or (keyword-to-byte-order order)
                  (throw (IllegalArgumentException.
                          (str "Invalid order: " (pr-str order)))))))
