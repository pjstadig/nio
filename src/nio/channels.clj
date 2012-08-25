(ns nio.channels
  (:require [clojure.java.io :as jio]
            [nio.channels.protocols :as proto])
  (:import (clojure.lang MultiFn)
           (java.nio ByteBuffer)
           (java.nio.channels Channels ByteChannel FileChannel
                              Pipe ReadableByteChannel
                              WritableByteChannel)
           (java.io File FileInputStream FileOutputStream InputStream
                    OutputStream RandomAccessFile)
           (java.net DatagramSocket MalformedURLException Socket
                     URL)))

;; Extend existing clojure.java.io functions to java.nio
(extend ReadableByteChannel
  jio/IOFactory
  (assoc jio/default-streams-impl
    :make-input-stream (fn [x opts] (Channels/newInputStream x))))

(extend WritableByteChannel
  jio/IOFactory
  (assoc jio/default-streams-impl
    :make-output-stream (fn [x opts] (Channels/newOutputStream x))))

(extend ByteChannel
  jio/IOFactory
  (assoc jio/default-streams-impl
    :make-input-stream (fn [x opts] (Channels/newInputStream x))
    :make-output-stream (fn [x opts] (Channels/newOutputStream x))))

;;;; some funny stuff going on because the things we want are private
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
   (with-open [input (channel input)
               output (channel output)]
     (@#'jio/do-copy input output))))

(prefer-method @#'jio/do-copy
               [FileChannel WritableByteChannel]
               [ReadableByteChannel FileChannel])

;;;; New java.nio coercion functions
(defn ^ReadableByteChannel readable-channel
  "Attempts to coerce its argument into an open
  java.nio.ReadableByteChannel.

  Default implementations are defined for ReadableByteChannel,
  ByteChannel, Pipe, InputStream, File, RandomAccessFile, Socket, and
  DatagramSocket arguments.

  Should be used inside with-open to ensure the ReadableByteChannel is
  properly closed."
  [x]
  (proto/make-readable-channel x))

(defn ^WritableByteChannel writable-channel
  "Attempts to coerce its argument into an open
  java.nio.WritableByteChannel.

  Default implementations are defined for WritableByteChannel,
  ByteChannel, Pipe, OutputStream, File, RandomAccessFile, Socket, and
  DatagramSocket arguments.

  Should be used inside with-open to ensure the WritableByteChannel is
  properly closed."
  [x]
  (proto/make-writable-channel x))

(defn ^ByteChannel channel
  "Attempts to coerce its argument into an open java.nio.ByteChannel.

  Default implementations are defined for ByteChannel, File,
  RandomAccessFile, Socket, and DatagramSocket arguments.

  Should be used inside with-open to ensure the WritableByteChannel is
  properly closed."
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
    (fn [^File x] (proto/make-channel (RandomAccessFile. x "rw")))))

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
          (proto/make-channel (jio/file x)))))
    :make-writable-channel
    (fn [^String x]
      (try
        (proto/make-writable-channel (jio/output-stream (URL. x)))
        (catch MalformedURLException e
          (proto/make-channel (jio/file x)))))))

(extend RandomAccessFile
  proto/NIOFactory
  (assoc default-channels-impl
    :make-channel (fn [^RandomAccessFile x] (.getChannel x))))
