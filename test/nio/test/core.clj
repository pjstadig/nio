(ns nio.test.core
  (:require [clojure.java.io :as jio]
            [clojure.test :refer :all]
            [nio.core :refer :all])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream
                    FileNotFoundException)
           (java.nio ByteBuffer CharBuffer DoubleBuffer FloatBuffer IntBuffer
                     LongBuffer ShortBuffer)
           (java.nio.channels FileChannel)))

(defn make-byte-array ^bytes []
  (into-array Byte/TYPE [0 113 0 117 0 117 0 120]))

(defn assert-buffer [type buffer-type vector & [result-vector]]
  (let [result-vector (or result-vector vector)
        buf (buffer (into-array type vector))]
    (is (identical? buf (buffer buf)))
    (is (instance? buffer-type buf))
    (is (= result-vector (buffer-seq buf)))
    (is (= result-vector (seq (buffer-to-array buf))))
    (dotimes [i (count result-vector)]
      (is (= (nth result-vector i) (buffer-nth buf i))))
    (is (= 0 (.position buf)))))

(deftest test-buffer
  (let [result-vector [0 113 0 117 0 117 0 120]
        buf (-> (ByteBuffer/allocate 8)
                (.put (make-byte-array))
                .rewind)]
    (is (identical? buf (buffer buf)))
    (is (instance? ByteBuffer buf))
    (is (= result-vector (buffer-seq buf)))
    (is (= result-vector (seq (buffer-to-array buf))))
    (dotimes [i (count result-vector)]
      (is (= (nth result-vector i) (buffer-nth buf i))))
    (is (= 0 (.position buf))))
  (assert-buffer Byte/TYPE ByteBuffer [0 113 0 117 0 117 0 120])
  (let [result-vector [\q \u \u \x]
        buf (char-buffer (make-byte-array))]
    (is (identical? buf (buffer buf)))
    (is (instance? CharBuffer buf))
    (is (= result-vector (buffer-seq buf)))
    (is (= result-vector (seq (buffer-to-array buf))))
    (dotimes [i (count result-vector)]
      (is (= (nth result-vector i) (buffer-nth buf i))))
    (is (= 0 (.position buf))))
  (let [result-vector [1.5132091212543083E-306]
        buf (double-buffer (make-byte-array))]
    (is (identical? buf (buffer buf)))
    (is (instance? DoubleBuffer buf))
    (is (= result-vector (buffer-seq buf)))
    (dotimes [i (count result-vector)]
      (is (= (nth result-vector i) (buffer-nth buf i))))
    (is (= 0 (.position buf))))
  (let [result-vector [(float 1.0377575E-38) (float 1.0744921E-38)]
        buf (float-buffer (make-byte-array))]
    (is (identical? buf (buffer buf)))
    (is (instance? FloatBuffer buf))
    (is (= result-vector
           (buffer-seq buf)))
    (dotimes [i (count result-vector)]
      (is (= (nth result-vector i) (buffer-nth buf i))))
    (is (= 0 (.position buf))))
  (let [result-vector [7405685 7667832]
        buf (int-buffer (make-byte-array))]
    (is (identical? buf (buffer buf)))
    (is (instance? IntBuffer buf))
    (is (= result-vector (buffer-seq buf)))
    (dotimes [i (count result-vector)]
      (is (= (nth result-vector i) (buffer-nth buf i))))
    (is (= 0 (.position buf))))
  (let [result-vector [31807174887145592]
        buf (long-buffer (make-byte-array))]
    (is (identical? buf (buffer buf)))
    (is (instance? LongBuffer buf))
    (is (= result-vector (buffer-seq buf)))
    (dotimes [i (count result-vector)]
      (is (= (nth result-vector i) (buffer-nth buf i))))
    (is (= 0 (.position buf))))
  (let [result-vector [113 117 117 120]
        buf (short-buffer (make-byte-array))]
    (is (identical? buf (buffer buf)))
    (is (instance? ShortBuffer buf))
    (is (= result-vector (buffer-seq buf)))
    (dotimes [i (count result-vector)]
      (is (= (nth result-vector i) (buffer-nth buf i))))
    (is (= 0 (.position buf))))
  (assert-buffer Character/TYPE CharBuffer [\f \o \o])
  (let [result-vector [\f \o \o]
        buf (buffer "foo")]
    (is (identical? buf (buffer buf)))
    (is (instance? CharBuffer buf))
    (is (= result-vector (buffer-seq buf)))
    (dotimes [i (count result-vector)]
      (is (= (nth result-vector i) (buffer-nth buf i))))
    (is (= 0 (.position buf))))
  (assert-buffer Double/TYPE DoubleBuffer [1.0 2.0 3.0])
  (assert-buffer Float/TYPE FloatBuffer [1.0 2.0 3.0])
  (assert-buffer Integer/TYPE IntBuffer [1 2 3])
  (assert-buffer Long/TYPE LongBuffer [1 2 3])
  (assert-buffer Short/TYPE ShortBuffer [1 2 3]))

(deftest test-buffer-seq
  (let [buf (byte-buffer (into-array Byte/TYPE [1 2 3 4 5]))]
    (.get buf)
    (.limit buf 3)
    (is (= [1 2 3] (buffer-seq buf)))
    (is (= 1 (.position buf)))
    (is (= 3 (.limit buf)))))

(deftest test-buffer-to-array
  (let [buf (byte-buffer (into-array Byte/TYPE [1 2 3 4 5]))]
    (.get buf)
    (.limit buf 3)
    (is (= [1 2 3] (seq (buffer-to-array buf))))
    (is (= 1 (.position buf)))
    (is (= 3 (.limit buf))))
  (let [buf (ByteBuffer/allocateDirect 5)]
    (.put buf (byte 1))
    (.put buf (byte 2))
    (.put buf (byte 3))
    (.flip buf)
    (.get buf)
    (is (= [1 2 3] (seq (buffer-to-array buf))))
    (is (= 1 (.position buf)))
    (is (= 3 (.limit buf)))))

(deftest test-mmap
  (let [m (mmap "test/foo.txt")]
    (is (= "foo.txt\n" (String. ^bytes (buffer-to-array m)
                                "UTF-8"))))
  (let [m (mmap "test/foo.txt")]
    (is (= (.get m) 102)))
  (let [m (mmap (jio/file "test/foo.txt"))]
    (is (= "foo.txt\n" (String. ^bytes (buffer-to-array m)
                                "UTF-8"))))
  (let [m (mmap (channel (jio/file "test/foo.txt")))]
    (is (= "foo.txt\n" (String. ^bytes (buffer-to-array m)
                                "UTF-8")))))

(deftest test-copy
  (with-open [in (readable-channel (-> "foo"
                                       .getBytes
                                       ByteArrayInputStream.))
              baos (ByteArrayOutputStream.)
              out (writable-channel baos)]
    (jio/copy in out)
    (is (= "foo" (String. (.toByteArray baos) "UTF-8"))))
  (with-open [in (channel "test/foo.txt")
              baos (ByteArrayOutputStream.)
              out (writable-channel baos)]
    (jio/copy in out)
    (is (= "foo.txt\n" (String. (.toByteArray baos) "UTF-8"))))
  (try
    (with-open [in (channel "test/foo.txt")
                out (channel "test/foo2.txt")]
      (jio/copy in out)
      (is (= "foo.txt\n" (slurp (jio/file "test/foo2.txt")))))
    (finally
      (.delete (jio/file "test/foo2.txt"))))
  (try
    (with-open [in (readable-channel (-> "foo"
                                         .getBytes
                                         ByteArrayInputStream.))
                out (channel "test/foo2.txt")]
      (jio/copy in out)
      (is (= "foo" (slurp (jio/file "test/foo2.txt")))))
    (finally
      (.delete (jio/file "test/foo2.txt"))))
  (try
    (let [in (buffer (-> "foo"
                         .getBytes))]
      (with-open [out (channel "test/foo2.txt")]
        (jio/copy in out)
        (is (= "foo" (slurp (jio/file "test/foo2.txt"))))))
    (finally
      (.delete (jio/file "test/foo2.txt"))))
  (let [out (buffer (byte-array 8))]
    (with-open [in (readable-channel "test/foo.txt")]
      (jio/copy in out)
      (is (= "foo.txt\n" (String. (.array out) "UTF-8"))))))

(deftest test-readable-channel
  (is (thrown? FileNotFoundException (readable-channel "bogus"))
      "opening non-existent file should throw an exception"))

(deftest test-file-channel
  (try
    (is (instance? FileChannel (writable-channel "test/test-file-channel.txt")))
    (is (instance? FileChannel (readable-channel "test/test-file-channel.txt")))
    (is (instance? FileChannel (channel "test/test-file-channel.txt")))
    (finally
      (.delete (jio/file "test/test-file-channel.txt")))))

(deftest test-byte-order
  (let [buf (ByteBuffer/allocate 0)]
    (set-byte-order! buf :big-endian)
    (is (= :big-endian (byte-order buf)))
    (set-byte-order! buf :little-endian)
    (is (= :little-endian (byte-order buf)))
    (set-byte-order! buf :big-endian)
    (is (= :big-endian (byte-order buf)))
    (is (thrown? IllegalArgumentException (set-byte-order! buf :foo)))))
