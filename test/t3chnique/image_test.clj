(ns t3chnique.image-test
  (:use clojure.test
        clojure.java.io
        t3chnique.image))

(deftest test-load-elysium
  (testing "Elysium.t3"
    (let [f (file (resource "Elysium.t3"))
          buf (load-image-file f)
          blocks (parse-image buf)]
      (is (not (empty? blocks))))))

(deftest test-load-ditch
  (testing "ditch3.t3"
    (let [f (file (resource "ditch3.t3"))
          buf (load-image-file f)
          blocks (parse-image buf)]
      (is (not (empty? blocks))))))
