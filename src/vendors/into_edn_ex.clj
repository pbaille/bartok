(ns vendors.into-edn-ex
  (:use utils.all)
  (:use bartok.primitives)
  (:require [into-edn :refer [into-edn]]
            [clojure.zip :as zip]
            [clojure.data.xml :as xml]
            [clojure.data.zip.xml :as zf]))

(def xml-string "
<responses>
  <response status=\"okay\">
    <ident>1234</ident>
    <title lang=\"English\">Creativity fails me</title>
    <children>
      <child><name>Humphrey</name><age>3</age></child>
      <child><name>Laura</name><age>5</age></child>
      <child><name>Mikey</name><age>7</age></child>
    </children>
  </response>
</responses>")

(def xml-spec
  [#(zf/xml-> % :response)
   {:status #(zf/xml1-> % (zf/attr :status))
    :ident  #(some-> %
                     (zf/xml1-> :ident zf/text)
                     (Long/parseLong))
    :title #(zf/xml1-> % :title zf/text)
    :title-lang #(zf/xml1-> % :title (zf/attr :lang))
    :children [#(zf/xml-> % :children :child)
               {:name #(zf/xml1-> % :name zf/text)
                :age  #(some-> %
                               (zf/xml1-> :age zf/text)
                               (Long/parseLong))}]}])


(def res (into-edn xml-spec (some-> xml-string
                           xml/parse-str
                           zip/xml-zip)))

(def scor (slurp "music-files/xml/noct21.xml"))

(def music-xml-spec
  [#(zf/xml-> % :part)
    [#(zf/xml-> % :measure)
      [#(zf/xml-> % :note) 
        {:pitch #(or (some-> (zf/xml1-> % :pitch :step zf/text)
                             (kwcat
                               (some-> (zf/xml1-> % :pitch :alter zf/text parse-int) alteration :name)
                               (zf/xml1-> % :pitch :octave zf/text)))
                     :none)}]]])

(def res2 (into-edn music-xml-spec (some-> scor
                           xml/parse-str
                           zip/xml-zip)))