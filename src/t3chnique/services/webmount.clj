(ns ^{:doc "Protocol for mountable web service"}
  t3chnique.services.webmount)

(defprotocol WebMount
  "Bidi routes and handler map for a REST web service"
  (routes [self]
    "Bidi routes (for mounting within other routes). The target of a
    match is a key in the map returned by handlers.")
  (handlers [self]
    "Map of key (returned by route match) to ring handler"))
