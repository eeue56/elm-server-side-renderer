module ServerSide.Constants exposing (..)


styleKey : String
styleKey = "STYLE"

eventKey : String
eventKey = "EVENT"

attributeKey : String
attributeKey = "ATTR"

attributeNamespaceKey : String
attributeNamespaceKey = "ATTR_NS"


knownKeys : List String
knownKeys =
    [ styleKey, eventKey, attributeKey, attributeNamespaceKey ]
