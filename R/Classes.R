setClass(
  "RawData",
  slots = list(
    time = "numeric",
    rfid = "character",
    antenna = "integer",
    delay = "integer",
    idList = "data.frame",
    uniAnt = "integer",
    rawAntCount = "integer",
    antCount = "integer",
    size = "integer",
    simplify = "logical"
  )
)

setClass(
  "Timeline",
  slots = list(
    phase = "character",
    start = "numeric",
    end = "numeric",
    binPhase = "character",
    binStart = "numeric",
    binEnd = "numeric",
    binSize = "numeric"
  )
)

setClass(
  "Events",
  slots = list(
    start = "numeric",
    end = "numeric",
    rfid = "character",
    mid = "character",
    from = "integer",
    to = "integer",
    length = "numeric",
    loc = "character",
    size = "integer",
    idList = "data.frame",
    threshold = "numeric"
  )
)

setClass(
  "Activity",
  slots = list(
    phase = "character",
    binSize = "numeric",
    rfid = "character",
    mid = "character",
    uloc = "character",
    visits = "matrix",
    time = "matrix",
    ratio = "numeric"
  )
)

setClass(
  "EcoHABData",
  slots = list(
    config = "data.frame",
    raw = "RawData",
    timeline = "Timeline",
    events = "Events",
    cage.visit = "Activity",
    tube.visit = "Activity",
    incohort = "Events",
    sociability = "Activity",
    solo = "Events",
    solitude = "Activity",
    inter = "Events",
    leading = "Activity"
  )
)

setGeneric("setBinSize", function(obj, ...) standardGeneric("setBinSize"))

setGeneric("calcEvents", function(obj, ...) standardGeneric("calcEvents"))

setGeneric("calcActivity", function(obj, ...) standardGeneric("calcActivity"))

setGeneric("calcSingleEvents", function(obj, ...) standardGeneric("calcSingleEvents"))

setGeneric("calcIncohortEvents", function(obj, ...) standardGeneric("calcIncohortEvents"))

setGeneric("calcLeadEvents", function(obj, ...) standardGeneric("calcLeadEvents"))

setGeneric("calcCoincidence", function(obj, ...) standardGeneric("calcCoincidence"))

setGeneric("adjustSociability", function(paired, single, ...) standardGeneric("adjustSociability"))

setGeneric("adjustLeading", function(leading, single, ...) standardGeneric("adjustLeading"))

setGeneric("filter", function(obj, ...) standardGeneric("filter"))

setGeneric("getEvents", function(obj, ...) standardGeneric("getEvents"))

setGeneric("getVisits", function(obj, ...) standardGeneric("getVisits"))

setGeneric("getDurations", function(obj, ...) standardGeneric("getDurations"))

setGeneric("getRatios", function(obj, ...) standardGeneric("getRatios"))

setGeneric("getTubeVisits", function(obj, ...) standardGeneric("getTubeVisits"))

setGeneric("getCageDurations", function(obj, ...) standardGeneric("getCageDurations"))

setGeneric("getSociability", function(obj, ...) standardGeneric("getSociability"))

setGeneric("getSolitude", function(obj, ...) standardGeneric("getSolitude"))

setGeneric("getLeadingCounts", function(obj, ...) standardGeneric("getLeadingCounts"))

setGeneric("getLeadingIndex", function(obj, ...) standardGeneric("getLeadingIndex"))

defaultConfig = data.frame(antenna = 1:8,
                           tube = c("ab", "ba", "bc", "cb", "cd", "dc", "da", "ad"),
                           cage = c("A", "B", "B", "C", "C", "D", "D", "A"))
