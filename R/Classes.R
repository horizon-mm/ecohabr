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
    following = "Activity"
  )
)

setGeneric("setBinSize", function(obj, ...) standardGeneric("setBinSize"))

setGeneric("calcEvents", function(obj, ...) standardGeneric("calcEvents"))

setGeneric("filter", function(obj, ...) standardGeneric("filter"))

setGeneric("adjustTubeEvents", function(obj, ...) standardGeneric("adjustTubeEvents"))

setGeneric("calcActivity", function(obj, ...) standardGeneric("calcActivity"))

setGeneric("calcSingleEvents", function(obj, ...) standardGeneric("calcSingleEvents"))

setGeneric("calcIncohortEvents", function(obj, ...) standardGeneric("calcIncohortEvents"))

setGeneric("calcFollowEvents", function(obj, ...) standardGeneric("calcFollowEvents"))

setGeneric("calcCoincidence", function(obj, ...) standardGeneric("calcCoincidence"))

setGeneric("adjustSociability", function(paired, single, ...) standardGeneric("adjustSociability"))

setGeneric("adjustFollowing", function(following, single, ...) standardGeneric("adjustFollowing"))

setGeneric("getEvents", function(obj, ...) standardGeneric("getEvents"))

setGeneric("getVisits", function(obj, ...) standardGeneric("getVisits"))

setGeneric("getDurations", function(obj, ...) standardGeneric("getDurations"))

setGeneric("getRatios", function(obj, ...) standardGeneric("getRatios"))

setGeneric("getTubeVisits", function(obj, ...) standardGeneric("getTubeVisits"))

setGeneric("getCageDurations", function(obj, ...) standardGeneric("getCageDurations"))

setGeneric("getSociability", function(obj, ...) standardGeneric("getSociability"))

setGeneric("getSolitude", function(obj, ...) standardGeneric("getSolitude"))

setGeneric("getFollowingCounts", function(obj, ...) standardGeneric("getFollowingCounts"))

setGeneric("getFollowingIndex", function(obj, ...) standardGeneric("getFollowingIndex"))

defaultConfig = data.frame(antenna = 1:8,
                           tube = c("ab", "ba", "bc", "cb", "cd", "dc", "da", "ad"),
                           cage = c("A", "B", "B", "C", "C", "D", "D", "A"))
defaultLocMap = matrix(c(NA, "ba", "B", "B", "err", "err", "A", "A",
                      "ab", NA, "B", "B", "err", "err", "A", "A",
                      "B", "B", NA, "cb", "C", "C", "err", "err",
                      "B", "B", "bc", NA, "C", "C", "err", "err",
                      "err", "err", "C", "C", NA, "dc", "C", "C",
                      "err", "err", "C", "C", "cd", NA, "C", "C",
                      "A", "A", "err", "err", "D", "D", NA, "ad",
                      "A", "A", "err", "err", "D", "D", "da", NA),
                    nrow = 8, dimnames = list(1:8, 1:8))