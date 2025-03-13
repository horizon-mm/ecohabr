# Slots in class "RawData"
# time, rfid, antenna are arrays read from raw txt files
# time: integer part is the system representation of date and time in second (origin 1970-1-1 0:00:00)
# rfid: string of 10 hexadecimal digits
# antenna: integers 1~8
# delay: in milliseconds
# idList: read from txt file with three columns "rfid", "mid" and "loc"
# uniAnt: unique antennas
# rawAntCount: number of total raw records at each antenna
# antCount: number of records at each antenna, verified to be from any mouse in idList
# size: number of verified records at all antenna
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

# Read timeline from a txt file with three columns
# phase: in the format of "day_x_dark" or "day_x_light"
# start: in the format of "yyyy-mm-dd HH::MM::SS"
# end: in the format of "yyyy-mm-dd HH::MM::SS"
# The start and end time will be transformed to system representation in second
# The following will be generated after giving a time bin
# binPhase: in the format of "day_x_dark/light_div_y"
# binStart and binEnd is system representation of start/end time in each bin
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

# Slots in class "Event"
# start, end, rfid, mid, from, to, length and loc are arrays of all events
# start and end are system representation of time in second plus milisecond
# rfid and mid are paired id for each mouse
# from and to are integers indicating start and end antennas
# length is the duration of the event in seconds plus milliseconds
# loc is the current location of mouse, determined by start and end atennas
# size: number of total events
# idList: data frame with three columns "rfid", "mid" and "loc"
# threshold: event-specific parameter
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

# Slots in class "Activity", a summary of class "Events" by given bins
# phase: same as binPhase defined in class Timeline
# binsize: defined in class Timeline
# rfid and mid are paired id for each mouse
# uloc: array of all locations (cages, tubes, antennas) included in the summary
# visits: a matrix of number of events at specific time bins and locations
# time: a matrix of accumulative time of events at specific time bins and locations
# Each column in visits and time matrix corresponds to one atom in uloc array
# ratio: event-specific parameter
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

# Slots in class "EcoHABData"
# config: data frame with three columns antenna, tube and cage
# See defaultConfig for details
# events: all events inferred from consecutive records in raw
# cage.visits: summary of all events at cages only
# tube.visits: summary of all events at tubes only
# incohort: all events when a specific mouse pair are in the same cage
# sciability: summary of all incohort events, where the ratio is excess sociability (concurrency - chance)
# solo: all events when a specific mouse is in a cage by itself
# solitude: summary of all solo events, where the ratio is proportion of time spent by itsef
# inter: all events when a mouse follows another mouse through any tube
# following: summary of all inter events, where the ratio is proportion of following events to total tube cross events
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

setGeneric("getCageVisits", function(obj, ...) standardGeneric("getCageVisits"))

setGeneric("getCageDurations", function(obj, ...) standardGeneric("getCageDurations"))

setGeneric("getSociability", function(obj, ...) standardGeneric("getSociability"))

setGeneric("getSolitude", function(obj, ...) standardGeneric("getSolitude"))

setGeneric("getFollowingCounts", function(obj, ...) standardGeneric("getFollowingCounts"))

setGeneric("getFollowingIndex", function(obj, ...) standardGeneric("getFollowingIndex"))

# Default EcoHAB configuration
# Four cages "A", "B", "C" and "D" are set clockwise and connected by four tubes
# Eight antennas are labeled clockwise from the tube between cage A and B
# Each row contains a unique antenna, the attached tube and the nearest cage
# Each tube is indicated by two letters of the cage it connects, in lower case
# The tube is directed, the former letter same as the cage in that row
defaultConfig = data.frame(antenna = 1:8,
                           tube = c("ab", "ba", "bc", "cb", "cd", "dc", "da", "ad"),
                           cage = c("A", "B", "B", "C", "C", "D", "D", "A"))
# The mapping between from-to pair and loc in the class "Event" derived from defaultConfig
# Row names indicate "from" antenna while column names indicate "to" antenna
defaultLocMap = matrix(c(NA, "ba", "B", "B", "err", "err", "A", "A",
                      "ab", NA, "B", "B", "err", "err", "A", "A",
                      "B", "B", NA, "cb", "C", "C", "err", "err",
                      "B", "B", "bc", NA, "C", "C", "err", "err",
                      "err", "err", "C", "C", NA, "dc", "D", "D",
                      "err", "err", "C", "C", "cd", NA, "D", "D",
                      "A", "A", "err", "err", "D", "D", NA, "ad",
                      "A", "A", "err", "err", "D", "D", "da", NA),
                    nrow = 8, dimnames = list(1:8, 1:8))