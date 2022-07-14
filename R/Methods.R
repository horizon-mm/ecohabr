#' Initialize class "RawData"
#'
#' @param RawData The new object of "RawData" class
#' @param rawDir A string describing path to raw data or parsed data folder
#' @param idFile A string describing path to ID list file (three columns: rfid, mid, initial location)
#' @param simplify A logical value indicating raw data (FALSE) or parsed data (TRUE)
#' @importFrom data.table fread
#'
#' @return An object of "RawData" class
#'
#'
setMethod("initialize", "RawData",
          function(.Object, rawDir = NULL, idFile = NULL, simplify = FALSE) {
            idList <- data.frame(rfid = character(0), mid = character(0),
                                 loc = character(0))
            time <- list()
            rfid <- list()
            antenna <- list()
            delay <- list()
            if(!is.null(idFile) & !is.null(rawDir)) {
              idList <-  read.delim(idFile)
              rawFiles <- file.path(rawDir, dir(rawDir, pattern = "0000.txt"), fsep = "\\")
              i <- 1L
              if (simplify) {
                for (file in rawFiles) {
                  data <- fread(file, sep = "\t", header = FALSE)
                  time[[i]] <- paste(data[, 2], data[, 3], sep = "_")
                  rfid[[i]] <- data[, 6]
                  antenna[[i]] <- data[, 4]
                  delay[[i]] <- data[, 5]
                  i <- i+1
                }
              } else {
                for (file in rawFiles) {
                  data <- fread(file, sep = "\t", header = FALSE)
                  time[[i]] <- data[, 1]
                  rfid[[i]] <- data[, 2]
                  antenna[[i]] <- data[, 3]
                  i <- i+1
                }
              }
              time <- unlist(time, use.names = FALSE)
              rfid <- unlist(rfid, use.names = FALSE)
              antenna <- unlist(antenna, use.names = FALSE)
              delay <- unlist(delay, use.names = FALSE)
              .Object@uniAnt <- sort(unique(antenna))
              .Object@rawAntCount <- as.integer(table(antenna))
              index <- which(rfid %in% idList$rfid)
              if (simplify) {
                .Object@time <- as.numeric(as.POSIXct(substr(time[index], 1, 19),
                                                      format = "%Y.%m.%d_%T")) +
                  as.numeric(substr(time[index], 21, 23))/1000
                .Object@delay <- delay[index]
              } else {
                .Object@time <- as.numeric(as.POSIXct(substr(time[index], 1, 15),
                                                      format = "%Y%m%d_%H%M%S")) +
                  as.numeric(substr(time[index], 16, 18))/1000
              }
              .Object@rfid <- rfid[index]
              .Object@antenna <- antenna[index]
              .Object@idList <- subset(idList, rfid %in% unique(.Object@rfid))
              .Object@antCount <- as.integer(table(.Object@antenna))
              .Object@size <- length(.Object@time)
              .Object@simplify <- simplify
            }
            return(.Object)
          })

#' Initialize class "Timeline"
#'
#' @param Timeline The new object of "Timeline" class
#' @param timeFile A string describing path to timeline file (three columns: phase name, start, end)
#'
#' @return An object of "Timeline" class
#'
#'
setMethod("initialize", "Timeline",
          function(.Object, timeFile = NULL) {
            if(!is.null(timeFile)) {
              timeline <- read.delim(timeFile)
              .Object@phase <- timeline$phase
              .Object@start <- as.numeric(as.POSIXct(timeline$start))
              .Object@end <- as.numeric(as.POSIXct(timeline$end))
              .Object@binPhase <- .Object@phase
              .Object@binStart <- .Object@start
              .Object@binEnd <- .Object@end
              .Object@binSize <- as.numeric(difftime(timeline$end,
                                                     timeline$start,
                                                     units = "secs"))
            }
            return(.Object)
          })

#' Initialize an object of "EcoHABData" class
#'
#' @param EcoHABData The new object of "EcoHABata" class
#' @param rawDir A string describing path to raw data or parsed data folder
#' @param idFile A string describing path to ID list file (three columns: rfid, mid, initial location)
#' @param timeFile A string describing path to timeline file (three columns: phase name, start, end)
#' @param config A data frame describing structure of EcoHAB
#' @param simplify A logical value indicating raw data (FALSE) or parsed data (TRUE)
#'
#' @return An object of "EcoHAB" class
#'
#'
setMethod("initialize", "EcoHABData",
          function(.Object, rawDir = NULL, idFile = NULL, timeFile = NULL,
                   config = NULL, simplify = FALSE) {
            if (is.null(config)) {
              .Object@config <- defaultConfig
            }
            .Object@raw <- new("RawData", rawDir, idFile, simplify)
            .Object@timeline <- new("Timeline", timeFile)
            return(.Object)
          })

#' Title
#'
#' @param RawData
#'
#' @return
#'
#'
#' @examples
setMethod("show", "RawData",
          function(object) {
            cat(is(object)[[1]], "\n",
                  "Valid readings: ", object@size, "\n",
                  "       All IDs: ", object@idList$mid, "\n",
                  "    Start time: ", as.character(as.POSIXct(floor(object@time[1])-2,
                                                      origin = "1970-01-01")), "\n",
                  "      End time: ", as.character(as.POSIXct(floor(object@time[length(object@time)])+2,
                                                      origin = "1970-01-01")), "\n")
          })
#' Title
#'
#' @param Events
#'
#' @return
#'
#'
#' @examples
setMethod("show", "Events",
          function(object) {
            cat(is(object)[[1]], "\n",
                  "Total events: ", object@size, "\n",
                  "     All IDs: ", object@idList$mid, "\n",
                  "  Start time: ", as.character(as.POSIXct(floor(object@start[1]),
                                                      origin = "1970-01-01")), "\n",
                  "    End time: ", as.character(as.POSIXct(floor(object@end[object@size]),
                                                      origin = "1970-01-01")), "\n")
          })
#' Title
#'
#' @param Activity
#'
#' @return
#'
#'
#' @examples
setMethod("show", "Activity",
          function(object) {
            cat(is(object)[[1]], "\n",
                  " No. of phases: ", length(unique(object@phase)), "\n",
                  "   Bin size(s): ", unique(object@binSize), "\n",
                  "       All IDs: ", unique(object@mid), "\n")
          })

#' Show content of an object of "EcoHABData" class
#'
#' @param EcoHABData
#'
#' @return
#'
#'
#' @examples
setMethod("show", "EcoHABData",
          function(object) {
            show(object@raw)
            show(object@events)
          })

#' Title
#'
#' @param RawData
#'
#' @return
#'
#'
#' @examples
setMethod("[", "RawData",
          function(x, i, j, ..., drop = FALSE) {
            x@time <- x@time[i]
            x@rfid <- x@rfid[i]
            x@antenna <- x@antenna[i]
            x@delay <- x@delay[i]
            x@idList <- subset(x@idList, rfid %in% unique(x@rfid))
            uniAnt <- sort(unique(x@antenna))
            x@rawAntCount <- x@rawAntCount[match(uniAnt, x@uniAnt)]
            x@uniAnt <- uniAnt
            x@antCount <- as.integer(table(x@antenna))
            x@size <- length(x@time)
            return(x)
          })
#' Title
#'
#' @param Events
#'
#' @return
#'
#'
#' @examples
setMethod("[", "Events",
          function(x, i, j, ..., drop = FALSE) {
            x@start <- x@start[i]
            x@end <- x@end[i]
            x@rfid <- x@rfid[i]
            x@mid <- x@mid[i]
            x@from <- x@from[i]
            x@to <- x@to[i]
            x@length <- x@length[i]
            x@loc <- x@loc[i]
            x@size <- length(x@start)
            x@idList <- subset(x@idList, rfid %in% unique(x@rfid))
            return(x)
          })
#' Title
#'
#' @param Activity
#'
#' @return
#'
#'
#' @examples
setMethod("[", "Activity",
          function(x, i, j, ..., drop = FALSE) {
            x@phase <- x@phase[i]
            x@binSize <- x@binSize[i]
            x@rfid <- x@rfid[i]
            x@mid <- x@mid[i]
            x@visits <- x@visits[i, , drop = drop]
            x@time <- x@time[i, , drop = drop]
            x@ratio <- x@ratio[i]
            return(x)
          })

#' Set bin size of an object of "Timeline" class
#'
#' @param Timeline An object of class "Timeline".
#' @param binSize A number indicating size of interested time bins in second.
#'
#' @return The object with new bin size setting.
#'
#'
#' @examples
setMethod("setBinSize", "Timeline",
          function(obj, binSize = 3600) {
            if(!is.null(obj) & length(obj@phase) > 0) {
              phase <- list()
              start <- list()
              end <- list()
              bin <- list()
              for(i in 1:length(obj@phase)) {
                j <- ceiling((obj@end[i] - obj@start[i]) / binSize)
                phase[[i]] <- paste(obj@phase[i], "div",
                                    sprintf(paste("%0", nchar(as.character(j)),
                                                  "d", sep = ""), 1:j), sep = "_")
                start[[i]] <- obj@start[i] + ((1:j) - 1) * binSize
                end[[i]] <- obj@start[i] + (1:j) * binSize
                end[[i]][j] <- obj@end[i]
                bin[[i]] <- end[[i]] - start[[i]]
              }
              obj@binPhase <- unlist(phase, use.names = FALSE)
              obj@binStart <- unlist(start, use.names = FALSE)
              obj@binEnd <- unlist(end, use.names = FALSE)
              obj@binSize <- unlist(bin, use.names = FALSE)
            }
            return(obj)
          })

#' Calculate events from given raw data, experimental layout and event threshold
#'
#' @param RawData Raw data to be calculated
#' @param config A data frame describing structure of EcoHAB
#' @param threshold Time threshold to detect same-antenna readings (in seconds)
#' @importFrom stringi stri_reverse
#'
#' @return An object of class "Events".
#'
#' @examples
setMethod("calcEvents", "RawData",
          function(obj, config = NULL, threshold = 2) {
            events <- new("Events")
            if (is.null(config)) {
              config <- defaultConfig
              # locmap <- defaultLocMap
            }
            locmap <- defaultLocMap # TODO: locmap for non-default configuration
            events@threshold <- threshold
            events@size <- 0L
            if(!is.null(obj)) {
              events@idList <- obj@idList
              start_ls <- list()
              end_ls <- list()
              rfid_ls <- list()
              mid_ls <- list()
              from_ls <- list()
              to_ls <- list()
              loc_ls <- list()
              for(i in 1:nrow(obj@idList)) {
                index <- which(obj@rfid == obj@idList$rfid[i])
                # no need for further processing if no data
                n <- length(index)
                if(n == 0)
                  next
                # transform consequential records into events
                # initiate event list for individual mouse
                start_i <- numeric(0)
                if (obj@simplify) {
                  start_i <- obj@time[c(1, index)]+obj@delay[c(1, index)] / 1000
                  start_i[1] <- start_i[1] - obj@delay[1] / 1000
                }
                else
                  start_i <- obj@time[c(1, index)]
                start_i[1] <- start_i[1] - threshold
                end_i <- obj@time[c(index, obj@size)]
                end_i[n+1] <- end_i[n+1] + threshold
                from_i <- obj@antenna[c(index[1], index)]
                to_i <- obj@antenna[c(index, index[n])]
                length_i <- end_i - start_i
                loc_i <- mapply(function(from, to, length) {
                                  # calculate location for each event
                                  # 1. If the event was at the same antenna, and the length was greater than the
                                  # threshold (2s by default), the mouse is assigned to the nearest cage
                                  # 2. If the event was at different antennas, and was in the same tube,
                                  # the mouse is assigned to that tube
                                  # 3. If the event was in two adjacent tubes connected to the same cage,
                                  # the mouse is assigned to that cage
                                  # 4. If the event was at different antennas but not in case 3 or 4,
                                  # an "err" tag will be assigned (at least two records were missed)
                                  # Note: in case that records were missed in the same tube, the mouse would
                                  # appear to be in that tube for unexpected long period and may be omitted manually
                                  if (from == to) {
                                    if (length >= threshold)
                                      return(config$cage[from])
                                    else
                                      return(as.character(from))
                                  } else {
                                    return(locmap[from, to])
                                    # if (config$cage[from] == config$cage[to])
                                    #   return(config$cage[from])
                                    # tube_from <- config$tube[from]
                                    # tube_to <- config$tube[to]
                                    # if (tube_from == stri_reverse(tube_to))
                                    #   return(tube_from)
                                    # else {
                                    #   cage_from <- config$cage[which(config$tube %in%
                                    #                                    c(tube_from,
                                    #                                      stri_reverse(tube_from)))]
                                    #   cage_to <- config$cage[which(config$tube %in%
                                    #                                  c(tube_to,
                                    #                                    stri_reverse(tube_to)))]
                                    #   cage_co <- intersect(cage_from, cage_to)
                                    #   if (length(cage_co) == 1)
                                    #     return(cage_co)
                                    #   else
                                    #     return("err")
                                    # }
                                  }
                                },
                                from_i, to_i, length_i)
                if (obj@simplify) {
                  # Include antenna events
                  end_i <- append(end_i, start_i[-1])
                  start_i <- append(start_i, obj@time[index])
                  ord_i <- order(start_i)
                  start_i <- start_i[ord_i]
                  end_i <- end_i[ord_i]
                  from_i <- append(from_i, obj@antenna[index])[ord_i]
                  to_i <- append(to_i, obj@antenna[index])[ord_i]
                  loc_i <- append(loc_i, as.character(obj@antenna[index]))[ord_i]
                }
                # Remove duplicate locations
                j <- length(loc_i)
                while (j > 1) {
                  if (loc_i[j] == loc_i[j-1] & from_i[j] == from_i[j-1] &
                      to_i[j] == to_i[j-1]) {
                    end_i[j-1] <- end_i[j]
                    to_i[j-1] <- to_i[j]
                    loc_i[j] <- NA
                  }
                  j <- j - 1
                }
                # Remove invalid locations
                valid <- which(!is.na(loc_i))
                nv <- length(valid)
                start_ls[[i]] <- start_i[valid]
                end_ls[[i]] <- end_i[valid]
                rfid_ls[[i]] <- rep(obj@idList$rfid[i], nv)
                mid_ls[[i]] <- rep(obj@idList$mid[i], nv)
                from_ls[[i]] <- from_i[valid]
                to_ls[[i]] <- to_i[valid]
                loc_ls[[i]] <- loc_i[valid]
                events@size <- events@size + nv
              }
              events@start <- unlist(start_ls, use.names = FALSE)
              ord <- order(events@start)
              events@start <- events@start[ord]
              events@end <- unlist(end_ls, use.names = FALSE)[ord]
              events@rfid <- unlist(rfid_ls, use.names = FALSE)[ord]
              events@mid <- unlist(mid_ls, use.names = FALSE)[ord]
              events@from <- unlist(from_ls, use.names = FALSE)[ord]
              events@to <- unlist(to_ls, use.names = FALSE)[ord]
              events@length <- events@end - events@start
              events@loc <- unlist(loc_ls, use.names = FALSE)[ord]
            }
            return(events)
          })

#' Title
#'
#' @param EcoHABData 
#'
#' @return
#' @export
#'
#' @examples
setMethod("calcEvents", "EcoHABData",
          function(obj, rec.threshold = 2, follow.threshold = 2,
                   mode = c("in_place", "delay")) {
            obj@events <- calcEvents(obj@raw, obj@config, rec.threshold)
            obj@incohort <- calcIncohortEvents(obj@events, unique(obj@config$cage))
            obj@solo <- calcSingleEvents(obj@events, unique(obj@config$cage))
            obj@inter <- calcLeadEvents(adjustTubeEvents(obj@events),
                                        unique(obj@config$tube),
                                        mode, follow.threshold)
            return(obj)
          })


#' Adjust tube passing events by adding waiting time at exit
#'
#' @param Events The object to be calculated 
#'
#' @return An object of "Events" contain only adjusted tube events
#'
#' @examples
setMethod("adjustTubeEvents", "Events",
          function(obj, config = defaultConfig) {
            # extract events of each mouse
            index_ls <- list()
            nid <- nrow(obj@idList)
            for (i in 1:nid)
              index_ls[[i]] <- which(obj@rfid == obj@idList$rfid[i])
            length_ls <- vapply(index_ls, length, integer(1))
            index_tube <- which(obj@loc %in% config$tube)
            obj@end[index_tube] <- vapply(index_tube,
                                          function(x) {
                                            j <- match(obj@rfid[x], obj@idList$rfid)
                                            k <- which(index_ls[[j]] == x) + 1
                                            if (k <= length_ls[j])
                                              if (obj@loc[index_ls[[j]][k]] == obj@to[x])
                                                obj@end[x] <- obj@end[index_ls[[j]][k]]
                                            return(obj@end[x])
                                            }, numeric(1))
            obj@length[index_tube] <- obj@end[index_tube] - obj@start[index_tube]
            return(obj[index_tube])
          })

#' Summarize the visits and duration to each cage in given time bins
#'
#' @param Events The object to be calculated
#' @param timeline A "Timeline" object describing starts and ends of each phase
#' @param binSize Size of calculation bins
#' @param verbose A logic value indicating whether to show extra information
#'
#' @return An object of class "Activity".
#'
#'
#' @examples
setMethod("calcActivity", "Events",
          function(obj, timeline = NULL, binSize = 3600, verbose = TRUE) {
            activity <- new("Activity")
            if(is.null(obj) | is.null(timeline))
              return(activity)
            timeline <- setBinSize(timeline, binSize)
            activity@uloc <- sort(unique(obj@loc))
            n <- length(activity@uloc)
            m <- nrow(obj@idList) * length(timeline@binPhase)
            index <- 1:obj@size
            activity@phase <- character(m)
            activity@binSize <- numeric(m)
            activity@rfid <- character(m)
            activity@mid <- character(m)
            activity@visits <- matrix(nrow = m, ncol = n, byrow = TRUE)
            activity@time <- matrix(nrow = m, ncol = n, byrow = TRUE)
            ka <- 1L # counter for row of activity matrix
            for (i in 1:nrow(obj@idList)) {
              index_i <- index[obj@rfid[index] == obj@idList$rfid[i]]
              for (j in 1:length(timeline@binPhase)) {
                activity@phase[ka] <- timeline@binPhase[j]
                activity@binSize[ka] <- timeline@binSize[j]
                activity@rfid[ka] <- obj@idList$rfid[i]
                activity@mid[ka] <- obj@idList$mid[i]
                # calculate visits
                # events spanning two or more phases will be counted once to the earliest
                index_jv <- index_i[obj@start[index_i] >= timeline@binStart[j]]
                index_jv <- index_jv[obj@start[index_jv] < timeline@binEnd[j]]
                activity@visits[ka, ] <- vapply(activity@uloc,
                                                function(x)
                                                  sum(obj@loc[index_jv] == x),
                                                integer(1))
                # calculate time in any locations
                # events spanning tow or more phases will be counted to all
                index_jt <- index_i[obj@start[index_i] < timeline@binEnd[j]]
                index_jt <- index_jt[obj@end[index_jt] > timeline@binStart[j]]
                duration <- obj@length[index_jt]
                if (length(duration) > 0) {
                  if (obj@start[index_jt][1] < timeline@binStart[j]) {
                    duration[1] <- duration[1] -
                      (timeline@binStart[j] - obj@start[index_jt][1])
                  }
                  if (obj@end[index_jt][length(duration)] > timeline@binEnd[j]) {
                    duration[length(duration)] <- duration[length(duration)] -
                      (obj@end[index_jt][length(duration)] - timeline@binEnd[j])
                  }
                }
                activity@time[ka, ] <- vapply(activity@uloc,
                                              function(x)
                                                sum(duration[obj@loc[index_jt] == x]),
                                              numeric(1))
                ka <- ka + 1L
              }
            }
            activity@ratio <- rowSums(activity@time) / activity@binSize
            return(activity)
          })

#' Remove some records by a given filter of loc, mid, or length
#'
#' @param Events
#'
#' @return An object of class "Events".
#'
#'
#' @examples
setMethod("filter", "Events",
          function(obj, keep.loc, keep.mid,
                   min.length = 0, max.length, verbose = TRUE) {
            keep <- 1:obj@size
            # Step wise filter out events
            if (!missing(keep.loc))
              keep <- keep[obj@loc[keep] %in% keep.loc]
            else
              keep.loc <- unique(obj@loc)
            if (!missing(keep.mid)) {
              keep <- keep[obj@mid[keep] %in% keep.mid]
              obj@idList <- subset(obj@idList, mid %in% keep.mid)
            } else
              keep.mid <- unique(obj@mid)
            if (min.length > 0)
              keep <- keep[obj@length[keep] >= min.length]
            if (!missing(max.length))
              keep <- keep[obj@length[keep] <= max.length]
            else
              max.length <- max(obj@length)
            if (verbose) {
              cat("Filtering events for animals", sort(keep.mid), "at",
                  sort(keep.loc), "longer than", min.length, "and shorter than",
                  max.length, "...\n")
              cat(obj@size, "events in total.\n")
              cat(obj@size - length(keep), "events dropped.\n")
            }
            obj@start <- obj@start[keep]
            obj@end <- obj@end[keep]
            obj@rfid <- obj@rfid[keep]
            obj@mid <- obj@mid[keep]
            obj@from <- obj@from[keep]
            obj@to <- obj@to[keep]
            obj@length <- obj@length[keep]
            obj@loc <- obj@loc[keep]
            obj@size <- length(keep)
            if (verbose)
              cat(obj@size, "events remain.\n")
            return(obj)
          })

#' Calculate mouse activity in given time bins
#' @description Calculate activity from events given by the input object of class "EcoHABData" and return it.
#' @param EcoHABData The object to be calculated
#' @param binSize Size of time bins in seconds. Default is 3600
#' @param cage.threshold Threshold of time spent in any cage. Events shorter than this value will be excluded.
#' @param tube.threshold Threshold of time spent in any cage. Events longer than this value will be excluded.
#' @param verbose A logic value indicating whether to show extra information
#'
#' @return An object of class "EcoHABData".
#' @export
#'
#' @examples
setMethod("calcActivity", "EcoHABData",
          function(obj, binSize = 3600, cage.threshold = 2,
                   tube.threshold = 60, verbose = TRUE) {
            obj@cage.visit <- calcActivity(filter(obj@events,
                                                  unique(obj@config$cage),
                                                  min.length = cage.threshold,
                                                  verbose = verbose),
                                           obj@timeline, binSize, verbose)
            obj@tube.visit <- calcActivity(filter(obj@events,
                                                  unique(obj@config$tube),
                                                  max.length = tube.threshold,
                                                  verbose = verbose),
                                           obj@timeline, binSize, verbose)
            obj@sociability <- adjustSociability(calcActivity(filter(obj@incohort,
                                                                     min.length = cage.threshold,
                                                                     verbose = verbose),
                                                              obj@timeline,
                                                              binSize, verbose),
                                                 obj@cage.visit)
            obj@solitude <- calcActivity(obj@solo, obj@timeline, binSize, verbose)
            obj@leading <- adjustLeading(calcActivity(filter(obj@inter,
                                                            max.length = tube.threshold,
                                                            verbose = verbose),
                                                     obj@timeline, binSize, verbose),
                                         obj@tube.visit)
            return(obj)
          })

#' Title
#'
#' @param Events
#'
#' @return
#'
#'
#' @examples
setMethod("calcSingleEvents", "Events",
          function(obj, uloc) {
            if (missing(uloc))
              uloc <- sort(unique(obj@loc))
            singleEvents <- new("Events")
            singleEvents@idList <- obj@idList
            singleEvents@threshold <- 0 # TODO
            singleEvents@size <- 0L
            start_ls <- list()
            end_ls <- list()
            rfid_ls <- list()
            mid_ls <- list()
            loc_ls <- list()
            for (loc in uloc) {
              index <- which(obj@loc == loc)
              for (rfid in obj@idList$rfid) {
                self <- index[obj@rfid[index] == rfid]
                ns <- length(self)
                if (ns == 0)
                  next
                others <- setdiff(index, self)
                start_s <- obj@start[self]
                end_s <- obj@end[self]
                j <- 1L
                while (j <= ns) {
                  others_j <- others[obj@end[others] > start_s[j]]
                  others_j <- others_j[obj@start[others_j] < end_s[j]]
                  # select overlapped events with j
                  if (length(others_j) > 0) {
                    for (k in others_j) {
                      # event j may change dynamically, make full comparison with k
                      if (obj@start[k] >= end_s[j])
                        break # no need to compare the rest
                      if (obj@end[k] <= start_s[j])
                        next # no overlap, do nothing
                      if ((obj@start[k] <= start_s[j]) &
                          (obj@end[k] < end_s[j])) {
                        start_s[j] <- obj@end[k]
                        next
                      } # adjust start of event j
                      if ((obj@start[k] > start_s[j]) &
                          (obj@end[k] >= end_s[j])) {
                        end_s[j] <- obj@start[k]
                        break
                      } # adjust end of event j
                      if ((obj@start[k] <= start_s[j]) &
                          (obj@end[k] >= end_s[j])) {
                        start_s <- start_s[-j]
                        end_s <- end_s[-j]
                        j <- j - 1L
                        ns <- ns -1L
                        break
                      } # delete event j
                      start_s <- append(start_s, obj@end[k], j)
                      end_s <- append(end_s, obj@start[k], j-1)
                      j <- j + 1L
                      ns <- ns + 1L
                      # split into two events and continue with the second
                    }
                  }
                  j <- j + 1L
                }
                i <- length(start_ls) + 1L
                start_ls[[i]] <- start_s
                end_ls[[i]] <- end_s
                rfid_ls[[i]] <- rep(rfid, ns)
                mid_ls[[i]] <- rep(obj@idList$mid[match(rfid, obj@idList$rfid)], ns)
                loc_ls[[i]] <- rep(loc, ns)
                singleEvents@size <- singleEvents@size + ns
              }
            }
            singleEvents@start <- unlist(start_ls)
            ord <- order(singleEvents@start)
            singleEvents@start <- singleEvents@start[ord]
            singleEvents@end <- unlist(end_ls)[ord]
            singleEvents@length <- singleEvents@end - singleEvents@start
            singleEvents@rfid <- unlist(rfid_ls)[ord]
            singleEvents@mid <- unlist(mid_ls)[ord]
            singleEvents@loc <- unlist(loc_ls)[ord]
            return(singleEvents)
          })

#' Title
#'
#' @param Events
#'
#' @return
#'
#'
#' @examples
setMethod("calcIncohortEvents", "Events",
          function(obj, uloc) {
            if (missing(uloc))
              uloc <- sort(unique(obj@loc))
            pairedEvents <- new("Events")
            pairedEvents@threshold <- 0 # TODO
            pairedEvents@size <- 0L
            # calculate combination of mouse pairs
            idPair <- list(rfid = combn(obj@idList$rfid, 2),
                           mid = combn(obj@idList$mid, 2))
            pairedEvents@idList <- data.frame(rfid = paste(idPair$rfid[1, ],
                                                           idPair$rfid[2, ],
                                                           sep = "_"),
                                              mid = paste(idPair$mid[1, ],
                                                          idPair$mid[2, ],
                                                          sep = "_"),
                                              loc = NA)
            # extract events of each mouse
            index_ls <- list()
            nid <- nrow(obj@idList)
            for (i in 1:nid)
              index_ls[[i]] <- which(obj@rfid == obj@idList$rfid[i])
            indexPair <- combn(1:nid, 2)
            start_ls <- list()
            end_ls <- list()
            rfid_ls <- list()
            mid_ls <- list()
            loc_ls <- list()
            # calculate in-cohort events for each mouse pair
            for (i in 1:(nid * (nid - 1) / 2)) {
              id_1 <- indexPair[1, i]
              id_2 <- indexPair[2, i]
              start_i <- list()
              end_i <- list()
              rfid_i <- list()
              mid_i <- list()
              loc_i <- list()
              size_i <- 0L
              for (loc in uloc) {
                index_1 <- index_ls[[id_1]][obj@loc[index_ls[[id_1]]] == loc]
                index_2 <- index_ls[[id_2]][obj@loc[index_ls[[id_2]]] == loc]
                if (length(index_1) == 0 | length(index_2) == 0)
                  next
                for (j in index_1) {
                  index_co <- index_2[obj@start[index_2] < obj@end[j]]
                  index_co <- index_co[obj@end[index_co] > obj@start[j]]
                  start_co <- obj@start[index_co]
                  end_co <- obj@end[index_co]
                  nc <- length(index_co)
                  if (nc > 0) {
                    if(obj@start[j] > start_co[1]) {
                      start_co[1] <- obj@start[j]
                    }
                    if(obj@end[j] < end_co[nc]) {
                      end_co[nc] <- obj@end[j]
                    }
                  }
                  k <- length(start_i) + 1L
                  start_i[[k]] <- start_co
                  end_i[[k]] <- end_co
                  size_i <- size_i + nc
                  loc_i[[k]] <- rep(loc, nc)
                }
              }
              start_ls[[i]] <- start_i
              end_ls[[i]] <- end_i
              rfid_ls[[i]] <- rep(paste(idPair$rfid[1, i],
                                        idPair$rfid[2, i], sep = "_"), size_i)
              mid_ls[[i]] <- rep(paste(idPair$mid[1, i],
                                       idPair$mid[2, i], sep = "_"), size_i)
              loc_ls[[i]] <- loc_i
              pairedEvents@size <- pairedEvents@size + size_i
            }
            pairedEvents@start <- unlist(start_ls)
            ord <- order(pairedEvents@start)
            pairedEvents@start <- pairedEvents@start[ord]
            pairedEvents@end <- unlist(end_ls)[ord]
            pairedEvents@rfid <- unlist(rfid_ls)[ord]
            pairedEvents@mid <- unlist(mid_ls)[ord]
            pairedEvents@length <- pairedEvents@end - pairedEvents@start
            pairedEvents@loc <- unlist(loc_ls)[ord]
            return(pairedEvents)
          })

#' Title
#'
#' @param Events
#'
#' @return
#'
#'
#' @examples
setMethod("calcLeadEvents", "Events",
          function(obj, uloc, mode = c("in_place", "delay"), threshold = 2) {
            if (missing(uloc))
              uloc <- sort(unique(obj@loc))
            mode <- mode[1]
            leadEvents <- new("Events")
            if (mode == "delay")
              leadEvents@threshold <- threshold
            leadEvents@size <- 0L
            # calculate permutation of mouse pairs
            rfidPair <- subset(expand.grid(obj@idList$rfid,
                                           obj@idList$rfid), Var1 != Var2)
            midPair <- subset(expand.grid(obj@idList$mid,
                                          obj@idList$mid), Var1 != Var2)
            leadEvents@idList <- data.frame(rfid = paste(rfidPair[, 1],
                                                         rfidPair[, 2],
                                                         sep = "|"),
                                            mid = paste(midPair[, 1],
                                                        midPair[, 2],
                                                        sep = "|"),
                                            loc = NA)
            # extract events of each mouse
            index_ls <- list()
            nid <- nrow(obj@idList)
            for (i in 1:nid)
              index_ls[[i]] <- which(obj@rfid == obj@idList$rfid[i])
            indexPair <- subset(expand.grid(1:nid, 1:nid), Var1 != Var2)
            start_ls <- list()
            end_ls <- list()
            from_ls <- list()
            to_ls <- list()
            rfid_ls <- list()
            mid_ls <- list()
            loc_ls <- list()
            # calculate leading events for each mouse pair
            for (i in 1:(nid ** 2 - nid)) {
              lead <- indexPair[i, 1]
              follow <- indexPair[i, 2]
              start_i <- list()
              end_i <- list()
              from_i <- list()
              to_i <- list()
              rfid_i <- list()
              mid_i <- list()
              loc_i <- list()
              for (loc in uloc) {
                index_l <- index_ls[[lead]][obj@loc[index_ls[[lead]]] == loc]
                index_f <- index_ls[[follow]][obj@loc[index_ls[[follow]]] == loc]
                if (length(index_l) == 0 | length(index_f) == 0)
                  next
                for (j in index_l) {
                  if (mode == "in_place")
                    threshold <- obj@end[j] - obj@start[j]
                  delay <- obj@start[index_f] - obj@start[j]
                  index_fi <- index_f[delay > 0 & delay <= threshold]
                  # follower should enter the tube after the leader and within threshold
                  # in case of "in-place", before the leader exits the tube
                  # in case of "delay", before certain time (default 2s) has passed
                  if (length(index_fi) > 0) {
                    start_j <- numeric(0)
                    end_j <- numeric(0)
                    from_j <- integer(0)
                    to_j <- integer(0)
                    size_j <- 0L
                    for (k in index_fi) {
                      if (obj@from[k] == obj@from[j] &
                          obj@to[k] == obj@to[j] &
                          obj@end[k] >= obj@end[j]) {
                        start_j <- append(start_j, obj@start[j])
                        end_j <- append(end_j, obj@end[k])
                        from_j <- append(from_j, obj@from[j])
                        to_j <- append(to_j, obj@to[j])
                        size_j <- size_j + 1L
                      }
                    }
                    m <- length(start_i) + 1L
                    start_i[[m]] <- start_j
                    end_i[[m]] <- end_j
                    from_i[[m]] <- from_j
                    to_i[[m]] <- to_j
                    rfid_i[[m]] <- rep(paste(rfidPair[i, 1], rfidPair[i, 2],
                                             sep = "|"), size_j)
                    mid_i[[m]] <- rep(paste(midPair[i, 1], midPair[i, 2],
                                            sep = "|"), size_j)
                    loc_i[[m]] <- rep(obj@loc[j], size_j)
                    leadEvents@size <- leadEvents@size + size_j
                  }
                }
              }
              start_ls[[i]] <- start_i
              end_ls[[i]] <- end_i
              from_ls[[i]] <- from_i
              to_ls[[i]] <- to_i
              rfid_ls[[i]] <- rfid_i
              mid_ls[[i]] <- mid_i
              loc_ls[[i]] <- loc_i
            }
            leadEvents@start <- unlist(start_ls, use.names = FALSE)
            ord <- order(leadEvents@start)
            leadEvents@start <- leadEvents@start[ord]
            leadEvents@end <- unlist(end_ls, use.names = FALSE)[ord]
            leadEvents@from <- unlist(from_ls, use.names = FALSE)[ord]
            leadEvents@to <- unlist(to_ls, use.names = FALSE)[ord]
            leadEvents@rfid <- unlist(rfid_ls, use.names = FALSE)[ord]
            leadEvents@mid <- unlist(mid_ls, use.names = FALSE)[ord]
            leadEvents@length <- leadEvents@end - leadEvents@start
            leadEvents@loc <- unlist(loc_ls, use.names = FALSE)[ord]
            return(leadEvents)
          })

#' Title
#'
#' @param Activity
#'
#' @return
#'
#'
#' @examples
setMethod("calcCoincidence", "Activity",
          function(obj) {
            binSize <- unique(obj@binSize)
            rfidPair <- combn(unique(obj@rfid), 2)
            expected <- list()
            n <- length(obj@uloc)
            for (i in 1:ncol(rfidPair)) {
              index_1 <- which(obj@rfid == rfidPair[1, i])
              index_2 <- which(obj@rfid == rfidPair[2, i])
              expected_i <- list()
              for (j in 1:n) {
                expected_i[[j]] <- obj@time[index_1, j] *
                  obj@time[index_2, j] / binSize ** 2
              }
              expected[[i]] <- rowSums(matrix(unlist(expected_i), ncol = n))
            }
            return(unlist(expected))
          })

#' Title
#'
#' @param Activity
#'
#' @return
#'
#'
#' @examples
setMethod("adjustSociability", "Activity",
          function(paired, single) {
            paired@ratio <- paired@ratio - calcCoincidence(single)
            return(paired)
          })

#' Title
#'
#' @param Activity
#'
#' @return
#'
#'
#' @examples
setMethod("adjustLeading", "Activity",
          function(leading, single) {
            follower <- unlist(strsplit(leading@mid,
                                        split = "|",
                                        fixed = T))[(1:length(leading@phase))*2]
            index <- match(paste(leading@phase, follower),
                           paste(single@phase, single@mid))
            leading@ratio <- rowSums(leading@visits) / rowSums(single@visits)[index]
            return(leading)
          })

#' Title
#'
#' @param Events
#'
#' @return  A data frame of events by given index.
#'
#'
#' @examples
setMethod("getEvents", "Events",
          function(obj) {
            data.frame(start = as.POSIXct(floor(obj@start), origin = "1970-01-01"),
                       end = as.POSIXct(floor(obj@end), origin = "1970-01-01"),
                       rfid = obj@rfid, mid = obj@mid,
                       from = obj@from, to = obj@to,
                       length = round(obj@length, 3), loc = obj@loc)
          })

#' Title
#'
#' @param EcoHABData
#'
#' @return A data frame of events by given index.
#' @export
#'
#' @examples
setMethod("getEvents", "EcoHABData",
          function(obj, index) {
            if (missing(index))
              index <- 1:obj@events@size
            getEvents(obj@events[index])
          })

#' Title
#'
#' @param Activity
#'
#' @return A data frame of visits to each cage.
#'
#'
#' @examples
setMethod("getVisits", "Activity",
          function(obj) {
            df <- data.frame(phase = obj@phase, binSize = obj@binSize,
                             rfid = obj@rfid, mid = obj@mid)
            df_visits <- data.frame(obj@visits)
            colnames(df_visits) <- paste("visits", obj@uloc, sep = "_")
            cbind(df, df_visits)
          })

#' Get the number of visits to each cage
#'
#' @param EcoHABData
#'
#' @return A data frame of visits to each cage.
#' @export
#'
#' @examples
setMethod("getTubeVisits", "EcoHABData",
          function(obj, index) {
            if (missing(index))
              index <- 1:length(obj@tube.visit@phase)
            getVisits(obj@tube.visit[index])
          })

#' Title
#'
#' @param Activity
#'
#' @return A data frame of time in each cage.
#'
#'
#' @examples
setMethod("getDurations", "Activity",
          function(obj) {
            df <- data.frame(phase = obj@phase, binSize = obj@binSize,
                             rfid = obj@rfid, mid = obj@mid)
            df_time <- data.frame(obj@time)
            colnames(df_time) <- paste("time", obj@uloc, sep = "_")
            cbind(df, df_time)
          })

#' Get the time in each cage
#'
#' @param EcoHABData
#'
#' @return A data frame of time in each cage.
#' @export
#'
#' @examples
setMethod("getCageDurations", "EcoHABData",
          function(obj, index) {
            if (missing(index))
              index <- 1:length(obj@cage.visit@phase)
            getDurations(obj@cage.visit[index])
          })

#' Title
#'
#' @param Activity
#'
#' @return A data frame of ratio of time in all cages.
#'
#'
#' @examples
setMethod("getRatios", "Activity",
          function(obj) {
            data.frame(phase = obj@phase, binSize = obj@binSize,
                       rfid = obj@rfid, mid = obj@mid, ratio = obj@ratio)
          })

#' Get the sociability index
#'
#' @param EcoHABData
#'
#' @return A data frame of ratio of time in all cages.
#' @export
#'
#' @examples
setMethod("getSociability", "EcoHABData",
          function(obj, index) {
            if (missing(index))
              index <- 1:length(obj@sociability@phase)
            getRatios(obj@sociability[index])
          })

#' Get the solitude index
#'
#' @param EcoHABData
#'
#' @return A data frame of ratio of time in all cages.
#' @export
#'
#' @examples
setMethod("getSolitude", "EcoHABData",
          function(obj, index) {
            if (missing(index))
              index <- 1:length(obj@solitude@phase)
            getRatios(obj@solitude[index])
          })

#' Get the number of leading-following events in each tube
#'
#' @param EcoHABData
#'
#' @return A data frame of leading-following in each tube.
#' @export
#'
#' @examples
setMethod("getLeadingCounts", "EcoHABData",
          function(obj, index) {
            if (missing(index))
              index <- 1:length(obj@leading@phase)
            getVisits(obj@leading[index])
          })

#' Get the leading index
#'
#' @param EcoHABData
#'
#' @return A data frame of leading index in all tubes.
#' @export
#'
#' @examples
setMethod("getLeadingIndex", "EcoHABData",
          function(obj, index) {
            if (missing(index))
              index <- 1:length(obj@leading@phase)
            getRatios(obj@leading[index])
          })
