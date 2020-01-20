#' Find day break
#'
#' Find the day break given time in POSIXct format
#' @param posixcttime time
#' @param dbtime String denoting time of daybreak in 24-hour format. Default is '07:00:00' (7AM)
#' @param timezone String denoting timezone. Default is system timezone.
#' @param prev If TRUE, find previous daybreak. If FALSE, find next daybreak.
#' @return Time of previous or next daybreaks
#' @import chron
#' @export
#' @examples
#' # Find next daybreak
#' next_day_break(Sys.time())
#  # Find previous daybreak
#' prev_day_break(Sys.time())

day_break = function(posixcttime,
                     dbtime='07:00:00',
                     timezone=extract_time_zone(Sys.time()),
                     prev=F) {
      retvec = unlist(lapply(
                   posixcttime,
                   elementary_day_break,
                   dbtime=dbtime,timezone=timezone,prev=prev))
      class(retvec) = class(posixcttime)
      retvec      
}

next_day_break = function(posixcttime,
                          dbtime='07:00:00',
                          timezone=extract_time_zone(Sys.time()),
                          prev=F) {
   day_break(posixcttime,dbtime=dbtime,timezone=timezone,prev=F)
}

prev_day_break = function(posixcttime,
                          dbtime='07:00:00',
                          timezone=extract_time_zone(Sys.time()),
                          prev=T) {
   day_break(posixcttime,dbtime=dbtime,timezone=timezone,prev=T)
}

extract_time_zone = function(x) {
   if (!any( class(x) %in% c("POSIXct", "POSIXt") )) { 
      stop('Object must be either POSIXct, or POSIXt') }
   format(x, format="%Z")
}

elementary_day_break = function(posixcttime,
                                dbtime='07:00:00',
                                timezone=extract_time_zone(Sys.time()),
                                prev=F) {
   splstr=strsplit(as.character(posixcttime),split=' ')[[1]]
   date=splstr[1]
   time=splstr[2]
   dbtime=dbtime
   if (prev) {
     if (times(time) < times(dbtime)) { 
        date=dates(paste(strsplit(date,split='-')[[1]][c(2,3,1)],collapse='/'))-1
        # need to put 20 and rearrange to deal with the peculiarities of chron package
        date=paste0('20',paste0(strsplit(as.character(date),split='/')[[1]][c(3,1,2)],collapse='-'))
     }
   } else {
     if (times(time) > times(dbtime)) { 
        date=dates(paste(strsplit(date,split='-')[[1]][c(2,3,1)],collapse='/'))+1
        # need to put 20 and rearrange to deal with the peculiarities of chron package
        date=paste0('20',paste0(strsplit(as.character(date),split='/')[[1]][c(3,1,2)],collapse='-'))
     }
   }
   origin = '1970-01-01 00:00.00 UTC'   #DEFAULT ORIGIN OF POSIXct OBJECTS
   datetime=as.POSIXct(paste(date,dbtime,timezone),origin=origin)

   if (extract_time_zone(datetime) == 'EDT' & timezone == 'EST') {
      datetime=as.POSIXct(paste(date,dbtime,"EDT"),origin=origin)
   }
   return(datetime)
}
