#' Read an HCA HDF5 file and return a list of dataframes, one for each subjects
#' @param i input HDF5 file
#' @param subjectIDs List of subjects IDs. If NULL, the subjects are automatically determined
#' @param start.time The start time of HCA HDF5 file. If NULL, it is automatically determined.
#' @param downsample If TRUE, Aggregate data by second (Default). If FALSE, don't aggregate. 
#' @param h5ls_path Path to h5ls utility. If NULL (Default), path is assumed to be in current environment,
#' @param h5dump_path Path to h5dump utility. If NULL (Default), path is assumed to be in current environment.
#' @return List of dataframes, one for each subjects.
#' @export
#' @examples
#' \dontrun{
#'   hcaReadHDF5('1547743375205_000095_arbC8123198@LT-GREEN_0000085500200.hdf5')
#' }

hcaSystemReadHDF5 = function(
          i, 
          subjectIDs = NULL, start.time = NULL, downsample = TRUE,
          h5ls_path = NULL, h5dump_path = NULL) {

   if (is.null(h5ls_path))   { h5ls_path   = 'h5ls'    }
   if (is.null(h5dump_path)) { h5dump_path = 'h5dump'  }
   hcaSystemReadHDF5_defense_func(i , h5ls_path, h5dump_path)

   subjects_in_this_file = hcaSystemReadHDF5_subj_defense_func(
      i , h5ls_path
   )
   if (is.null(subjectIDs)) { subjectIDs = subjects_in_this_file }
      
   if (is.null(start.time)) {
      start.time=find_start_time(i)
   }
      
   filedatalistsubject=list()
   for (j in subjectIDs) {
      if (!j %in% subjects_in_this_file) {next}
      
      dataset_path = paste0('/subjects/',j,'/antenna/ant1')
      sys_cmd = paste(
         h5dump_path,
         '-d',dataset_path,
         '--onlyattr=0',
         i
      )
      
      # get data as text, remove white spaces and collapse new lines
      h5_txt = system(sys_cmd, intern=T)
      h5_txt = paste(trimws(h5_txt),collapse='')
      
      # parse text in curly brackets
      h5_parsed = regmatches(h5_txt, gregexpr("(?<=\\{)[^{}]+(?=\\})", h5_txt, perl=TRUE))[[1]]
      
      # get header (first element)
      header_str = strsplit(h5_parsed[[1]],';')[[1]]
      
      # get names of column from header
      header_val = gregexpr('(?<=\\")[^{}]+(?=\\")', header_str, perl=TRUE)
      header_val = regmatches(header_str, header_val)
      header_val = unlist(header_val)
   
      # get types (integer or numeric) for each column
      header_types = trimws(unlist(lapply(strsplit(header_str,split = '"'),head,n=1)))
      
      # find whether columns are integers or numeric
      column_conversion_func = rep(NA,length(header_types))
      numeric_types = c('H5T_IEEE_F32LE','H5T_IEEE_F64LE')
      integer_types = c('H5T_STD_I32LE','H5T_STD_B8LE')
      column_conversion_func[header_types %in% numeric_types] = 'as.numeric'
      column_conversion_func[header_types %in% integer_types] = 'as.integer'
   
      # find number of rows
      num_rows = as.integer(gsub(' \\( | \\) \\/ \\( H5S_UNLIMITED \\) ','',h5_parsed[[2]]))
      
      # extract raw data (everything after the second column)
      h5_rawdat = do.call('rbind',strsplit(h5_parsed[3:(num_rows+2)],','))
      col_idx = seq_len(ncol(h5_rawdat))
      parse_column_raw = lapply(col_idx, function(i) { 
           v = do.call(column_conversion_func[i],list(h5_rawdat[,i])) 
           v = data.frame(v = v)
           colnames(v) = header_val[i]
           v
       })
      
      # coerce raw data to matrix
      subjdat = do.call('cbind',parse_column_raw)
   
      subjdat$t = subjdat$t/1000               # convert to seconds
      
      if (downsample) {
         subjdat = downsample_function(subjdat)
      }

     subjdat$t = subjdat$t + start.time       # add starttime and 
   
     filedatalistsubject[j] = list(subjdat)
   }
   
   return(filedatalistsubject)
}

#' downsample HCA timeseries
#'
#' go through timeseries for a subject and downsample to each row per second
#' @param subjdat subject data timeseries
#' @return Return downsampled timeseries
downsample_function = function(subjdat) {
         tds = floor(subjdat$t)      # unionize by seconds
         
         aggfunc = function(v,func) {
            aggregate(subjdat[,v],list(tds),func)[,'x']
         }
         
         Mode = function(x) { 
            ux = unique(x)
            ux[which.max(tabulate(match(x, ux)))]
          }
         
         
         mode_cols = 'antenna'
         mean_cols = c(
            'bx','by','separation',
            't','temperature','Climbing',
            'separation_*'
         )
         sum_cols = c('distance','transition')

         func_df = rbind(
            do.call('rbind',lapply(mean_cols, function(u) {
               ux = grep(glob2rx(u),colnames(subjdat),value=T)
               data.frame(u = ux , func = 'mean')
            }))
         ,
            do.call('rbind',lapply(mode_cols, function(u) {
               ux = grep(glob2rx(u),colnames(subjdat),value=T)
               data.frame(u = ux , func = 'Mode')
            }))
         ,
            do.call('rbind',lapply(sum_cols, function(u) {
               ux = grep(glob2rx(u),colnames(subjdat),value=T)
               data.frame(u = ux , func = 'sum')
            }))
         )
         
         subjdatds = lapply(colnames(subjdat), function(u) {
            sub_func_df = func_df[ func_df$u == u , ]
            if (nrow(sub_func_df) > 1) {
               warning('column',u,'has multiple functions for aggregation')
               agg_func = sub_func_df[1,'func']
               warning('taking the first function',agg_func)
            } else if (nrow(sub_func_df) == 0) {
               warning('column',u,'has does not have a function for aggregation')
               warning('defaulting to mean')
               agg_func = 'mean'
            } else {
               agg_func = sub_func_df[1,'func']
            }
            txt_str = paste0('aggfunc(',
                        "'",u,"'",",",agg_func,")")
            data.frame(x = eval(parse(text=txt_str)))
         })
         subjdatds = do.call('cbind',subjdatds)
         colnames(subjdatds) = colnames(subjdat)
         return(subjdatds)
}

#' defence for hcaSystemReadHDF5 inputs
#'
#' chech arguments for hcaSystemReadHDF5 function and stop if problematic
#' @param i input HDF5 file
#' @param h5ls_path Path to h5ls utility.
#' @param h5dump_path Path to h5dump utility.
#' @return NULL
hcaSystemReadHDF5_defense_func = function(
    i , h5ls_path , h5dump_path
) {
   check_if_executable_exists = function(x) {
      suppressWarnings({ cmdres = system(paste('which',x),intern=T) })
      bool1 = length(cmdres) == 1
      bool2 = file.exists(x)
      return( bool1 | bool2 )
   }
   
   if (!check_if_executable_exists(h5dump_path)) {
      stop('h5dump executable does not exist in location',h5dump_path)
   }
   if (!check_if_executable_exists(h5ls_path)) {
      stop('h5ls executable does not exist in location',h5ls_path)
   }
   if (!file.exists(i)) {
      stop('input file does not exist in location',i)
   }

}

#' defence for hcaSystemReadHDF5 subjects
#'
#' chech input file for hcaSystemReadHDF5 function and stop if no subjects
#' @param i input HDF5 file
#' @param h5ls_path Path to h5ls utility.
#' @return NULL
hcaSystemReadHDF5_subj_defense_func = function(i , h5ls_path) {
   suppressWarnings({
      ls_cmd_res = system(paste0(h5ls_path,' ',i,'/subjects'),intern=T)
   })
      
   if (!is.null(attributes(ls_cmd_res))) { if (attr(ls_cmd_res,'status') == 1) {
      stop('h5ls could not find subjects in file')
   }}
   
   subjects_in_this_file = gsub(" .*","",ls_cmd_res)
   return(subjects_in_this_file)
}

