-record(select, {
   lt = nil, 
   lte = nil, 
   offset_begin = 0, 
   gte = nil, 
   gt = nil, 
   offset_end = 0,
   limit = 0,
   target_bytes = 0,
   streaming_mode = iterator,
   iteration = 1,
   is_snapshot = false,
   is_reverse = false}).

-record(iterator, {
   tx,
   iteration,
   data = [],
   select,
   out_more = true}).
